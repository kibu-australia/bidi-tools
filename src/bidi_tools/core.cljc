(ns bidi-tools.core
  (:require [clojure.string :as string]
            [cemerick.url :refer [url-encode url-decode]]
            [bidi.bidi :as bidi]
            [clojure.walk :as walk]
            [schema.core :as s]
            [schema.coerce :as coerce]))

(def ^:private coercer #(coerce/coercer % coerce/json-coercion-matcher))

(defn- coerce-sequential [schema [k v]]
  (if-let [s (get schema k (get schema (s/optional-key k)))]
    (if (sequential? s)
      [k v]
      [k (first v)])
    (if (= 1 (count v))
      [k (first v)]
      [k v])))

(defn query-string->params
  ([q] (query-string->params q {s/Keyword s/Any}))
  ([q schema]
   (loop [params (string/split q #"&") params-map {}]
     (if-let [param (first params)]
       (let [[k v] (string/split param #"=")]
         (recur (rest params) (update-in params-map [k] conj v)))
       ((coercer schema)
        (into {} (map (partial coerce-sequential schema)) params-map))))))

(defn match-route-with-query [routes path]
  (let [[path query] (string/split path #"\?")
        query-params (query-string->params query)]
    (assoc (bidi/match-route routes path) :query-params query-params)))

;; cljc version of ring.util.codec FormEncodeable protocol
;; Difference is it accepts no custom encoding and uses UTF-8
;; This is required for `path-with-query-for` function
(defprotocol FormEncodeable
  (form-encode [x]))

(defn- encode-param [[k v]] (str k "=" (form-encode v)))

(defn- build-query-string [[k v]]
  (if (or (seq? v) (sequential? v))
    (map #(encode-param [(form-encode k) (url-decode (str %))]) v)
    [(encode-param [(form-encode k) (url-decode (str v))])]))

(defn- map->query-string [params] (string/join "&" (mapcat build-query-string params)))

(extend-protocol FormEncodeable
  #?(:cljs string :clj String)
  (form-encode [unencoded] (url-encode unencoded))

  #?(:cljs cljs.core.Keyword :clj clojure.lang.Keyword)
  (form-encode [unencoded] (form-encode (name unencoded)))

  #?(:cljs cljs.core.PersistentTreeMap :clj clojure.lang.PersistentTreeMap)
  (form-encode [params] (map->query-string params))

  #?(:cljs cljs.core.PersistentArrayMap :clj clojure.lang.PersistentArrayMap)
  (form-encode [params] (map->query-string params))

  #?(:cljs default :clj Object)
  (form-encode [x] (form-encode (str x))))

(defn path-with-query-for
  "Like path-for, but extra parameters will be appended to the url as query parameters
  rather than silently ignored"
  [route handler & {:as all-params}]
  (let [path (apply bidi/path-for route handler (apply concat (vec all-params)))
        {:keys [route-params]} (bidi/match-route route path)
        query-params' (apply dissoc all-params (keys route-params))
        query-params  (not-empty (into (sorted-map) (filter (fn [[_ v]] (some? v))) query-params'))]
    (apply str path (when query-params ["?" (form-encode query-params)]))))

(defn url-for [routes handler params]
  (apply path-with-query-for routes handler (apply concat (vec params))))

(defprotocol IBidiIdentity
  (bidi-identity [this]))

#?(:clj
   (extend-protocol IBidiIdentity
     clojure.core$keyword (bidi-identity [this] 'keyword)
     clojure.core$long    (bidi-identity [this] 'long)
     bidi.bidi$uuid       (bidi-identity [this] 'bidi/uuid)
     Object               (bidi-identity [this] this)))

#?(:cljs
   (extend-protocol IBidiIdentity
     function (bidi-identity [this]
                (condp = this
                  cljs.core/keyword 'keyword
                  cljs.core/long    'long
                  bidi.bidi/uuid    'uuid))
     default  (bidi-identity [this] this)))

(defn pr-routes [routes] (walk/postwalk bidi-identity routes))

(defn- get-handler [route]
  (when (map? route)
    (reduce-kv
     (fn [m k v]
       (if (map? v)
         (concat m (flatten (get-handler v)))
         (conj m v)))
     [] route)))

(defn get-handlers [routes]
  (into #{} (comp (mapcat get-handler) (filter identity)) routes))
