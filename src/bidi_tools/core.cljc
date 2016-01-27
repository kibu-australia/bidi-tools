(ns bidi-tools.core
  (:require [clojure.string :as string]
            [cemerick.url :refer [url-encode url-decode]]
            [bidi.bidi :as bidi]
            [clojure.walk :as walk]
            [schema.core :as s]))

(def ^:private default-query-params-schema
  {s/Keyword s/Any})

(defrecord QueryParam [schema reader writer]
  schema.core/Schema
  (spec [_] (s/spec schema))
  (explain [_] (s/explain schema)))

(defn query-param [schema reader writer]
  (QueryParam. schema reader writer))

(defn- write-query-param [schema [k v]]
  (let [write-fn (or (get-in schema [k :writer])
                     (get-in schema [(s/optional-key k) :writer])
                     identity)]
    [k (write-fn v)]))

(defn write-query-params [params schema]
  (let [params (s/validate schema params)]
    (into {} (map (partial write-query-param schema)) params)))

(defn- read-query-param [schema [k v]]
  (let [read-fn (or (get-in schema [k :reader])
                    (get-in schema [(s/optional-key k) :reader])
                    #(if (= 1 (count %)) (first %) %))]
    [k (read-fn v)]))

(defn read-query-params [params schema]
  (let [params (into {} (map (partial read-query-param schema)) params)]
    (s/validate schema params)))

(defn query-string->params
  ([q] (query-string->params q default-query-params-schema))
  ([q schema]
   (loop [params (string/split q #"&") params-map {}]
     (if-let [param (first params)]
       (let [[k v] (string/split param #"=")]
         (recur (rest params) (update-in params-map [(keyword k)] conj v)))
       (read-query-params params-map schema)))))

(defn match-route-with-query [routes path & {:keys [query-params-schema]}]
  (let [query-params-schema (or query-params-schema default-query-params-schema)
        [path query]        (string/split path #"\?")
        query-params        (query-string->params query query-params-schema)]
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

(defn get-query-params [route path params]
  (let [{:keys [route-params]} (bidi/match-route route path)
        query-params' (apply dissoc params (keys route-params))]
    (not-empty (into (sorted-map) (filter (fn [[_ v]] (some? v))) query-params'))))

(defn path-with-query-for
  "Like path-for, but extra parameters will be appended to the url as query parameters
  rather than silently ignored"
  [route handler query-params-schema & {:as all-params}]
  (let [path         (apply bidi/path-for route handler (apply concat (vec all-params)))
        query-params (get-query-params route path all-params)]
    (apply str path (when query-params ["?" (form-encode (write-query-params query-params query-params-schema))]))))

(defn url-for
  ([routes handler params]
   (url-for routes handler params default-query-params-schema))
  ([routes handler params query-params-schema]
   (apply path-with-query-for routes handler query-params-schema (apply concat (vec params)))))

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
