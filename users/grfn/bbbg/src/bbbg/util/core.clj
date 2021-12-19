(ns bbbg.util.core
  (:require
   [clojure.java.shell :refer [sh]]
   [clojure.string :as str])
  (:import
   java.util.UUID))

(defn remove-nils
  "Remove all keys with nil values from m"
  [m]
  (let [!m (transient m)]
    (doseq [[k v] m]
      (when (nil? v)
        (dissoc! !m k)))
    (persistent! !m)))


(defn alongside
  "Apply a pair of functions to the first and second element of a two element
  vector, respectively. The two argument form partially applies, such that:

  ((alongside f g) xy) ≡ (alongside f g xy)

  This is equivalent to (***) in haskell's Control.Arrow"
  ([f g] (partial alongside f g))
  ([f g [x y]] [(f x) (g y)]))

(defn map-kv
  "Map a pair of functions over the keys and values of a map, respectively.
  Preserves metadata on the incoming map.
  The two argument form returns a transducer that yields map-entries.

  (partial map-kv identity identity) ≡ identity"
  ([kf vf]
   (map (fn [[k v]]
          ;; important to return a map-entry here so that callers down the road
          ;; can use `key` or `val`
          (first {(kf k) (vf v)}))))
  ([kf vf m]
   (into (empty m) (map-kv kf vf) m)))

(defn filter-kv
  "Returns a map containing the elements of m for which (f k v) returns logical
  true. The one-argument form returns a transducer that yields map entries"
  ([f] (filter (partial apply f)))
  ([f m]
   (into (empty m) (filter-kv f) m)))

(defn map-keys
  "Map f over the keys of m. Preserves metadata on the incoming map. The
  one-argument form returns a transducer that yields map-entries."
  ([f] (map-kv f identity))
  ([f m] (map-kv f identity m)))

(defn map-vals
  "Map f over the values of m. Preserves metadata on the incoming map. The
  one-argument form returns a transducer that yields map-entries."
  ([f] (map-kv identity f))
  ([f m] (map-kv identity f m)))

(defn map-keys-recursive [f x]
  (cond
    (map? x) (map-kv f (partial map-keys-recursive f) x)
    (sequential? x) (map (partial map-keys-recursive f) x)
    :else x))

(defn denamespace [x]
  (if (keyword? x)
    (keyword (name x))
    (map-keys-recursive denamespace x)))

(defn reverse-merge
  "Like `clojure.core/merge`, except duplicate keys from maps earlier in the
  argument list take precedence

    => (merge {:x 1} {:x 2})
    {:x 2}

    => (sut/reverse-merge {:x 1} {:x 2})
    {:x 1}"
  [& ms]
  (apply merge (reverse ms)))

(defn invert-map
  "Invert the keys and vals of m. Behavior with duplicate vals is undefined.

  => (sut/invert-map {:x 1 :y 2})
  {1 :x 2 :y}"
  [m]
  (into {} (map (comp vec reverse)) m))

(defn ->uuid
  "Converts x to uuid, returning nil if x is nil or empty"
  [x]
  (cond
    (not x) nil
    (uuid? x) x
    (and (string? x) (seq x))
    (UUID/fromString x)))

(defn key-by
  "Create a map from a seq obtaining keys via f

    => (sut/key-by :x [{:x 1} {:x 2 :y 3}])
    {1 {:x 1}, 2 {:x 2 :y 3}}"
  [f l]
  (into {} (map (juxt f identity)) l))

(defn distinct-by
  "Like clojure.core/distinct, but can take a function f by which
  distinctiveness is calculated"
  [distinction-fn coll]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[f :as xs] seen]
                   (when-let [s (seq xs)]
                     (if (contains? seen (distinction-fn f))
                       (recur (rest s) seen)
                       (cons f (step (rest s) (conj seen (distinction-fn f)))))))
                 xs seen)))]
    (step coll #{})))

(defn pass [n]
  (let [{:keys [exit out err]} (sh "pass" n)]
    (if (= 0 exit)
      (str/trim out)
      (throw (Exception.
              (format "`pass` command failed\nStandard output:%s\nStandard Error:%s"
                      out
                      err))))))
