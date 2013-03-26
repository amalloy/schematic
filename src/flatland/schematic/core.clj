(ns flatland.schematic.core
  (:use [flatland.useful.fn :only [given to-fix]]
        [flatland.useful.string :only [classify]]
        [flatland.useful.utils :only [verify]]
        [flatland.useful.map :only [update update-each map-vals]])
  (:refer-clojure :exclude [struct get-in assoc-in select-keys])
  (:require [clojure.core :as core]))

(defn- boolean? [x] ;; really? no boolean? function in core?
  (or (true? x) (false? x)))

(defn struct
  ([fields]
     {:type :struct, :fields fields})
  ([fields & struct-options]
     (apply assoc (struct fields) struct-options)))

(defn get-in [schema fields]
  (core/get-in schema (interleave (repeat :fields) fields)))

(defn assoc-in [outer keys inner]
  (if-let [[k & ks] (seq keys)]
    (case (:type outer)
      (nil :struct) (-> outer
                        (assoc :type :struct)
                        (update-in [:fields k] assoc-in ks inner))
      (throw (IllegalArgumentException. (format "Can't add %s field to non-struct schema %s"
                                                k outer))))
    inner))

(defn select-keys [schema keys]
  (if (not= :struct (:type schema))
    (throw (IllegalArgumentException. (format "Can't select keys from %s schema" (:type schema))))
    (update schema :fields core/select-keys keys)))

(def ^{:dynamic true} *ignore-required-fields* false)

(defmulti error (fn [node schema] (:type schema)))

(defmethod error :struct [node {:keys [fields]}]
  (or (and node (not (map? node))
           (format "Expected struct, found %s" (class node)))

      (some identity
            (for [[k v] node]
              (let [field-schema (get fields (keyword k))]
                (or (and (not field-schema)
                         (format "Unexpected field: %s" (name k)))
                    (when-let [error (error v field-schema)]
                      (format "[%s: %s]" (name k) error))))))
      (and (not *ignore-required-fields*)
           (some identity
                 (for [[field-name schema] fields]
                   (and (:required schema)
                        (not-any? #(contains? node %)
                                  ((juxt keyword name) field-name))
                        (format "Missing required field: %s" (name field-name))))))))

(defmethod error :map [node schema]
  (or (and node (not (map? node))
           (format "Expected map, found %s" (class node)))
      (let [{key-schema :keys, val-schema :values} schema]
        (some identity
              (for [[k v] node]
                (if-let [error (error k key-schema)]
                  (format "[Invalid key %s: %s]" (name k) error)
                  (when-let [error (error v val-schema)]
                    (format "[Invalid value for key %s: %s]" (name k error)))))))))

(defmethod error :list [node schema]
  (or (and node (not (sequential? node))
           (format "Expected list, found %s" (class node)))
      (let [item-schema (:values schema)]
        (first (keep-indexed (fn [idx item]
                               (when-let [error (error item item-schema)]
                                 (format "[Item #%s: %s]" idx error)))
                             node)))))

(defmethod error :set [node schema]
  (or (and node (not (coll? node))
           (format "Expected set, found %s" (class node)))
      (let [item-schema (:values schema)]
        (some identity
              (if (map? node)
                (for [[k v] node]
                  (or (and (not (boolean? v))
                           (format "Value for existence-hash at key %s is non-boolean: %s"
                                   (pr-str k) (class v)))
                      (error k item-schema)))
                (for [item node]
                  (error item item-schema)))))))

(defmethod error :enum [node schema]
  (let [candidates (if (or (string? node) (keyword? node))
                     ((juxt name keyword) node)
                     [node])
        expected (:values schema)]
    (when-not (some #(contains? expected %) candidates)
      (format "Expected any of %s, got %s"
              (pr-str (list* (:values schema))) (pr-str node)))))

(let [known-types {:int integer?
                   :long integer?
                   :double number?
                   :float number?
                   :boolean boolean?
                   :string string?}]
  (defmethod error :default [node schema]
    (let [type (:type schema)
          validator (get known-types type)]
      (cond (nil? node) nil ; Okay to have fields missing.
            (nil? type) nil ; If there's no schema, we say anything matches. For types with more
                            ; specific needs (eg structs shouldn't have unexpected fields), make
                            ; sure there is a schema before checking that it matches.
            (nil? validator) (format "Unrecognized schema [%s] for value [%s]"
                                     (pr-str schema) (pr-str node))
            (not (validator node)) (format "Expected %s, got %s"
                                           (name type) (class node))))))

(defn matches? [node schema]
  (not (error node schema)))

(declare combine)

;; only call if x and y are the same type of schema
(defmulti ^:private combine* (fn [[x y]] (:type x)))

(defmethod combine* :default [[x y]]
  ;; We don't know anything about the schemas, but they're the same type. So we'll just
  ;; say the first schema is fine (eg, this is correct for :string or :int)
  x)

(defmethod combine* :map [maps]
  {:keys (apply combine (map :keys maps))
   :vals (apply combine (map :vals maps))})

(defmethod combine* :struct [structs]
  {:fields
   (apply merge-with (fn [a b]
                       (-> (combine a b)
                           (given (or (:required a) (:required b))
                                  (assoc :required true))))
          (map :fields structs))})

(defmethod combine* :list [lists]
  {:values (apply combine (map :values lists))})

(defmethod combine* :set [sets]
  {:values (apply combine (map :values sets))})

(defmethod combine* :enum [enums]
  {:values (set (mapcat :values enums))})

(declare ^:private mismatches*)

(defn- collapse-mismatches [schema keys]
  (for [k keys
        path (mismatches* (get schema k))]
    (cons k path)))

(defmulti ^:private mismatches* :type)
(defmethod mismatches* :default [_] nil)
(defmethod mismatches* :error [s]
  [[(core/select-keys s [:left :right])]])
(defmethod mismatches* :struct [s]
  (for [[k v] (:fields s)
        path (mismatches* v)]
    (cons k path)))
(defmethod mismatches* :list [s]
  (collapse-mismatches s [:values]))
(defmethod mismatches* :set [s]
  (collapse-mismatches s [:values]))
(defmethod mismatches* :map [s]
  (collapse-mismatches s [:keys :values]))

(defn mismatches
  "Searches through a schema combined with *throw-mismatches* false, locating all paths at which
  errors are present. Returns nil if no errors exist."
  [combined-schema]
  (seq (mismatches* combined-schema)))

(def ^:dynamic *throw-mismatches*
  "When set (the default), attempts to combine irreconcilable schemas (such as combining a boolean
  with a struct) will throw an exception. If this var is bound to false, then instead such attempts
  will result in a schema with type :error. Be very careful when changing this setting: it is
  intended primarily as a debugging aid, not as a production feature. In particular, this has the
  effect of returning a map that looks like a valid schema but contains :error nodes buried within
  it. A summary of the errors present can be obtained from the `mismatches` function, to help in
  locating the problematic part of the schema, but this will be much slower than simply allowing an
  exception to be thrown as soon as the attempt to combine is made."
  true)

(defn combine
  ([] {})
  ([schema]
     schema)
  ([x y]
     (cond (nil? x) y
           (nil? y) x
           :else (let [[xt yt] (map :type [x y])]

                   (cond (= xt yt) (assoc (combine* [x y])
                                     :type xt)
                         *throw-mismatches*
                         ,,(throw (IllegalArgumentException.
                                   (format "Cannot combine schemas of types %s and %s"
                                           xt yt)))
                         (= xt :error) x
                         :else {:type :error
                                :left xt, :right yt}))))
  ([x y & more]
     (reduce combine (list* x y more))))

(defn walk
  "Traverse all child types of the given schema, calling inner on each, then call outer on the result."
  [inner outer schema]
  (outer
   (case (:type schema)
     :struct           (update schema :fields map-vals inner)
     (:set :list :map) (-> schema
                           (given :values (update :values inner))
                           (given :keys   (update :keys   inner)))
     schema)))

(defn postwalk
  "Perform a depth-first, post-order traversal of all types within the given schema, replacing each
  and type with the result of calling f on it."
  [f schema]
  (walk (partial postwalk f) f schema))

(defn prewalk
  "Like postwalk, but do a pre-order traversal."
  [f schema]
  (walk (partial prewalk f) identity (f schema)))

(defn dissoc-fields
  "Traverse the given schema, removing the given fields at any level."
  [schema & fields]
  (prewalk (to-fix #(= :struct (:type %))
                   #(apply update % :fields dissoc fields))
           schema))
