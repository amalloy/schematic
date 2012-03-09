(ns schematic.core
  (:refer-clojure :exclude [struct])
  (:use [useful.fn :only [given]]
        [useful.string :only [classify]]
        [useful.utils :only [verify]]))

(defn- boolean? [x] ;; really? no boolean? function in core?
  (or (true? x) (false? x)))

(defn struct
  ([fields]
     {:type :struct, :fields fields})
  ([fields & struct-options]
     (apply assoc (struct fields) struct-options)))

(defmulti matches? (fn [node schema] (:type schema)))

(defmethod matches? nil [node schema]
  ;; if there's no schema, we say anything matches. for types with more specific needs
  ;; (eg structs shouldn't have unexpected fields), make sure there is a schema before
  ;; checking that it matches
  true)

(defmethod matches? :struct [node {:keys [fields]}]
  (and (or (nil? node)
           (map? node))
       (every? true? (for [[k v] node]
                       (let [field-schema (get fields k)]
                         (and field-schema
                              (matches? v field-schema)))))
       (every? true? (for [[field-name schema] fields
                           :when (:required schema)]
                       (contains? node field-name)))))

(defmethod matches? :map [node schema]
  (or (nil? node)
      (and (map? node)
           (let [{key-schema :keys, val-schema :values} schema]
             (every? true? (for [[k v] node]
                             (and (matches? k key-schema)
                                  (matches? v val-schema))))))))

(defmethod matches? :list [node schema]
  (or (nil? node)
      (and (coll? node)
           (let [item-schema (:values schema)]
             (every? true? (for [item node]
                             (matches? item item-schema)))))))

(defmethod matches? :set [node schema]
  (or (nil? node)
      (and (coll? node)
           (let [item-schema (:values schema)]
             (every? true? (if (map? node)
                             (for [[k v] node] ;; treat existence-hash as a set
                               (and (boolean? v)
                                    (matches? k item-schema)))
                             (for [item node]
                               (matches? item item-schema))))))))

(defmethod matches? :enum [node schema]
  (contains? (:values schema) node))

(let [known-types {:int integer?
                   :long integer?
                   :double number?
                   :float number?
                   :boolean boolean?
                   :string string?}]
  (defmethod matches? :default [node schema]
    (let [type (:type schema)]
      (when-let [validator (get known-types type)]
        (validator node)))))

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

(defn combine
  ([schema]
     schema)
  ([x y]
     (cond (nil? x) y
           (nil? y) x
           :else (let [[xt yt] (map :type [x y])]
                   (verify (= xt yt) (format "Cannot combine schemas of types %s and %s" xt yt))
                   (assoc (combine* [x y])
                     :type xt))))
  ([x y & more]
     (reduce combine (list* x y more))))
