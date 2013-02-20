(ns schematic.core-test
  (:use clojure.test)
  (:require [schematic.core :as schema]))

(deftest basics
  (are [type right wrong] (let [schema {:type type}]
                            (and (schema/matches? right schema)
                                 (not (schema/matches? wrong schema))
                                 (not (schema/error right schema))
                                 (schema/error wrong schema)))
       :boolean true 3
       :int 3 3.5
       :double 3.4 "number"
       :string "string" 10))

(deftest lists
  (let [schema {:type :list
                :values {:type :int}}
        good [2 3 5 7 11 13]
        bad [2 3 5 :horse 11 13]]
    (is (schema/matches? good schema))
    (is (not (schema/error good schema)))
    (is (not (schema/matches? bad schema)))
    (let [error (schema/error bad schema)]
      (is error)
      (are [fragment] (.contains error fragment)
           "#3" ;; should mention the index of invalid item
           "Keyword" ;; and the class
           "int")))) ;; and the expected schema

(deftest structs
  (let [schema {:type :struct
                :fields {:a {:type :int}
                         :b {:type :struct
                             :fields {:x {:type :float}
                                      :y {:type :float}}}}}]
    (testing "struct fields are optional"
      (is (schema/matches? {:a 3} schema)))
    (testing "extra fields not allowed"
      (is (not (schema/matches? {:a 1 :c 4} schema))))
    (is (schema/matches? {:a 2 :b {:x 4 :y 2.3}} schema))
    (is (schema/matches? {:b nil} schema)))

  (let [schema {:type :struct
                :fields {:a {:type :string}
                         :b {:type :struct
                             :required true
                             :fields {:c {:type :int}}}}}]
    (is (schema/matches? {:b {:c 4}} schema))
    (is (schema/matches? {:a "test" :b nil} schema))
    (let [accept-missing #(schema/matches? {:a "foo"} schema)]
      (is (not (accept-missing)))
      (is (binding [schema/*ignore-required-fields* true]
            (accept-missing))))))

(deftest enums
  (let [schema {:type :enum
                :values #{:this :that}}]
    (are [x] (schema/matches? x schema)
         :this :that "this" "that")
    (are [x] (not (schema/matches? x schema))
         "bah" nil 5)))

(deftest sets
  (let [schema {:type :set
                :values {:type :enum
                         :values #{:tall "short"}}}]
    (is (schema/matches? [:tall "short" :tall] schema)) ;; allow multiples, for "overwriting"
    (is (schema/matches? #{} schema))
    (is (not (schema/matches? [:tall 4] schema)))

    (testing "existence hashes"
      (is (schema/matches? {"short" true} schema))
      (is (not (schema/matches? {:tall 5} schema))))))


(deftest combining
  (let [a {:type :struct
           :fields {:left {:type :string}
                    :value {:type :int :required true}
                    :right {:type :string}}}
        b {:type :struct
           :fields {:name {:type :string :required true}
                    :value {:type :int}}}]
    (is (= {:type :struct
            :fields {:left {:type :string}
                     :right {:type :string}
                     :name {:type :string :required true}
                     :value {:type :int :required true}}}
           (schema/combine a b)))))

(deftest shortcuts
  (is (= {:type :struct
          :fields {:a {:type :int}}
          :required true}
         (schema/struct {:a {:type :int}}
                        :required true))))

(deftest selection
  (let [schema (schema/struct {:a {:type :struct
                                   :fields {:a1 {:type :int}}}
                               :b {:type :enum :values #{1 2 3}}
                               :c {:type :struct
                                   :fields {:c1 {:type :int}
                                            :c2 {:type :int}
                                            :c3 {:type :int}}}})]
    (is (= {:type :int}
           (schema/get-in schema [:a :a1])))
    (is (= {:type :struct
            :fields {:c2 {:type :int}
                     :c3 {:type :int}}}
           (-> schema
               (schema/get-in [:c])
               (schema/select-keys [:c2 :c3]))))))

(deftest strings
  (let [schema (schema/struct {:id {:type :string}})]
    (is (schema/matches? {"id" "nothing"} schema))))

(deftest error-reporting
  (let [schema (schema/struct {:name (schema/struct {:first {:type :string}
                                                     :last {:type :string}})
                               :gender {:required true
                                        :type :enum :values #{:male :female}}
                               :phone-numbers {:type :set
                                               :values {:type :int}}})]
    (are [obj error-string] (-> (schema/error obj schema)
                                (.contains error-string))
         {:gender 5} "gender"
         {:name {:first "dave"}} "gender"
         {:gender :male :name {:first 10}} "name"
         {:gender :female :phone-numbers {342 true, 294 "NOT BOOLEAN"}} "294"
         {:gender :male   :phone-numbers {"TEST" true}} "String"
         {:gender :male   :phone-numbers #{1 2 3 :etc}} "Keyword"
         {:gender :female :notes "psychic powers"} "notes")

    (are [obj] (not (schema/error obj schema))
         {:gender "female"}
         {:gender :male :name {:first "mark"} :phone-numbers [1 2 3 4]})))

(deftest dissoc-fields
  (let [struct (schema/struct {:foo {:type :string}
                               :bar {:type :string}})
        schema (schema/struct {:foo struct
                               :bar {:required true
                                     :type :enum :values #{:a :b}}
                               :bap {:type :int}
                               :baz {:type :set
                                     :values struct}})]
    (is (= {:type :struct
            :fields {:bar {:type :enum :required true :values #{:a :b}}
                     :baz {:type :set :values {:type :struct
                                               :fields {:bar {:type :string}}}}}}
           (schema/dissoc-fields schema :foo :bap)))))
