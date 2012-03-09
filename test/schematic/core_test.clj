(ns schematic.core-test
  (:use clojure.test)
  (:require [schematic.core :as schema]))

(deftest basics
  (are [type right wrong] (let [schema {:type type}]
                            (and (schema/matches? right schema)
                                 (not (schema/matches? wrong schema))))
       :boolean true 3
       :int 3 3.5
       :double 3.4 "number"
       :string "string" 10))

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
    (is (not (schema/matches? {:a "foo"} schema)))))

(deftest enums
  (let [schema {:type :enum
                :values #{:this :that}}]
    (are [x] (schema/matches? x schema)
         :this :that)
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
