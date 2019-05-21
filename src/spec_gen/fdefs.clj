(ns spec-gen.fdefs (:require [spec-gen.core :as sg] [clojure.spec.alpha :as s]))

(s/fdef
    sg/args->specs
  :args
  (s/cat
   :specs
   (s/coll-of
    (s/or :keyword-0 #'clojure.core/keyword? :ifn-1 :spec-gen.core/ifn))
   :args
   clojure.core/any?)
  :ret
  (s/coll-of
   (s/or
    :kind-0
    (s/coll-of
     (s/or
      :coll-0
      #'clojure.core/coll?
      :symbol-1
      #'clojure.core/symbol?))
    :kind-1
    (s/coll-of
     (s/or
      :ifn-0
      :spec-gen.core/ifn
      :symbol-1
      #'clojure.core/symbol?)))))

(s/fdef
    sg/valid-specs
  :args
  (s/cat
   :arg-0
   (s/coll-of
    (s/or :keyword-0 #'clojure.core/keyword? :ifn-1 :spec-gen.core/ifn))
   :arg-1
   clojure.core/any?)
  :ret
  (s/coll-of
   (s/or
    :kind-0
    (s/coll-of
     (s/or
      :keyword-0
      #'clojure.core/keyword?
      :java.lang.Boolean-1
      :spec-gen.fdefs/java.lang.Boolean))
    :kind-1
    (s/coll-of
     (s/or
      :ifn-0
      :spec-gen.core/ifn
      :java.lang.Boolean-1
      :spec-gen.fdefs/java.lang.Boolean)))))

(s/fdef
    sg/first-spec
  :args
  (s/cat
   :specs
   (s/coll-of
    (s/or :keyword-0 #'clojure.core/keyword? :ifn-1 :spec-gen.core/ifn))
   :value
   clojure.core/any?)
  :ret
  :spec-gen.core/ifn)

(s/fdef
    sg/gen-nested-spec
  :args
  (s/cat
   :specs
   (s/coll-of
    (s/or :keyword-0 #'clojure.core/keyword? :ifn-1 :spec-gen.core/ifn))
   :value
   clojure.core/any?)
  :ret
  (s/or :coll-0 #'clojure.core/coll? :ifn-1 :spec-gen.core/ifn))

(s/fdef
    sg/specs->sor
  :args
  (s/cat :set-of-specs #'clojure.core/set?)
  :ret
  (s/or
   :coll-0
   #'clojure.core/coll?
   :ifn-1
   :spec-gen.core/ifn
   :symbol-2
   #'clojure.core/symbol?))

(s/fdef
    sg/gen-list-spec
  :args
  (s/cat
   :specs
   (s/coll-of
    (s/or :keyword-0 #'clojure.core/keyword? :ifn-1 :spec-gen.core/ifn))
   :l
   clojure.core/any?)
  :ret
  #'clojure.core/coll?)

(s/fdef
    sg/args-list->specs
  :args
  (s/cat
   :specs
   (s/coll-of
    (s/or :keyword-0 #'clojure.core/keyword? :ifn-1 :spec-gen.core/ifn))
   :args-list
   (s/coll-of clojure.core/any?))
  :ret
  (s/coll-of
   (s/or
    :kind-0
    (s/coll-of #'clojure.core/symbol?)
    :kind-1
    (s/coll-of
     (s/or
      :ifn-0
      :spec-gen.core/ifn
      :symbol-1
      #'clojure.core/symbol?)))))
