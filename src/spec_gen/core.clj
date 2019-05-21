(ns spec-gen.core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(def spec-specs
  [(s/def ::spec #(some? (s/get-spec %)))])

(def atom-specs
  [(s/def ::number number?)
   (s/def ::string string?)
   (s/def ::keyword keyword?)
   (s/def ::symbol symbol?)])

(def coll-specs
  [(s/def ::map map?)
   (s/def ::set set?)
   (s/def ::vector vector?)
   (s/def ::list list?)
   (s/def ::lazy-seq (partial instance? clojure.lang.LazySeq))
   (s/def ::coll coll?)
   (s/def ::seq seq?)
   (s/def ::seqable seqable?)])

(def fn-specs
  [(s/def ::ifn ifn?)])

(def atom-coll-fn-specs (concat atom-specs coll-specs fn-specs))

(defn gen-type-spec
  [type]
  (eval `(s/def ~(keyword (str *ns*) (.getName type)) ~(fn [o] (instance? type o)))))

(defn check-specs
  [check-f specs value]
  (map #(vector % (check-f % value)) specs))

(def valid-specs (partial check-specs s/valid?))

(defn first-spec [specs value]
  (first (first (filter second (valid-specs specs value)))))

(declare gen-nested-spec)

(defn gen-or-spec
  "Takes a list of specs and a list of values,
  then returns a string which is a generated name for the spec, and a `s/or` spec.
  
  If `l` only contains values conforming to a single spec,
  it just returns that spec instead of the `s/or`-spec.
  
  The generated name looks like this `::<::number+::string>`, if there was
  a `::number` and a `::string` in the `set-of-specs`."
  [specs l]
  (let [set-of-specs (into (sorted-set) (map #(gen-nested-spec specs %) l))
        spec-name (str/join "+" (map #(apply str (rest (str %))) set-of-specs))]
    [spec-name
     (if (< 1 (count set-of-specs))
       `(s/or
         ~@(->> set-of-specs
                (map-indexed
                 #(vector
                   (if (keyword? %2)
                     (keyword (name %2))
                     (keyword (str "kind-" %1)))
                   %2))
                (apply concat)))
       (first set-of-specs))]))

(defn gen-list-spec
  [specs l]
  (let [[spec-name s-or] (gen-or-spec specs l)
        spec-name (keyword (str *ns*) (str "coll-of<" spec-name ">"))]
    `(s/def ~spec-name (s/coll-of ~s-or))))

(defn gen-nested-spec
  [specs value]
  (let [res (first-spec specs value)
        res (if (some #{res} coll-specs)
              (case res
                (::vector ::list) (eval (gen-list-spec specs value))
                ::lazy-seq (eval (gen-list-spec specs (take 100 value)))
                res)
              res)]
    (if res
      res
      (gen-type-spec (type value)))))

(defn spec-it! [name value] (eval `(s/def ~name ~(gen-nested-spec atom-coll-fn-specs value))))

(comment
  (let [values [1 "hej" 1]]
    (gen-or-spec all-specs values))
  
  (gen-nested-spec atom-coll-fn-specs [1 2 3])
  (gen-list-spec atom-coll-fn-specs [1 2 3 "string" ["hej"]])
  (gen-nested-spec atom-coll-fn-specs 5)
  
  (spec-it! ::hmmm (String. "hej"))
  (s/explain ::hmmm (String. "wat"))
  (s/explain ::hmmm 123)
  
  (spec-it! ::file (io/file "Example.txt"))
  (s/explain ::file "wat")
  (s/explain ::file (io/file "wat"))
  (s/explain ::file nil)
  (s/explain ::file 123)
  (s/explain ::file (io/file "Yoo.html"))
  
  (spec-it! ::cool [1 2 3]) ;;=> ::cool
  (gen-nested-spec atom-coll-fn-specs [1 2 3])
  (gen-or-spec atom-coll-fn-specs [1 2 3])
  (gen-list-spec atom-coll-fn-specs [1 2 3])
  (S/valid? :spec-gen.core/coll-of<spec-gen.core/number> [5 6])
  (s/valid? ::cool [5 6]) ;;=> true
  (s/valid? ::cool [5 6 "HUE"]) ;;=> false
  )




;; ==================== Generate specs for functions ==============


(defn args->specs
  "args is a map, with the keys being symbols, and vals being values.
  E.g. (args-list->specs {'x 10, 'y 20})"
  [specs args]
  (for [[arg value] args]
    [arg (gen-nested-spec specs value)]))

(defn args-list->specs
  [specs args-list]
  (->> args-list
       (map (partial args->specs specs))
       (apply concat)
       (group-by first)
       (map (fn [[k v]]
              (let [set-of-specs (into #{} (map second v))
                    set-of-specs (if (< 1 (count set-of-specs))
                                   `(s/or
                                     ~@(->> set-of-specs
                                            (map-indexed
                                             #(vector
                                               (if (keyword? %2)
                                                 (keyword (name %2))
                                                 (keyword (str "kind-" %1)))
                                               %2))
                                            (apply concat)))
                                   (first set-of-specs))]
                [k set-of-specs])))))

(comment
  (require '[miracle.save :refer :all])
  
  (let [lol "hej"]
    (gen-nested-spec atom-coll-fn-specs lol))
  
  (save-ns* *ns*)
  (unsave-ns* *ns*)
  
  (args-list->specs
   atom-coll-fn-specs
   ['([x 10], [y 20])
    {'x "hej"}])
  
  (def f-specs (into {}
                     (for [[f-var call-data] @miracle.save/f-saves]
                       [f-var (args-list->specs
                               atom-coll-fn-specs
                               (map :spec-args call-data))])))
  
  (s/fdef args->specs :args (s/cat :specs ::coll
                                   :args ::vector))
  (st/instrument)
  (args->specs 1 2)
  
  (defn var->sym
    "Feels a bit hacky."
    [v]
    (symbol (apply str (drop 2 (str v)))))  
  
  (def fdefs (for [[f-var args-specs] f-specs]
               `(s/fdef ~(var->sym f-var) :args (s/cat ~@(apply concat (map (fn [[sym spec]] [(keyword sym) spec]) args-specs))))
               ))
  
  (def lol (first fdefs))
  
  (doseq [fd fdefs] (eval fd))
  
  (clojure.spec.alpha/fdef
      (var->sym #'spec-gen.core/args->specs)
    :args
    (clojure.spec.alpha/cat
     :specs
     :spec-gen.core/coll-of<spec-gen.core/keyword>
     :args
     (clojure.spec.alpha/or
      :coll-of<spec-gen.core/coll-of<spec-gen.core/number+spec-gen.core/symbol>>
      :spec-gen.core/coll-of<spec-gen.core/coll-of<spec-gen.core/number+spec-gen.core/symbol>>
      :map
      :spec-gen.core/map)))
  
  
  
  )






