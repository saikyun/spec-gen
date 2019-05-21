(ns spec-gen.core
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [clojure.walk :refer [postwalk]]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.pprint :as pp]
            [clojure.java.io :as io])
  (:gen-class))

(def spec-specs
  [(s/def ::spec #(some? (s/get-spec %)))])

(def atom-specs
  [#'number?
   #'string?
   #'keyword?
   #'symbol?])

(def coll-specs
  [#'map?
   #'set?
   #'vector?
   #'list?
   (s/def ::lazy-seq (partial instance? clojure.lang.LazySeq))
   #'coll?
   #'seq?
   #'seqable?])

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

(def ^:dynamic *or-limit* 5)

(defn specs->sor
  [set-of-specs]
  (let [n (count set-of-specs)]
    (cond
      (= n 1) (first set-of-specs)
      (< n *or-limit*)
      `(s/or
        ~@(->> set-of-specs
               (map-indexed
                #(vector
                  (keyword
                   (str
                    (cond
                      (keyword? %2) (name %2)
                      (var? %2) (str/replace (name (:name (meta %2))) "?" "")
                      :default "kind")
                    "-"
                    %1))
                  %2))
               (apply concat)))
      :default `any?)))

(defn gen-list-spec
  [specs l]
  `(s/coll-of ~(specs->sor (into #{} (map #(gen-nested-spec specs %) l)))))

(defn gen-nested-spec
  [specs value]
  (let [res (first-spec specs value)
        res (cond
              (some #{res} [#'vector? #'list?]) (gen-list-spec specs value)
              (= res ::lazy-seq) (gen-list-spec specs (take 100 value))
              :default res)]
    (if res
      res
      (gen-type-spec (type value)))))

(defn spec-it! [name value] (eval `(s/def ~name ~(gen-nested-spec atom-coll-fn-specs value))))



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
       (map (fn [[k v]] [k (specs->sor (into #{} (map second v)))]))))

(defn alias-syms
  "Takes a coll of syms containing qualified namespaces, and turns them into aliased namespaces."
  [syms]
  (let [name->short (->> (ns-aliases *ns*)
                         (map (fn [[k v]] [(ns-name v) k]))
                         (into {}))]
    (postwalk
     #(cond
        (not (symbol? %)) %
        
        (some-> (namespace %)
                symbol
                name->short)
        (symbol (str (some-> (namespace %)
                             symbol
                             name->short))
                (name %))
        
        (= (namespace %)  (ns-name *ns*))
        (name %)
        
        :default %)
     syms)))

(defn var->sym
  "Feels a bit hacky."
  [v]
  (symbol (apply str (drop 2 (str v)))))

(defn fdef-one
  [f-var call-data]
  (let [f-specs {:args (args-list->specs
                        atom-coll-fn-specs
                        (map :spec-args call-data))
                 :ret (->> (map :ret call-data)
                           (map (partial gen-nested-spec atom-coll-fn-specs))
                           (into #{})
                           specs->sor)}]
    (let [{:keys [args ret]} f-specs]
      `(s/fdef ~(var->sym f-var)
         :args
         (s/cat ~@(apply concat (map (fn [[sym spec]] [(keyword sym) spec]) args)))
         :ret ~ret))))

(comment
  ;; This first comment-block illustrates some spec-generation
  ;; The next comment-block shows you a new way of creating
  ;; specs by running your program and growing specs organically
  
  
  ;; Some functions used to generate specs  
  ;; Probably not for end-users
  (gen-nested-spec atom-coll-fn-specs [1 2 3])
  (gen-nested-spec atom-coll-fn-specs [1 2 3 "string" ["hej"]])
  (gen-nested-spec atom-coll-fn-specs 5)
  
  ;; Generates a spec ::string2 based on the value
  (spec-it! ::string2 (String. "hej"))
  (s/explain ::string2 (String. "wat"))
  (s/explain ::string2 "hehe")
  (s/explain ::string2 123)
  
  (spec-it! ::file (io/file "Example.txt"))
  (s/explain ::file "wat")
  (s/explain ::file (io/file "wat"))
  (s/explain ::file nil)
  (s/explain ::file 123)
  (s/explain ::file (io/file "Yoo.html"))
  
  (spec-it! ::cool [1 2 3]) ;;=> ::cool
  (s/valid? ::cool [5 6]) ;;=> true
  (s/valid? ::cool [5 6 "HUE"]) ;;=> false
  )


(comment
  ;; Here follows a tutorial on how to use
  ;; spec-gen together with stored function data
  ;; in this case, we store the data using `miracle.save`
  
  ;; First we need to store some function calls
  (require '[miracle.save :refer :all])
  (reset! f-saves {}) ;; Just in case you've already saved some data
  
  ;; This starts storing all args and return values of all functions in this namespace
  (save-ns* *ns*)
  
  ;; This generates specs for each arg
  ;; Though right now we only use it to exercise our functions
  ;; and generate data
  (args-list->specs
   atom-coll-fn-specs
   ['([x [30 40]], [y 20])
    '([x [30 50]], [y 20])
    '([x [30 "wat"]], [y 20])
    '([x [30 "wat" :hehe]], [y 20])
    '([x [30 "wat" 'omg]], [y 20])
    '([x [30 "wat" {:ho 10}]], [y 20])])
  
  ;; We don't need any more data right now
  (unsave-ns* *ns*)
  
  ;; If you want to, you can double check what data we stored
  ;; it's kind of hard to read since it can be a lot of data
  (def fs (first @f-saves))
  (pp/pprint fs)
  
  ;; We can generate fdefs from this data though!
  (alias-syms (apply fdef-one fs))
  
  ;; Now, we want to generate specs for all the stared functions!
  (doseq [[f-var call-data] @f-saves]
    (eval (fdef-one f-var call-data)))
  
  ;; And then instrument it
  (st/instrument)
  
  
  ;; If you want to check all fdefs, just pprint instead of eval. :)
  (doseq [[f-var call-data] @f-saves]
    (pp/pprint (alias-syms (fdef-one f-var call-data))))
  
  ;; Here's an example on how to persist the fdefs
  (do (create-ns 'spec-gen.fdefs)
      (in-ns 'spec-gen.fdefs)
      (clojure.core/refer-clojure)
      (require '[spec-gen.core :as sg])
      (require '[clojure.spec.alpha :as s])
      (spit "src/spec_gen/fdefs.clj"
            (with-out-str
              (println "(ns spec-gen.fdefs (:require [spec-gen.core :as sg] [clojure.spec.alpha :as s]))")
              (println)
              (doseq [[f-var call-data] @miracle.save/f-saves]
                (clojure.pprint/pprint
                 (sg/alias-syms (sg/fdef-one f-var call-data)))
                (println))))
      (in-ns 'spec-gen.core))
  
  
  ;; Let's call the earlier function again
  (args-list->specs
   atom-coll-fn-specs
   ['([x [30 40]], [y 20])
    '([x [30 50]], [y 20])
    '([x [30 "wat"]], [y 20])
    '([x [30 "wat" :hehe]], [y 20])
    '([x [30 "wat" 'omg]], [y 20])
    '([x [30 "wat" {:ho 10}]], [y 20])])
  
  ;; And with some weird arguments
  (args-list->specs
   123
   ['([x [30 40]], [y 20])
    '([x [30 50]], [y 20])
    '([x [30 "wat"]], [y 20])
    '([x [30 "wat" :hehe]], [y 20])
    '([x [30 "wat" 'omg]], [y 20])
    '([x [30 "wat" {:ho 10}]], [y 20])])
  
  (args-list->specs
   ["hehe"]
   ['([x [30 40]], [y 20])
    '([x [30 50]], [y 20])
    '([x [30 "wat"]], [y 20])
    '([x [30 "wat" :hehe]], [y 20])
    '([x [30 "wat" 'omg]], [y 20])
    '([x [30 "wat" {:ho 10}]], [y 20])])
  
  
  
  
  
  
  ;; Please try creating some specs on your own!
  ;; Make changes `my-func` and evaluate the do-block and evaluate it as a whole :)
  (do (st/unstrument)
      (reset! f-saves {})
      
      (defn my-func
        [x]
        ;; Put code here!
        (+ x x))
      (save-var* #'my-func)
      
      ;; Gather data...
      (my-func "hej")
      (my-func 20)
      (my-func 30)
      
      (def fd (alias-syms (fdef-one #'my-func (get @f-saves #'my-func))))
      (pp/pprint fd)
      (eval fd)
      (st/instrument `my-func)
      
      (my-func 5))
  
  (my-func "hehe")  ;; Whoah! Spec assertion error
  
  ;; Try making some changes to my-func and call it with new values
  ;; in order to get it to work with strings as well :)
  ;; if you don't know how, I give an example down below.
  
  
  
  
  
  ;; If you want to make your function more flexible, you can unstrument it,
  ;; exercise it some more, then run `fdef-one` again, and instrument again
  ;; That way you can grow your fdef's organically :)
  
  ;; Here's an example, if we start out with the `my-func` taking only numbers...
  (defn my-func [x] (if (number? x) (+ x x) (map #(str "hello " % "!") x)))
  (save-var* #'my-func) ;; Need to resave when we overwrite the value in the var
  (st/instrument `my-func)
  (my-func 5) ;; works
  (my-func "hej") ;; spec error! (unless you already made `my-func` work with strings!)
  
  ;; Gosh darn it, we know we can use this with any `map`able value...
  
  (st/unstrument `my-func)
  (my-func "hej")
  ;; There we go! But we want it instrumented again
  
  (eval (fdef-one #'my-func (get @f-saves #'my-func)))
  (st/instrument `my-func)
  (my-func "hej") ;; now we've expanded the original spec!
  
  ;; if you want to look at the generated spec as text, you just pprint it again:
  (pp/pprint (alias-syms (fdef-one #'my-func (get @f-saves #'my-func))))
  
  ;; I hope you found this interesting!
  )






