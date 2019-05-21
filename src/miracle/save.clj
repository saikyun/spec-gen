(ns miracle.save
  (:require [clojure.pprint]))

(def ^:dynamic *max-saves* "The maximum number of saves per id." 100)

(defn inspect-map [map-to-print & {:keys [desired-level safe-count]
                                   :or {desired-level 4 safe-count 10}}]
  (binding [*print-level* desired-level *print-length* safe-count]
    (clojure.pprint/pprint map-to-print)))

(defn gensym? [s]       
  (re-find #"__\d+" s))

(defonce saves (atom {}))

(defn clear-saves! [] (reset! saves {}))

(defn new*
  "Creates a new save at `id`, with `bindings` containing the local bindings from where `save` was called."
  ([ref id]
   (new* id {}))
  ([ref id bindings]
   (swap! ref update id
          (fn [saves]
            (-> (conj saves bindings)
                (#(if (< (count %) *max-saves*)
                    (into '() (reverse (take *max-saves* %)))
                    %)))))))

(defn save-fn
  ([ref bindings id]
   (let [bindings `(into {} (remove #(gensym? (name (key %))) ~bindings))]
     `(new* ~ref ~id ~bindings))))

(defmacro get-env
  []
  (into {} (for [k (keys &env)]
             [`'~k k])))

(defmacro save
  "Used to save all local bindings, takes an identifier as a parameter.
  The identifier is used with `ld` in order to load the local bindings where `save` was called."
  [& args]
  (apply save-fn
         'miracle.save/saves
         (into {} (for [k (keys &env)]
                    [`'~k k]))
         args))

(defn ld
  "Loads the local bindings that have been saved using `save` with `id` as parameter."
  [id]
  (let [locals (first (get @saves id))]
    (when locals
      (println "Defining:")
      (inspect-map locals))
    (doseq [[sym val] locals]
      (try
        (eval `(def ~(symbol sym) '~val))
        (catch Exception e (prn sym val) (throw e))))))

(defn print-saves
  [id]
  (let [locals (take 10 (get @saves id))]
    (doseq [i (reverse (range (count locals)))]
      (println "Entry no." i)
      (inspect-map (first (drop i locals)))
      (prn))))

(comment
  ;; Example usage
  (defn add [x y] (save :a) (+ x y))
  (add 5 10)
  (ld :a)
  x ;; 5
  y ;; 10
  
  (add 20 30)
  (add 7 13)
  
  (print-saves :a))


;; ============== Saving function input and output ====================


(defonce f-saves (atom {}))

(defn which-args
  "Takes a list of arg-names, e.g. ([x] [x y] [x y {:keys [a b]}])
  and a number.
  Returns the arg-names that matches n-args."
  [arg-names-list n-args]
  (first
   (filter 
    some?
    (for [arg-names arg-names-list]
      (if (and (some #{'&} arg-names)
               (>= n-args (count (take-while #(not= % '&) arg-names))))
        arg-names
        (when (= n-args (count arg-names))
          arg-names))))))

(defmacro get-env
  "Returns the locally defined variables and their values."
  []
  (into {} (for [k (keys &env)]
             [`'~k k])))  

(defn destructure-bindings
  [arg-names values]
  `(let [~(which-args arg-names (count values)) '~values]
     (get-env)))

(defn gen-spec-args
  [arg-lists args]
  (let [arg-list (which-args arg-lists (count args))
        [before-and _] (split-with #(not= % '&) arg-list)
        nof-before-and (count before-and)]
    (concat
     (->> (map-indexed
           #(if (coll? %2) (keyword (str "arg-" %1)) %2)
           before-and)
          (#(map vector % args)))
     (when (< nof-before-and (count args))
       [['rest (drop (count before-and) args)]]))))

(defn save-wrapper
  "f-var should be a var or symbol holding a IFn."
  [f-var f args]
  (let [arg-lists (some-> (meta f-var) :arglists)
        ret (apply f args)
        
        args-to-save
        (into {}
              (if arg-lists
                (->> (eval (destructure-bindings arg-lists args))
                     (remove #(gensym? (name (key %)))))
                (->> (range (count args))                           ; Generate default arg-names
                     (map #(keyword (str "arg-" (str %))))
                     (#(map vector % args)))))
        
        spec-args  ;; argument specifically for generating specs
        (if arg-lists
          (gen-spec-args arg-lists args)
          (->> (range (count args))                                ; Generate default arg-names
               (map #(keyword (str "arg-" (str %))))
               (#(map vector % args))))]
    (new* f-saves
          f-var
          {:args args-to-save
           :spec-args spec-args
           :ret ret})
    ret))

(defn save-var*
  ([ns s]
   (save-var* (ns-resolve ns s)))
  ([v]
   (let [^clojure.lang.Var v (if (var? v) v (resolve v))
         ns (.ns v)
         s  (.sym v)]
     (if (and (ifn? @v) (-> v meta :macro not) (-> v meta ::saved not))
       (let [f @v
             vname (symbol (str ns "/" s))]
         (doto v
           (alter-var-root #(fn [& args] (save-wrapper v % args)))
           (alter-meta! assoc ::saved f)))))))

(defn unsave-var*
  ([ns s]
   (unsave-var* (ns-resolve ns s)))
  ([v]
   (let [^clojure.lang.Var v (if (var? v) v (resolve v))
         ns (.ns v)
         s  (.sym v)
         f  ((meta v) ::saved)]
     (when f
       (doto v
         (alter-var-root (constantly ((meta v) ::saved)))
         (alter-meta! dissoc ::saved))))))

(defn save-ns*
  [ns]
  (let [ns (the-ns ns)]
    (when-not ('#{clojure.core miracle.save} (.getName ns))
      (let [ns-fns (->> ns ns-interns vals (filter (comp fn? var-get)))]
        (doseq [f ns-fns]
          (save-var* f))))))

(defn unsave-ns*
  [ns]
  (let [ns-fns (->> ns the-ns ns-interns vals)]
    (doseq [f ns-fns]
      (unsave-var* f))))

(comment
  (do
    (defn yo ([x] x) ([x {:keys [a b]}] (+ x a b)) ([x y & rest] (apply + x y rest)))
    (def wat (partial yo 5))  
    (save-var* 'yo)  
    (save-var* 'wat)  
    (reset! f-saves {})
    (yo 5)
    (wat 5 10)
    (yo 5 {:a 20 :b 30})
    (yo 5 30 40)
    (yo 5 30 40 50)
    @f-saves)
  
  (unsave-var* 'yo)
  
  (require '[spec-gen.core :as sc])
  (save-ns* 'spec-gen.core)
  (sc/spec-it! ::yoo [1 2 3 {:heeh "my man"} ["wat"]])
  (unsave-ns* 'spec-gen.core)
  
  (def lol (partial save-wrapper 'yo))
  
  (do 
    (reset! f-saves {})
    (lol 5)
    (lol 5 {:a 20 :b 30})
    (lol 5 30 40)
    (lol 5 30 40 50)
    @f-saves))
