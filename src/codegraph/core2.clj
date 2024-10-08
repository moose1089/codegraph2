(ns codegraph.core2
  (:require
   [codegraph.other :as other])
  (:gen-class))

(def triggered (atom false))

(def current (atom nil))

(def relations (atom {}))

(def trigger #{"def"
               "defn"
               "defn-"
               "defmulti"
               "defmethod"})

(defn trigger? [sym]
  (contains? trigger sym))

(defn register-current! [sym]
  (reset! triggered false)
  (reset! current sym))

(defn register-relation! [f t]
  (swap! relations update-in [f] conj t))

(defn process [form]
  (let [analyses (:analysis form)]
    (doseq [f (keys analyses)]
;      (println "doing file" f)
      (let [definitions (:var-definitions (get analyses f))]
;        (println "doing definitions " definitions)
        (doseq [d definitions]
;          (println "doing def " d)
          (let [s (str (:ns d) "/" (:name d))]
;            (println "REGISTER " s " defined by " (:defined-by d))
            (register-current! s)))))
    (doseq [f (keys analyses)]
;      (println "doing file" f)
      (let [definitions (:var-usages (get analyses f))]
;        (println "doing definitions " definitions)
        (doseq [d definitions]
;          (println "doing def " d)
          (let [s (str (:from d) "/" (:name d))
                f (str (:from d) "/" (:from-var d))]
;            (println "REGISTER RELATION " f "->" s)
            (register-relation! f s)))))))

(defn draw-one [a b]
  (if (contains? (set (keys @relations)) b)
    (str "\"" a "\" -> \"" b "\"")))

(defn draw-many [a b]
  (map #(draw-one a %) b))

(defn graph [path]
  (read-string (str (slurp path) )))

(defn template [lines]
  (flatten
   ["# call with ` | dot -Tsvg > codegraph.svg`"
    "digraph G {"
    "edge ["
    "arrowtail = \"none\""
    "arrowhead = \"open\""
    "]"
    lines
    "}"]))

(defn output [lines]
  (doall (map println (template (remove nil? lines)))))

(defn -main [& args]
  (other/fact 5)
  (process (graph (first args)))
;  (println "RELATIONS" (string/join "\n" @relations))
  (output (mapcat (partial apply draw-many) @relations)))
