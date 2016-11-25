(ns codegraph.core
  (:require [clojure.walk :as walk])
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

(defn register-relation! [sym]
  (swap! relations update-in [@current] conj sym))

(defmulti process type)

(defmethod process :default [form] form)

(defmethod process clojure.lang.Symbol [form]
  (let [sym (name form)]
    (if (trigger? sym)
      (reset! triggered true)
      (if @triggered
        (register-current! sym)
        (register-relation! sym)))))

(defn draw-one [a b]
  (if (contains? (set (keys @relations)) b)
    (str "\"" a "\" -> \"" b "\"")))

(defn draw-many [a b]
  (map #(draw-one a %) b))

(defn graph [path]
  (read-string (str "[" (slurp path) "]")))

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
  (walk/prewalk process (graph (first args)))
  (output (mapcat (partial apply draw-many) @relations)))
