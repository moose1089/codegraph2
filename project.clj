(defproject codegraph "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot codegraph.core
  :target-path "target/%s"
  :aliases {"core2" ["run" "-m" "codegraph.core2"]}
  :profiles {:uberjar {:aot :all}})
