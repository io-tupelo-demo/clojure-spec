(defproject demo "0.1.0-SNAPSHOT"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [
                 [com.cognitect.aws/api "0.8.711"]
                 [com.cognitect.aws/endpoints "1.1.12.772"]
                 [com.cognitect.aws/s3 "869.2.1687.0"]
                 [org.clojure/clojure "1.12.0"]
                 [org.clojure/spec.alpha "0.5.238"]
                 [org.clojure/test.check "1.1.1"]
                 [prismatic/schema "1.4.1"]
                 [tupelo "24.12.03b"]
                 ]
  :plugins [
            [com.jakemccrary/lein-test-refresh "0.25.0"]
            [lein-ancient "0.7.0"]
            ]

  :global-vars {*warn-on-reflection* false}
  :main ^:skip-aot demo.core

  :source-paths            ["src/clj"]
  :java-source-paths       ["src/java"]
  :test-paths              ["test/clj"]
  :target-path             "target/%s"
  :compile-path            "%s/class-files"
  :clean-targets           [:target-path]

  :profiles {:dev     {:dependencies [ ]}
             :uberjar {:aot :all}}

  :jvm-opts ["-Xms500m" "-Xmx2g" ]
  )
