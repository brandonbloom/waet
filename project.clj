(defproject waet "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :global-vars {*warn-on-reflection* true
                #_#_*unchecked-math* :warn-on-boxed}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0-alpha6"]
                 [fipp "0.6.26"]
                 [instaparse "1.4.14"]])
