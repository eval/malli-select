(ns build
  (:refer-clojure :exclude [test])
  (:require [clojure.data.xml :as xml]
            [clojure.tools.build.api :as b]
            [clojure.tools.build.tasks.write-pom :as write-pom]
            [deps-deploy.deps-deploy :as dd]
            [clojure.java.io :as io]))

;; Monkey patch to have provided dependencies in the POM
;; SOURCE https://clojurians.zulipchat.com/#narrow/stream/180378-slack-archive/topic/tools-deps/near/326868214
(xml/alias-uri 'pom "http://maven.apache.org/POM/4.0.0")

(alter-var-root
 #'write-pom/to-dep
 (fn [old]
   (fn [[_ {:keys [scope]} :as pair]]
     (cond-> (old pair)
       scope
       (conj [::pom/scope scope])))))

(def lib 'dk.thinkcreate/malli-select)

(def class-dir "target/classes")

(defn test "Run all the tests." [opts]
  (let [opts           (map #(map name %) opts)
        basis          (b/create-basis {:aliases [:test]})
        cmds           (b/java-command
                        {:basis     basis
                         :main      'clojure.main
                         :main-args (doto (reduce into ["-m" "cognitect.test-runner"] opts) prn)})
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit) (throw (ex-info "Tests failed" {}))))
  opts)

(defn- pom-template [version version-type]
  [[:description "spec2-inspired selection of Malli schemas"]
   [:url "https://github.com/eval/malli-select"]
   [:licenses
    [:license
     [:name "MIT"]
     [:url "https://github.com/eval/malli-select/blob/main/LICENSE"]]]
   [:developers
    [:developer
     [:name "Gert Goet (@eval)"]]]
   (cond-> [:scm
            [:url "https://github.com/eval/malli-select"]
            [:connection "scm:git:https://github.com/eval/malli-select.git"]
            [:developerConnection "scm:git:ssh:git@github.com:eval/malli-select.git"]]
     (= :exact version-type) (conj [:tag (str "v" version)]))])


(defn- jar-opts [{:keys [version version-type] :as opts}]
  (assoc opts
         :lib lib   :version version
         :jar-file  (format "target/%s-%s.jar" lib version)
         :basis     (b/create-basis {})
         :class-dir class-dir
         :target    "target"
         :src-dirs  ["src"]
         :pom-data  (pom-template version version-type)))

(defn- git-version->version&type
  "`git-version` typically output of `git describe --tags`,
  e.g. `v1.2.3`, `v1.2.3-pre.1` or `v1.2.3-1-g<sha>`.
  Yields map with `version` and `type`."
  [git-version]
  (let [type          (condp re-find git-version
                        #"^v\d+\.\d+\.\d+$"         :exact
                        #"^v\d+\.\d+\.\d+-pre\.\d+" :pre  ;; pre-tag and any commit after
                        :build)
        exact-version (second (re-find #"v(\d+\.\d+\.\d+)" git-version))
        version       (case type
                        :exact exact-version
                        :pre   (str exact-version "-SNAPSHOT")
                        :build (subs git-version 1))]
    {:version version :version-type type}))

(comment
  (git-version->version&type "v1.2.3-123")

  #_:end)

(defn build
  "Build the JAR.
  Usage:
  $ clojure -T:build build :git-version $(printf '\" %s \"' $(git describe --tags))"
  [{:keys [git-version] :as opts}]
  {:pre [(let [git-version-re #"^v\d+\.\d+\.\d+"]
           (or (and git-version (re-find git-version-re git-version))
               (throw (ex-info (str "requires :git-version with value matching " (pr-str git-version-re) ", e.g. :git-version '\"v1.2.3\"\"'") {}))))]}
  (let [opts (merge opts (git-version->version&type git-version))]
    (b/delete {:path "target"})
    (let [opts (jar-opts opts)]
      (println "\nWriting pom.xml...")
      (b/write-pom opts)
      (println "\nCopying source...")
      (b/copy-dir {:src-dirs ["resources" "src"] :target-dir class-dir})
      (println "\nBuilding JAR..." (:jar-file opts))
      (b/jar opts))
    opts))

(defn- pom-path->version [pom-path]
  (->> (io/reader pom-path)
       xml/parse
       :content
       (filter map?)
       (filter (comp #(= "version" %) name :tag))
       first
       :content
       first))

(defn deploy "Deploy the JAR to Clojars." [opts]
  (let [{:keys [jar-file] :as opts} (jar-opts opts)
        pom-file                    (b/pom-path (select-keys opts [:lib :class-dir]))
        version                     (pom-path->version pom-file)
        deployable-version-re       #"^\d+\.\d+\.\d+(-SNAPSHOT)?$"]
    (when-not (re-find deployable-version-re version)
      (throw (ex-info (str "Can't deploy build-version"
                           " (version: "
                           (pr-str version) ")") {})))
    (dd/deploy {:installer :remote
                :artifact  (b/resolve-path jar-file)
                :pom-file  pom-file}))
  opts)

(defn release
  "build&deploy"
  [opts]
  (deploy (build (test opts))))

(comment
  (set! clojure.core/*print-namespace-maps* false)

  (def jopts (jar-opts {}))
  jopts

  #_:end)
