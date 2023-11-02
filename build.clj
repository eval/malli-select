(ns build
  "Build malli-select"
  (:refer-clojure :exclude [test])
  (:require [clojure.data.xml :as xml]
            [clojure.tools.build.api :as b]
            [clojure.tools.build.tasks.write-pom :as write-pom]
            [deps-deploy.deps-deploy :as dd]
            [clojure.java.io :as io]))

(set! clojure.core/*print-namespace-maps* false)

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

(defn- extract-keys-with-ns
  "E.g. `{:test/H true :foo :bar} ;;=> {:H true}`"
  [ns m]
  (update-keys (filter (comp #(= (name ns) %) namespace key) m)
               (comp keyword name)))

(defn ^#:fika{:examples [":test/d '\"some-dir\"'"
                         ":test/n '\"some.namespace-test\"'"
                         "# see all runner options\n:test/H true"]}
  test
  "Run all the tests.

  Passing options to test-runner possible, see examples." [opts]
  #_(prn :opts opts)
  (let [test-options   (extract-keys-with-ns "test" opts)
        test-options   (-> test-options
                           (update-keys (fn [k]
                                          ;; :H => "-H", :help => "--help"
                                          (let [k (name k)]
                                            (cond->> (str "-" k)
                                              (> (count k) 1) (str "-")))))
                           (update-vals str))
        basis          (b/create-basis {:aliases [:test]})
        cmds           (doto (b/java-command
                              {:basis         basis
                               :main          'clojure.main
                               #_#_:cp        ["/opt/homebrew/Cellar/clojure/1.11.1.1413/libexec/exec.jar"]
                               #_#_:main-args ["-m" "clojure.run.exec" ":dirs" "src"]
                               :main-args     (doto (reduce into ["-m" "cognitect.test-runner"] test-options) prn)}))
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit) (throw (ex-info "Tests failed" {}))))
  opts)

(b/java-command {:basis (b/create-basis {:aliases [:test]}) :main 'clojure.main})
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

(defn
  ^#:fika{:examples           [":build/git-version $(printf '\"%s\"'  $(git describe --tags))"]
          :option.git-version {:name :build/git-version
                               :desc "Output of `git describe --tags`, e.g. \"v1.2.3\", \"v1.2.3-pre.1\""}}
  build
  "Build the JAR."
  [{:build/keys [git-version] :as opts}]
  {:pre [(let [git-version-re #"^v\d+\.\d+\.\d+"]
           (or (and git-version (re-find git-version-re git-version))
               (throw (ex-info (str "requires :build/git-version with value matching " (pr-str git-version-re) ", e.g. :build/git-version '\"v1.2.3\"\"'") {}))))]}
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

(defn
  ^#:fika{:option.only-jar-version-type
          {:name "deploy/only-jar-version-type"
           :desc "Deploy the built jar based on the type of version it has. One of :full (default, e.g. \"1.2.3\"), :full-and-snapshot (also jar-versions like \"1.2.3-SNAPSHOT\"), :all (any jar that was built)."}}
  deploy
  "Deploy the built jar."
  [{:deploy/keys [only-jar-version-type] :or {only-jar-version-type :full} :as opts}]
  {:pre [(#{:full-and-snapshot :full :all} only-jar-version-type)]}
  (let [{:keys [jar-file] :as opts} (jar-opts opts)
        pom-file                    (b/pom-path (select-keys opts [:lib :class-dir]))
        version                     (pom-path->version pom-file)
        [v s]                       (re-find #"^\d+\.\d+\.\d+(-SNAPSHOT)?$" version)
        deploy?                     (or (= :all only-jar-version-type)
                                        (and (= :full-and-snapshot only-jar-version-type) v)
                                        (and (= :full only-jar-version-type)
                                             v
                                             (not s)))]
    (if deploy?
      (dd/deploy {:installer :remote
                  :artifact  (b/resolve-path jar-file)
                  :pom-file  pom-file})
      (println (str \newline "Skipping deploy of version " version " given only-jar-release-type " only-jar-version-type)))
    opts))


(defn ^#:fika{:examples [":build/git-version '\"v1.2.3\"'"
                         ":build/git-version $(printf '\"%s\"'  $(git describe --tags))"
                         ":build/git-version '\"v1.2.3\"' :deploy/only-jar-version-type :full-and-snapshot"]

              :options.from-commands            '[test build deploy]}
  release
  "Test, build and deploy.

Deploys *only* when name of the jar built has the right format, see option `deploy/only-jar-version-type`."
  [opts]
  #_(prn :release-opts opts)
  (deploy (build (test opts))))

(comment
  

  (def jopts (jar-opts {}))
  jopts

  #_:end)
