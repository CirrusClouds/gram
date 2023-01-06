(ns gram.core
  (:require
   [juxt.grab.alpha.schema :refer [compile-schema]]
   [juxt.grab.alpha.document :refer [compile-document]]
   [juxt.grab.alpha.parser :refer [parse]]
   [malli.core :as mal]
   [malli.transform :as mt]
   [malli.dev.pretty :as pretty]
   [malli.util :as mu]))

(declare schema->malli*)

(defn type-coerce [types-by-name name]
  (case name
    "String" :string
    "ID" :string
    "Int" :int
    "Float" 'float?
    "Boolean" :boolean
    (schema->malli* types-by-name (get types-by-name name))))

(defn update-vals [m f]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn typeref->malli [types-by-name
                      {:keys [juxt.grab.alpha.graphql/non-null-type
                              juxt.grab.alpha.graphql/list-type
                              juxt.grab.alpha.graphql/name] :as typeref}]
  (cond
    non-null-type
    (take-last 1 (typeref->malli types-by-name non-null-type))
    list-type [(into [:sequential] (typeref->malli types-by-name list-type))]
    name [{:optional true} (type-coerce types-by-name name)]))

(defn schema->malli* [types-by-name
                      {:keys [juxt.grab.alpha.graphql/kind
                              juxt.grab.alpha.graphql/input-values
                              juxt.grab.alpha.graphql/member-types
                              juxt.grab.alpha.graphql/enum-values] :as ast}]
  (case kind
    INPUT_OBJECT
    (into [:map]
          (map (fn [v]
                 (into [(keyword (:juxt.grab.alpha.graphql/name v))]
                       (typeref->malli
                        types-by-name
                        (:juxt.grab.alpha.graphql/type-ref v))))
               input-values))
    SCALAR (type-coerce types-by-name (:juxt.grab.alpha.graphql/name ast))
    ENUM (into [:enum] (map :juxt.grab.alpha.graphql/name enum-values))
    UNION (let [members (->> member-types
                             (map #(type-coerce types-by-name %))
                             (filter identity))]
            (if (seq members)
              (into [:or] members)
              nil))
    nil))

(defn query-typeref->malli [{:keys [juxt.grab.alpha.graphql/list-type
                                    juxt.grab.alpha.graphql/name] :as typeref}
                            schema]
  (cond
    list-type (into [:sequential]
                    [(query-typeref->malli list-type schema)])
    name (get schema name)))

(defn query->malli*
  [{:keys [juxt.grab.alpha.graphql/operation-type
           juxt.grab.alpha.graphql/variable-definitions]}
   schema]
  {:operation-type operation-type
   :variables (into {}
                    (mapv (fn [{:keys [juxt.grab.alpha.graphql/variable
                                       juxt.grab.alpha.graphql/type-ref]}]
                            [variable
                             (if (:juxt.grab.alpha.graphql/non-null-type type-ref)
                               {:schema (query-typeref->malli (:juxt.grab.alpha.graphql/non-null-type type-ref) schema)}
                               {:optional true
                                :schema (query-typeref->malli type-ref schema)})])
                          variable-definitions))})

(defn grab-compile-schema [filename]
  (-> filename slurp parse compile-schema))

(defn schema->malli [filename]
  (let [{:keys [juxt.grab.alpha.schema/types-by-name] :as schema}
        (grab-compile-schema filename)]
    {:grab-schema
     schema
     :malli-schema
     (->> (update-vals types-by-name (fn [type] (schema->malli* types-by-name type)))
          (filter (comp identity second))
          (into {}))}))

(defn grab-compile-document [filename schema]
  (-> filename slurp parse (compile-document schema)))

(defn query->malli [filename {:keys [grab-schema malli-schema]}]
  (let [{:keys [juxt.grab.alpha.document/operations-by-name]}
        (grab-compile-document filename grab-schema)]
    (update-vals operations-by-name (fn [operation] (query->malli* operation malli-schema)))))

(defn graphql->malli [schema-file query-file]
  (->> schema-file schema->malli (query->malli query-file)))

(graphql->malli "test-resources/schema-1.graphql" "test-resources/query-1.graphql")
