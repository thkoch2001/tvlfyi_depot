(ns bbbg.db
  (:gen-class)
  (:refer-clojure :exclude [get list])
  (:require [camel-snake-kebab.core :as csk :refer [->kebab-case ->snake_case]]
            [bbbg.util.core :as u]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [com.stuartsierra.component :as component]
            [config.core :refer [env]]
            [honeysql.format :as hformat]
            [migratus.core :as migratus]
            [next.jdbc :as jdbc]
            [next.jdbc.connection :as jdbc.conn]
            next.jdbc.date-time
            [next.jdbc.optional :as jdbc.opt]
            [next.jdbc.result-set :as rs]
            [next.jdbc.sql :as sql])
  (:import [com.impossibl.postgres.jdbc PGSQLSimpleException]
           com.zaxxer.hikari.HikariDataSource
           [java.sql Connection ResultSet Types]
           javax.sql.DataSource))

(s/def ::host string?)
(s/def ::database string?)
(s/def ::user string?)
(s/def ::password string?)

(s/def ::config
  (s/keys :opt [::host
                ::database
                ::user
                ::password]))

(s/fdef make-database
  :args
  (s/cat :config (s/keys :opt [::config])))

(s/fdef env->config :ret ::config)

(s/def ::db any?)

;;;

(def default-config
  (s/assert
   ::config
   {::host "localhost"
    ::database "bbbg"
    ::user "bbbg"
    ::password "password"}))

(defn dev-config [] default-config)

(defn env->config []
  (->>
   {::host (:pghost env)
    ::database (:pgdatabase env)
    ::user (:pguser env)
    ::password (:pgpassword env)}
   u/remove-nils
   (s/assert ::config)))

(defn ->db-spec [config]
  (-> default-config
      (merge config)
      (set/rename-keys
       {::host :host
        ::database :dbname
        ::user :username
        ::password :password})
      (assoc :dbtype "pgsql")))

(defn connection
  "Make a one-off connection from the given `::config` map, or the environment
  if not provided"
  ([] (connection (env->config)))
  ([config]
   (-> config
       ->db-spec
       (set/rename-keys {:username :user})
       jdbc/get-datasource
       jdbc/get-connection)))

(defrecord Database [config]
  component/Lifecycle
  (start [this]
    (assoc this :pool (jdbc.conn/->pool HikariDataSource (->db-spec config))))
  (stop [this]
    (some-> this :pool .close)
    (dissoc this :pool))

  clojure.lang.IFn
  (invoke [this] (:pool this)))

(defn make-database [config]
  (map->Database {:config config}))

;;;
;;; Migrations
;;;

(defn migratus-config
  [db]
  {:store :database
   :migration-dir "migrations/"
   :migration-table-name "__migrations__"
   :db
   (let [db (if (ifn? db) (db) db)]
     (cond
       (.isInstance Connection db)
       {:connection db}
       (.isInstance DataSource db)
       {:datasource db}
       :else (throw
              (ex-info "migratus-config called with value of unrecognized type"
                       {:value db}))))})

(defn generate-migration
  ([db name] (generate-migration db name :sql))
  ([db name type] (migratus/create (migratus-config db) name type)))

(defn migrate!
  [db] (migratus/migrate (migratus-config db)))

(defn rollback!
  [db] (migratus/rollback (migratus-config db)))

;;;
;;; Database interaction
;;;

(defn ->key-ns [tn]
  (let [tn (name tn)
        tn (if (str/starts-with? tn "public.")
             (second (str/split tn #"\." 2))
             tn)]
    (str "bbbg." (->kebab-case tn))))

(defn ->table-name [kns]
  (let [kns (name kns)]
    (->snake_case
     (if (str/starts-with? kns "public.")
       kns
       (str "public." (last (str/split kns #"\.")))))))

(defn ->column
  ([col] (->column nil col))
  ([table col]
   (let [col-table (some-> col namespace ->table-name)
         snake-col (-> col name ->snake_case (str/replace #"\?$" ""))]
     (if (or (not (namespace col))
             (not table)
             (= (->table-name table) col-table))
       snake-col
       ;; different table, assume fk
       (str
        (str/replace-first col-table "public." "")
        "_"
        snake-col)))))

(defn ->value [v]
  (if (keyword? v)
    (-> v name csk/->snake_case_string)
    v))

(defn process-key-map [table key-map]
  (into {}
        (map (fn [[k v]] [(->column table k)
                          (->value v)]))
        key-map))

(defn fkize [col]
  (if (str/ends-with? col "-id")
    (let [table (str/join "-" (butlast (str/split (name col) #"-")))]
      (keyword (->key-ns table) "id"))
    col))

(def ^:private enum-members-cache (atom {}))
(defn- enum-members
  "Returns a set of enum members as strings for the enum with the given name"
  [db name]
  (if-let [e (find @enum-members-cache name)]
    (val e)
    (let [r (try
              (-> (jdbc/execute-one!
                   (db)
                   [(format "select enum_range(null::%s) as members" name)])
                  :members
                  .getArray
                  set)
              (catch PGSQLSimpleException _
                nil))]
      (swap! enum-members-cache assoc name r)
      r)))

(def ^{:private true
       :dynamic true}
  *meta-db*
  "Database connection to use to query metadata"
  nil)

(extend-protocol rs/ReadableColumn
  String
  (read-column-by-label [x _] x)
  (read-column-by-index [x rsmeta idx]
    (if-not *meta-db*
      x
      (let [typ (.getColumnTypeName rsmeta idx)]
        ;; TODO: Is there a better way to figure out if a type is an enum?
        (if (enum-members *meta-db* typ)
          (keyword (csk/->kebab-case-string typ)
                   (csk/->kebab-case-string x))
          x)))))

(comment
  (->key-ns :public.user)
  (->key-ns :public.api-token)
  (->key-ns :api-token)
  (->table-name :api-token)
  (->table-name :public.user)
  (->table-name :bbbg.user)
  )

(defn as-fq-maps [^ResultSet rs _opts]
  (let [qualify #(when (seq %) (str "bbbg." (->kebab-case %)))
        rsmeta (.getMetaData rs)
        cols (mapv
              (fn [^Integer i]
                (let [ty (.getColumnType rsmeta i)
                      lab (.getColumnLabel rsmeta i)
                      n (str (->kebab-case lab)
                             (when (= ty Types/BOOLEAN) "?"))]
                  (fkize
                   (if-let [q (some-> rsmeta (.getTableName i) qualify not-empty)]
                     (keyword q n)
                     (keyword n)))))
              (range 1 (inc (.getColumnCount rsmeta))))]
    (jdbc.opt/->MapResultSetOptionalBuilder rs rsmeta cols)))

(def jdbc-opts
  {:builder-fn as-fq-maps
   :column-fn ->snake_case
   :table-fn ->snake_case})

(defmethod hformat/fn-handler "count-distinct" [_ field]
  (str "count(distinct " (hformat/to-sql field) ")"))

(defn fetch
  "Fetch a single row from the db matching the given `sql-map` or query"
  [db sql-map & [opts]]
  (s/assert
   (s/nilable (s/keys))
   (binding [*meta-db* db]
     (jdbc/execute-one!
      (db)
      (if (map? sql-map)
        (hformat/format sql-map)
        sql-map)
      (merge jdbc-opts opts)))))

(defn get
  "Retrieve a single record from the given table by ID"
  [db table id & [opts]]
  (when id
    (fetch
     db
     {:select [:*]
      :from [table]
      :where [:= :id id]}
     opts)))

(defn list
  "Returns a list of rows from the db matching the given sql-map, table or
  query"
  [db sql-map-or-table & [opts]]
  (s/assert
   (s/coll-of (s/keys))
   (binding [*meta-db* db]
     (jdbc/execute!
      (db)
      (cond
        (map? sql-map-or-table)
        (hformat/format sql-map-or-table)
        (keyword? sql-map-or-table)
        (hformat/format {:select [:*] :from [sql-map-or-table]})
        :else
        sql-map-or-table)
      (merge jdbc-opts opts)))))

(defn exists?
  "Returns true if the given sql query-map would return any results"
  [db sql-map]
  (binding [*meta-db* db]
    (pos?
     (:count
      (fetch db {:select [[:%count.* :count]], :from [[sql-map :sq]]})))))

(defn execute!
  "Given a database and a honeysql query map, perform an operation on the
  database and discard the results"
  [db sql-map & [opts]]
  (jdbc/execute!
   (db)
   (hformat/format sql-map)
   (merge jdbc-opts opts)))

(defn insert!
  "Given a database, a table name, and a data hash map, inserts the
  data as a single row in the database and attempts to return a map of generated
  keys."
  [db table key-map & [opts]]
  (binding [*meta-db* db]
    (sql/insert!
     (db)
     table
     (process-key-map table key-map)
     (merge jdbc-opts opts))))

(defn update!
  "Given a database, a table name, a hash map of columns and values
  to set, and a honeysql predicate, perform an update on the table.
  Will "
  [db table key-map where-params & [opts]]
  (binding [*meta-db* db]
    (execute! db
              {:update table
               :set (u/map-keys keyword (process-key-map table key-map))
               :where where-params
               :returning [:id]}
              opts)))

(defn delete!
  "Delete all rows from the given table matching the given where clause"
  [db table where-clause]
  (binding [*meta-db* db]
    (sql/delete! (db) table (hformat/format-predicate where-clause))))

(defmacro with-transaction [[sym db opts] & body]
  `(jdbc/with-transaction
     [tx# (~db) ~opts]
     (let [~sym (constantly tx#)]
       ~@body)))

(defn -main [& args]
  (let [db (component/start (make-database {::config (env->config)}))]
    (case (first args)
      "migrate" (migrate! db)
      "rollback" (rollback! db))))

(comment
  (def db (:db bbbg.core/system))
  (generate-migration db "init-schema")
  (migrate! db)


  )
