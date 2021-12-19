(ns bbbg.util.sql
  (:require [honeysql.core :as hsql]))

(defn count-where [cond]
  (hsql/call :count (hsql/call :case cond #sql/raw "1" :else nil)))
