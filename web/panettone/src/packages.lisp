(defpackage panettone.util
  (:use :cl :klatre)
  (:import-from :alexandria :when-let)
  (:export :integer-env))

(defpackage panettone.css
  (:use :cl :lass)
  (:export :styles))

(defpackage :panettone.authentication
  (:nicknames :authn)
  (:use :cl :panettone.util :klatre)
  (:import-from :defclass-std :defclass/std)
  (:import-from :alexandria :when-let)
  (:export
   :*user* :*ldap*
   :user :cn :dn :mail :displayname
   :connect-ldap :find-user :find-user-by-dn :authenticate-user))

(defpackage panettone.model
  (:nicknames :model)
  (:use :cl :panettone.util :klatre :postmodern)
  (:import-from :alexandria :if-let :define-constant)
  (:export
   :connect-postgres :ddl/init

   :issue
   :issue-comment
   :id :subject :body :author-dn :issue-id :status :created-at

   :get-issue :issue-exists-p :list-issues :create-issue :set-issue-status
   :delete-issue :issue-not-found

   :issue-comments :num-comments :create-issue-comment))

(defpackage panettone
  (:use :cl :klatre :easy-routes :iterate
        :panettone.util
        :panettone.authentication)
  (:import-from :defclass-std :defclass/std)
  (:import-from :alexandria :if-let :when-let)
  (:import-from
   :panettone.model
   :id :subject :body :author-dn :issue-id :status :created-at
   :issue-comments :num-comments)
  (:shadow :next)
  (:export :start-pannetone :config :main))
