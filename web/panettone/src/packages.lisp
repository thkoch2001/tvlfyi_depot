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
  (:use :cl :panettone.util :klatre :postmodern :iterate)
  (:import-from :alexandria :if-let :when-let :define-constant)
  (:export
   :connect-postgres :ddl/init

   :issue :issue-comment :issue-event
   :id :subject :body :author-dn :issue-id :status :created-at :acting-user-dn
   :field :previous-value :new-value

   :get-issue :issue-exists-p :list-issues :create-issue :set-issue-status
   :update-issue :delete-issue :issue-not-found

   :issue-events

   :issue-comments :num-comments :create-issue-comment))

(defpackage panettone
  (:use :cl :klatre :easy-routes :iterate
        :panettone.util
        :panettone.authentication)
  (:import-from :defclass-std :defclass/std)
  (:import-from :alexandria :if-let :when-let :switch :alist-hash-table)
  (:import-from :cl-ppcre :split)
  (:import-from
   :panettone.model
   :id :subject :body :author-dn :issue-id :status :created-at
   :field :previous-value :new-value :acting-user-dn
   :issue-comments :num-comments :issue-events)
  (:shadow :next)
  (:export :start-pannetone :config :main))
