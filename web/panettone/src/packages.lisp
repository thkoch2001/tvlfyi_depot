(defpackage panettone.util
  (:nicknames :util)
  (:use :cl :klatre)
  (:import-from :alexandria :when-let)
  (:export
   :integer-env :add-missing-base64-padding :and-where :and-where*
   :define-build-time-var :->dir))

(defpackage panettone.css
  (:use :cl :lass)
  (:export :styles))

(defpackage panettone.inline-markdown
  (:use :cl)
  (:import-from :alexandria :define-constant)
  (:export :render-inline-markdown))

(defpackage panettone.irc
  (:nicknames :irc)
  (:use :cl :usocket)
  (:export :noping :send-irc-notification))

(defpackage :panettone.authentication
  (:nicknames :authn)
  (:use :cl :panettone.util :klatre)
  (:import-from :defclass-std :defclass/std)
  (:import-from :alexandria :when-let :with-gensyms)
  (:export
   :*user*
   :auth-url
   :fetch-token
   :user :cn :dn :mail :displayname
   :find-user-by-dn
   :initialise-oauth2))

(defpackage panettone.model
  (:nicknames :model)
  (:use :cl :panettone.util :klatre :postmodern :iterate)
  (:import-from :alexandria :if-let :when-let :define-constant)
  (:export
   :prepare-db-connections
   :migrate
   :*pg-spec*

   :user-settings
   :user-dn :enable-email-notifications-p :settings-for-user
   :update-user-settings :enable-email-notifications

   :issue :issue-comment :issue-event :migration
   :id :subject :body :author-dn :issue-id :status :created-at :acting-user-dn
   :field :previous-value :new-value :+issue-statuses+

   :get-issue :issue-exists-p :list-issues :create-issue :set-issue-status
   :update-issue :delete-issue :issue-not-found :not-found-id

   :issue-events

   :issue-comments :num-comments :create-issue-comment
   :issue-commenter-dns :issue-subscribers))

(defpackage panettone.email
  (:nicknames :email)
  (:use :cl)
  (:import-from :alexandria :when-let :when-let*)
  (:import-from :panettone.model
   :settings-for-user :enable-email-notifications-p)
  (:import-from :panettone.authentication
   :find-user-by-dn :mail :displayname)
  (:export
   :*smtp-server* :*smtp-server-port* :*notification-from*
   :*notification-from-display-name* :*notification-subject-prefix*
   :notify-user :send-email-notification))

(defpackage panettone
  (:use :cl :klatre :easy-routes :iterate
        :panettone.util
        :panettone.authentication
        :panettone.inline-markdown)
  (:import-from :defclass-std :defclass/std)
  (:import-from :alexandria :if-let :when-let :switch :alist-hash-table)
  (:import-from :cl-ppcre :split)
  (:import-from :bordeaux-threads :make-thread)
  (:import-from
   :panettone.model
   :id :subject :body :author-dn :issue-id :status :created-at
   :field :previous-value :new-value :acting-user-dn
   :*pg-spec*)
  (:import-from :panettone.irc :send-irc-notification)
  (:shadow :next)
  (:export :start-panettone :config :main))
