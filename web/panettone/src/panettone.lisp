(in-package :panettone)
(declaim (optimize (safety 3)))

;;;
;;; Data model
;;;

(deftype issue-status () '(member :open :closed))

(defclass/std issue-comment ()
  ((body :type string)
   (author-dn :type string)
   (created-at :type local-time:timestamp
               :std (local-time:now)))
  (:documentation
   "DEPRECATED: use `PANETTONE.MODEL::ISSUE-COMMENT' insteaad"))

(defclass/std issue (cl-prevalence:object-with-id)
  ((subject body :type string :std "")
   (author-dn :type string)
   (comments :std nil :type list :with-prefix)
   (status :std :open :type issue-status)
   (created-at :type local-time:timestamp
               :std (local-time:now)))
  (:documentation
   "DEPRECATED: use `PANETTONE.MODEL::ISSUE' insteaad"))

(defclass/std user ()
  ((cn dn mail displayname :type string)))

;;;
;;; LDAP integration
;;;

(defvar *ldap* nil
  "The ldap connection")

(defun connect-ldap (&key
                       (host "localhost")
                       (port 389))
  (setq *ldap* (ldap:new-ldap :host host :port port)))

(defun ldap-entry->user (entry)
  (apply
   #'make-instance
   'user
   :dn (ldap:dn entry)
   (alexandria:mappend
    (lambda (field)
      (list field (car (ldap:attr-value entry field))))
    (list :mail
          :cn
          :displayname))))

(defun find-user/ldap (username)
  (check-type username (simple-array character (*)))
  (ldap:search
   *ldap*
   `(and (= objectClass organizationalPerson)
         (or
          (= cn ,username)
          (= dn ,username)))
   ;; TODO(grfn): make this configurable
   :base "ou=users,dc=tvl,dc=fyi")
  (ldap:next-search-result *ldap*))

(defun find-user (username)
  (check-type username (simple-array character (*)))
  (when-let ((ldap-entry (find-user/ldap username)))
    (ldap-entry->user ldap-entry)))

(defun find-user-by-dn (dn)
  (ldap:search *ldap* `(= objectClass organizationalPerson)
               :base dn
               :scope 'ldap:base)
  (when-let ((ldap-entry (ldap:next-search-result *ldap*)))
    (ldap-entry->user ldap-entry)))

(comment
 (user-by-dn "cn=glittershark,ou=users,dc=tvl,dc=fyi")
 )

(defun authenticate-user (user-or-username password)
  "Checks the given USER-OR-USERNAME has the given PASSWORD, by making a bind
request against the ldap server at *ldap*. Returns the user if authentication is
successful, `nil' otherwise"
  (when-let ((user (if (typep user-or-username 'user) user-or-username
                       (find-user user-or-username))))
    (let ((dn (dn user)))
      (multiple-value-bind (_r code-sym _msg)
          (ldap:bind
           (ldap:new-ldap :host (ldap:host *ldap*)
                          :port (ldap:port *ldap*)
                          :user dn
                          :pass password))
        (declare (ignore _r _msg))
        (when (equalp code-sym 'trivial-ldap:success)
          user)))))

(defun author (object)
  (find-user-by-dn (author-dn object)))

;;;
;;; Persistence
;;;

(defvar *p-system* nil
  "The persistence system for this instance of Panettone")

(defun initialize-persistence (data-dir)
  "Initialize the Panettone persistence system, storing data in DATA-DIR"
  (ensure-directories-exist data-dir)
  (setq *p-system*
        (cl-prevalence:make-prevalence-system
         (concatenate 'string
                      data-dir
                      "/snapshot.xml")))


  (when (null (cl-prevalence:find-all-objects *p-system* 'issue))
    (cl-prevalence:tx-create-id-counter *p-system*)))

(defun prevalence->postgresql (system &key force)
  "Idempotently migrate all data from the cl-prevalence system SYSTEM into the
global postgresql connection (eg as initialized by
`panettone.model:connect-postgres'). With FORCE=t, will clear the database first"
  (postmodern:with-transaction (prevalence->postgresql)
    (when force
      (postmodern:query (:delete-from 'issues)))
    (iter
      (for issue in (cl-prevalence:find-all-objects system 'issue))
      (counting
       (unless (panettone.model:issue-exists-p (get-id issue))
         (panettone.model:create-issue
          :id (get-id issue)
          :subject (subject issue)
          :body (or (body issue) "")
          :status (status issue)
          :author-dn (author-dn issue)
          :created-at (created-at issue)))
       into num-issues)
      (sum
       (iter
         (for comment in (issue-comments issue))
         (counting
          (unless (postmodern:query
                   (:select
                    (:exists
                     (:select 1
                      :from 'issue_comments
                      :where (:and
                              (:= 'issue_id (get-id issue))
                              (:= 'body (body comment))
                              (:= 'author_dn (author-dn comment))))))
                   :single)
            (panettone.model:create-issue-comment
             :body (body comment)
             :author-dn (author-dn comment)
             :issue-id (get-id issue)
             :created-at (created-at comment)))))
       into num-comments)
      (finally
       (let ((next-id (postmodern:query
                       (:select (:+ 1 (:max 'id))
                        :from 'issues)
                       :single)))
         (postmodern:query
          (postmodern:sql-compile
           `(:alter-sequence issues_id_seq :restart ,next-id))))
       (format t "Created ~A issues and ~A comments~&"
               num-issues num-comments)
       (return (values num-issues num-comments))))))

(comment
 (initialize-persistence "/home/grfn/code/depot/web/panettone/")
 (panettone.model:connect-postgres)
 (panettone.model:ddl/init)
 (prevalence->postgresql *p-system* :force t)
 )

;;;
;;; Views
;;;

(defvar *title* "Panettone")

(defvar *user* nil)

(setf (who:html-mode) :html5)

(defun render/footer-nav ()
  (who:with-html-output (*standard-output*)
    (:footer
     (:nav
      (if (find (hunchentoot:request-uri*)
                (list "/" "/issues/closed")
                :test #'string=)
          (who:htm (:span :class "placeholder"))
          (who:htm (:a :href "/" "All Issues")))
      (if *user*
          (who:htm
           (:form :class "form-link log-out"
                  :method "post"
                  :action "/logout"
                  (:input :type "submit" :value "Log Out")))
          (who:htm
           (:a :href "/login" "Log In")))))))

(defmacro render ((&key (footer t)) &body body)
  `(who:with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
      :lang "en"
      (:head
       (:title (who:esc *title*))
       (:link :rel "stylesheet" :type "text/css" :href "/main.css")
       (:meta :name "viewport"
              :content "width=device-width,initial-scale=1"))
      (:body
       (:div
        :class "content"
        ,@body
        (when ,footer
          (render/footer-nav)))))))

(defun render/alert (message)
  "Render an alert box for MESSAGE, if non-null"
  (check-type message (or null string))
  (who:with-html-output (*standard-output*)
    (when message
      (who:htm (:div :class "alert" (who:esc message))))))

(defun render/login (&key message (original-uri "/"))
  (render (:footer nil)
    (:div
     :class "login-form"
     (:header
      (:h1 "Login"))
     (:main
      :class "login-form"
      (render/alert message)
      (:form
       :method :post :action "/login"
       (:input :type "hidden" :name "original-uri"
               :value original-uri)
       (:div
        (:label :for "username"
                "Username")
        (:input :type "text"
                :name "username"
                :id "username"
                :placeholder "username"))
       (:div
        (:label :for "password"
                "Password")
        (:input :type "password"
                :name "password"
                :id "password"
                :placeholder "password"))
       (:input :type "submit"
               :value "Submit"))))))

(defun created-by-at (issue)
  (who:with-html-output (*standard-output*)
    (:span :class "created-by-at"
           "Opened by "
           (:span :class "username"
                  (who:esc
                   (or
                    (when-let ((author (author issue)))
                      (displayname author))
                    "someone")))
           " at "
           (:span :class "timestamp"
                  (who:esc
                   (format-dottime (created-at issue)))))))

(defun render/issue-list (&key issues)
  (who:with-html-output (*standard-output*)
    (:ol
     :class "issue-list"
     (dolist (issue issues)
       (let ((issue-id (get-id issue)))
         (who:htm
          (:li
           (:a :href (format nil "/issues/~A" issue-id)
               (:p
                (:span :class "issue-subject"
                       (who:esc (subject issue))))
               (:span :class "issue-number"
                      (who:esc (format nil "#~A" issue-id)))
               " - "
               (created-by-at issue)
               (let ((num-comments (length (issue-comments issue))))
                 (unless (zerop num-comments)
                   (who:htm
                    (:span :class "comment-count"
                           " - "
                           (who:esc
                            (format nil "~A comment~:p" num-comments))))))))))))))

(defun render/index (&key issues)
  (render ()
    (:header
     (:h1 "Issues")
     (when *user*
       (who:htm
        (:a
         :class "new-issue"
         :href "/issues/new" "New Issue"))))
    (:main
     (:div
      :class "issue-links"
      (:a :href "/issues/closed" "View closed issues"))
     (render/issue-list :issues issues))))

(defun render/closed-issues (&key issues)
  (render ()
    (:header
     (:h1 "Closed issues"))
    (:main
     (:div
      :class "issue-links"
      (:a :href "/" "View open isues"))
     (render/issue-list :issues issues))))

(defun render/new-issue (&optional message)
  (render ()
    (:header
     (:h1 "New Issue"))
    (:main
     (render/alert message)
     (:form :method "post"
            :action "/issues"
            :class "issue-form"
            (:div
             (:input :type "text"
                     :id "subject"
                     :name "subject"
                     :placeholder "Subject"))

            (:div
             (:textarea :name "body"
                        :placeholder "Description"
                        :rows 10))

            (:input :type "submit"
                    :value "Create Issue")))))

(defun render/new-comment (issue-id)
  (who:with-html-output (*standard-output*)
    (:form
     :class "new-comment"
     :method "post"
     :action (format nil "/issues/~A/comments" issue-id)
     (:div
      (:textarea :name "body"
                 :placeholder "Leave a comment"
                 :rows 5))
     (:input :type "submit"
             :value "Comment"))))

(defun render/issue (issue)
  (check-type issue issue)
  (let ((issue-id (get-id issue))
        (issue-status (status issue)))
    (render ()
      (:header
       (:h1 (who:esc (subject issue)))
       (:div :class "issue-number"
             (who:esc (format nil "#~A" issue-id))))
      (:main
       (:div
        :class "issue-info"
        (created-by-at issue)

        (when *user*
          (who:htm
           (:form :class "set-issue-status"
                  :method "post"
                  :action (format nil "/issues/~A/~A"
                                  issue-id
                                  (case issue-status
                                    (:open "close")
                                    (:closed "open")))
                  (:input :type "submit"
                          :class (case issue-status
                                   (:open "close-issue")
                                   (:closed "open-issue"))
                          :value (case issue-status
                                   (:open "Close")
                                   (:closed "Reopen")))))))
       (:p (who:esc (body issue)))
       (let ((comments (issue-comments issue)))
         (who:htm
          (:div
           :class "issue-comments"
           (dolist (comment comments)
             (let ((author (author comment)))
               (who:htm
                (:div
                 :class "comment"
                 (:p (who:esc (body comment)))
                 (:p
                  :class "comment-info"
                  (:span :class "username"
                         (who:esc (displayname author))
                         " at "
                         (who:esc (format-dottime (created-at comment)))))))))
           (when *user*
             (render/new-comment (get-id issue))))))))))

(defun render/not-found (entity-type)
  (render ()
    (:h1 (who:esc entity-type) "Not Found")))

;;;
;;; HTTP handlers
;;;

(defun @auth-optional (next)
  (let ((*user* (hunchentoot:session-value 'user)))
    (funcall next)))

(defun @auth (next)
  (if-let ((*user* (hunchentoot:session-value 'user)))
    (funcall next)
    (hunchentoot:redirect
     (format nil "/login?original-uri=~A"
             (drakma:url-encode
              (hunchentoot:request-uri*)
              :utf-8)))))

(defun @txn (next)
  (postmodern:with-transaction ()
    (funcall next)))

(defun @handle-issue-not-found (next)
  (handler-case (funcall next)
    (issue-not-found (err)
      (render/not-found
       (format nil "Issue #~A" (panettone.model:id err))))))

(defroute login-form ("/login" :method :get)
    (original-uri)
  (if (hunchentoot:session-value 'user)
      (hunchentoot:redirect (or original-uri "/"))
      (render/login :original-uri original-uri)))

(defroute submit-login ("/login" :method :post)
    (&post original-uri username password)
  (if-let ((user (authenticate-user username password)))
    (progn
      (setf (hunchentoot:session-value 'user) user)
      (hunchentoot:redirect (or original-uri "/")))
    (render/login :message "Invalid credentials")))

(defroute logout ("/logout" :method :post) ()
  (hunchentoot:delete-session-value 'user)
  (hunchentoot:redirect "/"))

(defroute index ("/" :decorators (@auth-optional)) ()
  (let ((issues (panettone.model:list-issues :status :open)))
    (render/index :issues issues)))

(defroute handle-closed-issues
    ("/issues/closed" :decorators (@auth-optional)) ()
  (let ((issues (panettone.model:list-issues :status :closed)))
    (render/closed-issues :issues issues)))

(defroute new-issue ("/issues/new" :decorators (@auth)) ()
  (render/new-issue))

(defroute handle-create-issue
    ("/issues" :method :post :decorators (@auth @txn))
    (&post subject body)
  (if (string= subject "")
      (render/new-issue "Subject is required")
      (progn
        (panettone.model:create-issue :subject subject
                                      :body body
                                      :author-dn (dn *user*))
        (hunchentoot:redirect "/"))))

(defroute show-issue
    ("/issues/:id" :decorators (@auth-optional @handle/issue-not-found))
    (&path (id 'integer))
  (handler-case
      (let* ((issue (panettone.model:get-issue id))
             (*title* (format nil "~A | Panettone"
                              (subject issue))))
        (render/issue issue))
    (issue-not-found (_)
      (declare (ignore _))
      (render/not-found "Issue"))))

(defroute handle-create-comment
    ("/issues/:id/comments"
     :decorators (@auth @handle-issue-not-found @txn)
     :method :post)
    (&path (id 'integer) &post body)
  (flet ((redirect-to-issue ()
           (hunchentoot:redirect (format nil "/issues/~A" id))))
    (cond
      ((string= body "")
       (redirect-to-issue))
      (:else
       (panettone.model:create-issue-comment
        :issue-id id
        :body body
        :author-dn (dn *user*))
       (redirect-to-issue)))))

(defroute close-issue
    ("/issues/:id/close" :decorators (@auth @handle-issue-not-found @txn)
                         :method :post)
    (&path (id 'integer))
  (panettone.model:set-issue-status id :closed)
  (hunchentoot:redirect (format nil "/issues/~A" id)))

(defroute open-issue
    ("/issues/:id/open" :decorators (@auth)
                         :method :put)
    (&path (id 'integer))
  (panettone.model:set-issue-status id :open)
  (hunchentoot:redirect (format nil "/issues/~A" id)))

(defroute styles ("/main.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  (apply #'lass:compile-and-write panettone.css:styles))

(defvar *acceptor* nil
  "Hunchentoot acceptor for Panettone's web server.")

(defun migrate-db ()
  "Migrate the database to the latest version of the schema

In this iteration, intiialize the DDL and move all data from the prevalence
snapshot to the DB. In future iterations, this will do things like adding new
tables and columns"
  (panettone.model:ddl/init)
  (prevalence->postgresql *p-system*))

(defun start-panettone (&key port data-dir
                          (ldap-host "localhost")
                          (ldap-port 389)
                          postgres-params)
  (connect-ldap :host ldap-host
                :port ldap-port)

  (initialize-persistence data-dir)
  (apply #'panettone.model:connect-postgres postgres-params)
  (migrate-db)

  (setq *acceptor*
        (make-instance 'easy-routes:routes-acceptor :port port))
  (hunchentoot:start *acceptor*))

(defun main ()
  (let ((port (integer-env "PANETTONE_PORT" :default 6161))
        (ldap-port (integer-env "LDAP_PORT" :default 389))
        (data-dir (or (uiop:getenvp "PANETTONE_DATA_DIR") "/var/lib/panettone")))
    (setq hunchentoot:*show-lisp-backtraces-p* nil)
    (start-panettone :port port
                     :data-dir data-dir
                     :ldap-port ldap-port)
    (sb-thread:join-thread
     (find-if (lambda (th)
                (string= (sb-thread:thread-name th)
                         (format nil "hunchentoot-listener-*:~A" port)))
              (sb-thread:list-all-threads)))))

(comment
 (setq hunchentoot:*catch-errors-p* nil)
 (start-panettone :port 6161
                  :data-dir "/tmp/panettone"
                  :ldap-port 3899)
 )
