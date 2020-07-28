(in-package :panettone)
(declaim (optimize (safety 3)))

;;;
;;; Views
;;;

(defvar *title* "Panettone")

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

(defun author (object)
  (find-user-by-dn (author-dn object)))

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
  (check-type issue model:issue)
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
       (let ((issue-id (model:id issue)))
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
  (check-type issue model:issue)
  (let ((issue-id (id issue))
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
             (render/new-comment (id issue))))))))))

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
  (pomo:with-transaction ()
    (catch
        ;; 'hunchentoot:handler-done is unexported, but is used by functions
        ;; like hunchentoot:redirect to nonlocally abort the request handler -
        ;; this doesn't mean an error occurred, so we need to catch it here to
        ;; make the transaction still get committed
        (intern "HANDLER-DONE" "HUNCHENTOOT")
      (funcall next))))

(defun @handle-issue-not-found (next)
  (handler-case (funcall next)
    (model:issue-not-found (err)
      (render/not-found
       (format nil "Issue #~A" (model:id err))))))

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
  (let ((issues (model:list-issues :status :open)))
    (render/index :issues issues)))

(defroute handle-closed-issues
    ("/issues/closed" :decorators (@auth-optional)) ()
  (let ((issues (model:list-issues :status :closed)))
    (render/closed-issues :issues issues)))

(defroute new-issue ("/issues/new" :decorators (@auth)) ()
  (render/new-issue))

(defroute handle-create-issue
    ("/issues" :method :post :decorators (@auth @txn))
    (&post subject body)
  (if (string= subject "")
      (render/new-issue "Subject is required")
      (progn
        (model:create-issue :subject subject
                                      :body body
                                      :author-dn (dn *user*))
        (hunchentoot:redirect "/"))))

(defroute show-issue
    ("/issues/:id" :decorators (@auth-optional @handle-issue-not-found))
    (&path (id 'integer))
  (handler-case
      (let* ((issue (model:get-issue id))
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
       (model:create-issue-comment
        :issue-id id
        :body body
        :author-dn (dn *user*))
       (redirect-to-issue)))))

(defroute close-issue
    ("/issues/:id/close" :decorators (@auth @handle-issue-not-found @txn)
                         :method :post)
    (&path (id 'integer))
  (model:set-issue-status id :closed)
  (hunchentoot:redirect (format nil "/issues/~A" id)))

(defroute open-issue
    ("/issues/:id/open" :decorators (@auth)
                         :method :put)
    (&path (id 'integer))
  (model:set-issue-status id :open)
  (hunchentoot:redirect (format nil "/issues/~A" id)))

(defroute styles ("/main.css") ()
  (setf (hunchentoot:content-type*) "text/css")
  (apply #'lass:compile-and-write panettone.css:styles))

(defvar *acceptor* nil
  "Hunchentoot acceptor for Panettone's web server.")

(defun migrate-db ()
  "Migrate the database to the latest version of the schema"
  (model:ddl/init))

(defun start-panettone (&key port
                          (ldap-host "localhost")
                          (ldap-port 389)
                          postgres-params)
  (connect-ldap :host ldap-host
                :port ldap-port)

  (apply #'model:connect-postgres postgres-params)
  (migrate-db)

  (setq *acceptor*
        (make-instance 'easy-routes:routes-acceptor :port port))
  (hunchentoot:start *acceptor*))

(defun main ()
  (let ((port (integer-env "PANETTONE_PORT" :default 6161))
        (ldap-port (integer-env "LDAP_PORT" :default 389)))
    (setq hunchentoot:*show-lisp-backtraces-p* nil)
    (setq hunchentoot:*log-lisp-backtraces-p* nil)
    (start-panettone :port port
                     :ldap-port ldap-port)
    (sb-thread:join-thread
     (find-if (lambda (th)
                (string= (sb-thread:thread-name th)
                         (format nil "hunchentoot-listener-*:~A" port)))
              (sb-thread:list-all-threads)))))

(comment
 (setq hunchentoot:*catch-errors-p* nil)
 (start-panettone :port 6161
                  :ldap-port 3899)
 )
