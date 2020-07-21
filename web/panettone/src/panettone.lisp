(defpackage panettone
  (:use :cl :klatre :easy-routes)
  (:import-from :defclass-std :defclass/std)
  (:import-from :alexandria :if-let)
  (:shadowing-import-from :alexandria :when-let)
  (:export :start-panettone :main))
(in-package :panettone)

(declaim (optimize (safety 3)))

;;;
;;; Data model
;;;

(defclass/std issue-comment ()
  ((body :type string)
   (author-dn :type string)))

(defclass/std issue (cl-prevalence:object-with-id)
  ((subject body :type string :std "")
   (author-dn :type string)
   (comments :std nil :type list :with-prefix)
   (created-at :type integer
               :std (get-universal-time))))

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
   (mapcan
    (lambda (field)
      (list field (car (ldap:attr-value entry field))))
    (list :mail
          :cn
          :displayname))))

(defun find-user/ldap (username)
  (declare (type (simple-array character (*)) username))
  (ldap:search
   *ldap*
   `(and (= objectClass organizationalPerson)
         (= cn ,username))
   ;; TODO(grfn): make this configurable
   :base "ou=users,dc=tvl,dc=fyi")
  (ldap:next-search-result *ldap*))

(defun find-user (username)
  (declare (type (simple-array character (*)) username))
  (ldap-entry->user
   (find-user/ldap username)))

(defun authenticate-user (user-or-username password)
  "Checks the given USER-OR-USERNAME has the given PASSWORD, by making a bind
request against the ldap server at *ldap*. Returns the user if authentication is
successful, `nil' otherwise"
  (let* ((user (if (typep user-or-username 'user) user-or-username
                   (find-user user-or-username)))
         (dn (dn user)))
    (multiple-value-bind (_r code-sym _msg)
        (ldap:bind
         (ldap:new-ldap :host (ldap:host *ldap*)
                        :port (ldap:port *ldap*)
                        :user dn
                        :pass password))
      (when (equalp code-sym 'trivial-ldap:success)
        user))))

;;;
;;; Persistence
;;;

(defvar *p-system* nil
  "The persistence system for this instance of Panettone")

(define-condition issue-not-found (error)
  ((id :type integer
       :initarg :id
       :reader not-found-id
       :documentation "ID of the issue that was not found"))
  (:documentation
   "Error condition for when an issue requested by ID is not
  found"))

(defun get-issue (system id)
  (restart-case
      (or
       (cl-prevalence:find-object-with-id system 'issue id)
       (error 'issue-not-found :id id))
    (different-id (new-id)
      :report "Use a different issue ID"
      :interactive (lambda ()
                     (format t "Enter a new ID: ")
                     (multiple-value-list (eval (read))))
      (get-issue system new-id))))

(defun list-issues (system)
  (cl-prevalence:find-all-objects system 'issue))

(defun create-issue (system &rest attrs)
  (cl-prevalence:tx-create-object
   system
   'issue
   (chunk-list 2 attrs)))

(defun add-comment (system issue-id &rest attrs)
  "Add a comment with the given ATTRS to the issue ISSUE-ID, and return the
updated issue"
  (let* ((comment (apply #'make-instance 'issue-comment attrs))
         (issue (get-issue system issue-id))
         (comments (append (issue-comments issue)
                           (list comment))))
    (cl-prevalence:tx-change-object-slots
     system
     'issue
     issue-id
     `((comments ,comments)))
    (setf (slot-value issue 'comments) comments)
    comments))

(defun initialize-persistence (data-dir)
  "Initialize the Panettone persistence system, storing data in DATA-DIR"
  (ensure-directories-exist data-dir)
  (setq *p-system* (cl-prevalence:make-prevalence-system data-dir))

  (when (null (list-issues *p-system*))
    (cl-prevalence:tx-create-id-counter *p-system*)))

;;;
;;; Views
;;;

(defvar *title* "Panettone")

(setf (who:html-mode) :HTML5)

(defmacro render (&body body)
  `(who:with-html-output-to-string (*standard-output* nil :prologue t)
     (:head
      (:title (who:esc *title*)))
     (:body ,@body)))

(defun render/login (&optional message)
  (render
    (:h1 "Login")
    (when message
      (who:htm (:div.alert (who:esc message))))
    (:form
     :method :post :action "/login"
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
             :value "Submit"))))

(defun render/index (&key issues)
  (render
    (:h1 "Issues")
    (:a :href "/issues/new" "New Issue")
    (:ul
     (loop for issue in issues
           do (who:htm
               (:li
                (:a :href (format nil "/issues/~A" (cl-prevalence:get-id issue))
                    (who:esc (subject issue)))))))))

(defun render/new-issue ()
  (render
    (:h1 "New Issue")
    (:form
     :method :post :action "/issues"
     (:div
      (:label :for "subject" "Subject")
      (:input :type :text
              :id "subject"
              :name "subject"
              :placeholder "Subject"))

     (:div
      (:textarea :name "body"))

     (:input :type :submit
             :value "Create Issue"))))

(defun render/issue (issue)
  (declare (type issue issue))
  (render
    (:h1 (who:esc (subject issue)))
    (:div (who:esc (body issue)))))

(defun render/not-found (entity-type)
  (render
    (:h1 (who:esc entity-type) "Not Found")))

;;;
;;; HTTP handlers
;;;

(defvar *user* nil)

(defun @auth (next)
  (if-let ((*user* (hunchentoot:session-value 'user)))
    (funcall next)
    (hunchentoot:redirect "/login")))

(defroute login-form ("/login" :method :get) ()
  (if (hunchentoot:session-value 'user)
      (hunchentoot:redirect "/")
      (render/login)))

(defroute submit-login ("/login" :method :post)
    (&post username password)
  (if-let ((user (authenticate-user username password)))
    (progn
      (setf (hunchentoot:session-value 'user) user)
      (hunchentoot:redirect "/"))
    (render/login "Invalid credentials")))

(defroute index ("/" :decorators (@auth)) ()
  (let ((issues (list-issues *p-system*)))
    (render/index :issues issues)))

(defroute new-issue ("/issues/new" :decorators (@auth)) ()
  (render/new-issue))

(defroute handle-create-issue
    ("/issues" :method :post :decorators (@auth))
    (&post subject body)
  (cl-prevalence:execute-transaction
   (create-issue *p-system*
                 'subject subject
                 'body body
                 'author-dn (dn *user*)))
  (cl-prevalence:snapshot *p-system*)
  (hunchentoot:redirect "/"))

(defroute show-issue ("/issues/:id" :decorators (@auth))
    (&path (id 'integer))
  (handler-case
      (render/issue (get-issue *p-system* id))
    (issue-not-found (_)
      (render/not-found "Issue"))))

(defvar *acceptor* nil
  "Hunchentoot acceptor for Panettone's web server.")

(defun start-panettone (&key port data-dir
                          (ldap-host "localhost")
                          (ldap-port 389))
  (connect-ldap :host ldap-host
                :port ldap-port)
  (initialize-persistence data-dir)

  (setq *acceptor*
        (make-instance 'easy-routes:routes-acceptor :port port))
  (hunchentoot:start *acceptor*))

(defun integer-env (var &key default)
  (or
   (when-let ((str (uiop:getenvp var)))
     (try-parse-integer str))
   default))

(defun main ()
  (let ((port (integer-env "PANETTONE_PORT" :default 6161))
        (ldap-port (integer-env "LDAP_PORT" :default 389))
        (data-dir (or (uiop:getenvp "PANETTONE_DATA_DIR") "/tmp/panettone")))
    (start-panettone :port port
                     :data-dir data-dir)
    (sb-thread:join-thread
     (find-if (lambda (th)
                (string= (sb-thread:thread-name th)
                         (format nil "hunchentoot-listener-*:~A" port)))
              (sb-thread:list-all-threads)))))

(comment
 (start-panettone :port 6161
                  :data-dir "/tmp/panettone"
                  :ldap-port 3899)
 )
