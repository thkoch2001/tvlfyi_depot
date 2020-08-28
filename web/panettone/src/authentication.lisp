(in-package :panettone.authentication)

(defvar *user* nil
  "The currently logged-in user")

(defvar *ldap* nil
  "The ldap connection")

(defvar *ldap-host* "localhost"
  "The host for the ldap connection")

(defvar *ldap-port* 389
  "The port for the ldap connection")

(defclass/std user ()
  ((cn dn mail displayname :type string)))

(defun connect-ldap (&key
                       (host "localhost")
                       (port 389))
  (setq *ldap-host* host
        *ldap-port* port
        *ldap* (ldap:new-ldap :host host :port port)))

(defun reconnect-ldap ()
  (setq *ldap* (ldap:new-ldap
                :host *ldap-host*
                :port *ldap-port*)))

(defmacro with-ldap ((&key (max-tries 1)) &body body)
  "Execute BODY in a context where ldap connection errors trigger a reconnect
and a retry"
  (with-gensyms (n try retry e)
    `(flet
         ((,try
              (,n)
            (flet ((,retry (,e)
                     (if (>= ,n ,max-tries)
                         (error ,e)
                         (progn
                           (reconnect-ldap)
                           (,try (1+ ,n))))))
              (handler-case
                  (progn
                    ,@body)
                (end-of-file (,e) (,retry ,e))
                (trivial-ldap:ldap-connection-error (,e) (,retry ,e))))))
       (,try 0))))

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
  (with-ldap ()
    (ldap:search
     *ldap*
     `(and (= objectClass organizationalPerson)
           (or
            (= cn ,username)
            (= dn ,username)))
     ;; TODO(grfn): make this configurable
     :base "ou=users,dc=tvl,dc=fyi")
    (ldap:next-search-result *ldap*)))

(defun find-user (username)
  (check-type username (simple-array character (*)))
  (when-let ((ldap-entry (find-user/ldap username)))
    (ldap-entry->user ldap-entry)))

(defun find-user-by-dn (dn)
  (with-ldap ()
    (progn
      (ldap:search *ldap* `(= objectClass organizationalPerson)
                   :base dn
                   :scope 'ldap:base)
      (when-let ((ldap-entry (ldap:next-search-result *ldap*)))
        (ldap-entry->user ldap-entry)))))

(comment
 (find-user-by-dn "cn=glittershark,ou=users,dc=tvl,dc=fyi")
 )

(defun authenticate-user (user-or-username password)
  "Checks the given USER-OR-USERNAME has the given PASSWORD, by making a bind
request against the ldap server at *ldap*. Returns the user if authentication is
successful, `nil' otherwise"
  (when-let ((user (if (typep user-or-username 'user) user-or-username
                       (find-user user-or-username))))
    (let ((dn (dn user)))
      (let ((code-sym
              (nth-value 1 (ldap:bind
                            (ldap:new-ldap :host (ldap:host *ldap*)
                                           :port (ldap:port *ldap*)
                                           :user dn
                                           :pass password)))))
        (when (equalp code-sym 'trivial-ldap:success)
          user)))))
