(in-package :panettone.authentication)

(defvar *user* nil
  "The currently logged-in user")

(defvar *ldap* nil
  "The ldap connection")

(defclass/std user ()
  ((cn dn mail displayname :type string)))

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
      (let ((code-sym
              (nth-value 1 (ldap:bind
                            (ldap:new-ldap :host (ldap:host *ldap*)
                                           :port (ldap:port *ldap*)
                                           :user dn
                                           :pass password)))))
        (when (equalp code-sym 'trivial-ldap:success)
          user)))))
