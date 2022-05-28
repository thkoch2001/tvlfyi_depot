(in-package :panettone.authentication)

(defvar *user* nil
  "The currently logged-in user")

(defclass/std user ()
  ((cn dn mail displayname :type string)))

;; Migrating user authentication to OAuth2 necessitates some temporary
;; workarounds while other parts of the panettone code are being
;; amended appropriately.

(defun fake-dn (username)
  "Users are no longer read directly from LDAP, but everything in
panettone is keyed on the DNs. This function constructs matching
'fake' DNs."
  (format nil "cn=~A,ou=users,dc=tvl,dc=fyi" username))

(defun find-user-by-dn (dn)
  "Previously this function looked up users in LDAP based on their DN,
however panettone now does not have direct access to a user database.

For most cases only the username is needed, which can be parsed out of
the user, however email addresses are temporarily not available."
  (let ((username
          (car (uiop:split-string (subseq dn 3) :separator '(#\,)))))
    (make-instance
     'user
     :dn dn
     :cn username
     :displayname username
     :mail nil)))

;; Implementation of standard OAuth2 authorisation flow.

(defvar *oauth2-auth-endpoint*
  (or (uiop:getenv "OAUTH2_AUTH_ENDPOINT")
      "https://auth.tvl.fyi/auth/realms/TVL/protocol/openid-connect/auth")

  "OAuth2 authorization endpoint URL")

(defvar *oauth2-token-endpoint*
  (or (uiop:getenv "OAUTH2_TOKEN_ENDPOINT")
      "https://auth.tvl.fyi/auth/realms/TVL/protocol/openid-connect/token")

  "OAuth2 token endpoint URL")

(defvar *oauth2-client-id*
  (or (uiop:getenv "OAUTH2_CLIENT_ID") "panettone"))

(defvar *oauth2-client-secret*
  (uiop:getenv "OAUTH2_CLIENT_SECRET"))

(defvar *oauth2-redirect-uri*
  (or (uiop:getenv "OAUTH2_REDIRECT_URI")
      "https://b.tvl.fyi/auth"))

(defun auth-url ()
  (format nil "~A?response_type=code&client_id=~A&redirect_uri=~A"
          *oauth2-auth-endpoint*
          (drakma:url-encode *oauth2-client-id* :utf-8)
          (drakma:url-encode *oauth2-redirect-uri* :utf-8)))

(defun claims-to-user (claims)
  (let ((username (cdr (assoc :preferred--username claims)))
        (email (cdr (assoc :email claims))))
    (make-instance
     'user
     :dn (fake-dn username)
     :cn username
     :mail email
     ;; TODO(tazjin): Figure out actual displayName mapping in tokens.
     :displayname username)))

(defun fetch-token (code)
  "Fetches the access token on completion of user authentication through
the OAuth2 endpoint and returns the resulting user object."

  (multiple-value-bind (body status)
      (drakma:http-request *oauth2-token-endpoint*
                           :method :post
                           :parameters `(("grant_type" . "authorization_code")
                                         ("client_id" . ,*oauth2-client-id*)
                                         ("client_secret" . ,*oauth2-client-secret*)
                                         ("redirect_uri" . ,*oauth2-redirect-uri*)
                                         ("code" . ,code))
                           :external-format-out :utf-8
                           :want-stream t)
    (if (/= status 200)
        (error "Authentication failed: ~A (~A)~%"
               (alexandria:read-stream-content-into-string body)
               status)

        ;; Returned JWT contains username and email, we can populate
        ;; all fields from that.
        (progn
          (setf (flexi-streams:flexi-stream-external-format body) :utf-8)
          (let* ((response (cl-json:decode-json body))
                 (access-token (cdr (assoc :access--token response)))
                 (payload (cadr (uiop:split-string access-token :separator '(#\.))))
                 (claims (cl-json:decode-json-from-string
                          (base64:base64-string-to-string payload))))
            (claims-to-user claims))))))
