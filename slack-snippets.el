;;; private/grfn/slack-snippets.el -*- lexical-binding: t; -*-

(require 'dash)
(require 'dash-functional)
(require 'request)

;;;
;;; Configuration
;;;

(defvar slack/token nil
  "Legacy (https://api.slack.com/custom-integrations/legacy-tokens) access token")

(defvar slack/include-public-channels 't
  "Whether or not to inclue public channels in the list of conversations")

(defvar slack/include-private-channels 't
  "Whether or not to inclue public channels in the list of conversations")

(defvar slack/include-im 't
  "Whether or not to inclue IMs (private messages) in the list of conversations")

(defvar slack/include-mpim nil
  "Whether or not to inclue multi-person IMs (multi-person private messages) in
  the list of conversations")

;;;
;;; Utilities
;;;

(defmacro comment (&rest _body)
  "Comment out one or more s-expressions"
  nil)

(defun ->list (vec) (append vec nil))

(defun json-truthy? (x) (and x (not (equal :json-false x))))

;;;
;;; Generic API integration
;;;

(defvar slack/base-url "https://slack.com/api")

(defun slack/get (path params &optional callback)
  "params is an alist of query parameters"
  (let* ((params-callback (if (functionp params) `(() . ,params) (cons params callback)))
         (params (car params-callback)) (callback (cdr params-callback))
         (params (append `(("token" . ,slack/token)) params))
         (url (concat (file-name-as-directory slack/base-url) path)))
    (request url
             :type "GET"
             :params params
             :parser 'json-read
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (funcall callback data))))))

(defun slack/post (path params &optional callback)
  (let* ((params-callback (if (functionp params) `(() . ,params) (cons params callback)))
         (params (car params-callback)) (callback (cdr params-callback))
         (url (concat (file-name-as-directory slack/base-url) path)))
    (request url
             :type "POST"
             :data (json-encode params)
             :headers `(("Content-Type"  . "application/json")
                        ("Authorization" . ,(format "Bearer %s" slack/token)))
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (funcall callback data))))))


;;;
;;; Specific API endpoints
;;;

;; Users

(defun slack/users (cb)
  "Returns users as (id . name) pairs"
  (slack/get
   "users.list"
   (lambda (data)
     (->> data
          (assoc-default 'members)
          ->list
          (-map (lambda (user)
                  (cons (assoc-default 'id user)
                        (assoc-default 'real_name user))))
          (-filter #'cdr)
          (funcall cb)))))

(comment
 (slack/get
  "users.list"
  (lambda (data) (setq response-data data)))

 (slack/users (lambda (data) (setq --users data)))

 )

;; Conversations

(defun slack/conversation-types ()
  (->>
   (list (when slack/include-public-channels  "public_channel")
         (when slack/include-private-channels "private_channel")
         (when slack/include-im               "im")
         (when slack/include-mpim             "mpim"))
   (-filter #'identity)
   (s-join ",")))

(defun channel-label (chan users-alist)
  (cond
   ((json-truthy? (assoc-default 'is_channel chan))
    (format "#%s" (assoc-default 'name chan)))
   ((json-truthy? (assoc-default 'is_im chan))
    (let ((user-id (assoc-default 'user chan)))
      (format "Private message with %s" (assoc-default user-id users-alist))))
   ((json-truthy? (assoc-default 'is_mpim chan))
    (->> chan
         (assoc-default 'purpose)
         (assoc-default 'value)))))

(defun slack/conversations (cb)
  "Calls `cb' with (id . '((label . \"label\") '(topic . \"topic\") '(purpose . \"purpose\"))) pairs"
  (slack/get
   "conversations.list"
   `(("types"            . ,(slack/conversation-types))
     ("exclude-archived" . "true"))
   (lambda (data)
     (setq --data data)
     (slack/users
      (lambda (users)
        (->> data
             (assoc-default 'channels)
             ->list
             (-map
              (lambda (chan)
                (cons (assoc-default 'id chan)
                      `((label   . ,(channel-label chan users))
                        (topic   . ,(->> chan
                                         (assoc-default 'topic)
                                         (assoc-default 'value)))
                        (purpose . ,(->> chan
                                         (assoc-default 'purpose)
                                         (assoc-default 'value)))))))
             (funcall cb)))))))

(comment
 (slack/get
  "conversations.list"
  '(("types" . "public_channel,private_channel,im,mpim"))
  (lambda (data) (setq response-data data)))

 (slack/get
  "conversations.list"
  '(("types" . "im"))
  (lambda (data) (setq response-data data)))

 (slack/conversations
  (lambda (convos) (setq --conversations convos)))

 )

;; Messages

(cl-defun slack/post-message
    (&key text channel-id (on-success #'identity))
  (slack/post "chat.postMessage"
              `((text    . ,text)
                (channel . ,channel-id)
                (as_user . t))
              on-success))

(comment

 (slack/post-message
  :text "hi slackbot"
  :channel-id slackbot-channel-id
  :on-success (lambda (data) (setq resp data)))

 )

;;;
;;; Posting code snippets to slack
;;;

(defun prompt-for-channel (cb)
  (slack/conversations
   (lambda (conversations)
     (ivy-read
      "Select channel: "
      ;; TODO want to potentially use purpose / topic stuff here
      (->> conversations
           (-filter (lambda (c) (assoc-default 'label (cdr c))))
           (-map (lambda (chan) (let ((label (assoc-default 'label (cdr chan)))
                                 (id (car chan)))
                             (propertize label 'channel-id id)))))
      :history 'slack/channel-history
      :action (lambda (selected)
                (let ((channel-id (get-text-property 0 'channel-id selected)))
                  (funcall cb channel-id)
                  (message "Sent message to %s" selected))))))
  nil)

(comment
 (prompt-for-channel #'message)
 (->> --convos
      (-filter (lambda (c) (assoc-default 'label (cdr c))))
      (-map (lambda (chan) (let ((label (assoc-default 'label (cdr chan)))
                       (id (car chan)))
                   (propertize label 'channel-id id)))))

 (->> --convos (car) (cdr) (assoc-default 'label))
 )

(defun slack-send-code-snippet (&optional snippet-text)
  (interactive
   (list (buffer-substring-no-properties (mark) (point))))
  (prompt-for-channel
   (lambda (channel-id)
     (slack/post-message
      :text       (format "```\n%s```" snippet-text)
      :channel-id channel-id))))

(provide 'slack-snippets)
