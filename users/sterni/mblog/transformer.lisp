(in-package :mblog)
(declaim (optimize (safety 3)))

(defparameter +discard-tags-with-children+ '("HEAD"))
(defparameter +discard-tags-only+ '("BODY" "HTML"))

;; This is basically a reimplementation of cxml's proxy-handler.
;; Couldn't be bothered to make a broadcast-handler because I
;; only need to pass through to one handler.
(defclass hax-proxy-handler (hax:default-handler)
  ((next-handler
    :initarg :next-handler
    :accessor proxy-next-handler)))

(macrolet ((def-proxy-handler (name (&rest args))
             `(defmethod ,name ((h hax-proxy-handler) ,@args)
                (,name (proxy-next-handler h) ,@args))))
  (def-proxy-handler hax:start-document (name p-id s-id))
  (def-proxy-handler hax:end-document ())
  (def-proxy-handler hax:start-element (name attrs))
  (def-proxy-handler hax:end-element (name))
  (def-proxy-handler hax:characters (data))
  (def-proxy-handler hax:unescaped (data))
  (def-proxy-handler hax:comment (data)))

(defclass apple-note-transformer (hax-proxy-handler)
  ((cid-lookup
    :initarg :cid-lookup
    :initform (lambda (cid) nil)
    :accessor transformer-cid-lookup)
   (discard-until
    :initarg :discard-until
    :initform nil
    :accessor transformer-discard-until)))

;; Define the “boring” handlers which just call the next handler
;; (i. e. the serializing handler) with the event _unless_
;; discard-until is not nil in which case the event is dropped.
(macrolet ((def-filter-handler (name (&rest args))
             `(defmethod ,name ((h apple-note-transformer) ,@args)
                (when (not (transformer-discard-until h))
                  (call-next-method h ,@args)))))
  (def-filter-handler hax:start-document (name p-id s-id))
  (def-filter-handler hax:end-document ())
  (def-filter-handler hax:characters (data))
  (def-filter-handler hax:unescaped (data))
  (def-filter-handler hax:comment (data)))

(defun parse-content-id (attrlist)
  (when-let (data (find-if (lambda (x)
                             (string= (hax:attribute-name x) "DATA"))
                           attrlist))
    (multiple-value-bind (starts-with-cid-p suffix)
        (starts-with-subseq "cid:" (hax:attribute-value data)
                            :return-suffix t :test #'char=)
      (if starts-with-cid-p suffix data))))

(defmethod hax:start-element ((h apple-note-transformer) name attrs)
  (with-accessors ((discard-until transformer-discard-until)
                   (next-handler proxy-next-handler)
                   (cid-lookup transformer-cid-lookup))
      h
      (cond
        (discard-until nil)
        ((member name +discard-tags-with-children+ :test #'string=)
         (setf discard-until name))
        ((member name +discard-tags-only+ :test #'string=) nil)
        ((string= name "OBJECT")
         (progn
           (setf discard-until "OBJECT")
           (when-let* ((cid (parse-content-id attrs))
                       (file (apply cid-lookup (list cid)))
                       (src (hax:make-attribute "SRC" file)))
             (hax:start-element next-handler "IMG" (list src))
             (hax:end-element next-handler "IMG"))))
        (t (call-next-method h name attrs)))))

(defmethod hax:end-element ((h apple-note-transformer) name)
  (with-accessors ((discard-until transformer-discard-until))
      h
      (cond
        ((and discard-until (string= discard-until name))
         (setf discard-until nil))
        ((member name +discard-tags-only+ :test #'string=) nil)
        (t (call-next-method h name)))))
