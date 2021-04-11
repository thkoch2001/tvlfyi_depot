(in-package :panettone.inline-markdown)
(declaim (optimize (safety 3)))

(define-constant +inline-markup-types+
  '(("~~" :del)
    ("*"  :em)
    ("`"  :code))
  :test #'equal)

(defun next-token (mkdn &optional (escaped nil))
  "Parses and returns the next token from the beginning of
  an inline markdown string which is not altered. The resulting
  tokens are either :normal (normal text), :special (syntactically
  significant) or :escaped (escaped using \\). If the string is
  empty, a pseudo-token named :endofinput is returned. Return value
  is a list where the first element is the token type, the second
  the token content and optionally the third the markup type."
  ; special tokens are syntactically significant characters
  ; or strings for our inline markdown subset. “normal” tokens
  ; the strings in between
  (let* ((special-toks #.'(cons (list "\\" :escape) +inline-markup-types+))
         (toks (loop
                 for tok in special-toks
                 for pos = (search (car tok) mkdn)
                 when pos collect (cons tok pos)))
         (next-tok
           (unless (null toks)
             (reduce (lambda (a b) (if (< (cdr a) (cdr b)) a b)) toks))))
    (cond
      ; end of input
      ((= (length mkdn) 0) (list :endofinput ""))
      ; no special tokens, just return entire string
      ((null next-tok) (list :normal mkdn))
      ; special token, but not at the beginning of the string
      ; so we return everything until the special token as
      ; a string
      ((> (cdr next-tok) 0) (list :normal (subseq mkdn 0 (cdr next-tok))))
      ; \ at the beginning of the string: we get the next
      ; token and mark it as escaped unless we are already
      ; escaping in which case we just return the backslash
      ; as a special token
      ((eq (cadr (car next-tok)) :escape)
       (if escaped
         (list :special "\\")
         (list :escaped
               (next-token (subseq mkdn 1) t))))
      ; any other special token at the beginning of the string
      ; here we also pass the markup type as a third list element
      ; to prevent unnecessesary lookups
      (t (list :special
               (subseq mkdn 0 (length (car (car next-tok))))
               (cadr (car next-tok)))))))

(defun token-length (tok-type tok-str)
  "Returns the string length consumed by a call
  to next-token returning the given token type and string."
  (check-type tok-type symbol)
  (if (eq tok-type :escaped)
    ; backslash + length of escaped token
    (progn
      (check-type tok-str list)
      (1+ (token-length (car tok-str) (cadr tok-str))))
    (progn
      (check-type tok-str string)
      (length tok-str))))

(defun write-tag (tag pos &optional (target *standard-output*))
  "Wrapper around who:convert-tag-to-string-list to
  only output a single :opening or :closing tag."
  (check-type tag symbol)
  (check-type pos symbol)
  (let
    ((index
       (cond
         ((eq pos :opening) 0)
         ((eq pos :closing) 3)
         (t (error 'simple-type-error)))))
    (dolist
      (tag-part (subseq
                  (who:convert-tag-to-string-list tag nil nil nil)
                  index (+ index 3)))
      (write-string tag-part target))))

(defun render-inline-markdown (s &optional (target *standard-output*) (in :normal))
  "Render inline markdown, a subset of markdown safe to render
  inside inline elements. The resulting html is directly written
  to a specified stream or *standard-output* to integrate well
  with cl-who."
  (check-type s string)
  (check-type target stream)
  (loop
    for (tok-type tok-str tok-markup) = (next-token s)
    do (setq s (subseq s (token-length tok-type tok-str)))
    when (eq tok-type :endofinput)
    return ""
    when (eq tok-type :normal)
    do (write-string (who:escape-string tok-str) target)
    when (eq tok-type :escaped)
    do (progn
         ; if normal tokens are escaped we treat the \ as if it were \\
         ;
         ; TODO(sterni): maybe also use the :normal behavior in :code except for #\`.
         (when (eq (car tok-str) :normal)
           (write-char #\\ target))
         (write-string (who:escape-string (cadr tok-str)) target))
    when (eq tok-type :special)
    do (cond
         ; we are on the outer level and encounter a special token:
         ; render surrounding tags and call ourselves to render
         ; inner content.
         ((eq in :normal)
          (progn
            (write-tag tok-markup :opening target)
            (setq s (render-inline-markdown s target tok-markup))
            (write-tag tok-markup :closing target)))
         ; we are on the inner level and encounter the token that initiated
         ; our markup again, meaning we need to return to the outer level.
         ; we return the remaining string to be consumed.
         ((eq in tok-markup) (return s))
         ; remaining case: we are on the inner level and encounter different markup.

         ; we don't support nested markup for simplicity reasons, so instead we
         ; just render any nested markdown tokens as if they were escaped. This
         ; only eliminates the slight use case for nesting :em inside :del, but
         ; shouldn't be too bad. As a side effect this is the precise behavior
         ; we want for :code.
         ;
         ; TODO(sterni): maybe bring back the restart-based system which allowed
         ;               to skip nested tokens if desired.
         (t (write-string (who:escape-string tok-str) target)))))
