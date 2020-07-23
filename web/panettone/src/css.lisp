(in-package :panettone.css)
(declaim (optimize (safety 3)))

(defparameter color/black
  "rgb(24, 24, 24)")

(defparameter color/gray
  "#8D8D8D")

(defparameter color/primary
  "rgb(106, 154, 255)")

(defparameter color/primary-light
  "rgb(150, 166, 200)")

(defparameter color/success
  "rgb(168, 249, 166)")

(defparameter color/success-2
  "rgb(168, 249, 166)")

(defparameter color/failure
  "rgb(247, 167, 167)")

(defun button (selector)
  `((,selector
     :background-color ,color/success
     :padding "0.5rem"
     :text-decoration "none"
     :transition "box-shadow" "0.15s" "ease-in-out")

    ((:and ,selector :hover)
     :box-shadow "0.25rem" "0.25rem" "0" "0" "rgba(0,0,0,0.08)")

    ((:and ,selector (:or :active :focus))
     :box-shadow "0.1rem" "0.1rem" "0" "0" "rgba(0,0,0,0.05)"
     :outline "none"
     :border "none")))

(defparameter issue-list-styles
  `((.issue-list
     :list-style-type "none"
     :padding-left 0

     (.issue-subject
      :font-weight "bold")

     (li
      :padding-bottom "1rem")

     ((li + li)
      :border-top "1px" "solid" ,color/gray)

     (a
      :text-decoration "none"
      :display "block")

     ((:and a :hover)
      :outline "none"

      (.issue-subject
       :color ,color/primary)))

    (.comment-count
     :color ,color/gray)))

(defparameter comment-styles
  `((.issue-comments
     :border-top "1px" "solid" ,color/gray
     :padding-top "1rem"
     :padding-left "2rem"

     (.comment-info
      :color ,color/gray
      :margin 0
      :padding-top "1rem")

     (.comment
      :padding-top "1rem"
      :padding-bottom "1rem"
      :border-bottom "1px" "solid" ,color/gray

      (p :margin 0)))))

(defparameter form-styles
  `(((:or (:and input (:or (:= type "text")
                           (:= type "password")))
          textarea)
     :width "100%"
     :padding "0.5rem"
     :outline "none"
     :border-top "none"
     :border-left "none"
     :border-right "none"
     :border-bottom "1px" "solid" ,color/gray
     :margin-bottom "1rem")

    (textarea
     :resize "vertical")

    ((:and input (:= type "submit"))
     :-webkit-appearance "none"
     :border "none"
     :cursor "pointer")

    ,@(button '(:and input (:= type "submit")))))

(defparameter issue-styles
  `((.issue-info
     :display "flex"
     :justify-content "space-between"
     :align-items "center"

     (.close-issue
      :background-color ,color/failure))))

(defparameter styles
  `(,@form-styles
    ,@issue-list-styles
    ,@issue-styles
    ,@comment-styles

    (body
     :font-family "sans-serif"
     :color ,color/black)

    (a :color "inherit")

    (.content
     :width "800px"
     :margin "0 auto")

    (header
     :display "flex"
     :align-items "center"
     :border-bottom "1px" "solid" ,color/black
     :margin-bottom "1rem"

     (h1
      :padding 0
      :flex 1)

     (.issue-number
      :color ,color/gray
      :font-size "1.5rem"))

    ,@(button '.new-issue)

    (.alert
     :padding "0.5rem"
     :margin-bottom "1rem"
     :background-color ,color/failure)

    (.login-form
     :width "300px"
     :margin "0 auto")

    (.created-by-at
     :color ,color/gray)))
