(in-package :panettone.css)
(declaim (optimize (safety 3)))

(defun button (selector)
  `((,selector
     :background-color "var(--success)"
     :padding "0.5rem"
     :text-decoration "none"
     :transition "box-shadow" "0.15s" "ease-in-out")

    ((:and ,selector :hover)
     :box-shadow "0.25rem" "0.25rem" "0" "0" "rgba(0,0,0,0.08)")

    ((:and ,selector (:or :active :focus))
     :box-shadow "0.1rem" "0.1rem" "0" "0" "rgba(0,0,0,0.05)"
     :outline "none"
     :border "none")))

(defparameter markdown-styles
  `((blockquote
     :border-left "5px" "solid" "var(--light)"-gray
     :padding-left "1rem"
     :margin-left "0rem")
    (pre
     :overflow-x "auto")))

(defparameter issue-list-styles
  `((.issue-list
     :list-style-type "none"
     :padding-left 0

     (.issue-subject
      :font-weight "bold")

     (li
      :padding-bottom "1rem")

     ((li + li)
      :border-top "1px" "solid" "var(--gray)")

     (a
      :text-decoration "none"
      :display "block")

     ((:and a :hover)
      :outline "none"

      (.issue-subject
       :color "var(--primary)")))

    (.comment-count
     :color "var(--gray)")

    (.issue-links
     :display "flex"
     :flex-direction "row"
     :align-items "center"
     :justify-content "space-between"
     :flex-wrap "wrap")

    (.issue-search
     ((:and input (:= type "search"))
      :padding "0.5rem"
      :background-image "url('static/search.png')"
      :background-position "10px 10px"
      :background-repeat "no-repeat"
      :background-size "1rem"
      :padding-left "2rem"
      :border "1px" "solid" "var(--gray)"))))

(defparameter issue-history-styles
  `((.issue-history
     :list-style "none"
     :border-top "1px" "solid" "var(--gray)"
     :padding-top "1rem"
     :padding-left "2rem"

     (.comment-info
      :color "var(--gray)"
      :margin 0
      :padding-top "1rem"

      (a :text-decoration "none")
      ((:and a :hover)
       :text-decoration "underline"))

     ((:or .comment .event)
      :padding-top "1rem"
      :padding-bottom "1rem"
      :border-bottom "1px" "solid" "var(--gray)"

      (p :margin 0))

     ((:and (:or .comment .event) :target)
      :border-color "var(--primary)"
      :border-bottom-width "3px")

     (.event
      :color "var(--gray)"))))

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
     :border-bottom "1px" "solid" "var(--gray)"
     :margin-bottom "1rem")

    (textarea
     :resize "vertical")

    ((:and input (:= type "submit"))
     :-webkit-appearance "none"
     :border "none"
     :cursor "pointer"
     :font-size "1rem")

    ,@(button '(:and input (:= type "submit")))

    (.form-link
     ((:and input (:= type "submit"))
      :background-color "initial"
      :color "inherit"
      :padding 0
      :text-decoration "underline")

     ((:and input (:= type "submit")
            (:or :hover :active :focus))
      :box-shadow 0 0 0 0))

    (.form-group
     :margin-top "1rem")

    (label.checkbox
     :cursor "pointer")))

(defparameter issue-styles
  `((.issue-info
     :display "flex"
     :justify-content "space-between"
     :align-items "center"

     ,@(button '.edit-issue)

     (.created-by-at
      :flex 1)

     (.edit-issue
      :background-color "var(--light)"-gray
      :flex 0
      :margin-right "0.5rem")

     (.close-issue
      :background-color "var(--failure)"))))

(defparameter styles
  `(,@form-styles
    ,@issue-list-styles
    ,@issue-styles
    ,@issue-history-styles
    ,@markdown-styles

    (body
     :font-family "sans-serif"
     :color "var(--text)"
     :background "var(--bg)"
     :--text "rgb(24, 24, 24)"
     :--bg "white"
     :--gray "#8D8D8D"
     :--primary "rgb(106, 154, 255)"
     :--primary-light "rgb(150, 166, 200)"
     :--success "rgb(168, 249, 166)"
     :--failure "rgb(247, 167, 167)"
     :--light-gray "#EEE")

    (:media "(prefers-color-scheme: dark)"
      (body
        :--text "rgb(240, 240, 240)"
        :--bg "black"
        :--gray "#8D8D8D"
        :--primary "rgb(106, 154, 255)"
        :--primary-light "rgb(150, 166, 200)"
        :--success "rgb(14, 130, 11)"
        :--failure "rgb(124, 14, 14)"
        :--light-gray "#222"))

    (a :color "inherit")

    (.content
     :max-width "800px"
     :margin "0 auto")

    (header
     :display "flex"
     :align-items "center"
     :border-bottom "1px" "solid" "var(--text)"
     :margin-bottom "1rem"

     (h1
      :padding 0
      :flex 1)

     (.issue-number
      :color "var(--gray)"
      :font-size "1.5rem"))

    (nav
     :display "flex"
     :color "var(--gray)"
     :justify-content "space-between"

     (.nav-group
      :display "flex"
      (>*
       :margin-left "0.5rem")))

    (footer
     :border-top "1px" "solid" "var(--gray)"
     :padding-top "1rem"
     :margin-top "1rem"
     :color "var(--gray)")

    ,@(button '.new-issue)

    (.alert
     :padding "0.5rem"
     :margin-bottom "1rem"
     :background-color "var(--failure)")

    (.login-form
     :max-width "300px"
     :margin "0 auto")

    (.created-by-at
     :color "var(--gray)")

    ;; screen-reader-only content
    (.sr-only
     :border 0
     :clip "rect(0 0 0 0)"
     :height "1px"
     :margin "-1px"
     :overflow "hidden"
     :padding 0
     :position "absolute"
     :width "1px")))
