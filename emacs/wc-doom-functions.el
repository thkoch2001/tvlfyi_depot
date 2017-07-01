(defun make-xpm-bar (color height width)
  "Create an XPM bitmap of a bar."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (create-image
      (concat
       (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
               (length (car data))
               (length data)
               color
               color)
       (apply #'concat
              (cl-loop with idx = 0
                       with len = (length data)
                       for dl in data
                       do (cl-incf idx)
                       collect
                       (concat "\""
                               (cl-loop for d in dl
                                        if (= d 0) collect (string-to-char " ")
                                        else collect (string-to-char "."))
                               (if (eq idx len) "\"};" "\",\n")))))
      'xpm t :ascent 'center))))

(defun my-tabbar-display-tab (tab)
    (let ((label (if tabbar--buffer-show-groups
                     (format "[%s]" (tabbar-tab-tabset tab))
                   (format "%s" (tabbar-tab-value tab))))
          (bar-color "#51afef")
          (bar-height 25)
          (bar-width 3)
          (selected-p (eq tab (tabbar-selected-tab (tabbar-current-tabset)))))
      (concat (when (and (display-graphic-p) selected-p)
                (make-xpm-bar bar-color bar-height bar-width))
              " "
              (if tabbar-auto-scroll-flag
                  label
                (tabbar-shorten
                 label (max 1 (/ (window-width)
                                 (length (tabbar-view
                                          (tabbar-current-tabset)))))))
              " ")))

(setq tabbar-tab-label-function #'my-tabbar-display-tab)
