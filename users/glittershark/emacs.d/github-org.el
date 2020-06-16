;;; ~/.doom.d/github-org.el -*- lexical-binding: t; -*-

(require 'ghub)

(defun grfn/alist->plist (alist)
  (->> alist
       (-mapcat (lambda (pair)
                  (list (intern (concat ":" (symbol-name (car pair))))
                        (cdr pair))))))

;;;

(cl-defstruct pull-request url number title author repository)

(defun grfn/query-pulls (query)
  (let ((resp (ghub-graphql "query reviewRequests($query: String!) {
    reviewRequests: search(
      type:ISSUE,
      query: $query,
      first: 100
    ) {
      issueCount
      nodes {
        ... on PullRequest {
          url
          number
          title
          author {
            login
            ... on User { name }
          }
          repository {
            name
            owner { login }
          }
        }
      }
    }
  }" `((query . ,query)))))
    (->> resp
         (alist-get 'data)
         (alist-get 'reviewRequests)
         (alist-get 'nodes)
         (-map
          (lambda (pr)
            (apply
             #'make-pull-request
             (grfn/alist->plist pr)))))))

(defun grfn/requested-changes ())

(defun grfn/pull-request->org-headline (format-string level pr)
  (check-type format-string string)
  (check-type level integer)
  (check-type pr pull-request)
  (s-format (concat (make-string level ?*) " " format-string)
            #'aget
            `((author . ,(->> pr (pull-request-author) (alist-get 'name)))
              (owner . ,(->> pr (pull-request-repository)
                             (alist-get 'owner)
                             (alist-get 'login)))
              (repo . ,(->> pr (pull-request-repository) (alist-get 'name)))
              (pr-link . ,(org-make-link-string
                           (pull-request-url pr)
                           (pull-request-title pr)))
              (today . ,(format-time-string "%Y-%m-%d %a")))))

(defun grfn/org-headlines-from-review-requests (level)
  "Create org-mode headlines at LEVEL from all review-requested PRs on Github"
  (interactive "*nLevel: ")
  (let* ((prs (grfn/query-pulls
               "is:open is:pr review-requested:glittershark archived:false"))
         (text (mapconcat
                (apply-partially
                 #'grfn/pull-request->org-headline
                 "TODO Review ${author}'s PR on ${owner}/${repo}: ${pr-link} :pr:
SCHEDULED: <${today}>"
                 level) prs "\n")))
    (save-mark-and-excursion
      (insert text))
    (org-align-tags 't)))

(defun grfn/org-headlines-from-requested-changes (level)
  "Create org-mode headlines at LEVEL from all PRs with changes requested
 on Github"
  (interactive "*nLevel: ")
  (let* ((prs (grfn/query-pulls
               (concat "is:pr is:open author:glittershark archived:false "
                       "sort:updated-desc review:changes-requested")))
         (text (mapconcat
                (apply-partially
                 #'grfn/pull-request->org-headline
                 "TODO Address review comments on ${pr-link} :pr:
SCHEDULED: <${today}>"
                 level) prs "\n")))
    (save-mark-and-excursion
      (insert text))
    (org-align-tags 't)))
