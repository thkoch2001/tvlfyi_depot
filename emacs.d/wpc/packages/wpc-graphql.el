;;; wpc-graphql.el --- Packages related to GraphQL -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Hosts various packages and functions to make working with GraphQL more
;; enjoyable.

;;; Code:

(quelpa '(graphql.el
          :fetcher github
          :repo "vermiculus/graphql.el"))
(require 'graphql.el)


(provide 'wpc-graphql)
;;; wpc-graphql.el ends here
