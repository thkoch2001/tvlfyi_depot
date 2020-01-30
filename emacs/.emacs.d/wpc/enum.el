;;; enum.el --- Enumerable protocol for Elisp -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Heavily influenced by Elixir.

;; I will not be implement every function in the Enum library, since I don't
;; need every function.  Some of the streaming functionality may prove difficult
;; to write in Elisp.  We shall see.

;; TODO: Implement the following functions:
;; - all?/2
;; - any?/2
;; - at/3
;; - chunk_by/2
;; - chunk_every/{2,3,4}
;; - chunk_while/4
;; - concat/1
;; - concat/2
;; - count/{1,2}
;; - dedup/1 # prefer calling this function dedupe
;; - dedup_by/2 # same as above
;; - drop/2
;; - drop_every/2
;; - drop_while/2
;; - each/2
;; - empty?/1
;; - fetch/2
;; - fetch!/2
;; - filter/2
;; - find/3
;; - find_index/2
;; - find_value/3
;; - flat_map/2
;; - flat_map_reduce/3
;; - group_by/3
;; - intersperse/2
;; - into/{2,3}
;; - join/2
;; - map/2
;; - map_every/3
;; - map_join/3
;; - map_reduce/3
;; - max/2
;; - max_by/3
;; - member?/2 # consider calling this contains?
;; - min/2
;; - min_by/2
;; - min_max/2 # This is a great function because of O(n) time.
;; - min_max_by/3
;; - random/1 # Consider just sample with num=1
;; - reduce/{2,3}
;; - reduce_while/3
;; - reject/2
;; - reverse/{1,2}
;; - reverse_slice/3
;; - scan/{2,3}
;; - shuffle/1
;; - slice/{2,3}
;; - sort/{1,2}
;; - sort/2
;; - sort_by/3
;; - split/2
;; - split_while/2
;; - split_with/2
;; - sum/1
;; - take/2
;; - take_every/2
;; - take_random/2 # prefer calling this function sample
;; - take_while/2
;; - to_list/1
;; - uniq/1 # prefer calling this unique
;; - uniq_by/2 # prefer calling this unique-by
;; - unzip/1
;; - with_index/2
;; - zip/{1,2}

;; TODO: Consider how to handle dispatching by type.

;; TODO: Which types should be supported herein?
;; - linked-lists
;; - associative-lists
;; - cycles

;; Warning: This module is a total work-in-progress, and it's quite possible
;; that I may never even finish it.

;;; Code:

(defun enum/count (xs)
  "Return the number of elements in `XS'."
  (cond
   ((alist/instance? xs) (alist/count xs))
   ((list/instance?  xs) (list/length xs)))
  )

(provide 'enum)
;;; enum.el ends here
