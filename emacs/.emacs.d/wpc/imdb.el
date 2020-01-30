;;; imdb.el --- Internet Movie Database -*- lexical-binding: t -*-
;; Author: William Carroll <wpcarro@gmail.com>

;;; Commentary:
;; Some Elisp to help me pick movies more quickly.

;;; Code:

(require 'f)
(require 'macros)
(require 'pcre2el)
(require 'random)
(require 'maybe)

;; TODO: How do you support types herein?
(cl-defstruct movie
  name
  year
  director
  watched?)

;; TODO: Support famous directors like:
;; - Wes Anderson
;; - Woody Allen
;; - Tarantino
;; - Coen Brothers
;; - Alfonso Cauron
;; - Alejandro González Iñárritu
;; - Alfred Hitchcock
;; - Stanley Kubrick

;; TODO: Dump this into SQL.

(defconst imdb/kubrick-films
  (->> '((:watched? nil :year 1951 :name "Flying Padre")
         (:watched? nil :year 1953 :name "Fear and Desire")
         (:watched? nil :year 1953 :name "The Seafarers")
         (:watched? nil :year 1955 :name "Killer's Kiss")
         (:watched? nil :year 1956 :name "The Killing")
         (:watched? nil :year 1957 :name "Paths of Glory")
         (:watched? nil :year 1960 :name "Spartacus")
         (:watched? nil :year 1962 :name "Lolita")
         (:watched? nil :year 1964 :name "Dr. Strangelove")
         (:watched? nil :year 1968 :name "2001: A Space Odyssey")
         (:watched? t   :year 1971 :name "A Clockwork Orange")
         (:watched? nil :year 1975 :name "Barry Lyndon")
         (:watched? nil :year 1980 :name "The Shining")
         (:watched? t   :year 1987 :name "Full Metal Jacket")
         (:watched? nil :year 1999 :name "Eyes Wide Shut"))
       (list/map (lambda (x)
                   (make-movie :name (plist-get :name x)
                               :year (plist-get :year x)
                               :director "Stanley Kubrick"
                               :watched? (plist-get :watched? x))))))

(defconst imdb/non-top-250
  (->> '("Doctor Zhivago"
         )
       (list/map #'imdb/new-movie)))

(defun imdb/new-movie (name)
  "Create a new movie with NAME."
  (make-movie :name name
              :year nil
              :director nil
              :watched? nil))

(defun imdb/org->movie (line)
  "Parse an org LINE into a movie struct."
  (let ((match (s-match
                (pcre-to-elisp "^\*\*\s(TODO|DONE)\s(.+)$")
                line)))
    (if (maybe/some? match)
        (make-movie :name (list/get 2 match)
                    :year nil
                    :director nil
                    :watched? (equal "DONE" (list/get 1 match)))
      (error (s-concat "Parsing error: " line)))))

;; TODO: Store these in a database or define them herein.
(defun imdb/org->movies ()
  "Parse entire IMDB org file into movie structs."
  (->> "~/Dropbox/org/imdb_top_250.org"
       f-read
       (s-split "\n")
       (-drop 1)
       (list/filter (>> (s-starts-with? "** ")))
       (list/map #'imdb/org->movie)))

(defun imdb/watched? (movie)
  "Return t if MOVIE has been watched."
  (movie-watched? movie))

(defconst imdb/movies (imdb/org->movies)
  "Structs of all watched movies.")

(defun imdb/unwatched ()
  "Return list of unwatched movies."
  (->> imdb/movies
       (list/filter (lambda (x) (not (imdb/watched? x))))))

(defun imdb/name (movie)
  "Return name of MOVIE."
  (movie-name movie))


(defun imdb/suggest ()
  "Randomly select movie from unwatched list."
  (->> (imdb/unwatched)
       (random/choice)
       (imdb/name)))

(defun imdb/unwatched-list ()
  "Dump all unwatched movies into a list."
  (f-write-text (->> (imdb/unwatched)
                     (list/map #'imdb/name)
                     (s-join "\n"))
                'utf-8
                "/tmp/unwatched.txt"))

(macros/comment
 (imdb/org->movies)
 (imdb/unwatched-list)
 (imdb/suggest)
 )

(provide 'imdb)
;;; imdb.el ends here
