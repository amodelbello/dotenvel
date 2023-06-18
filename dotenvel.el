;;; dotenvel.el --- summary -*- lexical-binding: t -*-

;; Author: Amo DelBello
;; Maintainer: Amo DelBello
;; Version: 1.0.0
;; Package-Requires: ()
;; Homepage: https://github.com/amodelbello/dotenvel
;; Keywords: dotenv, environment, configuration


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; dotenvel is a package that allows the use of environment variables
;; stored in a .env file, similar to a technique used in the node.js ecosystem.
;;
;; VARIABLES:
;;
;; dotenvel-env-filepath  : Path to .env file (defaults to .emacs.d/.env)
;; dotenvel-env           : alist that stores environment variables
;;
;; COMMANDS:
;; dotenvel-load             : Loads the .env file into dotenvel-env
;;
;; FUNCTIONS:
;; dotenvel-get              : Takes a string parameter and returns corresponding value

;;; Code:

(defvar dotenvel-env-filepath (format "%s%s" user-emacs-directory ".env")
  "The path to the .env file.")

(defvar dotenvel-env '()
  "The alist that stores .env variables.")

(defun dotenvel-get-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun dotenvel-file-contents-to-list ()
  "Read the contents of env file and split elements into a list."
  (split-string
   (dotenvel-get-file-contents dotenvel-env-filepath)
   "\\(\n+\\)"))

(defun dotenvel-filter-entries (lst)
  "Filter out commented lines in LST."
  (seq-filter
   (lambda (item) (not (string-prefix-p "#" item)))
   lst))

(defun dotenvel-parse-entries (lst)
  "Split the strings in LST by equal signs and trim whitespace and quotes."
  (mapcar (lambda (entry)
            (let ((quoteless (replace-regexp-in-string "['\"]" "" entry)))
              (mapcar #'string-trim
                      (split-string quoteless "="))))
          lst))

(defun dotenvel-load ()
  "Load the values from .env file."
  (interactive)
  (setq dotenvel-env (dotenvel-parse-entries
                      (dotenvel-filter-entries
                       (dotenvel-file-contents-to-list)))))

(defun dotenvel-get (field &optional default)
  "Get the value of FIELD from dotenvel-env.
Use DEFAULT if no value is found."
  (or
   (car (cdr (assoc field dotenvel-env)))
   default))

(provide 'dotenvel)

;;; dotenvel.el ends here
