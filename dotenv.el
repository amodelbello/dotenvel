;;; dotenv.el --- Dotenv for config variables -*- lexical-binding: t -*-

;; Author: Amo DelBello
;; Maintainer: Amo DelBello
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/amodelbello/dotenv.el
;; Keywords: convenience, dotenv, environment, configuration


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

;; dotenv is a package that allows the use of environment variables
;; stored in a .env file, similar to a technique used in the node.js ecosystem.
;;
;; VARIABLES:
;;
;; dotenv-env-filepath  : Path to .env file (defaults to .emacs.d/.env)
;; dotenv-env           : alist that stores environment variables
;;
;; COMMANDS:
;; dotenv-load             : Loads the .env file into dotenv-env
;;
;; FUNCTIONS:
;; dotenv-get              : Takes a string parameter and returns corresponding value

;;; Code:

(defvar dotenv-env-filepath (format "%s%s" user-emacs-directory ".env")
  "The path to the .env file.")

(defvar dotenv-env '()
  "The alist that stores .env variables.")

(defun dotenv-get-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun dotenv-file-contents-to-list ()
  "Read the contents of env file and split elements into a list."
  (split-string
   (dotenv-get-file-contents dotenv-env-filepath)
   "\\(\n+\\)"))

(defun dotenv-filter-entries (lst)
  "Filter out commented lines in LST."
  (seq-filter
   (lambda (item) (not (string-prefix-p "#" item)))
   lst))

(defun dotenv-parse-entries (lst)
  "Split the strings in LST by equal signs and trim whitespace and quotes."
  (mapcar (lambda (entry)
            (let ((quoteless (replace-regexp-in-string "['\"]" "" entry)))
              (mapcar #'string-trim
                      (split-string quoteless "="))))
          lst))

(defun dotenv-load ()
  "Load the values from .env file."
  (interactive)
  (setq dotenv-env (dotenv-parse-entries
                    (dotenv-filter-entries
                     (dotenv-file-contents-to-list)))))

(defun dotenv-get (field &optional default)
  "Get the value of FIELD from dotenv-env.
Use DEFAULT if no value is found."
  (or
   (car (cdr (assoc field dotenv-env)))
   default))

(provide 'dotenv)

;;; dotenv.el ends here
