;;; dot-env.el --- Dotenv for config variables -*- lexical-binding: t -*-

;; Author: Amo DelBello
;; Maintainer: Amo DelBello
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/amodelbello/dot-env.el
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

;; dot-env is a package that allows the use of environment variables
;; stored in a .env file, similar to a technique used in the node.js ecosystem.
;;
;; VARIABLES:
;;
;; dot-env-filepath  : Path to .env file (defaults to .emacs.d/.env)
;; dot-env-environment           : alist that stores environment variables
;;
;; COMMANDS:
;; dot-env-load             : Loads the .env file into dot-env-environment
;;
;; FUNCTIONS:
;; dot-env-get              : Takes a string parameter and returns corresponding value

;;; Code:

(defvar dot-env-filepath (format "%s%s" user-emacs-directory ".env")
  "The path to the .env file.")

(defvar dot-env-environment '()
  "The alist that stores .env variables.")

(defun dot-env-get-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun dot-env-file-contents-to-list ()
  "Read the contents of env file and split elements into a list."
  (split-string
   (dot-env-get-file-contents dot-env-filepath)
   "\\(\n+\\)"))

(defun dot-env-filter-entries (lst)
  "Filter out commented lines in LST."
  (seq-filter
   (lambda (item) (not (string-prefix-p "#" item)))
   lst))

(defun dot-env-parse-entries (lst)
  "Split the strings in LST by equal signs and trim whitespace and quotes."
  (mapcar (lambda (entry)
            (let ((quoteless (replace-regexp-in-string "['\"]" "" entry)))
              (mapcar #'string-trim
                      (split-string quoteless "="))))
          lst))

(defun dot-env-load ()
  "Load the values from .env file."
  (interactive)
  (setq dot-env-environment (dot-env-parse-entries
                             (dot-env-filter-entries
                              (dot-env-file-contents-to-list)))))

(defun dot-env-get (field &optional default)
  "Get the value of FIELD from dot-env-environment.
Use DEFAULT if no value is found."
  (or
   (car (cdr (assoc field dot-env-environment)))
   default))

(provide 'dot-env)

;;; dot-env.el ends here
