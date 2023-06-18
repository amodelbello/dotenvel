;;; tests.el --- summary -*- lexical-binding: t -*-

;; Author: Amo DelBello
;; Maintainer: Amo DelBello
;; Version: 1.0.0
;; Package-Requires: (dotenvel)
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

;; This packages provides the tests for `ert'.
;; Note: you must run the tests from this file after buffer evaluation.
;; Otherwise the relative paths below will not work.

;;; Code:

;; dotenvel-file-contents-to-list
(ert-deftest dotenvel-file-contents-to-list-test ()
  "Test that a file is loaded and split by newlines."
  (setq dotenvel-env-filepath ".env-normal")
  (setq result (dotenvel-file-contents-to-list))
  (should (= 2 (length result)))

  (setq dotenvel-env-filepath ".env-comments")
  (setq result (dotenvel-file-contents-to-list))
  (should (= 3 (length result))))


;; dotenvel-filter-entries
(ert-deftest dotenvel-filter-entries-test ()
  "Test that comments are filtered out."
  (setq lst '("TEST=one"
              "# Comment"))
  (setq result (dotenvel-filter-entries lst))
  (should (= 1 (length result)))

  (setq lst2 '("TEST=one"
               "NOT=a comment" ))
  (setq result (dotenvel-filter-entries lst2))
  (should (= 2 (length result))))


;; dotenvel-parse-entries
(ert-deftest dotenvel-parse-entries-test ()
  "Test that spaces and quotes are removed."
  (setq lst '(" TEST = one"
              "TEST2= two"
              "TEST3 =\"three\""
              "TEST4= 'four'"))
  (setq want (list '("TEST" "one")
                   '("TEST2" "two")
                   '("TEST3" "three")
                   '("TEST4" "four")))
  (setq got (dotenvel-parse-entries lst))
  (should (equal got want)))


;; dotenvel-get
(ert-deftest dotenvel-get ()
  "Test that you can get an env variable from a loaded config."
  (setq old-env-filepath dotenvel-env-filepath)
  (setq dotenvel-env-filepath ".env-comments")
  (dotenvel-load)
  (should (string-equal "does not exist" (dotenvel-get "OTHER" "does not exist")))
  (should (string-equal "test-val" (dotenvel-get "TEST_VAL" "default")))
  (should (string-equal "test-val2" (dotenvel-get "TEST_VAL2" "default")))

  ;; reload real config
  (setq dotenvel-env-filepath old-env-filepath)
  (dotenvel-load))

;;; tests.el ends here
