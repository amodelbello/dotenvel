;;; tests.el --- Tests for dot-env -*- lexical-binding: t -*-

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

;; This packages provides the tests for `ert'.
;; Note: you must run the tests from this file after buffer evaluation.
;; Otherwise the relative paths below will not work.

;;; Code:

;; dot-env-file-contents-to-list
(ert-deftest dot-env-file-contents-to-list-test ()
  "Test that a file is loaded and split by newlines."
  (setq dot-env-filepath ".env-normal")
  (setq result (dot-env-file-contents-to-list))
  (should (= 2 (length result)))

  (setq dot-env-filepath ".env-comments")
  (setq result (dot-env-file-contents-to-list))
  (should (= 3 (length result))))


;; dot-env-filter-entries
(ert-deftest dot-env-filter-entries-test ()
  "Test that comments are filtered out."
  (setq lst '("TEST=one"
              "# Comment"))
  (setq result (dot-env-filter-entries lst))
  (should (= 1 (length result)))

  (setq lst2 '("TEST=one"
               "NOT=a comment" ))
  (setq result (dot-env-filter-entries lst2))
  (should (= 2 (length result))))


;; dot-env-parse-entries
(ert-deftest dot-env-parse-entries-test ()
  "Test that spaces and quotes are removed."
  (setq lst '(" TEST = one"
              "TEST2= two"
              "TEST3 =\"three\""
              "TEST4= 'four'"))
  (setq want (list '("TEST" "one")
                   '("TEST2" "two")
                   '("TEST3" "three")
                   '("TEST4" "four")))
  (setq got (dot-env-parse-entries lst))
  (should (equal got want)))


;; dot-env-get
(ert-deftest dot-env-get ()
  "Test that you can get an env variable from a loaded config."
  (setq old-env-filepath dot-env-filepath)
  (setq dot-env-filepath ".env-comments")
  (dot-env-load)
  (should (string-equal "does not exist" (dot-env-get "OTHER" "does not exist")))
  (should (string-equal "test-val" (dot-env-get "TEST_VAL" "default")))
  (should (string-equal "test-val2" (dot-env-get "TEST_VAL2" "default")))

  ;; reload real config
  (setq dot-env-filepath old-env-filepath)
  (dot-env-load))

;;; tests.el ends here
