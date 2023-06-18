;;; tests.el --- summary -*- lexical-binding: t -*-

;; Author: Amo DelBello
;; Maintainer: Amo DelBello
;; Version: 1.0.0
;; Package-Requires: (dotenv)
;; Homepage: https://github.com/amodelbello/dotenv
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

;; dotenv-file-contents-to-list
(ert-deftest dotenv-file-contents-to-list-test ()
  "Test that a file is loaded and split by newlines."
  (setq dotenv-env-filepath ".env-normal")
  (setq result (dotenv-file-contents-to-list))
  (should (= 2 (length result)))

  (setq dotenv-env-filepath ".env-comments")
  (setq result (dotenv-file-contents-to-list))
  (should (= 3 (length result))))


;; dotenv-filter-entries
(ert-deftest dotenv-filter-entries-test ()
  "Test that comments are filtered out."
  (setq lst '("TEST=one"
              "# Comment"))
  (setq result (dotenv-filter-entries lst))
  (should (= 1 (length result)))

  (setq lst2 '("TEST=one"
               "NOT=a comment" ))
  (setq result (dotenv-filter-entries lst2))
  (should (= 2 (length result))))


;; dotenv-parse-entries
(ert-deftest dotenv-parse-entries-test ()
  "Test that spaces and quotes are removed."
  (setq lst '(" TEST = one"
              "TEST2= two"
              "TEST3 =\"three\""
              "TEST4= 'four'"))
  (setq want (list '("TEST" "one")
                   '("TEST2" "two")
                   '("TEST3" "three")
                   '("TEST4" "four")))
  (setq got (dotenv-parse-entries lst))
  (should (equal got want)))


;; dotenv-get
(ert-deftest dotenv-get ()
  "Test that you can get an env variable from a loaded config."
  (setq old-env-filepath dotenv-env-filepath)
  (setq dotenv-env-filepath ".env-comments")
  (dotenv-load)
  (should (string-equal "does not exist" (dotenv-get "OTHER" "does not exist")))
  (should (string-equal "test-val" (dotenv-get "TEST_VAL" "default")))
  (should (string-equal "test-val2" (dotenv-get "TEST_VAL2" "default")))

  ;; reload real config
  (setq dotenv-env-filepath old-env-filepath)
  (dotenv-load))

;;; tests.el ends here
