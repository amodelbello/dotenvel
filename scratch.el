(defvar dotenvel-env-filepath (format "%s%s" user-emacs-directory ".env")
  "The path to the .env file.")

(defun get-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun file-contents-to-list ()
  "Read the contents of env file and split by newline."
  (split-string (get-file-contents dotenvel-env-filepath) "\\(\n+\\)"))

(setq env (file-contents-to-list))
(cdr env)

;; Need functions
;; - is valid key/value
