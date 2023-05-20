(defvar dotenvel-env-filepath (format "%s%s" user-emacs-directory ".env")
  "The path to the .env file")

(defun get-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(setq contents (get-file-contents dotenvel-env-filepath))
