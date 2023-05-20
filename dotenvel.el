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
   (lambda (item) (not (s-starts-with-p "#" item)))
   lst))

(defun dotenvel-parse-entries (lst)
  "Split the strings in LST by equal signs and trim whitespace."
  (mapcar (lambda (entry)
            (let ((quoteless (replace-regexp-in-string "\"" "" entry)))
              (mapcar #'string-trim
                      (split-string quoteless "="))))
          lst))

(defun dotenvel-get (field &optional default)
  "Get the value of FIELD from dotenvel-env.
Use DEFAULT if no value is found."
  (or
   (car (cdr (assoc field dotenvel-env)))
   default))

(defun dotenvel-load ()
  "Load the values from .env file."
  (interactive)
  (setq dotenvel-env (dotenvel-parse-entries
                      (dotenvel-filter-entries
                       (dotenvel-file-contents-to-list)))))
