(defvar dotenvel-env-filepath (format "%s%s" user-emacs-directory ".env")
  "The path to the .env file.")

(defvar dotenvel-env '()
  "The alist that stores .env variables.")

(defun get-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun file-contents-to-list ()
  "Read the contents of env file and split elements into a list."
  (split-string (get-file-contents dotenvel-env-filepath) "\\(\n+\\)"))

(defun filter-entries (lst)
  "Filter out commented lines in LST."
  (seq-filter
   (lambda (item) (not (s-starts-with-p "#" item)))
   lst))

(defun parse-entries (lst)
  "Split the strings in LST by equal signs and trim whitespace."
  (mapcar (lambda (entry)
            (mapcar #'string-trim
                    (split-string entry "=")))
          lst))


(filter-entries (file-contents-to-list))
(parse-entries (filter-entries (file-contents-to-list)))


(defun dotenvel-load ()
  "Load the values from .env file."
  (interactive)
  (setq dotenvel-env (parse-entries
                      (filter-entries
                       (file-contents-to-list)))))

(defun dotenvel-get (field)
  "Get the value of FIELD from dotenvel-env."
  (interactive)
  (car (cdr (assoc field dotenvel-env))))

(dotenvel-get "GOODVAL")

;; Need functions
;; - is valid key/value
;;   - doesn't start with a #
;;   - has a single equal sign
;;   - trim whitespace
