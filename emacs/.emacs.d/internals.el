(setq straight-repository-branch "develop")

(defun tkn/bootstrap-straight ()
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(defvar tkn/conf-list (sort (file-expand-wildcards (tkn/emacs-d-filename "*.org")) 'string<)
  "A list of config files to be sourced.")

(defun tkn/resolve-filename (filename)
  "If FILENAME is a symlink, resolves it, otherwise evalutes to FILENAME"
  (let ((file-dir (file-name-directory filename))
	(symlink-dest (file-symlink-p filename)))
    (if symlink-dest
	(expand-file-name (concat file-dir symlink-dest))
      filename)))

(defun tkn/org-babel-load-file (file)
  "The original function only tangles if the source file is newer
than the tangled file. This is not sensible if the source file is
a symlink, since the symlink date does not change"
  (let ((target-file (tkn/resolve-filename file)))
    (org-babel-load-file target-file)
    (message "Loaded %s" file)))

  ;; (let* ((tangled-file (concat (file-name-sans-extension file) ".el")))
  ;;   (org-babel-tangle-file file tangled-file "emacs-lisp")
  ;;   (load-file tangled-file)
  ;;   (message "Loaded %s" tangled-file)))

(defun tkn/load-configs ()
  "Load each config file referenced in `tkn/conf-list`. If it is
an `org` file, runs it through `org-babel-load-file`, otherwise
assumes it is `elisp`"
  (let ((load-f (lambda (file-name)
		  (if (string-match ".org\\'" file-name)
		      (tkn/org-babel-load-file file-name)
		    (load-file file-name)))))
    (mapcar load-f tkn/conf-list)))
