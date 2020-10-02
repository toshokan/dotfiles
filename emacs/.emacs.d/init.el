(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(require 'org)

(defun tkn/emacs-d-filename (rel-name)
  "Evaluates to an absolute path to a file named `rel-name` in
the user's Emacs directory"
  (concat
   (expand-file-name user-emacs-directory) rel-name))

(defvar tkn/conf-list (sort (file-expand-wildcards (tkn/emacs-d-filename "*.org")) 'string<)
  "A list of config files to be sourced.")

(defun tkn/org-babel-load-file (file)
  "The original function only tangles if the source file is newer
than the tangled file. This is not sensible if the source file is
a symlink, since the symlink date does not change"
  (let* ((tangled-file (concat (file-name-sans-extension file) ".el")))
    (org-babel-tangle-file file tangled-file "emacs-lisp")
    (load-file tangled-file)
    (message "Loaded %s" tangled-file)))

(defun tkn/load-configs ()
  "Load each config file referenced in `tkn/conf-list`. If it is
an `org` file, runs it through `org-babel-load-file`, otherwise
assumes it is `elisp`"
  (let ((load-f (lambda (file-name)
		  (if (string-match ".org\\'" file-name)
		      (tkn/org-babel-load-file file-name)
		    (load-file file-name)))))
    (mapcar load-f tkn/conf-list)))

;; Load all necessary configs
(tkn/load-configs)
(put 'dired-find-alternate-file 'disabled nil)
