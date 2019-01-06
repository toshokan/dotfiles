(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defun tkn/emacs-d-filename (rel-name)
  (concat
   (expand-file-name user-emacs-directory) rel-name))

(defun tkn/load-org-config-file-from-emacs-d (file-name)
  (org-babel-load-file
   (tkn/emacs-d-filename file-name)))

(defun tkn/load-configs ()
  (tkn/load-org-config-file-from-emacs-d "config.org"))

(tkn/load-configs)
