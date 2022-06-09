(setq vc-follow-symlinks t)

(defun tkn/emacs-d-filename (rel-name)
  "Evaluates to an absolute path to a file named `rel-name` in
the user's Emacs directory"
  (concat
   (expand-file-name user-emacs-directory) rel-name))

;; Pull in all internals
(load-file (tkn/emacs-d-filename "internals.el"))

(tkn/bootstrap-straight)
(straight-use-package 'org)
(require 'org)

;; Load all necessary configs
(tkn/load-configs)
