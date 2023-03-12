(setq vc-follow-symlinks t)

(setq tkn/ignored-modules '())

(defun tkn/emacs-d-filename (rel-name)
  "Evaluates to an absolute path to a file named `rel-name` in
the user's Emacs directory"
  (concat
   (expand-file-name user-emacs-directory) rel-name))

;; Pull in all internals
(load-file (tkn/emacs-d-filename "internals.el"))

(tkn/bootstrap-straight)

(straight-use-package 'use-package)

(use-package org
  :straight t
  :config (add-to-list 'org-modules 'org-habit t))

;; Load all necessary configs
(tkn/load-configs)
