(setq vc-follow-symlinks t)

(setq tkn/ignored-modules '())

(defun tkn/emacs-d-filename (rel-name)
  "Evaluates to an absolute path to a file named `rel-name` in
the user's Emacs directory"
  (concat
   (expand-file-name user-emacs-directory) rel-name))

;; Pull in all internals
(load-file (tkn/emacs-d-filename "internals.el"))

(tkn/bootstrap-elpaca)
(elpaca `(,@elpaca-order))


;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

(use-package org
  :config (add-to-list 'org-modules 'org-habit t))

(elpaca-wait)

;; Load all necessary configs
(tkn/load-configs)
