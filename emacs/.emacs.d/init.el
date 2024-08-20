(setq vc-follow-symlinks t)

(setq tkn/ignored-modules '())

(defun tkn/emacs-d-filename (rel-name)
  "Evaluates to an absolute path to a file named `rel-name` in
the user's Emacs directory"
  (concat
   (expand-file-name user-emacs-directory) rel-name))

;; Pull in all internals
(load-file (tkn/emacs-d-filename "internals.el"))

                                        ;(tkn/bootstrap-elpaca)                  
                                        ;(elpaca `(,@elpaca-order))

                                        ;(defun +elpaca-unload-seq (e) "Unload seq before continuing the elpaca build, then continue to build the recipe E."
;; (and (featurep 'seq) (unload-feature 'seq t))
;; (elpaca--continue-build e))
                                        ;(elpaca `(seq :build ,(append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
;;              elpaca--pre-built-steps
;;            elpaca-build-steps))
;; (list '+elpaca-unload-seq 'elpaca--activate-package))))


;; Install use-package support
;; (elpaca elpaca-use-package
;;   ;; Enable :elpaca use-package keyword.
;;   (elpaca-use-package-mode)
;;   ;; Assume :elpaca t unless otherwise specified.
;;   (setq elpaca-use-package-by-default t))

;; (elpaca-wait)

(tkn/bootstrap-straight)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package org
  :config (add-to-list 'org-modules 'org-habit t))

;; (elpaca-wait)

;; Load all necessary configs
(tkn/load-configs)
(put 'dired-find-alternate-file 'disabled nil)
