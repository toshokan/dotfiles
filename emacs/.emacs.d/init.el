(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; ++ Configuration management
(require 'conf-manager
	 (expand-file-name (concat user-emacs-directory "confmgr.el")))
(conf-manager/initialize)

(setq conf-manager/enable-exwm nil
      conf-manager/enable-base t)

(conf-manager/load-files)
;; --

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" default)))
 '(package-selected-packages
   (quote
    (flycheck-irony pdf-tools company-auctex auctex magit which-key cider base16-theme solarized-theme zenburn-theme rainbow-delimiters expand-region multiple-cursors ace-jump-mode ace-window hydra company-irony irony company-lsp lsp-haskell lsp-ui lsp-mode haskell-mode counsel ivy smartparens yasnippet-snippets yasnippet company exwm use-package))))
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 78 :foundry "IBM " :family "IBM Plex Mono")))))
