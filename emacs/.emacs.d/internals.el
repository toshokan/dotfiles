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

(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))

(defun tkn/bootstrap-elpaca ()
  (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
         (build (expand-file-name "elpaca/" elpaca-builds-directory))
         (order (cdr elpaca-order))
         (default-directory repo))
    (add-to-list 'load-path (if (file-exists-p build) build repo))
    (unless (file-exists-p repo)
      (make-directory repo t)
      (when (< emacs-major-version 28) (require 'subr-x))
      (condition-case-unless-debug err
          (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                   ((zerop (call-process "git" nil buffer t "clone"
                                         (plist-get order :repo) repo)))
                   ((zerop (call-process "git" nil buffer t "checkout"
                                         (or (plist-get order :ref) "--"))))
                   (emacs (concat invocation-directory invocation-name))
                   ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                         "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                   ((require 'elpaca))
                   ((elpaca-generate-autoloads "elpaca" repo)))
              (progn (message "%s" (buffer-string)) (kill-buffer buffer))
            (error "%s" (with-current-buffer buffer (buffer-string))))
        ((error) (warn "%s" err) (delete-directory repo 'recursive))))
    (unless (require 'elpaca-autoloads nil t)
      (require 'elpaca)
      (elpaca-generate-autoloads "elpaca" repo)
      (load "./elpaca-autoloads")))
  (add-hook 'after-init-hook #'elpaca-process-queues))

(defun tkn/get-conf-list()
  "A list of config files to be sourced."
  (sort (file-expand-wildcards (tkn/emacs-d-filename "*.org")) 'string<))

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
    (mapcar load-f (tkn/get-conf-list))))
