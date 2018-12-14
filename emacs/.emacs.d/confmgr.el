(provide 'conf-manager)

(setq conf-manager/config-map '((conf-manager/enable-exwm . "exwm.org")
				(conf-manager/enable-base . "config.org")))

(defun conf-manager/initialize ()
  (setq conf-manager/enable-base t))

(setq conf-manager/enable-exwm nil
      conf-manager/enable-base nil)

(defun conf-manager/load-if-enabled (conf-item)
  (let* ((symbol (car conf-item))
	 (enabled (and (boundp symbol)
		       (symbol-value symbol))))
    (when enabled
      (org-babel-load-file
       (concat
	(expand-file-name user-emacs-directory) (cdr conf-item))))))

(defun conf-manager/load-files ()
  (interactive)
  (mapc 'conf-manager/load-if-enabled conf-manager/config-map))

