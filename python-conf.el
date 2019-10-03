(defun rlbr/get-venv-name (&optional library-root)
  "Generate venv name based off of the base-name of the library root"
  (file-name-base
   (directory-file-name
    (if library-root
	library-root
      (elpy-library-root)))))

(defun rlbr/handle-name-conflicts (venv-name)
  "Deal with potential name conflicts in venv"
  (let ((venv-conflicts)
	(venv-partition-name))
    (setq venv-partition-name (rlbr/split-venv-with-number venv-name))
    (setq venv-conflicts
	  (seq-filter
	   (lambda (item) (string-equal (cdr item) venv-name))
	   (mapcar #'rlbr/split-venv-with-number (pyvenv-virtualenv-list))))
    (when venv-conflicts
      (setcar venv-partition-name (1+ (apply 'max (mapcar #'car venv-conflicts)))))
    (rlbr/join-venv-with-number venv-partition-name)))

;; We'll save using file-precious-flag, so avoid destroying
;; symlinks.  (If we're not already visiting the buffer, this is
;; handled by find-file-visit-truename, above.)
(defun rlbr/save-buffer-func-to-file (visit-file func args)
  "Rip off of custom-save-all"
  (let* ((filename visit-file)
	 (recentf-exclude (if recentf-mode
			      (append
			       `(,(concat "\\`" (regexp-quote (recentf-expand-file-name visit-file)) "\\'")
				 ,(concat "\\`" (regexp-quote (file-truename (recentf-expand-file-name visit-file))) "\\'"))
			       recentf-exclude)))
	 (old-buffer (find-buffer-visiting filename))
	 old-buffer-name)
    (with-current-buffer
	(let ((find-file-visit-truename t))
	  (or old-buffer
	      (let ((delay-mode-hooks t))
		(find-file-noselect filename))))
      (when old-buffer
	(setq old-buffer-name
	      (buffer-file-name))
	(set-visited-file-name
	 (file-chase-links filename)))
      (unless (eq major-mode
		  'emacs-lisp-mode)
	(delay-mode-hooks
	  (emacs-lisp-mode)))
      (let ((inhibit-read-only t)
	    (print-length nil)
	    (print-level nil))
	(apply func args))
      (let ((file-precious-flag t))
	(save-buffer))
      (if old-buffer
	  (progn
	    (set-visited-file-name
	     old-buffer-name)
	    (set-buffer-modified-p nil))
	(kill-buffer (current-buffer))))))

(defun rlbr/setup-python-venv-dirlocals (&optional library-root)
  "Setup .dir-locals file in library root and tell vc system to ignore .dir-locals file"
  (let* ((library-root (if library-root
			   library-root
			 (elpy-library-root)))
	 (default-directory library-root)
	 (dir-locals-path (expand-file-name
			   ".dir-locals.el"))
	 (venv-name (rlbr/get-venv-name
		     library-root)))
    (rlbr/save-buffer-func-to-file dir-locals-path 'add-dir-local-variable
				   `(python-mode pyvenv-workon ,venv-name))
    (let* ((vc-root (vc-find-root dir-locals-path ".git"))
	   (vc-ignore-file (vc-call-backend 'Git 'find-ignore-file vc-root)))
      (if (apply 'string-equal (mapcar 'directory-file-name (mapcar 'file-truename (list vc-root library-root))))
	  (progn
	    (unless (file-exists-p vc-ignore-file)
	      (with-temp-buffer
		(write-file vc-ignore-file)))
	    (vc-ignore ".dir-locals.el"))
	(when (y-or-n-p (format "Ignore .dir-locals.el in repo '%s' ?" vc-root))
	  (unless (file-exists-p vc-ignore-file)
	    (with-temp-buffer
	      (write-file vc-ignore-file)))
	  (vc-ignore ".dir-locals.el"))))))

(defun rlbr/init-python-venv-in-library-root (&optional library-root)
  "If no venv is specified in the library root .dir-locals file, prompt to either create one or use default"
  (let ((venv-name (rlbr/get-venv-name)))
    (setq venv-name (rlbr/handle-name-conflicts venv-name))
    (if (y-or-n-p (format "Create venv '%'?" venv-name))
	(pyvenv-create venv-name python-command))))
