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

(defun rlbr/setup-python-venv-dirlocals (&optional library-root)
  "Setup .dir-locals file in library root and tell vc system to ignore .dir-locals file"
  (let (library-root (if library-root
			 library-root
		       (elpy-library-root)))
    ))

(defun rlbr/init-python-venv-in-library-root (&optional library-root)
  "If no venv is specified in the library root .dir-locals file, prompt to either create one or use default"
  (let ((venv-name (rlbr/get-venv-name)))
    (setq venv-name (rlbr/handle-name-conflicts venv-name))
    (if (y-or-n-p (format "Create venv '%'?" venv-name))
	(pyvenv-create venv-name python-command))))
