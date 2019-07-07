(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      package-selected-packages)

(defvar org-init/settings.org-message-depth 4
  "What depth of settings.org headers to message at startup.")
;;; init.el
(with-temp-buffer
  (insert-file "~/.emacs.d/settings.org")
  (goto-char (point-min))
  (while (not (eobp))
    (forward-line 1)
    (cond
     ;; Report Headers
     ((looking-at
       (format "\\*\\{2,%s\\} +.*$"
               org-init/settings.org-message-depth))
      (message "%s" (match-string 0)))
     ;; Evaluate Code Blocks
     ((looking-at "^#\\+BEGIN_SRC +emacs-lisp *$")
      (let ((l (match-end 0)))
        (search-forward "\n#+END_SRC")
        (eval-region l (match-beginning 0))))
     ;; Finish on the next level-1 header
     ((looking-at "^\\* ")
      (goto-char (point-max))))))
