(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'package)
(when (version< emacs-version "26.2")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))
(setq package-archives '(("GNU ELPA" . "https://elpa.gnu.org/packages/")
			 ("MELPA Stable" . "https://stable.melpa.org/packages/")
			 ("MELPA" . "https://melpa.org/packages/"))
      package-archive-priorities '(("MELPA Stable" . 10)
				   ("GNU ELPA" . 5)
				   ("MELPA" . 0)))



(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      package-selected-packages)
(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))
