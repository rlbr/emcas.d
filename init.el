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
(require 'org)
(org-babel-load-file
 (expand-file-name "settings.org"
                   user-emacs-directory))
