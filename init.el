;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)


(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    ein
    iedit
    elpy
    flycheck
    magit
    ssh-config-mode
    web-mode
    dracula-theme
    yaml-mode
    py-autopep8
    apache-mode
    js2-mode
    js2-refactor
    xref-js2
    visual-regexp-steroids))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)
;; BASIC
(require 'ssh-config-mode)
(add-to-list 'auto-mode-alist '("~/.ssh/config\\'" . ssh-config-mode))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(require 'elpy)
(global-set-key (kbd "M-<up>") 'elpy-nav-move-line-or-region-up)
(global-set-key (kbd "M-<down>") 'elpy-nav-move-line-or-region-down)
;; Window movement
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(global-set-key (kbd "C-;") 'iedit-mode)
;; Tramp
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(add-to-list 'tramp-remote-path "/system/xbin")
(add-to-list 'tramp-remote-path "/data/data/com.termux/file/usr/bin")

;; JavaScript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
			   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; Web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; visual-regexp-steroids
(require 'visual-regexp-steroids)
(define-key global-map (kbd "C-c r") 'vr/select-replace)
(define-key global-map (kbd "C-c q") 'vr/select-query-replace)
;; Custom
(setq dired-listing-switches "-alh")
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(global-set-key (kbd "C-c d") 'duplicate-line)
(require 'recentf)
(recentf-mode 1)
(global-set-key (kbd "C-<tab>") 'recentf-open-files)
(global-set-key (kbd "C-x g") 'magit-status)
(setq inhibit-startup-screen t)
;; (setq vc-handled-backends nil)
;; use external ls
(setq ls-lisp-use-insert-directory-program t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(cond
 ;; Windows fixes
 ((string-equal system-type "windows-nt")
  (progn
    (defun quote-exe (path)
      (w32-short-file-name path))
    (defun start-external-shell ()
      (interactive)
      (start-process-shell-command (format "cmd(%s)" default-directory) nil "start default.bat"))
    (global-set-key (kbd "C-S-C") 'start-external-shell)
    (setq insert-directory-program "C:/Program Files/git/usr/bin/ls.exe")
    (setq find-program (quote-exe "C:/Program Files/git/usr/bin/find.exe"))
    (setq grep-program (quote-exe "C:/Program Files/git/usr/bin/grep.exe"))
    (setq python-shell-interpreter (quote-exe (executable-find "python")))
    (setq python-check-command (quote-exe (executable-find "flake8")))
    (setq delete-by-moving-to-trash t)
    ))

 ;; Linux-specific
 ((string-equal system-type "gnu/linux")
  (progn
    (setq python-shell-interpreter "python3")
    (setq elpy-rpc-python-command python-shell-interpreter)
    (defun get-elpa-package-install-directory (pkg)
      "Return the install directory of elpa PKG. Return nil if it is not found."
      (let ((elpa-dir package-user-dir))
	(when (file-exists-p elpa-dir)
	  (let* ((pkg-match (concat "\\`" (symbol-name pkg) "-[0-9]+"))
		 (dir (car (directory-files elpa-dir 'full pkg-match))))
            (when dir (file-name-as-directory dir))))))
    (setq vr/command-python
	  (format "python3 %s" (expand-file-name "regexp.py" (get-elpa-package-install-directory 'visual-regexp-steroids))))
    )))

(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; --------------------------------------

(load-theme 'dracula t)
(global-linum-mode t) ;; enable line numbers globally

;; PYTHON CONFIGURATION
;; --------------------------------------
(elpy-enable)
;;(debug-on-variable-change 'python-check-command)
;; (elpy-use-ipython)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable py-autopep8 formatting on save
(require 'py-autopep8)
(defun python-mode-keys ()
  "Modify python-mode local key map"
  (local-set-key (kbd "C-c C-p") 'py-autopep8))
(add-hook 'python-mode-hook 'python-mode-keys)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(setq elpy-syntax-check-command python-check-command)
;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" default)))
 '(package-selected-packages
   (quote
    (js2-mode js2-refactor xref-js2 python-django django-mode visual-regexp-steroids pcre2el vimrc-mode iedit transient magit dracula-theme py-autopep8 flycheck elpy ein better-defaults ssh-config-mode yaml-mode apache-mode web-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Roboto Mono" :foundry "outline" :slant normal :weight normal :height 113 :width normal)))))
