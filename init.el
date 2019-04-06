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
    ; material-theme
    dracula-theme
    yaml-mode
    py-autopep8
    apache-mode
    visual-regexp-steroids))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)
;; BASIC
(add-to-list 'auto-mode-alist '("~/.ssh/config\\'" . ssh-config-mode))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-;") 'iedit-mode)
(setq dired-listing-switches "-alh")
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-c C-d") 'duplicate-line)
(require 'recentf)
(recentf-mode 1)
(global-set-key (kbd "C-<tab>") 'recentf-open-files)
(global-set-key (kbd "C-x g") 'magit-status)
(setq inhibit-startup-screen t)
(setq ls-lisp-use-insert-directory-program t)      ;; use external ls
(cond
 ;; Windows fixes
 ((string-equal system-type "windows-nt")
  (progn
    (defun quote-exe (path)
      (w32-short-file-name path))
    (defun start-external-shell ()
      (interactive)
      (start-process-shell-command (format "cmd(%s)" default-directory) nil "start cmd"))
    (global-set-key (kbd "C-S-C") 'start-external-shell)
    (setq insert-directory-program "C:/Program Files/git/usr/bin/ls.exe")
    (setq find-program (quote-exe "C:/Program Files/git/usr/bin/find.exe"))
    (setq grep-program (quote-exe "C:/Program Files/git/usr/bin/grep.exe"))
    (setq python-shell-interpreter (quote-exe (executable-find "python")))
    (setq python-check-command (quote-exe (executable-find "flake8")))
    ))
 ;; Linux-specific
 ((string-equal system-type "gnu/linux")
  (progn
    (setq python-shell-interpreter "python3")
    (setq elpy-rpc-python-command python-shell-interpreter)
    )))

(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; --------------------------------------

; (setq inhibit-startup-message t) ;; hide the startup message
; (load-theme 'material t) ;; load material theme
(load-theme 'dracula t)
(global-linum-mode t) ;; enable line numbers globally

;; PYTHON CONFIGURATION
;; --------------------------------------
(elpy-enable)
;(debug-on-variable-change 'python-check-command)
; (elpy-use-ipython)

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
					;(setq python-flymake-command "\"c:/Program Files/Python37/Scripts/flake8.exe\"")

;(setq python-check-command "\"c:/Program Files/Python37/Scripts/flake8.exe\"")
; (setq elpy-rpc-python-command "\"c:/Program Files/Python37/pythonw.exe\" ")
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
    (python-django django-mode visual-regexp-steroids pcre2el vimrc-mode iedit transient magit dracula-theme py-autopep8 flycheck elpy ein better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Monkey patch
;(defun quote-exe (path)
;  (if (string-match-p (regexp-quote " ") path)
;      (shell-quote-argument path)
					;    path))
;(defun quote-exe (path)
;  (replace-regexp-in-string " " "\\ " path)

					;
;(advice-add 'executable-find :filter-return #'quote-exe)
