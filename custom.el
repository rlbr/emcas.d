(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms
   (quote
    ((".*" "~/.emacs.d/autosave/" t))))
 '(aw-dispatch-when-more-than 1)
 '(backup-directory-alist
   (quote
    ((".*" . "~/.emacs.d/backup"))))
 '(custom-enabled-themes
   (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("35b0b0e531731e270708ddb342dc2e576a31fb298dcbc56a206596a43afac54f"
     "1e922202769ed113de72a203c85726f98b5c86523c55629f95489c083fe2472a"
     "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e"
     default)))
 '(delete-by-moving-to-trash t)
 '(dired-listing-switches "-alh")
 '(elpy-eldoc-show-current-function
   nil)
 '(hippie-expand-try-functions-list
   (quote
    (try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol)))
 '(ibuffer-formats
   (quote
    ((mark
      modified
      read-only
      locked
      " "
      (name 18 18 :left :elide)
      " "
      (size-h 9 -1 :right)
      " "
      (mode 16 16 :left :elide)
      " "
      filename-and-process)
     (mark
      " "
      (name 16 -1)
      " "
      filename))))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (meghanada
     elmacro
     ace-window
     winum
     spaceline
     use-package
     kv
     lispy
     ibuffer-vc
     ag
     string-inflection
     tiny
     powershell
     diffview
     ivy-hydra
     php-mode
     swiper
     mode-line-bell
     htmlize
     cython-mode
     blacken
     dired-narrow
     lua-mode
     browse-kill-ring
     clipmon
     free-keys
     org-link-minor-mode
     js2-mode
     js2-refactor
     xref-js2
     python-django
     visual-regexp-steroids
     pcre2el
     vimrc-mode
     iedit
     transient
     magit
     dracula-theme
     flycheck
     elpy
     ein
     better-defaults
     ssh-config-mode
     yaml-mode
     apache-mode
     web-mode
     undo-tree)))
 '(reb-re-syntax (quote string))
 '(safe-local-variable-values
   (quote ((project-venv))))
 '(tramp-use-ssh-controlmaster-options
   nil
   nil
   (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default
    ((t
      (:family "Roboto Mono"
	       :foundry "outline"
	       :slant normal
	       :weight normal
	       :height 113
	       :width normal))))
 '(line-number-current-line
   ((t
     (:foreground "yellow"
		  :weight bold)))))
(put 'upcase-region 'disabled nil)
