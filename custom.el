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
 '(browse-kill-ring-display-duplicates
   nil)
 '(company-show-numbers t)
 '(delete-by-moving-to-trash t)
 '(dired-listing-switches
   "-alhv")
 '(elpy-eldoc-show-current-function
   nil)
 '(elpy-rpc-timeout 4)
 '(explicit-shell-file-name
   "/bin/bash")
 '(find-file-suppress-same-file-warnings
   t)
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
 '(ibuffer-expert t)
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
 '(ibuffer-show-empty-filter-groups
   nil)
 '(inhibit-startup-screen t)
 '(kill-ring-max 500)
 '(magit-status-sections-hook
   (quote
    (magit-insert-status-headers
     magit-insert-merge-log
     magit-insert-rebase-sequence
     magit-insert-am-sequence
     magit-insert-sequencer-sequence
     magit-insert-bisect-output
     magit-insert-bisect-rest
     magit-insert-bisect-log
     magit-insert-ignored-files
     magit-insert-untracked-files
     magit-insert-unstaged-changes
     magit-insert-staged-changes
     magit-insert-stashes
     magit-insert-unpushed-to-pushremote
     magit-insert-unpushed-to-upstream-or-recent
     magit-insert-unpulled-from-pushremote
     magit-insert-unpulled-from-upstream)))
 '(menu-bar-mode nil)
 '(org-goto-interface
   (quote
    outline-path-completionp))
 '(org-imenu-depth 5)
 '(org-outline-path-complete-in-steps
   nil)
 '(package-selected-packages
   (quote
    (ibuffer-tramp
     dockerfile-mode
     docker
     docker-compose-mode
     better-shell
     ahk-mode
     realgud
     format-all
     autodisass-java-bytecode
     meghanada
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
 '(python-check-command "flake8")
 '(reb-re-syntax (quote string))
 '(safe-local-variable-values
   (quote ((project-venv))))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
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
 '(company-tooltip-search
   ((t
     (:background "#f1fa8c"
		  :foreground "dark gray"))))
 '(line-number-current-line
   ((t
     (:foreground "yellow"
		  :weight bold)))))
(put 'upcase-region 'disabled nil)
