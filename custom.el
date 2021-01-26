(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms
   '((".*" "~/.emacs.d/autosave/" t)))
 '(aw-dispatch-when-more-than 1)
 '(backup-directory-alist
   '((".*" . "~/.emacs.d/backup")))
 '(browse-kill-ring-display-duplicates
   nil)
 '(company-show-numbers t)
 '(custom-safe-themes
   '("60940e1f2fa3f4e61e7a7ed9bab9c22676aa25f927d5915c8f0fa3a8bf529821"
     "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d"
     "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
     "855eb24c0ea67e3b64d5d07730b96908bac6f4cd1e5a5986493cbac45e9d9636"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     default))
 '(delete-by-moving-to-trash t)
 '(dired-listing-switches
   "-alhv")
 '(elpy-eldoc-show-current-function
   nil)
 '(elpy-rpc-timeout 4)
 '(elpy-test-pytest-runner-command
   '("pytest"))
 '(elpy-test-runner
   'elpy-test-pytest-runner)
 '(explicit-shell-file-name
   "/bin/bash")
 '(find-file-suppress-same-file-warnings
   t)
 '(hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol))
 '(ibuffer-expert t)
 '(ibuffer-formats
   '((mark
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
      filename)))
 '(ibuffer-show-empty-filter-groups
   nil)
 '(iedit-toggle-key-default
   "M-]")
 '(inhibit-startup-screen t)
 '(kill-ring-max 500)
 '(magit-status-sections-hook
   '(magit-insert-status-headers
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
     magit-insert-unpulled-from-upstream))
 '(menu-bar-mode nil)
 '(org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)))
 '(org-babel-tangle-comment-format-beg
   "file:%file::%start-line")
 '(org-export-with-sub-superscripts
   '{})
 '(org-goto-interface
   'outline-path-completionp)
 '(org-imenu-depth 5)
 '(org-outline-path-complete-in-steps
   nil)
 '(org-src-lang-modes
   '(("ocaml" . tuareg)
     ("elisp" . emacs-lisp)
     ("ditaa" . artist)
     ("asymptote" . asy)
     ("dot" . fundamental)
     ("sqlite" . sql)
     ("calc" . fundamental)
     ("C" . c)
     ("cpp" . c++)
     ("C++" . c++)
     ("screen" . shell-script)
     ("shell" . sh)
     ("bash" . sh)))
 '(package-selected-packages
   '(dired-rsync
     projectile
     plantuml-mode
     shift-number
     number
     diminish
     paradox
     smart-mode-line-atom-one-dark-theme
     counsel
     ibuffer-tramp
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
     clipmon
     free-keys
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
     undo-tree))
 '(python-check-command "flake8")
 '(reb-re-syntax 'string)
 '(safe-local-variable-values
   '((project-venv)))
 '(scroll-bar-mode nil)
 '(sml/theme 'atom-one-dark)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tramp-use-ssh-controlmaster-options
   nil)
 '(winum-mode t))
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
