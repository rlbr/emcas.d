;;; masm-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "masm-mode" "masm-mode.el" (0 0 0 0))
;;; Generated autoloads from masm-mode.el

(autoload 'masm-mode "masm-mode" "\
Major mode for editing MASM assembly programs.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.asm\\'" . masm-mode))

(add-to-list 'auto-mode-alist '("\\.inc\\'" . masm-mode))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "masm-mode" '("masm-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; masm-mode-autoloads.el ends here
