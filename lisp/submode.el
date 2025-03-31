;;; submode.el --- Detect and highlight for embedded languages -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

(add-to-list 'auto-mode-alist '("\\.sqli\\'" . sql-mode))
(use-package mmm-mode
  :ensure t
  :custom
  (mmm-global-mode 'maybe)
  :config
  (set-face-background 'mmm-default-submode-face nil)
  (mmm-add-classes
   '((embedded-sql
      :submode sql-mode
      :face mmm-code-submode-face
      :front "\\(--SQL\\)"
      :back "\\(--SQL-END\\)")))
  (mmm-add-mode-ext-class 'prog-mode nil 'embedded-sql))

(provide 'submode)
;;; submode.el ends here
