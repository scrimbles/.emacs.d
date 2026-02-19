;;; markdown.el --- Configuration specific for markdown rendering -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

(use-package markdown-mode
  :ensure t
  :hook
  (markdown-mode . visual-line-mode)
  :config
  (setq markdown-asymmetric-header t))

(provide 'markdown)
;;; markdown.el ends here
