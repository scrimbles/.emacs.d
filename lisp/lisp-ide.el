;;; lisp-ide.el --- Lisp development (elisp, common lisp, etc.)
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

(setq-default indent-tabs-mode nil)


;;; LISP
;; no tabs in lisp
(add-hook 'emacs-lisp-mode-hook (lambda () (setq-local indent-tabs-mode nil)))
(add-hook 'lisp-mode-hook (lambda () (setq-local indent-tabs-mode nil)))
(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)
(add-hook 'lisp-mode-hook #'prettify-symbols-mode)

(use-package sly
  :ensure t)


(provide 'lisp-ide)
;;; lisp-ide.el ends here
