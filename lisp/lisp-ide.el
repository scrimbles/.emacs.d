;;; lisp-ide.el --- Lisp development (elisp, common lisp, etc.)
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

(setq-default indent-tabs-mode nil)

;;; Clojure
;; CIDER is considered the best clojure mode for emacs
;; think SLIME for clojure
(use-package cider
  :ensure t)

;; Register Clojure with eglot
(add-hook 'clojure-mode-hook #'eglot-ensure)


;;; LISP
;; no tabs in lisp
(add-hook 'emacs-lisp-mode-hook (lambda () (setq-local indent-tabs-mode nil)))
(add-hook 'lisp-mode-hook (lambda () (setq-local indent-tabs-mode nil)))
(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)
(add-hook 'lisp-mode-hook #'prettify-symbols-mode)

(use-package sly
  :ensure t
  :hook (sly-mrepl-mode . (lambda () (display-line-numbers-mode -1)))
  :config (setq inferior-lisp-program "ros dynamic-space-size=8192 -- -Q run"))


(provide 'lisp-ide)
;;; lisp-ide.el ends here
