;;; numerical --- Adding support for statistical analysis -*- lexical-binding: t -*-
;;; Commentary:
;;  This adds support for Julia, R and Fortran
;;; Code:


;;; R Markdown
(use-package poly-markdown
  :ensure t)

(use-package poly-R
  :after poly-markdown
  :ensure t
  :config
  (add-to-list 'auto-mode-alist
               '("\\.[rR]md\\'" . poly-gfm+r-mode))
  (setq markdown-code-block-braces t))

;;; R (Emacs Speaks Statistics)
(use-package ess
  :after poly-R
  :ensure t
  :config
  (load "ess-autoloads"))

;;; Julia
(use-package vterm :ensure t)
(use-package julia-repl)
(use-package eglot-jl)
(use-package julia-mode
  :mode "\\.jl\\'"
  :interpreter ("julia" . julia-mode)
  :init (setenv "JULIA_NUM_THREADS" "6")
  :config
  (add-hook 'julia-mode-hook 'julia-repl-mode)
  (add-hook 'julia-mode-hook 'eglot-jl-init)
  (add-hook 'julia-mode-hook 'eglot-ensure)
  (add-hook 'julia-mode-hook (lambda () (setq julia-repl-set-terminal-backend 'vterm))))

(setq eglot-jl-julia-command "/usr/local/bin/julia")
(setq julia-repl-executable-records
      '((default "~/.juliaup/bin/julia")
        (master "~/.juliaup/bin/julia")))

;;; Fortran 90+
(add-to-list 'eglot-server-programs '(f90-mode . ("fortls" "--notify_init" "--nthreads=4")))

(provide 'numerical)
;;; numerical.el ends here
