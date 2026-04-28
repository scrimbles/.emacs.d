;;; numerical --- Adding support for statistical analysis -*- lexical-binding: t -*-
;;; Commentary:
;;  This adds support for Julia, R and Fortran
;;; Code:


;;; Python
(use-package python
  :ensure t
  :after (eglot apheleia)
  :config
  ;; check that pip-installed executables present
  (unless (executable-find "pipx")
    (user-error "Executable `pipx' not installed, please install"))
  (unless (executable-find "ty")
    (message "Installing `ty' language server for Python")
    (shell-command "pipx install ty")
    (message "Installation of `ty' complete"))
  (unless (executable-find "ruff")
    (message "Installing `ruff' formatter for Python")
    (shell-command "pipx install ruff")
    (message "Installation of `ruff' complete"))

  ;; wire up modes
  (add-to-list 'auto-mode-alist '("/uv\\.lock\\'" . toml-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

  ;; use `ty' as lsp server
  (cl-pushnew '((python-mode python-ts-mode) . ("ty" "server"))
              eglot-server-programs
              :test #'equal)

  ;; use `ruff' for formatting
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff)))

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
(use-package julia-repl :ensure t)
(use-package eglot-jl :ensure t)
(use-package julia-mode
  :mode "\\.jl\\'"
  :interpreter ("julia" . julia-mode)
  :init (setenv "JULIA_NUM_THREADS" "6")
  :config
  (add-hook 'julia-mode-hook 'julia-repl-mode)
  (add-hook 'julia-mode-hook 'eglot-jl-init)
  (add-hook 'julia-mode-hook 'eglot-ensure)
  (add-hook 'julia-mode-hook (lambda () (setq julia-repl-set-terminal-backend 'vterm))))

(setq eglot-jl-julia-command (expand-file-name "~/.juliaup/bin/julia"))
(setq julia-repl-executable-records
      `((default ,(expand-file-name "julia" (expand-file-name ".juliaup/bin" "~")))
        (master ,(expand-file-name "julia" (expand-file-name ".juliaup/bin" "~")))))

;;; Fortran 90+
(add-to-list 'eglot-server-programs '(f90-mode . ("fortls" "--notify_init" "--nthreads=4")))

(provide 'numerical)
;;; numerical.el ends here
