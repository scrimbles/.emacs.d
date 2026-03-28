;;; lsp.el --- All of my language-specific configuration for `eglot' -*-lexical-binding: t -*-
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

;;; Basic `flycheck' setup
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package tree-sitter
  :ensure t
  :config
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

(use-package tree-sitter-langs
  :ensure )

;;; Basic `eglot' setup
(use-package eglot
  :bind
  (("s-." . eglot-code-actions)
   ("<f12>" . eglot-find-typeDefinition)
   ("<f2>" . eglot-rename)
   ("C-c d" . starmacs/toggle-inline-diagnostics))
  :hook
  (js-mode . eglot-ensure)
  (typescript-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure)
  (typescriptreact-mode . eglot-ensure)
  (tsx-ts-mode . eglot-ensure)
  (python-base-mode . eglot-ensure)
  (f90-mode . eglot-ensure)
  (zig-mode . eglot-ensure)
  (haskell-mode . eglot-ensure)
  :config
  (setq lsp-prefer-flymake nil)
  (setq flymake-show-diagnostics-at-end-of-line t)

  (defun starmacs/toggle-inline-diagnostics ()
    "Toggle flymake inline diagnostics at end of line."
    (interactive)
    (setq flymake-show-diagnostics-at-end-of-line
          (not flymake-show-diagnostics-at-end-of-line))
    (when flymake-mode (flymake-mode 1))
    (message "Inline diagnostics %s"
             (if flymake-show-diagnostics-at-end-of-line "enabled" "disabled")))

  (cl-pushnew '((js-mode typescript-mode typescriptreact-mode) . ("typescript-language-server" "--stdio"))
              eglot-server-programs
              :test #'equal))

(use-package eldoc-box
  :hook
  (eglot-managed-mode . eldoc-box-hover-mode)
  :config
  (setq eldoc-box-only-multi-line t)
  (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t))


;;; TypeScript
(use-package web-mode
  :ensure t)

(define-derived-mode typescriptreact-mode web-mode "TypescriptReact"
  "A major mode for tsx.")

(use-package apheleia
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '(npx "prettier"
              "--trailing-comma"  "all"
              "--arrow-parens"    "always"
              "--printWidth"      "120"
              "--tab-width"       "2"
              "--single-quote"    "true"
              "--semi"            "true"
              "--use-tabs"        "false"
              "--bracketSpacing"  "true"
              file))
  (add-to-list 'apheleia-mode-alist '(typescriptreact-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(typescript-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(web-mode . prettier))
  (apheleia-global-mode t))

(setq-default indent-tabs-mode nil)

;;; Go
(use-package go-mode
  :ensure t
  :hook
  (go-mode . eglot-ensure)
  (go-mode . tree-sitter-hl-mode)
  (go-ts-mode . eglot-ensure)
  :config
  (setq go-ts-indent-level 4))

;;; Zig
(use-package zig-mode
  :ensure t
  :mode ("\\.zig\\'")
  :hook
  (zig-mode . eglot-ensure)
  (zig-mode . tree-sitter-hl-mode))

;;; LaTeX
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq-default Tex-engine 'xetex)
  (setq-default TeX-PDF-mode t))

(use-package preview-latex
  :defer t
  :ensure auctex)

;;; Protobuf
(use-package protobuf-mode
  :ensure t)


;;; EXPERIMENT
(use-package tide
  :commands (tide-mode)
  :functions setup-tide-mode
  :diminish tide-mode
  :after (company flycheck)
  :hook ((js2-mode . setup-tide-mode)
         (rjsx-mode . setup-tide-mode)
         (typescript-mode . setup-tide-mode)
         (typescriptreact-mode . setup-tide-mode)
         (web-mode . (lambda () (when (or (string= "tsx" web-mode-content-type) (string= "jsx" web-mode-content-type)) (setup-tide-mode)))))
  :init
  (defun setup-tide-mode ()
    "Setup tide mode."
    (interactive)
    (tide-setup)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode 1)
    (tide-hl-identifier-mode 1))

  (with-eval-after-load 'js2-mode
    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))

  (with-eval-after-load 'rjsx-mode
    (add-hook 'rjsx-mode-hook #'setup-tide-mode)
    (flycheck-disable-checker 'javascript-jshint)
    (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append))

  :config
  (setq tide-format-options
        '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
          t
          :placeOpenBraceOnNewLineForFunctions
          nil))
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; Adds tslint to web-mode
  (flycheck-add-mode 'typescript-tslint 'web-mode)

  ;; formats the buffer before saving

  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-tide)))

(use-package add-node-modules-path
  :ensure t
  :hook
  (typescript-ts-mode . add-node-modules-path)
  (typescriptreact-mode . add-node-modules-path)
  (typescript-mode . add-node-modules-path))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescriptreact-mode))
  :hook
  (typescript-mode . (lambda () (setq tab-width 2)))
  (typescriptreact-mode . (lambda () (setq tab-width 2)))
  (typescript-ts-mode . (lambda () (setq tab-width 2)))
  :custom
  (typescript-indent-level 2))

(use-package prettier
  :ensure t
  :hook
  (typescript-mode . prettier-mode)
  (typescriptreact-mode . prettier-mode)
  (typescript-ts-mode . prettier-mode)
  (web-mode . prettier-mode))



(provide 'lsp)
;;; lsp.el ends here
