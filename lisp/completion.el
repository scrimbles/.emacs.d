;;; completion.el --- Completion-related config (company, copilot, etc.)
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

;; (setq *starmacs/copilot-enabled* t)
;;
;; (defun starmacs/copilot-mode-hook ()
;;   (when (and (boundp '*starmacs/copilot-enabled*)
;;              *starmacs/copilot-enabled*)
;;     copilot-mode-hook))
;;
;; (use-package copilot
;;   :vc (:url "github.com/zerolfx/copilot.el")
;;   :hook (prog-mode . copilot-mode)
;;   :config
;;   (define-key copilot-completion-map (kbd "C-f") 'copilot-accept-completion) ; using forward motion to accept completion like Warp
;;   (define-key copilot-completion-map (kbd "<right>") 'copilot-accept-completion)
;;   (unless (copilot-installed-version)
;;   (copilot-install-server))
;;   :ensure t)

(use-package company
  :ensure t
  :hook ((prog-mode) . (lambda () (company-mode)))
  :bind (:map company-mode-map
              ("<tab>" . 'company-indent-or-complete-common)
              :map company-active-map
              ("C-n" . 'company-select-next-or-abort)
              ("C-p" . 'company-select-previous-or-abort))
  :custom
  (company-idle-delay nil) ; don't try to complete until asked
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (lsp-completion-provider :capf) ; used for eglot integration

  (company-show-quick-access t)
  :config
  (company-tng-configure-default))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))


(provide 'completion)
;;; completion.el ends here
