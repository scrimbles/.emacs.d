;;; copilot-config.el --- AI code completion via GitHub Copilot -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Ghost-text code completion using copilot.el.
;; Run M-x copilot-login on first use.
;;
;;; Code:

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest)
  :ensure t
  :config
  ;; Override the minor mode lighter
  (setcdr (assq 'copilot-mode minor-mode-alist) '(" ✦"))

  ;; Accept with C-<tab>, dismiss with anything else
  (define-key copilot-completion-map (kbd "C-<tab>") #'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-S-<tab>") #'copilot-accept-completion-by-line)

  ;; Register as a minor mode lighter for base-line's allowlist
  (setq copilot-lighter " ✦")

  (defun starmacs/toggle-copilot ()
    "Toggle copilot-mode in the current buffer."
    (interactive)
    (copilot-mode (if copilot-mode -1 1)))

  ;; Fainter ghost text than default comment face
  (set-face-attribute 'copilot-overlay-face nil
                      :foreground "#555555"
                      :italic t)

  (global-set-key (kbd "C-c C-l") #'starmacs/toggle-copilot))

(provide 'copilot-config)
;;; copilot-config.el ends here
