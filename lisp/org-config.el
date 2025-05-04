;;; org-config.el --- Config for Org mode and Org Roam -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

(use-package org
  :ensure t
  :hook
  (org-mode . visual-line-mode)
  (org-mode . variable-pitch-mode)
  (org-mode . (lambda () (indent-tabs-mode -1)))
  (org-mode . (lambda () (set-face-attribute 'org-block nil :foreground nil :font "Berkeley Mono" :height 120 :inherit 'fixed-pitch)))

  :custom
  (org-startup-with-inline-images t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/Library"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :config
  (org-roam-db-autosync-enable)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch))

(provide 'org-config)
;;; org-config.el ends here
