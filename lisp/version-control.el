;;; version-control.el --- Settings for git, etc. -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

(use-package magit
  :ensure t)

;; forge allows magit to connect to Github
(use-package forge
  :ensure t
  :after magit)

(use-package why-this
  :ensure t
  :custom (why-this-idle-delay 0)
  :bind
  ("C-c b" . why-this-mode)
  ("C-c w" . why-this)
  :config
  (set-face-foreground 'why-this-face "#007777")
  (set-face-background 'why-this-annotate-heat-map-cold "#E1FAFF")
  (set-face-background 'why-this-annotate-heat-map-warm "#F8E8E8"))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(provide 'version-control)
;;; version-control.el ends here
