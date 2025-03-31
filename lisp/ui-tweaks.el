;;; ui-tweaks.el --- UI QoL changes and theming
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

(set-fringe-mode 10)        ; Give some breathing room
(toggle-frame-maximized)    ; Always start maximized
(column-number-mode)
(global-display-line-numbers-mode t)
(pixel-scroll-precision-mode 1)
(setq-default cursor-type '(bar . 2))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Highlight current line
(if (window-system)
    (global-hl-line-mode 1))

;; Got bit by the Atom bug again (RIP), so I'm gonna give Anisochromatic a break for a while.
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-org-config))
(load-theme 'doom-one t)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 35)
  (doom-modeline-def-modeline 'main
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes input-method buffer-encoding major-mode process vcs "  ")))


;; Trailing whitespace should be highlighted, and deleted on save.
;; In addition, tabs and newlines should be displayed in a subtle way, allowing for users to more easily check the formatting used.
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default tab-width 4)
(setq whitespace-style (quote (face tabs newline tab-mark)))

(setq whitespace-display-mappings
      '(;(newline-mark 10 [172 10])
        (tab-mark 9 [187 9] [92 9])))
(global-whitespace-mode 't)

(custom-set-faces
 '(whitespace-tab((t (:foreground "#525252"))))
 '(whitespace-newline((t (:foreground "#525252")))))



(provide 'ui-tweaks)
;;; ui-tweaks.el ends here
