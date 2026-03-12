;;; ui-tweaks.el --- UI QoL changes and theming  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

(set-fringe-mode 10)        ; Give some breathing room
(column-number-mode)
(global-display-line-numbers-mode t)
(pixel-scroll-precision-mode 1)
(setq-default cursor-type '(bar . 2))
(blink-cursor-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                comint-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (use-package acme-theme
;;   :vc (:url "https://github.com/noonels/emac" :rev :newest)
;;   :ensure t
;;   :config
;;   (load-theme 'acme t))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one))

(use-package base-line
  :vc (:url "https://github.com/isomatter-labs/base-line" :rev :newest)
  :ensure t
  :hook (after-init . base-line-mode))

(use-package hot-line
  :vc (:url "https://github.com/isomatter-labs/hot-line" :rev :newest)
  :ensure t
  :config
  (hot-line-mode 1))

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
 '(whitespace-tab((t (:foreground "#B8B09A"))))
 '(whitespace-newline((t (:foreground "#B8B09A")))))

;; CUSTOM FONTS
(defvar starmacs/fixed-pitch-height 120)
(defvar starmacs/mode-line-height 150)
(defvar starmacs/variable-pitch-height 130)


(defvar starmacs/variable-pitch-font "Go")
(defvar starmacs/title-font "Go Medium")
(defvar starmacs/fixed-pitch-font "Berkeley Mono")
(defvar starmacs/mode-line-font "VGA Medium")


(use-package dont-talk-to-computers
  :vc (:url "https://github.com/isomatter-labs/dont-talk-to-computers" :rev :newest)
  :ensure t
  :custom
  (dont-talk-to-computers-modes '(vterm-mode-hook comint-mode-hook erc-mode-hook))
  :config
  (dont-talk-to-computers-mode 1))

(set-face-attribute 'default nil :font starmacs/fixed-pitch-font :height starmacs/fixed-pitch-height)
(set-face-attribute 'fixed-pitch nil :font starmacs/fixed-pitch-font :height starmacs/fixed-pitch-height)

(set-face-attribute 'variable-pitch nil :font starmacs/variable-pitch-font :height starmacs/variable-pitch-height)
(set-face-attribute 'mode-line nil
                    :font starmacs/mode-line-font :height 150)

;; Ensure that comments are italic to further distinguish them
(set-face-italic 'font-lock-comment-face t)
(set-face-italic 'font-lock-doc-face t)

(provide 'ui-tweaks)
;;; ui-tweaks.el ends here
