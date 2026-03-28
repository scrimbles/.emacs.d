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

;; (use-package squeak-theme
;;   :vc (:url "https://github.com/isomatter-labs/squeak-theme" :rev :newest)
;;   :ensure t
;;   :config
;;   (load-theme 'squeak t))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one))

(use-package base-line
  :vc (:url "https://github.com/isomatter-labs/base-line" :rev :newest)
  :ensure t
  :hook (after-init . base-line-mode)
  :custom
  (base-line-minor-mode-allowlist '(llm-completion-mode copilot-mode)))

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

;; Provide indent guides for whitespace-dependent languages
(use-package indent-bars
  :custom
  (indent-bars-pattern ". . . . ")
  (indent-bars-width-frac 0.25)
  (indent-bars-pad-frac 0.2)
  (indent-bars-zigzag 0.1)
  (indent-bars-color-by-depth '(:regexp "rainbow-delimiters-depth-\\([0-9]+\\)-face" :blend 1))
  (indent-bars-highlight-current-depth '(:color "white" :blend 0.4 :face (:weight semi-bold)))
  (indent-bars-no-descend-lists 'skip) ; prevent extra bars in nested lists + skip intermediate bars
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
                                       if_statement with_statement while_statement)))
  (indent-bars-prefer-character t)
  :hook ((python-base-mode yaml-mode) . indent-bars-mode))

(provide 'ui-tweaks)
;;; ui-tweaks.el ends here
