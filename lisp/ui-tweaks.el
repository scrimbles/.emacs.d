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
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package acme-theme
  :vc (:url "https://github.com/noonels/emac" :rev :newest)
  :ensure t
  :config
  (load-theme 'acme t))

(use-package base-line
  :vc (:url "https://github.com/isomatter-labs/base-line" :rev :newest)
  :ensure t
  :hook (after-init . base-line-mode))

(setq visible-bell t)
(setq ring-bell-function
      (lambda ()
        (let ((orig-bg (face-background 'mode-line-active)))
          (set-face-background 'mode-line-active "#ef8e49")
          (run-with-idle-timer 0.1 nil
                               (lambda (bg) (set-face-background 'mode-line-active bg))
                               orig-bg))))


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
(defvar starmacs/terminal-font-height 150)


(defvar starmacs/variable-pitch-font "Go")
(defvar starmacs/title-font "Go Medium")
(defvar starmacs/fixed-pitch-font "Go Mono")
(defvar starmacs/mode-line-font "VGA Medium")
(defvar starmacs/terminal-font "VGA Medium")


;;; Create a terminal-face to distinguish interactive elements from code areas
(defface starmacs/terminal-face
  '((default :font "VGA Medium" :height starmacs/terminal-font-height))
  "Adds a little more pizazz to the terms.")

(setq starmacs/terminal-face-remap-cookie
      (face-remap-add-relative 'default 'starmacs/terminal-face))

(face-remap-remove-relative starmacs/terminal-face-remap-cookie)

(define-minor-mode starmacs/terminal-face-remap-mode
  "Remap the face for terminal buffers."
  :local t
  :init-value nil
  (if starmacs/terminal-face-remap-mode
      (setq starmacs/terminal-face-remap-cookie
            (face-remap-add-relative 'default 'starmacs/terminal-face))
    (face-remap-remove-relative starmacs/terminal-face-remap-cookie)))

;; use terminal face in terminal-like applications (vterm, erc)
(add-hook 'vterm-mode-hook #'starmacs/terminal-face-remap-mode)
(add-hook 'erc-mode-hook #'starmacs/terminal-face-remap-mode)

;; use terminal face when echo-ing to minibuffer
(with-current-buffer (get-buffer " *Echo Area 0*") ; the leading space character is correct
  (setq-local face-remapping-alist '((default starmacs/terminal-face))))

(set-face-attribute 'default nil :font starmacs/fixed-pitch-font :height starmacs/fixed-pitch-height)
(set-face-attribute 'fixed-pitch nil :font starmacs/fixed-pitch-font :height starmacs/fixed-pitch-height)

(set-face-attribute 'variable-pitch nil :font starmacs/variable-pitch-font :height starmacs/variable-pitch-height)
(set-face-attribute 'mode-line nil
                    :font starmacs/mode-line-font
                    :height starmacs/mode-line-height)

;; Ensure that comments are italic to further distinguish them
(set-face-italic 'font-lock-comment-face t)
(set-face-italic 'font-lock-doc-face t)

(provide 'ui-tweaks)
;;; ui-tweaks.el ends here
