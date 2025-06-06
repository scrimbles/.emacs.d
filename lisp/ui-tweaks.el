;;; ui-tweaks.el --- UI QoL changes and theming
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

;; ;; Got bit by the Atom bug again (RIP), so I'm gonna give Anisochromatic a break for a while.
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
 '(whitespace-tab((t (:foreground "#525252"))))
 '(whitespace-newline((t (:foreground "#525252")))))

;; CUSTOM FONTS
(defvar starmacs/fixed-pitch-height 140)
(defvar starmacs/variable-pitch-height 130)


(defvar starmacs/variable-pitch-font "Mona Sans")
(defvar starmacs/title-font "Hubot-Sans")
(defvar starmacs/fixed-pitch-font "VGA Medium")

(set-face-attribute 'default nil :font starmacs/fixed-pitch-font :height starmacs/fixed-pitch-height)
(set-face-attribute 'fixed-pitch nil :font starmacs/fixed-pitch-font :height starmacs/fixed-pitch-height)

(set-face-attribute 'variable-pitch nil :font starmacs/variable-pitch-font :height starmacs/variable-pitch-height)
(set-face-attribute 'mode-line nil
                    :font starmacs/fixed-pitch-font)

;; Ensure that comments are italic to further distinguish them
(progn                                  ; TODO: figure out a non-ridiculous way to accomplish this
  (set-face-italic 'font-lock-comment-face t)
  (set-face-italic 'font-lock-doc-face t))

(provide 'ui-tweaks)
;;; ui-tweaks.el ends here
