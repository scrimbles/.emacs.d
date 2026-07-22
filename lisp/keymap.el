;;; keymap.el --- Helix-like keybindings -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

(use-package pcre2el :ensure t)

(use-package ultra-scroll
  :vc (:url "https://github.com/jdtsmith/ultra-scroll") ; if desired (emacs>=v30)
  :init
  (setq scroll-conservatively 3 ; or whatever value you prefer, since v0.4
        scroll-margin 0)        ; important: scroll-margin>0 not yet supported
  :config
  (ultra-scroll-mode 1))

(use-package hel
  :vc (:url "https://github.com/anuvyklack/hel.git" :rev "main")
  :custom (inhibit-startup-screen t)
  :config (hel-mode))

(provide 'keymap)
;;; keymap.el ends here
