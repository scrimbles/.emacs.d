;;; welcome.el --- A minimal spash screen, for aesthetics -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:


(defun no-linum ()
  "Turn off line numbers."
  (display-line-numbers-mode -1))

(use-package cold-open
  :vc (:url "https://github.com/isomatter-labs/cold-open" :rev :newest)
  :ensure t
  :custom
  (cold-open-image "~/.emacs.d/img/emacs.png")
  (cold-open-tagline "A hackable text editor for the 21st Century!")
  :init
  (cold-open-setup))

(provide 'welcome)
;;; welcome.el ends here
