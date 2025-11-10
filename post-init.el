;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Author: M Cooper Healy <m.cooper.healy@gmail.com
;;; Code:

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(require 'modernity)  ; overhaul the basic user experience (vertico, consult, project, etc.)
(require 'ui-tweaks)  ; theme and other cosmetic changes
(require 'acme-mouse) ; BY THE POWER OF GLENDA!
(require 'tree)
(require 'version-control)
(require 'org-config)

(require 'lisp-ide)   ; settings for better LISP development (including elisp)
(require 'lsp)        ; modern LSP usage, like any modern editor
(require 'submode)    ; detect embedded SQL and HTML

(require 'completion) ; company, copilot, etc. -- anything that completes user input
(require 'ai)

;; (require 'keymap)
(require 'packages)   ; misc packages that require little enough configuration that they don't require their own module
(require 'news)       ; configure `Newstcker'

(require 'welcome)    ; welcome dashboard a la Doom, but with /waaaaaay/ fewer moving parts
;;; post-init.el ends here
