;;; mail.el --- Mail configuration via mu4e -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

(use-package wanderlust
  :ensure t
  :init (autoload 'wl "wl" "Wanderlust" t))

(setq elmo-imap4-use-modified-utf7 t)

;; SMTP
(setq
 wl-smtp-connection-type   'starttls         ; Use TLS
 wl-smtp-posting-port      587               ; The SMTP port
 wl-smtp-authenticate-type "plain"           ; Authentication type
 wl-smtp-posting-user      "m.cooper.healy"  ; Username
 wl-smtp-posting-server    "smtp.gmail.com"  ; SMTP server
 wl-local-domain           "gmail.com"       ; The SMTP server again
 wl-message-id-domain      "smtp.gmail.com") ; And... Again?

(setq
 ;; All system folders (draft, trash, spam, etc) are placed in the
 ;; [Gmail]-folder, except inbox. "%" means it's an IMAP-folder
 wl-default-folder "%inbox"
 wl-draft-folder   "%[Gmail]/Drafts"
 wl-trash-folder   "%[Gmail]/Trash"

 ;; The below is not necessary when you send mail through Gmail's SMTP server,
 ;; see https://support.google.com/mail/answer/78892?hl=en&rd=1
 ;; wl-fcc            "%[Gmail]/Sent"

 wl-from "M Cooper Healy <m.cooper.healy@gmail.com>"  ; Our From: header field
 wl-fcc-force-as-read t           ; Mark sent mail (in the wl-fcc folder) as read
 wl-default-spec "%")             ; For auto-completion

(provide 'mail)
;;; mail.el ends here
