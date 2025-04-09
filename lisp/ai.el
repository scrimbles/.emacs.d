;;; ai.el --- Configuration for ChatGPT and whatnot
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main"))

(setq starmacs/chatgpt-api-key (expand-file-name "chatgpt-api-key.txt"))

(use-package chatgpt-shell
  :ensure t
  :vc (:url "https://github.com/xenodium/chatgpt-shell")
  :config
  (unless
      (file-exists-p starmacs/chatgpt-api-key)
    (make-empty-file starmacs/chatgpt-api-key))

  (setq chatgpt-shell-openai-key (replace-regexp-in-string "\n\\'" "" (with-temp-buffer
                                                                        (insert-file-contents (expand-file-name "chatgpt-api-key.txt"))
                                                                        (buffer-string)))))

(provide 'ai)
;;; ai.el ends here
