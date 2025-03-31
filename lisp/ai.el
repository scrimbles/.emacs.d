;;; ai.el --- Configuration for ChatGPT and whatnot
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

(setq starmacs/chatgpt-api-key (expand-file-name "chatgpt-api-key.txt"))

(use-package chatgpt-shell
  :ensure t
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("dist" "*.el"))
  :config
  (unless
      (file-exists-p starmacs/chatgpt-api-key)
    (make-empty-file starmacs/chatgpt-api-key))

  (setq chatgpt-shell-openai-key (replace-regexp-in-string "\n\\'" "" (with-temp-buffer
                                                                        (insert-file-contents (expand-file-name "chatgpt-api-key.txt"))
                                                                        (buffer-string)))))

(provide 'ai)
;;; ai.el ends here
