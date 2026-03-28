;;; ai.el --- AI chat + MCP integration -*- lexical-binding: t -*-
;;
;;; Commentary:
;; gptel (LLM chat) via GitHub Copilot + mcp.el (tool servers).
;; Uses existing Copilot subscription — no extra API costs.
;;
;;; Code:

(defun ai--setup-mcp ()
  "Set up MCP and gptel-mcp if npx is available."
  (use-package mcp
    :ensure t
    :custom
    (mcp-hub-servers
     `(("filesystem" . (:command "npx"
                        :args ("-y" "@modelcontextprotocol/server-filesystem"
                               ,(expand-file-name "~"))))))
    :config
    (require 'mcp-hub))

  (use-package gptel-mcp
    :vc (:url "https://github.com/lizqwerscott/gptel-mcp.el" :rev :newest)
    :ensure t
    :after (gptel mcp))

  (message "ai.el: MCP servers configured"))

(defun ai--try-setup-mcp ()
  "Set up MCP if npx is available, otherwise install node and retry."
  (cond
   ((executable-find "npx")
    (ai--setup-mcp))
   ((executable-find "brew")
    (message "ai.el: npx not found, installing node via Homebrew...")
    (let ((proc (start-process "ai-install-node" "*ai-install-node*"
                               "brew" "install" "node")))
      (set-process-sentinel
       proc
       (lambda (_proc event)
         (if (string-match-p "finished" event)
             (progn
               (setenv "PATH" (shell-command-to-string
                               "printf %s \"$(/opt/homebrew/bin/brew shellenv | grep PATH | head -1 | sed 's/.*=\"//;s/\".*$//')\""))
               (setq exec-path (parse-colon-path (getenv "PATH")))
               (if (executable-find "npx")
                   (ai--setup-mcp)
                 (message "ai.el: node installed but npx still not found — MCP disabled")))
           (message "ai.el: failed to install node — MCP disabled"))))))
   (t
    (message "ai.el: npx not found and no package manager available — MCP disabled"))))

;;; gptel — LLM chat via GitHub Copilot
(use-package gptel
  :vc (:url "https://github.com/karthink/gptel" :rev :newest)
  :ensure t
  :config
  (require 'gptel-gh)
  (gptel-make-gh-copilot "Copilot")

  ;; Ollama local fallback
  (gptel-make-openai "Ollama"
    :host "localhost:11434"
    :protocol "http"
    :endpoint "/v1/chat/completions"
    :stream t
    :models '(qwen3:8b qwen2.5-coder:7b qwen2.5-coder:3b))

  ;; Default to Ollama (switch to Copilot via C-c M-g when quota resets)
  (setq gptel-backend (alist-get "Ollama" gptel--known-backends nil nil #'equal))
  (setq gptel-model 'qwen3:8b)
  (setq gptel-default-mode 'org-mode)

  :bind
  (("C-c g" . gptel)
   ("C-c G" . gptel-send)
   ("C-c M-g" . gptel-menu)))

;; Set up MCP after gptel
(with-eval-after-load 'gptel
  (ai--try-setup-mcp))

(provide 'ai)
;;; ai.el ends here
