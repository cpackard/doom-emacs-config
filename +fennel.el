;;; +fennel.el -*- lexical-binding: t; -*-

(defun fennel-love-2d-repl ()
  (interactive)
  (fennel-proto-repl "love ."))

(after! fennel-mode
  (map! :after fennel-mode
        :localleader
        :prefix ("r" . "repl")
        :desc "base repl" "s" #'fennel-repl
        :desc "proto repl" "p" #'fennel-proto-repl
        :desc "LÖVE repl" "l" #'fennel-love-2d-repl))

(after! (:and lsp-mode fennel-mode)
  (add-to-list 'lsp-language-id-configuration
               '(fennel-mode . "fennel"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "fennel-ls")
                    :activation-fn (lsp-activate-on "fennel")
                    :server-id 'fennel-ls))

  (add-hook 'fennel-mode-hook #'lsp))
