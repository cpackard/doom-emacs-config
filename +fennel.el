;;; +fennel.el -*- lexical-binding: t; -*-

(defun fennel-love-2d-repl ()
  (interactive)
  (setenv "LOVE_DEBUG" nil)
  (let ((default-directory (doom-project-root)))
    (fennel-proto-repl "love .")))

(defun fennel-love-2d-repl-debug ()
  (interactive)
  (setenv "LOVE_DEBUG" "1")
  (let ((default-directory (doom-project-root)))
    (fennel-proto-repl "love . --debug")))

(defun file-to-module ()
  "Convert a fennel filename to module format."
  (let* ((fennel-filename (file-relative-name (buffer-file-name) (doom-project-root)))
         (replaced (replace-regexp-in-string "/" "." fennel-filename)))
    (if (string-suffix-p ".fnl" replaced)
        (substring replaced 0 (- (length replaced) 4))
      replaced)))

(defvar last-fennel-test nil)

(defun fennel-test-module ()
  "Test the current fennel module."
  (interactive)
  (setenv "LOVE_DEBUG" "1")
  (let ((test-command (format "cd %s && love . --test %s" (doom-project-root) (file-to-module))))
    (setq last-fennel-test test-command)
    (compile test-command)))

(defun fennel-test-last ()
  "Re-run the previous fennel test."
  (interactive)
  (setenv "LOVE_DEBUG" "1")
  (compile last-fennel-test))

(defun fennel-test-all ()
  "Run all tests for the current project."
  (interactive)
  (setenv "LOVE_DEBUG" nil)
  (compile (format "cd %s && love . --test" (doom-project-root))))

(defun fennel-love-2d-base-repl ()
  (interactive)
  (setenv "LOVE_DEBUG" nil)
  (let ((default-directory (doom-project-root)))
    (fennel-repl "love .")))

(defun fnl-lsp-config ()
  "Update the fennel-ls configuration."
  (interactive)
  (lsp-notify "workspace/didChangeConfiguration"
              (list :settings
                    (list :fennel-ls
                          (list :extra-globals "love love.graphics"
                                :checks (list :unused-definition t
                                              :unnecessary-method t
                                              :bad-unpack t
                                              :var-never-set t
                                              :op-with-no-arguments t
                                              :unknown-module-field json-false)))))
  (message "fennel-ls config updated"))

(defun reopen-buffer-file ()
  "Kill the current buffer and reopen the file it is visiting."
  (interactive) ; Make the function callable via M-x and keybindings.
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (kill-buffer)
          (find-file file-name))
      (message "Buffer is not visiting a file!"))))

(after! fennel-mode
  (map! :after fennel-mode
        :map fennel-mode-map
        :localleader
        (:prefix ("b" . "buffer")
         :desc "reopen buffer file" "r" #'reopen-buffer-file)
        (:prefix ("=" . "format")
         :desc "format buffer" "=" #'fennel-format
         :desc "format region" "r" #'fennel-format-region)
        (:prefix ("l" . "lsp")
         :desc "update lsp config" "c" #'fnl-lsp-config)
        (:prefix ("r" . "repl")
         :desc "comma command" "," #'fennel-proto-repl-comma-command
         :desc "develop repl" "d" #'fennel-love-2d-repl-debug
         :desc "interrupt repl" "i" #'fennel-proto-repl-interrupt
         :desc "LÖVE repl" "l" #'fennel-love-2d-repl
         :desc "macro expand" "m" #'fennel-proto-repl-macroexpand
         :desc "proto repl" "p" #'fennel-proto-repl
         :desc "reload file" "r" #'fennel-proto-repl-reload
         :desc "base repl" "s" #'fennel-repl
         (:prefix ("e" . "eval")
          :desc "eval buffer" "b" #'fennel-proto-repl-eval-buffer
          :desc "eval last sexp" "e" #'fennel-proto-repl-eval-last-sexp
          :desc "eval defun" "f" #'fennel-proto-repl-eval-defun
          :desc "eval last & next" "n" #'fennel-proto-repl-eval-form-and-next
          :desc "eval current form" "p" #'fennel-proto-repl-eval-print-last-sexp
          :desc "eval region" "r" #'fennel-proto-repl-eval-region)
         (:prefix ("h" . "help")
          :desc "show args" "a" #'fennel-proto-repl-show-arglist
          :desc "show docs" "d" #'fennel-proto-repl-show-documentation
          :desc "show var docs" "v" #'fennel-proto-repl-show-var-documentation))
        (:prefix ("t" . "test")
         :desc "all" "a" #'fennel-test-all
         :desc "rerun" "r" #'fennel-test-last
         :desc "module" "t" #'fennel-test-module)))

(defun nil-hash ()
  (interactive)
  (insert "#"))

(with-eval-after-load 'lispy
  (lispy-define-key lispy-mode-map "#" 'nil-hash))

(after! (:and lsp-mode fennel-mode)
  (add-to-list 'lsp-language-id-configuration
               '(fennel-mode . "fennel"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "fennel-ls")
                    :activation-fn (lsp-activate-on "fennel")
                    :server-id 'fennel-ls))

  (add-hook 'fennel-mode-hook #'lsp))
