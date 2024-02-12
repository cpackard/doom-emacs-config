;;; +rust.el -*- lexical-binding: t; -*-

(use-package! lsp-mode
  :defer t
  ;; :after-call pre-command-hook
  :config
  (setq
   ;; General settings
   lsp-inlay-hint-enable t
   lsp-ui-peek-always-show t
   lsp-ui-sideline-show-hover t
   lsp-auto-guess-root nil
   ;; Rust settings
   lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
   lsp-rust-analyzer-display-chaining-hints t
   lsp-rust-analyzer-cargo-watch-command "clippy"
   lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil
   lsp-rust-analyzer-display-closure-return-type-hints t
   lsp-rust-analyzer-display-parameter-hints nil
   lsp-rust-analyzer-display-reborrow-hints nil)

  (lsp-register-custom-settings
   '(("rust-analyzer.experimental.localDocs" t t)))

  (add-hook 'rust-mode-hook #'lsp-inlay-hints-mode))

(use-package! dap-mode
  :defer t
  :config
  (require 'dap-lldb)
  (require 'dap-cpptools)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)

  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb-mi"
         :request "launch"
         :name "LLDB::Run"
         :gdbpath "rust-lldb"
         :console "external"
         ;; :target "${workspaceRoot}/target/debug/${workspaceRootFolderName}"
         :target "/Users/cpackard/Documents/rust_book/hello_cargo/target/debug/hello_cargo"
         ;; :cwd "${workspaceRoot}"
         :cwd "/Users/cpackard/Documents/rust_book/hello_cargo"
         :valuesFormatting "parseText"
         :dap-compilation "cargo build"
         :dap-compilation-dir "/Users/cpackard/Documents/rust_book/hello_cargo")))


(defun extract-substring-from-url (input-url regex)
  "Extract the substring based on the given regex pattern."
  (when (string-match regex input-url)
    (match-string 1 input-url)))

(setq rustc-sysroot
      (let ((sysroot (string-trim-right (shell-command-to-string "rustc --print sysroot"))))
        (format "%s/share/doc/rust/html" sysroot)))

;; (open-rust-doc-url "https://doc.rust-lang.org/stable/alloc/string/struct.String.html")
;; (open-rust-doc-url "https://docs.rs/forecast/0.1.0/forecast/struct.Hourly.html")
(defun open-rust-doc-url (&optional rust-doc-url)
  "Open the Rust documentation URL in the browser."
  (interactive)
  (let* ((doc-url (or rust-doc-url
                      ;; shamelessly stolen from `lsp-rust-analyzer-open-external-docs`
                      (-if-let* ((params (lsp-make-rust-analyzer-open-external-docs-params
                                          :text-document (lsp--text-document-identifier)
                                          :position (lsp--cur-position)))
                                 (url (lsp-request "experimental/externalDocs" params)))
                          url
                        nil))))
    (if doc-url
        (let* ((package-crate-regex "https://docs\\.rs/[^/]+/[0-9]+\\.[0-9]+\\.[0-9]+/\\(.*\\)")
               (stdlib-docs-regex "https://doc\\.rust-lang\\.org/[^/]+/\\(.*\\)")
               (is-package-crate (string-match package-crate-regex doc-url))
               (url-regex (if is-package-crate
                              package-crate-regex
                            stdlib-docs-regex))

               (url-suffix (extract-substring-from-url doc-url url-regex))

               (url-prefix (format "file://%s" (if is-package-crate
                                                   (format "%s/target/doc" (projectile-project-root))
                                                 rustc-sysroot)))
               (url-prefix (if (string-suffix-p "/" url-prefix)
                               (substring url-prefix 0 (- (length url-prefix) 1))
                             url-prefix))

               (full-url (format "%s/%s" url-prefix url-suffix)))
          (+evil-window-vsplit-a)
          (let ((docs-win (selected-window)))
            (xwidget-webkit-browse-url full-url)
            (set-window-buffer docs-win xwidget-webkit-last-session-buffer)
            (set-window-dedicated-p docs-win t) ; Make the window dedicated to the buffer
            (set (make-local-variable 'kill-buffer-hook)
                 (lambda ()
                   (let ((win (get-buffer-window docs-buffer)))
                     (when win (delete-window win)))))))
      (lsp--warn "Couldn't find documentation URL or your version of rust-analyzer doesn't support this extension"))))

(map! :after rustic
      :map rustic-mode-map
      :leader
      (:prefix ("c" . "code")
       :desc "Jump to browser docs" "K" #'open-rust-doc-url))

(map! :after rustic
      :map rustic-mode-map
      :localleader
      (:prefix ("h" . "help")
       :desc "Jump to documentation" "h" #'+lookup/documentation
       :desc "Jump to browser docs" "b" #'open-rust-doc-url))
