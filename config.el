;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Christian Packard"
      user-mail-address "christian@cpacklabs.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:

(setq doom-font (font-spec :family "Fira Code" :size 14)
      doom-big-font (font-spec :family "Fira Code" :size 20)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 16))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-city-lights)
;; (setq doom-theme 'doom-flatwhite)
;; (setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; Modeline
;; - add current workspace name
;; - add major mode icon
(after! doom-modeline
  (setq doom-modeline-persp-name t
        doom-modeline-major-mode-icon t))

;; Start Doom Emacs maximized
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)

;; Delete whitespace on save, including in markdow-mode
(setq ws-butler-global-exempt-modes '(special-mode comint-mode term-mode eshell-mode diff-mode))

;; Which-key and Evil Key Bindings - Spacemacs style
(load! "+bindings")

;; Clojure mode & Cider Configuration + key bindings
(load! "+clojure")

;; Structural Editing - Smartparens
(load! "+smartparens.el")

;; Python mode bindings
(load! "+python.el")

;; Fennel mode bindings
(load! "+fennel.el")

;; Org mode bindings
(load! "+org.el")

;; mu4e bindings
(load! "+mu4e.el")

;; code review
(load! "+code-review.el")

;; SQL settings
(load! "+sql.el")

;; Jira settings
(load! "+jira.el")

;; flycheck settings
(load! "+flycheck.el")

;; global keybindings
(load! "+keys.el")

;; copilot settings
(load! "+copilot.el")

;; NOTE: cpackard added these 05/22/2023
(setq parinfer-rust-library "~/.emacs.d/parinfer-rust/parinfer-rust-darwin.so")

;;   > Checking Doom core for irregularities...
;; ! Your $HOME is recognized as a project root
;; Emacs will assume $HOME is the root of any project living under $HOME. If this
;; isn't desired, you will need to remove ".git" from
;; `projectile-project-root-files-bottom-up' (a variable)
(after! projectile
  (setq projectile-project-root-files-bottom-up
        (remove ".git"
                projectile-project-root-files-bottom-up)))

;; def portal to the dev namespace to allow dereferencing via @dev/portal
;; (defun portal.api/open ()
;;   (interactive)
;;   (cider-nrepl-sync-request:eval
;;     "(do (ns dev)
;;          (def portal ((requiring-resolve 'portal.api/open) {:launcher :emacs}))
;;          (add-tap (requiring-resolve 'portal.api/submit)))"))

;; (defun portal.api/clear ()
;;   (interactive)
;;   (cider-nrepl-sync-request:eval "(portal.api/clear)"))

;; (defun portal.api/close ()
;;   (interactive)
;;   (cider-nrepl-sync-request:eval "(portal.api/close)"))

(+global-word-wrap-mode t)

;; By default, =[= and =]= are [[https://github.com/noctuid/lispyville/tree/master#additional-movement-key-theme][bound]] to =lispyville-previous-opening= and
;; =lispyville-next-closing= respectively. If you use a language which makes frequent
;; use of brackets (e.g. Clojure, Racket, Scheme), you can insert a bracket pair =[]=
;; by typing ={=. If you prefer to use the bracket keys for input, you can rebind
;; them like below:
(map! :after (lispy lispyville)
      :map lispy-mode-map-lispy
      ;; unbind individual bracket keys
      "[" nil
      "]" nil
      ;; re-bind commands bound to bracket keys by default
      "M-[" #'lispyville-previous-opening
      "M-]" #'lispyville.next-opening)

(map! :after evil
      :niv "M-n" #'evil-pop-paste-next
      :niv "M-p" #'evil-paste-pop
      :niv "C-n" #'evil-next-line
      :niv "C-p" #'evil-previous-line
      :nv  "C-'" #'comment-dwim)

;; Rebind C-v to scroll-up command to mimic expected Emacs functionality
;; of C-v / M-v for scrolling the page up and down respectively.
(map! :map vertico-map
      :g "C-v" #'vertico-scroll-up)

;; Github Copilot
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

;; GPG
(after! epa
  (setf epg-pinentry-mode 'loopback)
  ;; Hack for Emacs 29.1
  (fset 'epg-wait-for-status 'ignore))

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


;; Golden Ratio
;; https://github.com/doomemacs/doomemacs/issues/2225#issuecomment-568287835
(use-package! golden-ratio
  :after-call pre-command-hook
  :config
  (golden-ratio-mode 1)
  ;; Using this hook for resizing windows is less precise than
  ;; `doom-switch-window-hook'.
  (remove-hook 'window-configuration-change-hook #'golden-ratio)
  (add-hook 'doom-switch-window-hook #'golden-ratio))

(use-package! dap-mode
  :defer t
  :config
  ;; (require 'dap-codelldb)
  (require 'dap-lldb)
  (require 'dap-cpptools)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  ;; (dap-codelldb-setup)
  (dap-gdb-lldb-setup)
  ;; (setq lsp-rust-analyzer-debug-lens-extra-dap-args
  ;;       '(:MIMode "gdb"
  ;;         :miDebuggerPath "rust-gdb"
  ;;         :stopAtEntry t
  ;;         :externalConsole :json-false
  ;;         :setupCommands [#s(hash-table size 12 data
  ;;                                       ("description" "Enable pretty-printing for gdb" "text" "-enable-pretty-printing" "ignoreFailures" t))]
  ;;         ))

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
         :dap-compilation-dir "/Users/cpackard/Documents/rust_book/hello_cargo"
         ))

  ;; (dap-register-debug-template
  ;;  "Rust::GDB Run Configuration"
  ;;  (list :type "gdb"
  ;;        :request "launch"
  ;;        :name "GDB::Run"
  ;;        :gdbpath "rust-gdb"
  ;;        :target "${workspaceRoot}/target/debug/${workspaceRootFolderName}.exe"
  ;;        :cwd "${workspaceRoot}"
  ;;        :valuesFormatting "prettyPrinters"))

  ;; (dap-register-debug-template "Rust::LLDB Run Configuration"
  ;;                              (list :type "lldb"
  ;;                                    :request "launch"
  ;;                                    :name "LLDB::Run"
  ;;                                    :gdbpath "rust-lldb"
  ;;                                    :target nil
  ;;                                    :cwd nil
  ;;                                    :env '(("RUST_BACKTRACE" . "full"))
  ;;                                    :args nil
  ;;                                    :sourceLanguages '("rust"))

  ;;                              )
  )

;; (dap-register-debug-template "Rust::GDB Run Configuration"
;;                              (list :type "gdb"
;;                                    :request "launch"
;;                                    :name "GDB::Run"
;;                                    :gdbpath "rust-gdb"
;;                                    :target nil
;;                                    :cwd nil))

;; (dap-register-debug-template
;;  "Rust::LLDB Run Configuration"
;;  (list :type "lldb"
;;        :request "launch"
;;        :name "LLDB::Run"
;;        :gdbpath "rust-lldb"
;;        ;; :miDebuggerPath "~/.cargo/bin/rust-lldb"
;;        :target nil
;;        :cwd nil))

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
