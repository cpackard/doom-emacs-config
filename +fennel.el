;;; +fennel.el -*- lexical-binding: t; -*-

(defun fennel-love-2d-repl ()
  (interactive)
  (with-environment-variables (("LOVE_DEBUG" "1")
                               ("FENNEL_PROTO_REPL_ACTIVE" "1"))
    (let ((default-directory (doom-project-root)))
      (fennel-proto-repl "love ."))))

(defun fennel-love-2d-base-repl ()
  (interactive)
  (setenv "LOVE_DEBUG" nil)
  (let ((default-directory (doom-project-root)))
    (fennel-repl "love .")))

(defun fennel-love-2d-repl-debug ()
  (interactive)
  (setenv "LOVE_DEBUG" "1")
  (let ((default-directory (doom-project-root)))
    (fennel-proto-repl "love .")))

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
  (with-environment-variables (("LOVE_DEBUG" "1")
                               ("SDL_VIDEODRIVER" "dummy")
                               ("FAITH_DIFF" "diff -u %s %s | diff-so-fancy"))
    (let ((test-command (format "cd %s && love . --test %s" (doom-project-root) (file-to-module))))
      (setq last-fennel-test test-command)
      (compile test-command))))

(defun fennel-test-last ()
  "Re-run the previous fennel test."
  (interactive)
  (with-environment-variables (("LOVE_DEBUG" "1")
                               ("SDL_VIDEODRIVER" "dummy")
                               ("FAITH_DIFF" "diff -u %s %s | diff-so-fancy"))
    (compile last-fennel-test)))

(defun fennel-test-all ()
  "Run all tests for the current project."
  (interactive)
  (with-environment-variables (("SDL_VIDEODRIVER" "dummy")
                               ("FAITH_DIFF" "diff -u %s %s | diff-so-fancy"))
    (compile (format "cd %s && love . --test" (doom-project-root)))))

(defun fennel-love-2d-base-repl ()
  (interactive)
  (setenv "LOVE_DEBUG" nil)
  (let ((default-directory (doom-project-root)))
    (fennel-repl "love .")))

(defun my/outline-tab-behavior ()
  "Custom tab behavior for lines starting with `;;;`."
  (interactive)
  (message "running tab-behavior")
  (if (and (bound-and-true-p outline-minor-mode) ;; Check if outline-minor-mode is active
           (save-excursion
             (beginning-of-line)
             (looking-at-p "^;;;"))) ;; Check if line starts with three semicolons
      (outline-cycle)
    (evil-jump-item)))

;;; Highlight docstring words surrounded by backticks.

(defface fennel-docstring-backtick-face
  '((t :foreground "dark cyan")) ;; Choose your color
  "Face for highlighting backtick-surrounded words in Fennel docstrings."
  :group 'fennel)

(defun highlight-backticks-in-quotes ()
  "Highlight words surrounded by backticks within double-quoted strings or ;; comment lines."
  (font-lock-add-keywords
   nil
   '(("\".*?\""
      (0 (let ((start (match-beginning 0))
               (end (match-end 0)))
           (save-excursion
             (goto-char start)
             (while (re-search-forward "`[^`]+`" end t)
               (add-text-properties
                (match-beginning 0) (match-end 0)
                '(face fennel-docstring-backtick-face))))
           nil)))
     (";+.*$"
      (0 (let ((start (match-beginning 0))
               (end (match-end 0)))
           (save-excursion
             (goto-char start)
             (while (re-search-forward "`[^`]+`" end t)
               (add-text-properties
                (match-beginning 0) (match-end 0)
                '(face fennel-docstring-backtick-face))))
           nil))))
   'append)
  (font-lock-flush)
  (font-lock-ensure))

(defun fennel-enable-backtick-highlighting ()
  "Enable backtick highlighting in Fennel files."
  (when (derived-mode-p 'fennel-mode)
    (highlight-backticks-in-quotes)))

(add-hook 'fennel-mode-hook #'fennel-enable-backtick-highlighting)

(defun nil-hash ()
  (interactive)
  (insert "#"))

(after! fennel-mode
  (autoload 'love2d-fennel "./love2d-fennel.el" nil t)

  (after! tree-sitter
    (require 'fennel-ts-mode)
    (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-ts-mode))
    (add-hook 'fennel-ts-mode-hook 'fennel-proto-repl-minor-mode))

  (defun project-proto-repl ()
    (when (functionp #'fennel-proto-repl-live-repls)
      (let ((repls (fennel-proto-repl-live-repls)))
        (when-let ((live-repl (car repls)))
          live-repl))))

  (defun fennel-link-project-proto-repl ()
    (interactive)
    (when-let ((live-repl (project-proto-repl)))
      (fennel-proto-repl-link-buffer live-repl)))

  (defun reopen-buffer-file ()
    "Kill the current buffer and reopen the file it is visiting."
    (interactive)          ; Make the function callable via M-x and keybindings.
    (let ((file-name (buffer-file-name)))
      (if file-name
          (progn
            (kill-buffer)
            (find-file file-name))
        (message "Buffer is not visiting a file!"))))

  (defun my/get-fennel-module-name ()
    (let* ((root (doom-project-root))
           (filename (buffer-file-name (buffer-base-buffer)))
           (path (abbreviate-file-name
                  (if root
                      (file-relative-name filename root)
                    filename)))
           (dotted-path (string-replace "/" "." path))
           (module (string-replace ".fnl" "" dotted-path))
           (module-name (if (s-ends-with? ".init" module)
                            (string-replace ".init" "" module)
                          module)))

      module-name))

  (defun my/reload-parent-modules (module)
    (let* ((module-name module)
           (dirs (split-string module-name "\\."))
           (bl (butlast dirs))
           (parent-dir (string-join bl ".")))
      (setq module-name parent-dir)

      (when (not (string-equal "src" module-name))
        (message "reloading %s" module-name)
        (fennel-reload-form module-name)
        (my/reload-parent-modules module-name))))

  (defun my/fennel-reload ()
    (interactive)
    (if (project-proto-repl)
        (fennel-proto-repl-reload nil)
      (let ((module-name (my/get-fennel-module-name)))
        (message "reloading %s" module-name)
        (fennel-reload nil)
        (my/reload-parent-modules module-name))))

  (defun my/set-fennel-module-name ()
    (interactive)
    (when (doom-project-root)
      (let* ((module-name (my/get-fennel-module-name)))
        (setq fennel-module-name module-name))))

  (defun my/enable-proto-repl-minor-mode ()
    (interactive)
    (when (project-proto-repl)
      (fennel-proto-repl-minor-mode 1)))

  (map! :after fennel-mode
        :map fennel-mode-map
        :localleader
        (:prefix ("b" . "buffer")
         :desc "reopen buffer file" "r" #'reopen-buffer-file)
        (:prefix ("=" . "format")
         :desc "format buffer" "=" #'fennel-format
         :desc "format region" "r" #'fennel-format-region)
        ;; (:prefix ("l" . "lsp")
        ;;  :desc "update lsp config" "c" #'fnl-lsp-config)
        (:prefix ("r" . "repl")
         :desc "comma command" "," #'fennel-proto-repl-comma-command
         :desc "develop repl" "d" #'fennel-love-2d-repl-debug
         :desc "interrupt repl" "i" #'fennel-proto-repl-interrupt
         :desc "LÖVE repl" "l" #'fennel-love-2d-repl
         :desc "LÖVE base repl" "L" #'fennel-love-2d-base-repl
         :desc "macro expand" "m" #'fennel-proto-repl-macroexpand
         :desc "proto repl" "p" #'fennel-proto-repl
         :desc "reload file" "r" #'my/fennel-reload
         :desc "base repl" "s" #'fennel-repl
         :desc "join project repl" "z" #'fennel-link-project-proto-repl
         (:prefix ("e" . "eval")
          :desc "eval buffer" "b" #'fennel-proto-repl-eval-buffer
          :desc "eval last sexp" "e" #'fennel-eval-last-sexp
          :desc "eval defun" "f" #'fennel-proto-repl-eval-defun
          :desc "eval last & next" "n" #'fennel-eval-form-and-next
          :desc "eval current form" "p" #'fennel-eval-toplevel-form
          :desc "eval region" "r" #'fennel-eval-region)
         (:prefix ("h" . "help")
          :desc "show args" "a" #'fennel-proto-repl-show-arglist
          :desc "show docs" "d" #'fennel-proto-repl-show-documentation
          :desc "show var docs" "v" #'fennel-proto-repl-show-var-documentation))
        (:prefix ("t" . "test")
         :desc "all" "a" #'fennel-test-all
         :desc "rerun" "r" #'fennel-test-last
         :desc "module" "t" #'fennel-test-module))

  (map! :after fennel-ts-mode
        :map fennel-ts-mode-map
        :localleader
        (:prefix ("b" . "buffer")
         :desc "reopen buffer file" "r" #'reopen-buffer-file)
        (:prefix ("=" . "format")
         :desc "format buffer" "=" #'fennel-format
         :desc "format region" "r" #'fennel-format-region)
        ;; (:prefix ("l" . "lsp")
        ;;  :desc "update lsp config" "c" #'fnl-lsp-config)
        (:prefix ("r" . "repl")
         :desc "comma command" "," #'fennel-proto-repl-comma-command
         :desc "develop repl" "d" #'fennel-love-2d-repl-debug
         :desc "interrupt repl" "i" #'fennel-proto-repl-interrupt
         :desc "LÖVE repl" "l" #'fennel-love-2d-repl
         :desc "LÖVE base repl" "L" #'fennel-love-2d-base-repl
         :desc "macro expand" "m" #'fennel-proto-repl-macroexpand
         :desc "proto repl" "p" #'fennel-proto-repl
         :desc "reload file" "r" #'my/fennel-reload
         :desc "base repl" "s" #'fennel-repl
         :desc "join project repl" "z" #'fennel-link-project-proto-repl
         (:prefix ("e" . "eval")
          :desc "eval buffer" "b" #'fennel-proto-repl-eval-buffer
          :desc "eval last sexp" "e" #'fennel-eval-last-sexp
          :desc "eval defun" "f" #'fennel-proto-repl-eval-defun
          :desc "eval last & next" "n" #'fennel-eval-form-and-next
          :desc "eval current form" "p" #'fennel-eval-toplevel-form
          :desc "eval region" "r" #'fennel-eval-region)
         (:prefix ("h" . "help")
          :desc "show args" "a" #'fennel-proto-repl-show-arglist
          :desc "show docs" "d" #'fennel-proto-repl-show-documentation
          :desc "show var docs" "v" #'fennel-proto-repl-show-var-documentation))
        (:prefix ("t" . "test")
         :desc "all" "a" #'fennel-test-all
         :desc "rerun" "r" #'fennel-test-last
         :desc "module" "t" #'fennel-test-module))

  (defun insert-lambda ()
    "Insert the lowercase lambda character (λ) at point."
    (interactive)
    (insert "λ"))

  (map! :i "M-l" #'insert-lambda)

  (with-eval-after-load 'lispy
    (lispy-define-key lispy-mode-map "#" 'nil-hash)
    (setq lispy-parens-preceding-syntax-alist
          (append lispy-parens-preceding-syntax-alist '((fennel-mode "[#`',.@]+")))))

  (after! (:and lsp-mode fennel-mode)
    (add-to-list 'lsp-language-id-configuration
                 '(fennel-mode . "fennel"))

    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "fennel-ls")
                      :activation-fn (lsp-activate-on "fennel")
                      :server-id 'fennel-ls))

    (add-hook 'fennel-mode-hook #'lsp)
    (add-hook 'fennel-mode-hook 'outline-minor-mode)
    (add-hook 'fennel-mode-hook #'my/enable-proto-repl-minor-mode)
    (add-hook 'fennel-mode-hook #'my/set-fennel-module-name)
    (add-hook 'fennel-mode-hook #'fennel-link-project-proto-repl)

    (add-hook 'fennel-ts-mode-hook #'lsp)
    (add-hook 'fennel-ts-mode-hook 'outline-minor-mode)
    (add-hook 'fennel-ts-mode-hook #'my/enable-proto-repl-minor-mode)
    (add-hook 'fennel-ts-mode-hook #'my/set-fennel-module-name)
    (add-hook 'fennel-ts-mode-hook #'fennel-link-project-proto-repl)

    (with-eval-after-load 'evil
      (define-key evil-normal-state-map (kbd "<tab>") nil)
      (define-key evil-motion-state-map (kbd "<tab>") nil)
      (evil-define-key 'normal outline-minor-mode-map (kbd "<tab>") #'my/outline-tab-behavior)
      (evil-define-key 'motion outline-minor-mode-map (kbd "<tab>") #'my/outline-tab-behavior))))

;;; Docstrings in the Fennel REPL's completion popup
;; fennel-complete returns bare candidates, so company/corfu have no doc
;; source for the selected one. `,doc' is that source.
(defvar +fennel-doc-cache (make-hash-table :test 'equal)
  "Cache of `,doc' output, keyed by symbol name.")

(defun +fennel-doc-string (candidate)
  "Ask the running Fennel REPL for CANDIDATE's docstring, or nil."
  (when-let* ((sym (and candidate (substring-no-properties (format "%s" candidate))))
              (proc (ignore-errors (inferior-lisp-proc))))
    (let ((cached (gethash sym +fennel-doc-cache 'miss)))
      (if (not (eq cached 'miss))
          cached
        (puthash
         sym
         (when-let ((buf (fennel-repl-redirect-one
                          proc (format ",doc %s" sym) " *fennel-doc*")))
           (with-current-buffer buf
             (let* ((raw (ansi-color-apply
                          (buffer-substring-no-properties (point-min) (point-max))))
                    ;; the game logs to the same stdout the REPL answers on,
                    ;; so a "[info reload] ..." line can land mid-redirect
                    (text (string-trim
                           (string-join
                            (seq-remove
                             (lambda (l)
                               (string-match-p
                                "\\`\\[\\(debug\\|info\\|warn\\|error\\)\\b" l))
                             (split-string raw "\n"))
                            "\n"))))
               (unless (or (string-empty-p text)
                           (string-suffix-p "not found" text))
                 text))))
         +fennel-doc-cache)))))

(defun +fennel-doc-buffer (candidate)
  "A buffer holding CANDIDATE's docstring, for `:company-doc-buffer'."
  (when-let ((doc (+fennel-doc-string candidate)))
    (with-current-buffer (get-buffer-create " *fennel-capf-doc*")
      (erase-buffer)
      (insert doc)
      (goto-char (point-min))
      (current-buffer))))

(defun +fennel-docsig (candidate)
  "CANDIDATE's arglist line, for the echo area."
  (car (split-string (or (+fennel-doc-string candidate) "") "\n" t)))

(defun +fennel-complete-with-docs (result)
  (when result
    (append result (list :company-doc-buffer #'+fennel-doc-buffer
                         :company-docsig    #'+fennel-docsig))))

(after! fennel-mode
  (advice-add 'fennel-complete :filter-return #'+fennel-complete-with-docs)
  ;; docstrings change under you on hot-reload; drop the cache whenever you
  ;; actually evaluate something (completion redirects bypass this hook)
  (add-hook 'fennel-repl-mode-hook
            (lambda ()
              (add-hook 'comint-input-filter-functions
                        (lambda (_) (clrhash +fennel-doc-cache) nil)
                        nil t))))

(defvar +fnl-markup-faces
  '(font-lock-comment-face font-lock-comment-delimiter-face
    font-lock-doc-face font-lock-doc-markup-face)
  "Faces whose regions get markdown-ish rendering.")

(defvar +fnl-markup-italic t
  "Set to nil if *earmuffed* names get mistaken for emphasis.")

(defun +fnl-markup-region-p (pos)
  (let* ((f (get-text-property pos 'face))
         (fs (if (listp f) f (list f))))
    (or (seq-some (lambda (x) (memq x +fnl-markup-faces)) fs)
        (nth 4 (syntax-ppss pos)))))          ; fallback: plain comment

(defun +fnl-markup-matcher (regexp)
  "Font-lock MATCHER for REGEXP, restricted to comments/docstrings."
  (lambda (limit)
    (let (found)
      (while (and (not found) (re-search-forward regexp limit t))
        ;; probe both ends: line-anchored regexps start *outside* the comment
        (when (or (+fnl-markup-region-p (match-beginning 0))
                  (+fnl-markup-region-p (max (point-min) (1- (match-end 0)))))
          (setq found t)))
      found)))

(defun +fnl-markup-keywords ()
  (append
   `((,(+fnl-markup-matcher "^\\s-*\\(;;;;+.*\\)$")
      (1 'markdown-header-face-2 prepend))
     (,(+fnl-markup-matcher "^\\s-*;;+ \\(#+ .*\\)$")
      (1 'markdown-header-face-3 prepend))
     (,(+fnl-markup-matcher "^\\s-*;;+\\(?:  \\)\\s-*[^ \n].*$")
      (0 'markdown-code-face prepend))
     (,(+fnl-markup-matcher "^\\s-*;;+\\s-+\\([-+*]\\|[0-9]+\\.\\)\\s-")
      (1 'markdown-list-face prepend))
     (,(+fnl-markup-matcher "\\*\\*[^*\n]+?\\*\\*")
      (0 'markdown-bold-face prepend))
     (,(+fnl-markup-matcher "`[^`\n]+?`")
      (0 'markdown-inline-code-face prepend))
     (,(+fnl-markup-matcher "\\[\\([^]\n]+\\)\\](\\([^)\n]+\\))")
      (1 'markdown-link-face prepend) (2 'markdown-url-face prepend)))
   (when +fnl-markup-italic
     `((,(+fnl-markup-matcher
          "\\(?:^\\|[[:space:](]\\)\\(\\*[^*\n]+?\\*\\)\\(?:[[:space:]).,;:!?]\\|$\\)")
        (1 'markdown-italic-face prepend))))))

(defun +fnl-markdownish-comments-h ()
  (require 'markdown-mode nil t)             ; needed for the faces
  (font-lock-add-keywords nil (+fnl-markup-keywords) 'append)
  (goto-address-prog-mode +1)                ; live URLs in comments, free
  (font-lock-flush))

(add-hook 'fennel-mode-hook #'+fnl-markdownish-comments-h)

(after! markdown-mode
  (add-to-list 'markdown-code-lang-modes '("fnl" . fennel-mode))
  (setq markdown-fontify-code-blocks-natively t))

(defvar resonance/fennel-proto-repl--async-callback nil
  "Permanent callback for the game's idle (id 0) output.")

(defun resonance/fennel-proto-repl-async-output (fn id)
  "Resolve id 0 to an async-output callback, the way FN resolves -1."
  (or (funcall fn id)
      (when (and (= id 0)
                 fennel-proto-repl--buffer
                 (buffer-live-p (get-buffer fennel-proto-repl--buffer)))
        (or resonance/fennel-proto-repl--async-callback
            (setq resonance/fennel-proto-repl--async-callback
                  (make-fennel-proto-repl-callback
                   :values #'ignore
                   :error  #'fennel-proto-repl--error-handler
                   :print  #'fennel-proto-repl--print))))))

(advice-add 'fennel-proto-repl--get-callbacks
            :around #'resonance/fennel-proto-repl-async-output)

(after! fennel-mode
  (setq fennel-proto-repl-eldoc-fontify-markdown t))

(advice-add 'fennel-proto-repl--font-lock-doc-buffer :before
            (lambda (&rest _) (setq-local markdown-hide-markup t)))
