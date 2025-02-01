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
    (fennel-repl "love . --debug")))

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
  (letenv! (("LOVE_DEBUG" "1")
            ("SDL_VIDEODRIVER" "dummy")
            ("FAITH_DIFF" "diff -u %s %s | diff-so-fancy"))
    (let ((test-command (format "cd %s && love . --test %s" (doom-project-root) (file-to-module))))
      (setq last-fennel-test test-command)
      (compile test-command))))

(defun fennel-test-last ()
  "Re-run the previous fennel test."
  (interactive)
  (letenv! (("LOVE_DEBUG" "1")
            ("SDL_VIDEODRIVER" "dummy")
            ("FAITH_DIFF" "diff -u %s %s | diff-so-fancy"))
    (compile last-fennel-test)))

(defun fennel-test-all ()
  "Run all tests for the current project."
  (interactive)
  (letenv! (("SDL_VIDEODRIVER" "dummy")
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

;;; Highlight docstring words starting with a backtick and ending in a single quote.

;; (defface fennel-docstring-backtick-single-quote-face
;;   '((t :foreground "dark cyan"))  ;; You can choose a different color if needed
;;   "Face for highlighting words starting with a backtick and ending with a single quote in Fennel docstrings."
;;   :group 'fennel)

;; (defun fennel-font-lock-extend-docstrings-backtick-single-quote ()
;;   (font-lock-add-keywords
;;    nil
;;    '(("`\\([^']+\\)'" 1 'fennel-docstring-backtick-single-quote-face prepend))))

;; (add-hook 'fennel-mode-hook #'fennel-font-lock-extend-docstrings-backtick-single-quote)

(defun nil-hash ()
  (interactive)
  (insert "#"))

(defun my/fennel-tree-sitter-defaults ()
  (let* ((keywords "#|%|\\\\*|\\\\+|-|->|->>|-\\\\?>|-\\\\?>>|\\\\.|\\\\.\\\\.|/|//|:|<|<=|=|>|>=|\\\\?\\\\.|\\\\^|\\\\$\\\\.\\\\.\\\\.\\\\.|accumulate|and|band|bnot|bor|bxor|collect|comment|do|doto|each|eval-compiler|fcollect|fn|for|global|hashfn|icollect|if|import-macros|include|lambda|length|let|local|lshift|lua|macro|macrodebug|macros|match|match-try|not|not=|or|partial|pick-args|pick-values|quote|require-macros|rshift|set|set-forcibly!|tset|values|var|when|while|with-open|~=|λ|faccumulate|case|case-try")
         (function-builtins "pairs|ipairs|type|tostring|print|unpack|require")
         (constant-builtins "_G|os|io|math")
         (core "
(comment) @comment

(number) @number

[\"false\" \"true\"] @boolean

(if_form call: (symbol) @conditional)

")
         (self-patterns "
(sequence_arguments
  item: (
    (symbol_binding) @type
    (#eq? @type \"self\" )
  )
)

(
  (symbol) @type
  (#eq? @type \"self\" )
)

(
  (symbol_binding) @type
  (#eq? @type \"self\" )
)

(
  (symbol_fragment) @type
  (#eq? @type \"self\" )
)

(fn_form
  name: (multi_symbol
          base: (symbol_fragment) @type
          member: (symbol_fragment) @function))

")
         (locals-patterns "
(local_form
  call: (symbol) @keyword
  (binding_pair
    lhs: (symbol_binding) @variable
  )
)

(let_form
  call: (symbol) @keyword
  vars: (let_vars
    (binding_pair
      lhs: (symbol_binding) @variable
    )
  )
)

(set_form
  call: (symbol) @keyword
  (binding_pair lhs: (symbol_binding) @variable.special)
)

(var_form
  call: (symbol) @keyword
  (binding_pair
    lhs: (symbol_binding) @variable.special
  )
)

")
         (fn-patterns "
(fn_form
  call: (symbol) @keyword
  name: (symbol)? @function
  args: (sequence_arguments item: (symbol_binding) @variable.parameter)?
  docstring: (docstring content: (string_content) @doc)?
)
")
         (rest-patterns "
(rest_binding
  lhs: (symbol_option) @keyword
  rhs: (symbol_binding) @variable
)
")
         (macro-patterns "
(import_macros_form call: (symbol) @keyword)

(hashfn_reader_macro macro: \"#\" @keyword)

(macro_form
  call: (symbol) @keyword
  name: (symbol) @function
  args: (sequence_arguments item: (symbol_binding) @variable.parameter)?
  docstring: (docstring content: (string_content) @doc)?
)

(unquote_reader_macro expression: (symbol) @variable.special)

(icollect_form
  call: (symbol) @keyword

)

(accumulate_form
  call: (symbol) @keyword
  iter_body: (iter_body
    (accumulator_pair accumulator_binding: (symbol_binding) @variable.special)
  )
)

(faccumulate_form
  call: (symbol) @keyword
  iter_body: (for_iter_body
    (accumulator_pair accumulator_binding: (symbol_binding) @variable.special)
    index: (symbol_binding) @variable
  )
)

(iter_option option: (symbol_option) @keyword)

")
         (case-patterns "
(case_form
   call: (symbol) @keyword.conditional
)

(case_pair
  lhs: [
    (symbol_binding) @variable
    (sequence_binding item: (symbol_binding) @variable)
  ]
)

(case_guard
  call: (symbol) @keyword.conditional
  item: (symbol_binding) @variable
)
")
         (table-patterns "
(table_binding_pair
  value: (symbol_binding) @variable
)

(table_pair
  value: (symbol) @variable
)

")
         (defaults (format
                    "
(multi_symbol member: (symbol_fragment) @property)

(multi_symbol_method
  method: (symbol_fragment) @method.call
)

(iter_body binding: (symbol_binding) @variable )

((symbol) @keyword
 (#match? @keyword \"^(%s)$\"
))

(
  (symbol_binding) @keyword
  (#eq? @keyword \"...\")
)

(
  (symbol) @keyword
  (#eq? @keyword \"$...\")
)

((symbol) @function.builtin
 (#match? @function.builtin \"^(%s)$\"
))

((symbol) @constant.builtin
 (#match? @constant.builtin \"^(%s)$\"
))

(multi_symbol
  base:
    ((symbol_fragment) @constant.builtin
     (#match? @constant.builtin \"^(%s)$\"
    ))
)

(list
  call:
    ((symbol) @function.call
     (#not-match? @function.call \"^(%s)$\"
    ))
)

(list_binding item: (symbol_binding) @variable)

(string_binding open: \":\") @constant
(string open: \":\") @constant
(string_content (escape_sequence) @escape)
(string) @string

"
                    keywords
                    function-builtins
                    constant-builtins
                    constant-builtins
                    keywords))
         (all-patterns (concat core
                               self-patterns
                               locals-patterns
                               fn-patterns
                               rest-patterns
                               macro-patterns
                               case-patterns
                               table-patterns
                               defaults)))
    (setq-local tree-sitter-hl-default-patterns all-patterns)
    nil))

(after! fennel-mode
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
    (interactive)        ; Make the function callable via M-x and keybindings.
    (let ((file-name (buffer-file-name)))
      (if file-name
          (progn
            (kill-buffer)
            (find-file file-name))
        (message "Buffer is not visiting a file!"))))

  (defun my/fennel-reload ()
    (interactive)
    (if (project-proto-repl)
        (fennel-proto-repl-reload nil)
      (fennel-reload nil)))

  (defun my/set-fennel-module-name ()
    (interactive)
    (when (doom-project-root)
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
         :desc "module" "t" #'fennel-test-module))

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
    (add-hook 'fennel-mode-hook #'my/fennel-tree-sitter-defaults)
    (with-eval-after-load 'evil
      (define-key evil-normal-state-map (kbd "<tab>") nil)
      (define-key evil-motion-state-map (kbd "<tab>") nil)
      (evil-define-key 'normal outline-minor-mode-map (kbd "<tab>") #'my/outline-tab-behavior)
      (evil-define-key 'motion outline-minor-mode-map (kbd "<tab>") #'my/outline-tab-behavior))))
