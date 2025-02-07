;;; fennel-ts-mode.el --- A major-mode for editing Fennel code, powered by Tree Sitter -*- lexical-binding: t; -*-

;; Copyright © 2025 Christian Packard and contributors

;; Author: Christian Packard
;; URL: https://git.sr.ht/~technomancy/fennel-mode
;; Version: 0.1.0
;; Created: 2025-02-04
;; Package-Requires: ((emacs "29.4"))
;; Keywords: languages, tools

;;; Commentary:

;; Provides font-lock, indentation, navigation, and REPL for Fennel code.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'fennel-mode)
(require 'treesit)

(defun base-fennel-mode-setup ()
  (setq-local comment-add 1)            ; default to `;;' in comment-region
  (setq-local comment-column 40)
  (setq-local comment-end "")
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-use-syntax t)
  (setq-local electric-pair-open-newline-between-pairs nil)
  (setq-local electric-pair-skip-whitespace 'chomp)
  (setq-local fill-paragraph-function #'lisp-fill-paragraph)
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local lisp-doc-string-elt-property 'fennel-doc-string-elt)
  ;; (setq-local lisp-indent-function #'fennel-indent-function)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local normal-auto-fill-function #'do-auto-fill)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local outline-level 'lisp-outline-level)
  (setq-local outline-regexp ";;;;* [^ \t\n]\\|(")
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local inferior-lisp-program fennel-program)
  (setq-local comint-prompt-regexp
              (format "^\\(?:%s\\|%s\\) "
                      (regexp-quote fennel-mode-repl-prompt)
                      (regexp-quote fennel-mode-repl-subprompt)))
  (setq-local comint-use-prompt-regexp t)
  ;; NOTE: won't work if the fennel module name has changed but beats nothing
  (setq-local inferior-lisp-load-command "((. (require :fennel) :dofile) %s)")
  (add-to-list 'imenu-generic-expression `(nil ,fennel-local-fn-pattern 1))
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions 'fennel-complete)
  (add-hook 'xref-backend-functions #'fennel--xref-backend nil t)
  (set-syntax-table fennel-mode-syntax-table)
  ;; (fennel-font-lock-setup)
  (add-hook 'paredit-mode-hook #'fennel-paredit-setup nil t))

(defvar fennel--treesit-keywords
  (append fennel-keywords
          '("..."
            "$...")
          nil))

(defvar fennel--treesit-builtins
  (append fennel-builtin-functions
          '("unpack"
            "pack")
          nil))

(defvar fennel--treesit-builtin-tables
  '("_G" "io" "math" "os"))

(defvar fennel--treesit-settings
  (treesit-font-lock-rules
   ;; L1 features
   :feature 'comment
   :language 'fennel
   '((comment) @font-lock-comment-face)

   :feature 'definition
   :language 'fennel
   '((fn_form
      name: [(multi_symbol
              base: (symbol_fragment) @font-lock-type-face
              member: (symbol_fragment) @font-lock-function-name-face)
             ((symbol) @font-lock-function-name-face)]
      args: (sequence_arguments item: (symbol_binding) @font-lock-variable-name-face))
     ;; (fn_form
     ;;  name: (symbol) @font-lock-function-name-face
     ;;  args: (sequence_arguments item: (symbol_binding) @font-lock-variable-name-face))
     (macro_form
      name: (symbol) @font-lock-function-name-face
      args: (sequence_arguments item: (symbol_binding) @font-lock-variable-name-face)))


   ;; L2 features
   :feature 'string
   :language 'fennel
   '(((string open: _ @open) @font-lock-string-face
      (:match "[^:]" @open))
     (string open: ":") @font-lock-builtin-face
     (string_binding ":") @font-lock-builtin-face
     (docstring content: (string_content)) @font-lock-doc-face)

   :feature 'keyword
   :language 'fennel
   :override t
   `(
     ;; match any symbols or symbol bindings for keywords
     ((symbol) @font-lock-keyword-face
      (:match ,(regexp-opt fennel--treesit-keywords 'symbols) @font-lock-keyword-face))
     ((symbol_binding) @font-lock-keyword-face
      (:match ,(regexp-opt fennel--treesit-keywords 'symbols) @font-lock-keyword-face))
     (case_guard
      call: (symbol) @font-lock-keyword-face))

   :feature 'type
   :language 'fennel
   :override t
   '(
     ;; match any instance of `self' a keyword
     ((symbol) @font-lock-type-face
      (:match "\\`self\\'" @font-lock-type-face))
     ((symbol_binding) @font-lock-type-face
      (:match "\\`self\\'" @font-lock-type-face))
     ((symbol_fragment) @font-lock-type-face
      (:match "\\`self\\'" @font-lock-type-face)))

   ;; L3 features
   :feature 'assignment
   :language 'fennel
   '(
     ;; local / let / set / var
     (local_form
      (binding_pair lhs: (symbol_binding) @font-lock-variable-name-face))
     (let_form
      call: (symbol) @keyword
      vars: (let_vars
             (binding_pair
              lhs: (symbol_binding) @font-lock-variable-name-face)))
     (set_form
      (binding_pair lhs: (symbol_binding) @font-lock-warning-face))
     (var_form
      (binding_pair
       lhs: (symbol_binding) @font-lock-warning-face))
     ;; &rest
     (rest_binding
      lhs: (symbol_option) @font-lock-keyword-face
      rhs: (symbol_binding) @font-lock-variable-name-face)
     ;; accumulate / faccumulate
     (accumulate_form
      iter_body: (iter_body
                  (accumulator_pair accumulator_binding: (symbol_binding) @font-lock-warning-face)))
     (faccumulate_form
      iter_body: (for_iter_body
                  (accumulator_pair accumulator_binding: (symbol_binding) @font-lock-warning-face)
                  index: (symbol_binding) @font-lock-variable-name-face))
     ;; case
     (case_guard
      item: (symbol_binding) @font-lock-variable-name-face)
     (case_pair
      lhs: [
            (symbol_binding) @font-lock-variable-name-face
            (sequence_binding item: (symbol_binding) @font-lock-variable-name-face)])
     ;; table bindings
     (table_binding_pair
      value: (symbol_binding) @font-lock-variable-name-face)
     (table_pair
      value: (symbol) @font-lock-variable-name-face)
     ;; iter bodies
     (iter_body binding: (symbol_binding) @font-lock-variable-name-face)
     (list_binding item: (symbol_binding) @font-lock-variable-name-face))

   :feature 'builtin
   :language 'fennel
   `(((symbol) @font-lock-builtin-face
      (:match ,(regexp-opt fennel--treesit-builtins 'symbols) @font-lock-builtin-face))
     (unquote_reader_macro expression: (symbol) @font-lock-warning-face))

   :feature 'number
   :language 'fennel
   '((number) @font-lock-number-face)

   :feature 'escape-sequence
   :language 'fennel
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :feature 'constant
   :language 'fennel
   `(["true" "false"] @font-lock-constant-face
     (multi_symbol
      base:
      ((symbol_fragment) @font-lock-constant-face
       (:match ,(regexp-opt fennel--treesit-builtin-tables 'symbols) @font-lock-constant-face))))

   ;; L4 features
   :feature 'bracket
   :language 'fennel
   '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

   :feature 'function
   :language 'fennel
   '((list call: (symbol) @font-lock-function-call-face)
     (multi_symbol
      member:
      (symbol_fragment) @font-lock-function-call-face :anchor)
     (multi_symbol_method
      method: (symbol_fragment) @font-lock-function-call-face))

   :feature 'property
   :language 'fennel
   '((multi_symbol member: (symbol_fragment) @font-lock-property-name-face))))

(defun fennel-ts-setup ()
  "Setup treesit for fennel-ts-mode."
  ;; definition, type, assignment, builtin, constant, keyword,
  ;; comment, doc, operator, property,
  ;; escape-sequence, key (in key-value pairs).
  (setq-local treesit-font-lock-feature-list
              ;; '(( comment definition)
              ;;   ( keyword string type)
              ;;   ( assignment builtin constant escape-sequence number)
              ;;   ( bracket delimiter function variable property))
              '(( comment definition)
                ( keyword string type)
                (assignment builtin constant escape-sequence number)
                ( bracket function property)))

  (setq-local treesit-font-lock-settings fennel--treesit-settings)
  ;; (setq-local imenu-create-index-function
  ;;             #'fennel-imenu-treesit-create-index)
  ;; (setq-local treesit-defun-type-regexp (rx (or "function" "class")
  ;;                                           "_definition"))
  ;; (setq-local treesit-defun-name-function
  ;;             #'fennel--treesit-defun-name)

  ;; (setq-local treesit-simple-indent-rules
  ;;             fennel-ts-indent-rules)
  (message "starting fnl treesit-major-mode-setup...")
  (setq-local treesit-font-lock-level 4)
  (treesit-major-mode-setup)
  (message "finished fnl treesit-major-mode-setup."))

;;;###autoload
(define-derived-mode fennel-ts-mode fennel-mode "Fennel-ts"
  "Major mode for editing Fennel files, using tree-sitter library.

\\{fennel-ts-mode-map}"
  :syntax-table fennel-mode-syntax-table
  (when (treesit-ready-p 'fennel)
    (treesit-parser-create 'fennel)
    (fennel-ts-setup)
    (base-fennel-mode-setup)))
;; (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-ts-mode))
;; (add-to-list 'interpreter-mode-alist '("fennel" . fennel-ts-mode))


;;; FIXME: use the more explicit method mentioned here: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;;;###autoload
(setq major-mode-remap-alist
      '((fennel-mode . fennel-ts-mode)))

(provide 'fennel-ts-mode)
;;; fennel-ts-mode.el ends here
