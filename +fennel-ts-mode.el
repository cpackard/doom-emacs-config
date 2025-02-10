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

;;; Base fennel setup
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

;;; fennel-ts faces
(defgroup fennel--tree-sitter-faces nil
  "Faces for highlighting code."
  :group 'treesit)

(defface fennel--font-lock-property-name-face
  '((default :inherit font-lock-constant-face :slant italic))
  "Face for properties."
  :group 'fennel--tree-sitter-faces)

;; EB7186
;; E9C49C
;; DD78D3
(defface fennel--font-lock-self-face
  '((default :inherit font-lock-type-face :foreground "#D4AA6C"))
  "Face for the `self` keyword."
  :group 'fennel--tree-sitter-faces)

(defface fennel--font-lock-builtin-face
  ;; '((default :inherit font-lock-builtin-face :foreground "#7886DD"))
  '((default :inherit font-lock-keyword-face))
  "Face for builtins."
  :group 'fennel--tree-sitter-faces)

;; goldenrod
(defface fennel--font-lock-macro-call-face
  '((default :inherit font-lock-preprocessor-face :slant italic :foreground "#EB7186"))
  "Face for macro calls."
  :group 'fennel--tree-sitter-faces)

(defface fennel--font-lock-function-call-face
  '((default :inherit (link font-lock-function-name-face) :underline nil :weight semi-bold))

  "Face for function calls."
  :group 'fennel--tree-sitter-faces)

;;; fennel-ts keywords and builtins
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

;;; custom queries used for font-lock
(defconst fennel--treesit-import-macros-query
  (treesit-query-compile
   'fennel
   '((import_macros_form
      imports: (table_binding
                item: (table_binding_pair value: (symbol_binding) @macro-import)))))
  "Query for all macro imports.")

(defconst fennel--treesit-macros-defun-query
  (treesit-query-compile
   'fennel
   '((macro_form name: (symbol) @macro-import)))
  "Query for all macro function definitions.")

(defconst fennel--treesit-multi-sym-fn-defuns-query
  (treesit-query-compile
   'fennel
   '((fn_form name: (multi_symbol base: (symbol_fragment) @multi-sym-base))))
  "Query for all multi-sym function definitions.")

(defun fennel--treesit-type-defun-p (node)
  "Check whether NODE is a type definition."
  (let* ((is-type-defun nil)
         (root-node (treesit-buffer-root-node 'fennel))
         (multi-sym-defuns (treesit-query-capture
                            root-node
                            fennel--treesit-multi-sym-fn-defuns-query)))
    (dolist (pair multi-sym-defuns)
      ;; (message "comparing fn def %s to local %s"
      ;;          (treesit-node-text (cdr pair))
      ;;          (treesit-node-text node))
      (when (string-equal (treesit-node-text (cdr pair))
                          (treesit-node-text node))
        (setq is-type-defun t)))
    is-type-defun))

(defun fennel--treesit-fontify-type-defun (node override start end &rest _)
  "Fontify type definitions.
NODE is the symbol being called.
OVERRIDE is the override flag described in `treesit-font-lock-rules'.
START and END mark the region to be
fontified."
  (if (fennel--treesit-type-defun-p node)
      (treesit-fontify-with-override
       (treesit-node-start node) (treesit-node-end node)
       'font-lock-type-face override start end)
    (treesit-fontify-with-override
     (treesit-node-start node) (treesit-node-end node)
     'font-lock-variable-name-face override start end)))

(defun fennel--treesit-macro-call-p (node)
  "Check whether NODE is a macro call."
  (let* ((is-macro-call nil)
         (root-node (treesit-buffer-root-node 'fennel))
         (macro-imports (treesit-query-capture
                         root-node
                         fennel--treesit-import-macros-query))
         (macro-defuns (treesit-query-capture
                        root-node
                        fennel--treesit-macros-defun-query)))
    (dolist (pair macro-imports)
      (when (string-equal (treesit-node-text (cdr pair))
                          (treesit-node-text node))
        (setq is-macro-call t)))
    (dolist (pair macro-defuns)
      (when (string-equal (treesit-node-text (cdr pair))
                          (treesit-node-text node))
        (setq is-macro-call t)))
    is-macro-call))

(defun fennel--treesit-fontify-function-call (node override start end &rest _)
  "Fontify function calls.
NODE is the symbol being called.
OVERRIDE is the override flag described in `treesit-font-lock-rules'.
START and END mark the region to be
fontified."
  (if (fennel--treesit-macro-call-p node)
      (treesit-fontify-with-override
       (treesit-node-start node) (treesit-node-end node)
       'fennel--font-lock-macro-call-face override start end)
    (treesit-fontify-with-override
     (treesit-node-start node) (treesit-node-end node)
     'fennel--font-lock-function-call-face override start end)))

;;; font lock rules
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
      args: (sequence_arguments item: (symbol_binding) :? @font-lock-variable-name-face))
     ;; (fn_form
     ;;  name: (symbol) @font-lock-function-name-face
     ;;  args: (sequence_arguments item: (symbol_binding) @font-lock-variable-name-face))
     (macro_form
      name: (symbol) @font-lock-function-name-face
      args: (sequence_arguments item: (symbol_binding) @font-lock-variable-name-face))
     (hashfn_reader_macro
      macro: _ @font-lock-function-name-face
      expression: (list item: ((symbol) @font-lock-variable-name-face
                               (:match "$.*" @font-lock-variable-name-face)))))


   ;; L2 features
   :feature 'string
   :language 'fennel
   '(((string open: _ @open) @font-lock-string-face
      (:match "[^:]" @open))
     (string open: ":") @font-lock-builtin-face
     (table_pair value: (string) @font-lock-string-face)
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
     ((symbol) @fennel--font-lock-self-face
      (:match "\\`self\\'" @fennel--font-lock-self-face))
     ((symbol_binding) @fennel--font-lock-self-face
      (:match "\\`self\\'" @fennel--font-lock-self-face))
     ((symbol_fragment) @fennel--font-lock-self-face
      (:match "\\`self\\'" @fennel--font-lock-self-face)))

   ;; L3 features
   :feature 'assignment
   :language 'fennel
   '(
     ;; local / let / set / var
     (local_form
      (binding_pair lhs: (symbol_binding) @fennel--treesit-fontify-type-defun))
     (let_form
      call: (symbol) @keyword
      vars: (let_vars
             (binding_pair
              lhs: (symbol_binding) @font-lock-variable-name-face)))
     (set_form
      (binding_pair lhs: (symbol_binding) @font-lock-warning-face))
     (set_form
      (binding_pair
       lhs: (multi_symbol member: (symbol_fragment) @font-lock-warning-face :anchor)))
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
   `(((symbol) @fennel--font-lock-builtin-face
      (:match ,(regexp-opt fennel--treesit-builtins 'symbols) @fennel--font-lock-builtin-face))
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
     (nil) @font-lock-constant-face
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
   '((list
      call: [(symbol) @fennel--treesit-fontify-function-call
             (multi_symbol
              member:
              (symbol_fragment) @fennel--font-lock-function-call-face :anchor)])
     (multi_symbol_method
      method: (symbol_fragment) @fennel--font-lock-function-call-face))

   :feature 'property
   :language 'fennel
   :override 'append
   '((multi_symbol member: (symbol_fragment) @fennel--font-lock-property-name-face)
     (multi_symbol_method
      method: (symbol_fragment) @fennel--font-lock-property-name-face))))

;;; imenu
(defun fennel--treesit-defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ((or "fn_form" "macro_form")
     (treesit-node-text
      (treesit-node-child-by-field-name
       node "name")
      t))))

;;; indent rules
(defconst fennel--treesit-indent-rules
  `((fennel
     ((parent-is "case_guard") prev-sibling 0))))

;;; treesitter setup
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
  (setq-local treesit-defun-type-regexp (rx (or "fn" "macro")
                                            "_form"))
  (setq-local treesit-defun-name-function
              #'fennel--treesit-defun-name)

  ;; (setq-local treesit-simple-indent-rules
  ;;             fennel-ts-indent-rules)
  (setq-local treesit-font-lock-level 4)
  (treesit-major-mode-setup))

;;; Define major mode
;;;###autoload
(define-derived-mode fennel-ts-mode fennel-mode "Fennel-ts"
  "Major mode for editing Fennel files, using tree-sitter library.

\\{fennel-ts-mode-map}"
  :syntax-table fennel-mode-syntax-table
  (when (treesit-ready-p 'fennel)
    (treesit-parser-create 'fennel)
    (fennel-ts-setup)
    (base-fennel-mode-setup)))

;;; FIXME: use the more explicit method mentioned here: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;;;###autoload
(setq major-mode-remap-alist
      '((fennel-mode . fennel-ts-mode)))

(provide 'fennel-ts-mode)
;;; fennel-ts-mode.el ends here
