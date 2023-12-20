;;; +copilot.el -*- lexical-binding: t; -*-

(after! copilot
  (setq custom--clojure-indent-basic 0)
  (setq custom--elisp-indent-basic 0)
  (add-to-list 'copilot--indentation-alist '(clojure-mode custom--clojure-indent-basic))
  (add-to-list 'copilot--indentation-alist '(emacs-lisp-mode custom--elisp-indent-basic)))
