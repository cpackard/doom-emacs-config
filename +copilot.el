;;; +copilot.el -*- lexical-binding: t; -*-

;; Github Copilot
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (setq custom--clojure-indent-basic 0)
  (setq custom--elisp-indent-basic 0)
  (setq custom--fennel-indent-basic 0)
  (add-to-list 'copilot-indentation-alist '(clojure-mode custom--clojure-indent-basic))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode custom--elisp-indent-basic))
  (add-to-list 'copilot-indentation-alist '(fennel-mode custom--fennel-indent-basic)))

;; A company front-end with icons.
;; https://github.com/sebastiencs/company-box
;;
;; Recommended for copilot: https://github.com/copilot-emacs/copilot.el
(use-package! company-box
  :hook (company-mode . company-box-mode))
