;;; +golden-ratio.el -*- lexical-binding: t; -*-

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
