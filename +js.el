;;; +js.el -*- lexical-binding: t; -*-

(after! rjsx-mode
  ;; Disable LSP formatting to use prettier
  (setq-hook! 'rjsx-mode-hook +format-with-lsp nil)
  ;; format-on-save doesn't work by default with rjsx-mode
  (add-hook 'rjsx-mode-hook (lambda () (add-hook 'before-save-hook #'+format-buffer-h))))
