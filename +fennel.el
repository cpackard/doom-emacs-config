;;; +fennel.el -*- lexical-binding: t; -*-

(defun fennel-love-2d-repl ()
  (interactive)
  (fennel-proto-repl "love ."))

(after! fennel-mode
   (autoload 'fennel-proto-repl (expand-file-name "~/.emacs.d/.local/straight/build-28.2/fennel-mode/fennel-proto-repl.el") nil t)
   (add-hook 'fennel-mode-hook 'fennel-proto-repl-minor-mode)
   (map! :after fennel-mode
         :localleader
         :prefix ("r" . "repl")
         :desc "base repl" "s" #'fennel-repl
         :desc "LÖVE repl" "l" #'fennel-love-2d-repl))



