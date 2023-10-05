;;; +fennel.el -*- lexical-binding: t; -*-

(defun fennel-love-2d-repl ()
  (interactive)
  (fennel-proto-repl "love ."))

(after! fennel-mode
  (map! :after fennel-mode
        :localleader
        :prefix ("r" . "repl")
        :desc "base repl" "s" #'fennel-repl
        :desc "LÖVE repl" "l" #'fennel-love-2d-repl))
