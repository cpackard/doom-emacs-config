;;; +code-review.el -*- lexical-binding: t; -*-

(after! code-review
  ;; See pretty symbols
  (add-hook 'code-review-mode-hook #'emojify-mode)
  ;; Define line wrap in comment sections.
  (setq code-review-fill-column 80)
  ;; Change how code-review splits the buffer when opening a new PR.
  ;; Defaults to #'switch-to-buffer-other-window.
  (setq code-review-new-buffer-window-strategy #'switch-to-buffer)
  ;; Change the destination where binary files is downloaded.
  (setq code-review-download-dir "/tmp/code-review/")
  ;; Use passwords configured for forge. The default is 'code-review.
  (setq code-review-auth-login-marker 'forge)
  ;; include *Code-Review* buffer into current workspace
  (add-hook 'code-review-mode-hook
            (lambda ()
              (persp-add-buffer (current-buffer))))
  ;; Add keybindings
  (map! :map magit-mode-map
        :prefix "C-c"
        :desc "open review" "r" #'code-review-forge-pr-at-point)
  (map! :mode code-review-mode
        :localleader
        :desc "next comment" "n" #'code-review-comment-jump-next
        :desc "prev comment" "p" #'code-review-comment-jump-previous))
