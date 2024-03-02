;;; +irc.el -*- lexical-binding: t; -*-

(setq auth-sources '("~/.authinfo.gpg"))

(defun fetch-irc-password (&rest params)
  (require 'auth-source)
  (let ((match (car (apply 'auth-source-search params))))
    (if match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "Password not found for %S" params))))

(defun fetch-nickserv-password (server)
  (fetch-irc-password :user "cpackard" :machine "irc.libera.chat"))

(after! circe
  (set-irc-server! "irc.libera.chat"
    `(:tls t
      :port 6697
      :nick "cpackard"
      :sasl-username "cpackard"
      :sasl-password fetch-nickserv-password
      :channels ("#fennel" "#emacs" "#emacs-circe"))))
