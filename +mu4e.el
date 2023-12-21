;;; +mu4e.el -*- lexical-binding: t; -*-

(after! (:and mu4e f)
  ;; Automatically wrap long lines
  (setq-default auto-fill-function 'message-do-auto-fill)

  (let ((emails-file (if (file-directory-p (expand-file-name "~/.config/doom"))
                         (expand-file-name "~/.config/doom/.emails")
                       (expand-file-name "~/.doom.d/.emails"))))
    (pcase-let ((`(,icloud-email ,fastmail-email ,gmail-email)
                 (s-split "\n" (f-read-text emails-file))))
      (progn
        ;; we installed this with homebrew
        (setq mu4e-mu-binary (executable-find "mu"))

        ;; this is the directory we created before:
        (setq mu4e-root-maildir "~/.maildir")

        ;; this command is called to sync imap servers:
        (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
        ;; how often to call it in seconds:
        (setq mu4e-update-interval 300)

        ;; save attachment to desktop by default
        ;; or another choice of yours:
        (setq mu4e-attachment-dir "~/Downloads")

        ;; rename files when moving - needed for mbsync:
        (setq mu4e-change-filenames-when-moving t)

        ;; list of your email adresses:
        (setq +mu4e-personal-addresses `(,icloud-email ,fastmail-email))

        ;; extend the default bookmark actions
        (setq mu4e-bookmarks
              '((:name "Today's Inbox"
                 :query "date:today..now AND maildir:/fastmail/INBOX"
                 :key  ?i)
                (:name "Today's messages"
                 :query "date:today..now AND NOT (maildir:/fastmail/Archive OR maildir:/gmail/Archive OR maildir:/icloud/Archive)"
                 :key ?t)
                (:name "Last 7 days"
                 :query "date:7d..now"
                 :hide-unread t
                 :key ?w)
                (:name "Flagged messages"
                 :query "flag:flagged"
                 :key ?f)))

        ;; check your ~/.maildir to see how the subdirectories are called
        ;; for the generic imap account:
        ;; e.g `ls ~/.maildir/example'
        (setq   mu4e-maildir-shortcuts
                '(("/icloud/INBOX" . ?i)
                  ("/icloud/Sent Messages" . ?I)
                  ("/fastmail/INBOX" . ?f)
                  ("/fastmail/Sent" . ?F)))

        ;; This controls the account context one is in. Helpful for instance, when composing an email. You can then select the context, which sets at the same time the sender.
        (setq mu4e-contexts
              `(,(make-mu4e-context
                  :name "icloud"
                  :enter-func
                  `(lambda () (mu4e-message (format "Enter %s context" ,icloud-email)))
                  :leave-func
                  `(lambda () (mu4e-message (format "Leave %s context" ,icloud-email)))
                  :match-func
                  `(lambda (msg)
                     (when msg
                       (mu4e-message-contact-field-matches msg
                                                           :to ,icloud-email)))
                  :vars `((user-mail-address . ,icloud-email)
                          (user-full-name . "Christian Packard")
                          (mu4e-drafts-folder . "/icloud/Drafts")
                          (mu4e-refile-folder . "/icloud/Archive")
                          (mu4e-sent-folder . "/icloud/Sent Messages")
                          (mu4e-trash-folder . "/icloud/Deleted Messages")))
                ,(make-mu4e-context
                  :name "fastmail"
                  :enter-func
                  `(lambda () (mu4e-message (format "Enter %s context" ,fastmail-email)))
                  :leave-func
                  `(lambda () (mu4e-message (format "Leave %s context" ,fastmail-email)))
                  :match-func
                  `(lambda (msg)
                     (when msg
                       (mu4e-message-contact-field-matches msg
                                                           :to ,fastmail-email)))
                  :vars `((user-mail-address . ,fastmail-email)
                          (user-full-name . "Christian Packard")
                          ;; check your ~/.maildir to see how the subdirectories are called
                          ;; e.g `ls ~/.maildir/example'
                          (mu4e-drafts-folder . "/fastmail/Drafts")
                          (mu4e-refile-folder . "/fastmail/Archive")
                          (mu4e-sent-folder . "/fastmail/Sent")
                          (mu4e-trash-folder . "/fastmail/Trash")))))

        (if gmail-email
            (progn
              (setq mu4e-contexts
                    (append mu4e-contexts `(,(make-mu4e-context
                                              :name "gmail"
                                              :enter-func
                                              `(lambda () (mu4e-message (format "Enter %s context" ,gmail-email)))
                                              :leave-func
                                              `(lambda () (mu4e-message (format "Leave %s context" ,gmail-email)))
                                              :match-func
                                              `(lambda (msg)
                                                 (when msg
                                                   (mu4e-message-contact-field-matches msg
                                                                                       :to ,gmail-email)))
                                              :vars `((user-mail-address . ,gmail-email)
                                                      (user-full-name . "Christian Packard")
                                                      (mu4e-drafts-folder . "/gmail/Drafts")
                                                      (mu4e-refile-folder . "/gmail/Archive")
                                                      (mu4e-sent-folder . "/gmail/Sent")
                                                      (mu4e-trash-folder . "/gmail/Trash"))))))
              (setq   mu4e-maildir-shortcuts
                      (append mu4e-maildir-shortcuts
                              '(("/gmail/INBOX" . ?g)
                                ("/gmail/Sent" . ?G))))))


        (setq mu4e-context-policy 'pick-first) ;; start with the first (default) context;
        (setq mu4e-compose-context-policy 'ask) ;; ask for context if no context matches;


        ;; gpg encryptiom & decryption:
        ;; this can be left alone
        ;; (require 'epa-file)
        ;; (epa-file-enable)
        ;; (setq epa-pinentry-mode 'loopback)
        ;; (auth-source-forget-all-cached)

        ;; don't keep message compose buffers around after sending:
        (setq message-kill-buffer-on-exit t)

        ;; send function:
        (setq send-mail-function 'sendmail-send-it
              message-send-mail-function 'sendmail-send-it)

        ;; send program:
        ;; this is exeranal. remember we installed it before.
        (setq sendmail-program (executable-find "msmtp"))

        ;; select the right sender email from the context.
        (setq message-sendmail-envelope-from 'header)

        ;; chose from account before sending
        ;; this is a custom function that works for me.
        ;; well I stole it somewhere long ago.
        ;; I suggest using it to make matters easy
        ;; of course adjust the email adresses and account descriptions
        (defun timu/set-msmtp-account ()
          (if (message-mail-p)
              (save-excursion
                (let*
                    ((from (save-restriction
                             (message-narrow-to-headers)
                             (message-fetch-field "from")))
                     (account
                      (cond
                       ((string-match icloud-email from) "icloud")
                       ((string-match fastmail-email from) "fastmail")
                       ((string-match gmail-email from) "gmail"))))
                  (setq message-sendmail-extra-arguments (list '"-a" account))))))

        (add-hook 'message-send-mail-hook 'timu/set-msmtp-account)

        ;; mu4e cc & bcc
        ;; this is custom as well
        (add-hook 'mu4e-compose-mode-hook
                  (defun timu/add-cc-and-bcc ()
                    "My Function to automatically add Cc & Bcc: headers.
                   This is in the mu4e compose mode."
                    (save-excursion (message-add-header "Cc:\n"))
                    (save-excursion (message-add-header "Bcc:\n"))))

        ;; mu4e address completion
        (add-hook 'mu4e-compose-mode-hook 'company-mode)))))
