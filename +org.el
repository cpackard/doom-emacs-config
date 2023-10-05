;;; +org.el -*- lexical-binding: t; -*-

(after! org
  ;; Hide org markup characters.
  (setq org-hide-emphasis-markers t)

  ;; Insert Org headings at point, not after the current subtree (this is enabled by default by Doom).
  (setq org-insert-heading-respect-content nil)

  ;; Enable logging of done tasks, and log stuff into the LOGBOOK drawer by default
  (setq org-log-done t)
  (setq org-log-into-drawer t)

  ;; Disable electric-mode, which is now respected by Org and which creates some confusing indentation sometimes.
  (add-hook! 'org-mode-hook (electric-indent-local-mode -1))

  ;; Use fixed-pitch font for org tables
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)

  ;; (add-hook! 'org-mode-hook 'doom-docs-mode)
  (custom-theme-set-faces!
    doom-theme
    '(org-level-4 :inherit outline-4 :height 1.1)
    '(org-level-3 :inherit outline-3 :height 1.25)
    '(org-level-2 :inherit outline-2 :height 1.5)
    '(org-level-1 :inherit outline-1 :height 1.75)
    '(org-document-title :height 2.0 :underline nil))

  ;; Use org-appear to reveal emphasis markers when moving the cursor over them.
  (add-hook! 'org-mode-hook :append #'org-appear-mode)

  ;; Enable variable and visual line mode in Org mode by default.
  (add-hook! 'org-mode-hook :append
             #'visual-line-mode
             #'variable-pitch-mode))

    ;; (add-hook! 'org-mode-hook #'mixed-pitch-mode)
    ;; (add-hook! 'org-mode-hook #'solaire-mode)

    ;; (setq mixed-pitch-variable-pitch-cursor nil)
