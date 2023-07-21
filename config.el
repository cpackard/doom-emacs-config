;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Christian Packard"
      user-mail-address "christian@cpacklabs.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:

(setq doom-font (font-spec :family "Fira Code" :size 13 :weight 'semi-light)
      doom-big-font (font-spec :family "Fira Code" :size 24)
      doom-variable-pitch-font (font-spec :size 13))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; Modeline
;; - add current workspace name
;; - add major mode icon
(after! doom-modeline
  (setq doom-modeline-persp-name t
        doom-modeline-major-mode-icon t))

;; Start Doom Emacs maximized
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)

;; Delete whitespace on save, including in markdow-mode
(setq ws-butler-global-exempt-modes '(special-mode comint-mode term-mode eshell-mode diff-mode))

;; Which-key and Evil Key Bindings - Spacemacs style
(load! "+bindings")

;; Clojure mode & Cider Configuration + key bindings
(load! "+clojure")

;; Structural Editing - Smartparens
(load! "+smartparens.el")

;; NOTE: cpackard added these 05/22/2023
(setq parinfer-rust-library "~/.emacs.d/parinfer-rust/parinfer-rust-darwin.so")

;;   > Checking Doom core for irregularities...
;; ! Your $HOME is recognized as a project root
;; Emacs will assume $HOME is the root of any project living under $HOME. If this
;; isn't desired, you will need to remove ".git" from
;; `projectile-project-root-files-bottom-up' (a variable)
(after! projectile (setq projectile-project-root-files-bottom-up (remove ".git"
                                                                  projectile-project-root-files-bottom-up)))

;; def portal to the dev namespace to allow dereferencing via @dev/portal
;; (defun portal.api/open ()
;;   (interactive)
;;   (cider-nrepl-sync-request:eval
;;     "(do (ns dev)
;;          (def portal ((requiring-resolve 'portal.api/open) {:launcher :emacs}))
;;          (add-tap (requiring-resolve 'portal.api/submit)))"))

;; (defun portal.api/clear ()
;;   (interactive)
;;   (cider-nrepl-sync-request:eval "(portal.api/clear)"))

;; (defun portal.api/close ()
;;   (interactive)
;;   (cider-nrepl-sync-request:eval "(portal.api/close)"))

;; Associate `.cljc' files with clojure-mode
(add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojure-mode))
