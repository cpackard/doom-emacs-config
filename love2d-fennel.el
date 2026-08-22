;;; love2d-fennel.el --- A toolkit for developing love2d with fennel -*- lexical-binding: t -*-

;; Copyright © 2024 Alexander Griffith

;; Author: Alexander Griffith
;; URL: https://gitlab.com/alexjgriffith/love2d-fennel.el
;; Version: 0.1.2
;; Created: 2024-05-18
;; Package-Requires: ((emacs "26.1"))
;;
;; Keywords: languages, tools

;;; Commentary:

;; Provides ease of use functions for making games with love2d and fennel
;; Extends fennel-mode

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'fennel-mode)

(defcustom love2d-fennel-args ""
  "Flags passed to the love command line argument."
  :group 'love2d-fennel
  :type 'string
  :package-version `(love2d-fennel "0.1.1"))

(defcustom love2d-fennel-program "love"
  "The command to run love2d."
  :group 'love2d-fennel
  :type 'string
  :package-version `(love2d-fennel "0.1.1"))


(defcustom love2d-fennel-lovejs-program "websocket-stdio -b love -d "
  "The command to run lovejs."
  :group 'love2d-fennel
  :type 'string
  :package-version `(love2d-fennel "0.1.2"))

(defcustom love2d-fennel-search-levels 4
  "How many directories to search upwards for main.lua."
  :group 'love2d-fennel
  :type 'number
  :package-version `(love2d-fennel "0.1.1"))


(defun love2d-fennel--project-root ()
  (let ((counter 0)
        (counter-max love2d-fennel-search-levels)
        (directory "."))
    (while (and (not (member "main.lua"  (directory-files directory)))
                (< counter counter-max))
      (setq directory (concat directory "/.."))
      (setq counter (+ counter 1)))
    (if (< counter counter-max) (file-truename directory) nil)))

(defun love2d-fennel--absolute-to-relative (path root?)
  (replace-regexp-in-string)
  (concat (regexp-quote (expand-file-name (or root? "~/"))) "/\\(.*?\\)")
  "\\1" path)

(defun love2d-fennel-get-module-name ()
  (let ((love2d-project (love2d-fennel--project-root)))
    (when love2d-project
      (replace-regexp-in-string
       "\\.fnl$" ""
       (replace-regexp-in-string
        "/" "."
        (love2d-fennel--absolute-to-relative (buffer-file-name)
                                             love2d-project))))))

(defun love2d-fennel-get-module (ask? last-module) ;; Adapted from fennel-mode.el
  "Ask for the name of a module for the current file; return keyword.

If ASK? or LAST-MODULE were not supplied, asks for the name of a module."
  (let ((module (if (or ask? (not last-module))
                    (read-string "Module: " (or last-module (love2d-fennel-get-module-name) (file-name-base nil)))
                  last-module)))
    (setq fennel-module-name module)    ; remember for next time
    (intern (concat ":" module))))

;;;###autoload
(defun love2d-fennel-reload (ask?) ;; Adapted from fennel-mode.el
  "Reload the module for the current file.

ASK? forces module name prompt.

Tries to reload in a way that makes it retroactively visible; if
the module returns a table, then existing references to the same
module will have their contents updated with the new
value.  Requires installing `fennel.searcher'.

Queries the user for a module name upon first run for a given
buffer, or when given a prefix arg."
  (interactive "P")
  (when buffer-file-name
    (comint-check-source buffer-file-name)
    (when (and (file-exists-p (concat (file-name-base nil) ".lua"))
               (yes-or-no-p "Lua file for module exists; delete it first?"))
      (delete-file (concat (file-name-base nil) ".lua"))))
  (let ((module (love2d-fennel-get-module ask? fennel-module-name))
        (inferior-lisp-buffer (get-buffer fennel-repl--buffer-name)))
    ;; this shouldn't work with lexical binding...
    (comint-send-string (inferior-lisp-proc) (fennel-reload-form module)))
  (when fennel-mode-switch-to-repl-after-reload
    (switch-to-lisp t)))

;;;###autoload
(defun love2d-fennel-run-love (&optional buffer-name)
  "Run love with fennel-repl or start new fennel repl if no main.lua file is found."
  (interactive)
  (let ((buffer (get-buffer (or buffer-name fennel-repl--buffer-name "*Fennel REPL*")))
        (love-project (love2d-fennel--project-root)))
    (if love-project
        (progn
          (when (and buffer (get-buffer-process buffer))
            (kill-process (get-buffer-process buffer))
            (sleep-for 0.1)
            (with-current-buffer buffer (erase-buffer)))
          (fennel-repl (concat love2d-fennel-program " " love-project " " love2d-fennel-args)))
      (fennel-repl fennel-program))))

;;;###autoload
(defun love2d-fennel-run-lovejs (&optional buffer-name)
  "Run love with fennel-repl or start new fennel repl if no main.lua file is found."
  (interactive)
  (let ((buffer (get-buffer (or buffer-name fennel-repl--buffer-name "*Fennel REPL*")))
        (love-project (love2d-fennel--project-root)))
    (if love-project
        (progn
          (when (and buffer (get-buffer-process buffer))
            (kill-process (get-buffer-process buffer))
            (sleep-for 0.1)
            (with-current-buffer buffer (erase-buffer)))
          (fennel-repl (concat love2d-fennel-lovejs-program " " love-project " " love2d-fennel-args)))
      (fennel-repl fennel-program))))

(provide 'love2d-fennel)
;;; love2d-fennel.el ends here
