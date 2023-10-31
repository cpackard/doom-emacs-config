;;; +sql.el -*- lexical-binding: t; -*-

;; If sql-postgres-login-params is non-nil, you'll be prompted when connecting, which defeats the purpose of defining connections.
(setq sql-postgres-login-params nil)

(defun cp/db-connect-sql (connection &optional product)
  (or product (setq product 'postgres))
  (setq sql-product product)
  (let ((db-conn-file (expand-file-name "~/.db-connections.el.gpg")))
    (when (file-exists-p db-conn-file)
      (message db-conn-file)
      (require 'cp/database-urls db-conn-file)
      (setq sql-connection-alist cp/database-urls)
      (sql-connect connection))))

(defun cp/db-connect-sql-local ()
  (interactive)
  (cp/db-connect-sql 'local))

(defun cp/db-connect-sql-dev ()
  (interactive)
  (cp/db-connect-sql 'dev))

(defun cp/db-connect-sql-prod-ro ()
  (interactive)
  (cp/db-connect-sql 'prod-ro))

(defun cp/db-connect-sql-prod ()
  (interactive)
  (cp/db-connect-sql 'prod))

(map! :leader
      (:prefix-map ("o" . "open")
                   (:prefix ("s" . "SQL")
                    :desc "local" "l" #'cp/db-connect-sql-local
                    :desc "dev" "d" #'cp/db-connect-sql-dev
                    :desc "prod-ro" "p" #'cp/db-connect-sql-prod-ro
                    :desc "prod" "P" #'cp/db-connect-sql-pro)))

(after! sqlformat
  ;; Use pg_format for formatting: https://github.com/darold/pgFormatter
  ;; `-s2` = 2 spaces for indentation
  ;; `-g` = don't group statements
  ;; `-u1` = lowercase keywords
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g" "-u1")))
