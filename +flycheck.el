;;; +flycheck.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix ("p" . "project")
       :desc "lint" "l" #'flycheck-projectile-list-errors))
