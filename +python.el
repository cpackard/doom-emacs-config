;;; +python.el -*- lexical-binding: t; -*-

(after! dap-mode
  (setq dap-python-debugger 'debugpy)
  (dap-register-debug-template "core-api"
                               (list :type "python"
                                     :args "runserver --noreload"
                                     :cwd (expand-file-name "~/onsiteiq/core_api")
                                     :module nil
                                     :console "integratedTerminal"
                                     :program (expand-file-name "~/onsiteiq/core_api/manage.py")
                                     :env '(("DEBUG" . "1"))
                                     :request "launch"
                                     :name "core-api dap: Django"
                                     :django t))

  (map! :map python-mode-map
        :localleader
        :prefix ("d" . "DAP")
        :desc "debug" "d" #'dap-debug
        :desc "continue" "c" #'dap-continue
        :desc "step in" "i" #'dap-step-in
        :desc "step out" "o" #'dap-step-out
        :desc "step over" "n" #'dap-next
        :desc "toggle breakpoint" "b" #'dap-breakpoint-toggle
        :desc "evaluate expression" "e" #'dap-eval-thing-at-point
        :desc "restart" "r" #'dap-debug-restart
        :desc "disconnect" "q" #'dap-disconnect
        :desc "close" "Q" #'dap-delete-all-sessions))

(defun python-pytest-all ()
  (interactive)
  (python-pytest-directories (list (python-pytest--project-root))))

(after! (:and dap-mode python)
  (map! :localleader
        :map nose-mode-map
        :prefix "t"
        "r" nil ;; #'nosetests-again
        "a" nil ;; #'nosetests-all
        "s" nil ;; #'nosetests-one
        "v" nil ;; #'nosetests-module
        "A" nil ;; #'nosetests-pdb-all
        "O" nil ;; #'nosetests-pdb-one
        "V" nil ;; #'nosetests-pdb-module
        :desc "re-run last test" "r" #'python-pytest-repeat
        :desc "all tests" "a" #'python-pytest-all))

(after! python
  (map! :map python-mode-map
        :localleader
        :desc "open repl" "'" #'+python/open-repl)
  (map! :map python-mode-map
        :localleader
        :prefix ("s" . "send")
        :desc "send buffer" "b" #'python-shell-send-buffer
        :desc "send function" "f" #'python-shell-send-defun
        :desc "send region" "r" #'python-shell-send-region
        :desc "send statement" "s" #'python-shell-send-statement)
  (add-hook! '(before-save-hook)
    (defun +python--isort-buffer ()
      (when (eq major-mode 'python-mode)
        (call-interactively 'py-isort-buffer)))))
