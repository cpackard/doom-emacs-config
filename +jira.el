;;; +jira.el -*- lexical-binding: t; -*-

(after! org-jira
  ;; Create the org-jira-working-dir directory
  (when (not (file-directory-p "~/.org-jira"))
    (make-directory "~/.org-jira"))
  ;; Set the URL for your Jira server.
  (setq jiralib-url "https://onsiteiqdev.atlassian.net")
  ;; You can define one or more custom JQL queries to run and have your results inserted into an org file.
  ;; Please note this feature still requires some testing - things that may work in the existing proj-key named buffers (EX.org etc.) may behave unexpectedly in the custom named buffers.
  ;; One thing you may notice is if you create an issue in this type of buffer, the auto-refresh of the issue will appear in the PROJ-KEY.org specific buffer (you will then need to refresh this JQL buffer by re-running the command C-c ij).
  (setq org-jira-custom-jqls
        '(
          (:jql " project IN (TF, TECH) and assignee = 'Christian Packard' and createdDate > '2023-01-01' and createdDate < '2024-01-01' order by created DESC "
           :limit 20
           :filename "this-years-work")
          (:jql "
           project IN (TF, TECH)
           and sprint in openSprints()
           and assignee = 'Christian Packard'
           order by priority, created DESC "
           :limit 20
           :filename "active-sprint")))
  (setq org-jira-jira-status-to-org-keyword-alist
        '(("To Do" . "TODO")
          ("In Progress" . "DOING")
          ("In Review" . "REVIEW")
          ("Done" . "DONE")
          ("Blocked" . "BLOCKED")))
  (defconst org-jira-progress-issue-flow
    '(("Blocked" . "Blocked")
      ("To Do" . "To Do")
      ("In Progress" . "In Progress")
      ("In Review" . "In Review")
      ("Done" . "Done"))))
