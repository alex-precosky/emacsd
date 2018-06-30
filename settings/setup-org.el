; set up org-directory and other values in a site specific file, in .emacs.d/hosts
(define-key global-map (kbd "M-<f6>") 'org-capture)

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-log-done 'time)

(if (boundp 'org-directory)
    (progn 
    (setq org-agenda-files (list (concat org-directory "/notes.org")))

  (setq org-capture-templates
	`(("t" "Todo" entry (file+headline ,(concat org-directory "/notes.org") "Incoming Tasks")
        "* TODO %?\n  %i\n")
   ("j" "Journal" entry (file+olp+datetree ,(concat org-directory "/journal.org"))
        "* %?\nEntered on %U\n  %i\n  %a")))
  ) ; progn

) ;endif
(provide 'setup-org)

