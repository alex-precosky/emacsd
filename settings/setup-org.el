; set up org-directory and other values in a site specific file, in .emacs.d/hosts
(define-key global-map (kbd "M-<f6>") 'org-capture)

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))


(setq org-agenda-files (list (concat org-directory "/notes.org")))

(provide 'setup-org)

