(setq org-directory "C:/Users/Alex/Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map (kbd "M-<f6>") 'org-capture)

(provide 'setup-org)


