(require 'flycheck)
(require 'flycheck-pos-tip)


(add-hook 'html-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(setq flycheck-idle-change-delay 10)

(provide 'setup-flycheck)
