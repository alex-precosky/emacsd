(require 'flycheck)
(require 'flycheck-pos-tip)


(add-hook 'html-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)


(provide 'setup-flycheck)
