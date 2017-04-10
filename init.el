;; install first:
;; dash (modern list API)

;; Set up load path
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
(add-to-list 'load-path settings-dir)

(setq inhibit-startup-message t)

(setq frame-title-format "emacs")

(menu-bar-mode -1)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(column-number-mode)

(show-paren-mode)

(global-hl-line-mode)


(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/")
	     t)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/")
	     t)

(package-initialize)


;; Setup packages
(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(
     magit
     paredit
     move-text
     htmlize
     visual-regexp
     markdown-mode
     fill-column-indicator
     flycheck
     flycheck-pos-tip
     flx
     f
     flx-ido
     dired-details
     css-eldoc
     yasnippet
     smartparens
     ido-vertical-mode
     ido-at-point
     ido-ubiquitous
     simple-httpd
     guide-key
     restclient
     highlight-escape-sequences
     whitespace-cleanup-mode
     elisp-slime-nav
     dockerfile-mode
     prodigy
     string-edit
     smooth-scrolling
     undo-tree
     smex
     skewer-mode
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
      (init--install-packages)))

;; Lets start with a smattering of sanity
(require 'sane-defaults)



;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
;;(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
;;(eval-after-load 'grep '(require 'setup-rgrep))
;;(eval-after-load 'shell '(require 'setup-shell))
(require 'setup-yasnippet)


;; Font lock dash.el
(eval-after-load "dash" '(dash-enable-font-lock))

(eval-after-load 'flycheck '(require 'setup-flycheck))

;; Highlight escape sequences
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Visual regexp
(require 'visual-regexp)
(define-key global-map (kbd "M-&") 'vr/query-replace)
(define-key global-map (kbd "M-/") 'vr/replace)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)


;; Setup key bindings
(require 'key-bindings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (skewer-mode yasnippet whitespace-cleanup-mode visual-regexp undo-tree string-edit smooth-scrolling smex smartparens simple-httpd restclient prodigy paredit move-text markdown-mode magit ido-vertical-mode ido-ubiquitous ido-at-point htmlize highlight-escape-sequences guide-key flycheck-pos-tip flx-ido fill-column-indicator elisp-slime-nav dockerfile-mode dired-details css-eldoc))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )