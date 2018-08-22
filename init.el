;; used https://github.com/magnars as a starting point

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

(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

(package-initialize)


;; tramp
(setq tramp-default-method "plink")

;; Setup packages
(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(
     realgud
     magit
     company
     ggtags
     helm
     elpy
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
     dired-details
     css-eldoc
     yasnippet
     smartparens
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
     jedi
     powerline
     monokai-theme
     conda
     tramp
     srefactor
     cmake-mode
     nyan-mode
     basic-mode
     git-gutter
     ansible
     neotree
     ivy
     counsel
     swiper
     )))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
      (init--install-packages)))

;; Lets start with a smattering of sanity
(require 'sane-defaults)


; site-specific config files
(defvar host (substring (shell-command-to-string "hostname") 0 -1))
(defvar host-dir (concat "~/.emacs.d/hosts/" host))
(add-to-list 'load-path host-dir)

(let ((init-host-feature (intern (concat "init-" host))))
  (require (intern (concat "init-" host)) nil 'noerror))


;; Setup extensions
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(require 'setup-org)

;; git-gutter
(global-git-gutter-mode +1)

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)

;;(eval-after-load 'grep '(require 'setup-rgrep))
;;(eval-after-load 'shell '(require 'setup-shell))
(require 'setup-yasnippet)

(require 'ivy)
(require 'counsel)
(ivy-mode 1)
(setq magit-completing-read-function 'ivy-completing-read)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)


;; Font lock dash.el
(eval-after-load "dash" '(dash-enable-font-lock))

(eval-after-load 'flycheck '(require 'setup-flycheck))

;; python
(elpy-enable)

;; python shell to ipython
(require 'python)
(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "--pylab")

;; c stuff
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(require 'srefactor)
(require 'srefactor-lisp)

(add-hook 'c-mode-common-hook
	  (semantic-mode 1) )

(global-set-key (kbd "M-TAB") 'company-complete-common)

(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)




;; conda mode 
(require 'conda)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "c:/Users/Alex/Anaconda3")
 '(custom-safe-themes
   (quote
    ("c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" "8ed752276957903a270c797c4ab52931199806ccd9f0c3bb77f6f4b9e71b9272" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" default)))
 '(electric-indent-mode nil)
 '(nyan-mode t)
 '(package-selected-packages
   (quote
    (counsel ivy-yasnippet swiper elpy helm ggtags company-c-headers conda flymake-solidity solidity-mode powerline csv-mode nyan-mode monokai-theme jedi skewer-mode yasnippet whitespace-cleanup-mode visual-regexp undo-tree string-edit smooth-scrolling smex smartparens simple-httpd restclient prodigy paredit move-text markdown-mode magit ido-vertical-mode ido-completing-read ido-at-point htmlize highlight-escape-sequences guide-key flycheck-pos-tip flx-ido fill-column-indicator elisp-slime-nav dockerfile-mode dired-details css-eldoc))))


;; eshell support
(conda-env-initialize-eshell)
;; if you want auto-activation (see below for details), include:
(conda-env-autoactivate-mode t)


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

;; Makes buffer switching show even closed files
(setq ido-use-virtual-buffers t)


(require 'powerline)
(powerline-default-theme)

;; S-arrow navigation for moving between windows
(windmove-default-keybindings)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(load-theme 'monokai)


(server-start)
