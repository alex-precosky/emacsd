;; used https://github.com/magnars as a starting point

;; install first:
;; dash (modern list API)

;; Uncomment this when debugging things
;; (setq debug-on-error t)

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
     org-bullets
     flymake-google-cpplint
     multiple-cursors
     flycheck-clang-analyzer
     clang-format
     git-timemachine
     projectile
     expand-region
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

;; gdb
(eval-after-load "gud"
  '(progn 
     (define-key gud-mode-map (kbd "<up>") 'comint-previous-input)
     (define-key gud-mode-map (kbd "<down>") 'comint-next-input)))

;; git-gutter
(global-git-gutter-mode +1)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

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
(with-eval-after-load 'flycheck
  (require 'flycheck-clang-analyzer)
  (flycheck-clang-analyzer-setup))

(setq clang-format-style-option "google")


;; python
(elpy-enable)

;; python shell to ipython
(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
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
(require 'whitespace)

(setq-default set-indent-tabs-mode nil)

(add-hook 'c-mode-common-hook
	  (semantic-mode 1) )
(add-hook 'c-mode-common-hook
	  'company-mode )
(add-hook 'c-mode-common-hook
	  'flycheck-mode )
(add-hook 'c-mode-common-hook
	  'whitespace-mode )
(add-hook 'c-mode-common-hook
          (lambda () (setq indent-tabs-mode nil)))

(setq-default c-basic-offset 4)

(global-set-key (kbd "M-TAB") 'company-complete-common)

(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

; multiple cursors
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   (quote
    (company-bbdb company-nxml company-css company-eclim company-semantic company-gtags company-xcode company-cmake company-capf company-files
		  (company-dabbrev-code company-clang company-etags company-keywords)
		  company-oddmuse company-dabbrev)))
 '(conda-anaconda-home "c:/Users/Alex/Anaconda3")
 '(custom-safe-themes
   (quote
    ("bd7b7c5df1174796deefce5debc2d976b264585d51852c962362be83932873d9" "c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8ef69017c68" "8ed752276957903a270c797c4ab52931199806ccd9f0c3bb77f6f4b9e71b9272" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" default)))
 '(electric-indent-mode nil)
 '(flycheck-clang-args (quote ("-Wsign-conversion")))
 '(flycheck-clang-warnings (quote ("all" "extra")))
 '(nyan-mode t)
 '(package-selected-packages
   (quote
    (projectile git-timemachine clang-format flycheck-clang-analyzer multiple-cursors org-bullets counsel ivy-yasnippet swiper elpy helm ggtags company-c-headers conda flymake-solidity solidity-mode powerline csv-mode nyan-mode monokai-theme jedi skewer-mode yasnippet whitespace-cleanup-mode visual-regexp undo-tree string-edit smooth-scrolling smex smartparens simple-httpd restclient prodigy paredit move-text markdown-mode magit ido-vertical-mode ido-completing-read ido-at-point htmlize highlight-escape-sequences guide-key flycheck-pos-tip flx-ido fill-column-indicator elisp-slime-nav dockerfile-mode dired-details css-eldoc)))
 '(realgud:gdb-command-name "gdb-multiarch")
 '(whitespace-style
   (quote
    (face trailing tabs empty indentation space-after-tab space-before-tab tab-mark))))

(require 'flymake-google-cpplint)
(add-hook 'c++-mode-hook 'flymake-google-cpplint-load)


;; put backup files in one spot
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))



;; conda mode 
(require 'conda)



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


;; gdb
(setq gdb-many-windows 1)


;; Setup key bindings
(require 'key-bindings)

;; Makes buffer switching show even closed files
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")


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

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(load-theme 'monokai)


(server-start)
