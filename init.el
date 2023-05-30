;; -*- mode: emacs-lisp -*-
;; Emacs Config


;; To have a clean appearance.
(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; I'm thinking about adding transparency, but that depends a lot on display system.

;; Also display column number.
(column-number-mode t)

;; Set window size
(when window-system
  (set-frame-size (selected-frame) 100 30))

;; Set font
(set-face-attribute 'default nil
		    ;; Sarasa font is narrow, for matching 2 char = 1 CN char.
		    :family "Sarasa Term SC"
		    :foundry "outline"
		    :slant 'normal
		    :weight 'normal
		    :height 160
		    :width 'normal)
;; the former approach is slower.
;; (add-to-list 'default-frame-alist
;;              '(font . "-*-Monaco-*-*-*-mono-21-*-*-*-c-*-iso8859-1"))

;; Toggle some options.
(setq
 make-backup-files nil
 inhibit-startup-screen t
 visible-bell 1

 ;; quoted insert use hex number
 read-quoted-char-radix 16

 ;; Configure auto saving directory
 backup-directory-alist `(("." . "~/.emacs.d/.cache"))
 
 ) ; end `setq`

;; Open URLs in browser
(when (executable-find "wslview")
  (setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "wslview"))

;; Automatic generated config are set and loaded here.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; Emulate the title bar's right-click menu
;;(define-prefix-command 'keymap-titlebar-menu)
;;(global-set-key (kbd "M-SPC") 'keymap-titlebar-menu)
;;(define-key keymap-titlebar-menu (kbd "x") 'toggle-frame-maximized)
;;(define-key keymap-titlebar-menu (kbd "c") 'save-buffers-kill-terminal)

;;
;; Package Settings
;;

;; load package manager
(when (version< emacs-version "27")
  (require 'package))

;; Set elpa mirror:
;; https://mirrors.tuna.tsinghua.edu.cn
(setq package-archives
      ;; https ones will prompt insecure hint.
      '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
	("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

;; Fetch package list if there's not one.
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; 'use-package itself need to be installed.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; initialize package manager
(when (version< emacs-version "27")
  (package-initialize))

;;
;; Load Packages
;;

;; chinese input method
(use-package pyim
  :ensure t
  :init
  (setq default-input-method "pyim")
  :config
  (use-package pyim-basedict
    :ensure t)
  (setq-default pyim-punctuation-translate-p '(no))
  (pyim-default-scheme 'quanpin)
  (setq pyim-page-length 5)
  (pyim-basedict-enable))



(use-package magit
  :ensure t)

(use-package eglot
  :ensure t)

;;
;; Additional Packages
;;


;; smartparens for lisp files.
(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  :config
  (add-hook 'racket-mode-hook #'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode))


(use-package julia-mode
  :ensure t
  :config
  (use-package julia-repl
    :ensure t
    :hook ((julia-mode . julia-repl-mode)))
  (use-package ein
    :ensure t))


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))


(use-package racket-mode
	    :ensure t
	    :config
	    (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable))


(use-package rust-mode
  :ensure t
  :after (eglot)
  ;; credit to https://gist.github.com/casouri/0ad2c6e58965f6fd2498a91fc9c66501
  :init
  (defun setup-rust ()
    (setq-local eglot-workspace-configuration
		'(:rust-analyzer
		  (:procMacro
		   (:atteributes (:enable t) :enable t)
		   :cargo
		   (:buildScripts (:enable t))
		   :diagnosticss
		   (:disabled ["unresolved-proc-macro"
			       "unresolved-macro-call"])))))
  (defclass eglot-rust-analyzer (eglot-lsp-server) ())
  (cl-defmethod eglot-initialization-options ((server eglot-rust-analyzer))
    eglot-workspace-configuration)
  (add-to-list 'eglot-server-programs
	       '(rust-mode . (eglot-rust-analyzer "rust-analyzer")))
  :hook ((rust-mode . setup-rust)
	 (rust-mode . eglot-ensure)))


(use-package haskell-mode
  :ensure t)


(use-package hindent
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))


;; typescript support
(use-package tide
  :ensure t
  :config
  (use-package typescript-mode
    :ensure t)
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))
  (add-hook 'typescript-mode-hook #'setup-tide-mode))



