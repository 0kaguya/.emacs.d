;; -*- lexical-binding: t -*-
;; Emacs Config


;; To have a clean appearance.
(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)

;; Also display column number.
(column-number-mode t)

;; Turn off beep
(setq visible-bell 1)

(when (display-graphic-p)

  ;; (set-frame-position (selected-frame) 100 100)
  ;; (add-to-list 'default-frame-alist '(top  . 0.2))
  ;; (add-to-list 'default-frame-alist '(left . 0.2))
  (set-frame-size (selected-frame) 100 30)
  
  ;; ;; Add transparency and switch to a dark theme
  ;; ;; - need Emacs 29 or above
  ;; ;; - need `emacs-pgtk' on Wayland
  (when (>= emacs-major-version 29)
    (set-frame-parameter nil 'alpha-background 70)
    ;; how to set a dark theme: just revert color.
    (add-to-list 'default-frame-alist '(background-color . "#000000"))
    (add-to-list 'default-frame-alist '(foreground-color . "#ffffff"))
    ;; ... also mode line's color.
    (set-face-attribute 'mode-line nil :background "#000000"
			               :foreground "#ffffff")
    ;; setup a dimmed visible bell
    (defvar ring-bell-lock nil)
    (setq visible-bell nil
	  ring-bell-function (lambda ()
                               (unless ring-bell-lock 
				 (setq ring-bell-lock t)
				 (let ((current (face-attribute 'mode-line :background))
				       (flash "#aaaaaa"))
				   (set-face-attribute 'mode-line nil :background flash)
				   (run-with-idle-timer 0.1
							nil
							(lambda ()
							  (set-face-attribute 'mode-line nil
									      :background current)
							  (setq ring-bell-lock nil)
							  ))))))

    ;; unsure what system theme looks like; just turn that off.
    (set-frame-parameter nil 'undecorated t)
    
  )
  
  ;; Set font
  (set-face-attribute 'default nil
		      ;; Sarasa font is narrow, for matching 2 char = 1 CN char.
		      :family "Sarasa Term SC"
		      :foundry "outline"
		      :slant 'normal
		      :weight 'normal
		      :height 160
		      :width 'normal)
  
  ;; (add-to-list 'default-frame-alist
  ;;              '(font . "-*-Monaco-*-*-*-mono-21-*-*-*-c-*-iso8859-1"))
  
  )


;; Toggle some options.
(setq
 make-backup-files nil
 ;; Configure auto saving directory
 backup-directory-alist `(("." . ,(concat user-emacs-directory ".cache")))

 ;; quoted insert use hex number
 read-quoted-char-radix 16

 ;; don't want a pop-up window.
 use-dialog-box nil
 )


;; Automatic generated config are set and loaded here.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Emulate the title bar's right-click menu
;;(define-prefix-command 'keymap-titlebar-menu)
;;(global-set-key (kbd "M-SPC") 'keymap-titlebar-menu)
;;(define-key keymap-titlebar-menu (kbd "x") 'toggle-frame-maximized)
;;(define-key keymap-titlebar-menu (kbd "c") 'save-buffers-kill-terminal)

;;
;; Package Settings
;;

;; load package manager
(when (< emacs-major-version 27)
  (require 'package))

;; Set elpa mirror:
;; https://mirrors.tuna.tsinghua.edu.cn
(setq package-archives
      ;; https:// will have prompt with insecure warning
      '(("gnu"          . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	("nongnu"       . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
	("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa")
	("melpa"        . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("org"          . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
	))

;; initialize package manager
(when (< emacs-major-version 27)
  (package-initialize))

;; Fetch package list if there's not one.
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; 'use-package itself need to be installed.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; verbose mode show package load time
(setq use-package-verbose t)

;; do not byte compile init file.
;;(eval-when-compile
;;  (require 'use-package))

;; required feature from `use-package'
(require 'bind-key)


;;
;; Load Packages
;;

;; interop with system package manager
(use-package use-package-ensure-system-package
  :functions use-package-ensure-package-exist?
  :ensure t
  :defer t)

(use-package esup
  :ensure t
  :defer t)

;; chinese input method
(when (display-graphic-p)
  (use-package pyim
    :ensure t
    :defer t
    :init
    (setq default-input-method "pyim")
    :config
    (use-package pyim-basedict
      :ensure t)
    (setq-default pyim-punctuation-translate-p '(yes))
    (pyim-default-scheme 'quanpin)
    (setq pyim-page-length 5)
    (pyim-basedict-enable)))


;; git frontend
(use-package magit
  :ensure-system-package git
  :ensure t
  :defer t)

;; lsp support
(use-package eglot
  :ensure t
  :defer t)

;; use `company' for completion
(use-package company
  :ensure t)

;; smartparens for lisp files.
(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  :hook ((emacs-lisp-mode . smartparens-strict-mode)))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package org
  :ensure t
  :defer t)

(load (concat user-emacs-directory "conditional"))



