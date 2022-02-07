;; To have a clean appearance.
(blink-cursor-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Also display column number.
(column-number-mode t)

;; Set window size
(when window-system
  (set-frame-size (selected-frame) 80 30))

;; Set font
(set-face-attribute 'default nil
		    ;; Sarasa font is narrow, for matching 2 char = 1 CN char.
		    :family "Sarasa Term SC"
		    :foundry "outline"
		    :slant 'normal
		    :weight 'normal
		    :height 120
		    :width 'normal)
;; commented out: it is slower to take effect
;; (add-to-list 'default-frame-alist
;;              '(font . "-*-Monaco-*-*-*-mono-21-*-*-*-c-*-iso8859-1"))

;; Toggle some options.
(setq
 make-backup-files nil
 inhibit-startup-screen t
 
 ;; Configure auto saving directory
 backup-directory-alist `(("." . "~/.emacs.d/.cache"))
 
 ;; Open URLs in browser
 browse-url-browser-function 'browse-url-generic
 browse-url-generic-program "start"
 
 ) ; end `setq`

;; Automatic generated config are set and loaded here.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; Here start the builtin package manager.
(require 'package)

;; Set elpa mirror:
;; https://mirrors.tuna.tsinghua.edu.cn
(setq package-archives
      ;; https ones will prompt insecure hint.
      '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

;; Fetch package list if there's not one.
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; 'use-package itself need to be installed.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; commented out since it's slow and
;; people said it's not needed after Emacs 27.
;; (package-initialize)

;; Emulate the title bar's right-click menu
(define-prefix-command 'keymap-titlebar-menu)
(global-set-key (kbd "M-SPC") 'keymap-titlebar-menu)
(define-key keymap-titlebar-menu (kbd "x") 'toggle-frame-maximized)
(define-key keymap-titlebar-menu (kbd "c") 'save-buffers-kill-terminal)

;; Additional Packages

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))
