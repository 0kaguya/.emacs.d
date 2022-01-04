;; To have a clean appearance.
(blink-cursor-mode -1)
(line-number-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Set font
(set-face-attribute 'default nil
		    :family "Monaco"
		    :foundry "outline"
		    :slant 'normal
		    :weight 'normal
		    :height 105
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
 
 ;; Set elpa mirror to
 ;; https://mirrors.tuna.tsinghua.edu.cn
 package-archives
 '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
   ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))
 ) ; end `setq`

;; Automatic generated config are set and loaded here.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)


;; commented out since it's slow and
;; people said after Emacs 27 it's not needed.
;; (package-initialize)

