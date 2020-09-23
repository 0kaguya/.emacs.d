;; configure auto saving directory
(setq backup-directory-alist
      `(("." . "~/.emacs.saves")))
;; mirrors.tuna.tsinghua.edu.cn
(setq package-archives '(("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			 ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)
;; github.com/ocaml-community/utop
;; Use the opam installed utop
(setq utop-command "opam config exec -- utop -emacs")
;; open url with xdg-open
(setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "xdg-open")
;; configuring ProofGeneral
(add-hook 'coq-mode-hook
	  (lambda ()
	    (setq coq-compile-before-require t)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(inhibit-startup-screen t)
 '(line-number-mode nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(package-selected-packages (quote (proof-general geiser utop))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :foundry "outline" :slant normal :weight normal :height 105 :width normal)))))
