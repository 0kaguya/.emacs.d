(setq package-archives '(("gnu" . "https://mirrors4.tuna.tsinghua.edu.cn/elpa/gnu/")
			            ("melpa" . "https://mirrors4.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)
(custom-set-variables
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(inhibit-startup-screen t)
 '(line-number-mode nil)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (geiser flycheck-haskell intero company-coq proof-general)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

(custom-set-faces
 '(default ((t (:family "Monaco" :foundry "outline" :slant normal :weight normal :height 105 :width normal)))))

(add-hook 'haskell-mode-hook
	  (lambda ()
	    (setq haskell-process-type 'stack-ghci)))

