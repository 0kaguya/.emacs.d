(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
			            ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)
(custom-set-variables
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(blink-cursor-mode nil)
 '(inhibit-startup-screen t)
 '(line-number-mode nil)
 '(menu-bar-mode nil)
 '(package-selected-packages (quote (flycheck-haskell intero company-coq proof-general)))
 '(scroll-bar-mode nil)
 '(make-backup-files nil)
 '(tool-bar-mode nil))

(custom-set-faces
 '(default ((t (:family "Monaco" :foundry "outline" :slant normal :weight normal :height 105 :width normal)))))

(add-hook 'haskell-mode-hook
	  (lambda ()
	    (setq haskell-process-type 'stack-ghci)))

