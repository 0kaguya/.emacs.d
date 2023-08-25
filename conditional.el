;; -*- lexical-binding: t -*-

(defun ensure-package (&rest package-list)
  "check if it is a installed package. if is not, install it."
  (dolist (name package-list)
    (unless (package-installed-p name)
      (package-install name))))

;; Additional Packages

;; Fetch package list if there's not one.
(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

;; `use-package' itself need to be installed.
(ensure-package 'use-package)

(eval-when-compile
  (require 'package)
  (require 'use-package))

;; package load time
;;(setq use-package-verbose t
;;      use-package-compute-statistics t)

;; do not byte compile init file.
;;(eval-when-compile
;;  (require 'use-package))

;; required feature from `use-package'
(require 'bind-key)

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




(if (string-search "TREE_SITTER" system-configuration-features)
    (require 'treesit)
  (use-package tree-sitter
    :ensure t)
  )

;; use `company' for completion
(use-package company
  :ensure t)

;; smartparens for lisp files.
(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  :hook ((emacs-lisp-mode . smartparens-strict-mode)))

(use-package macrostep
  :ensure t
  :commands macrostep-expand
  :init
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)
  )

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package org
  :ensure t
  :defer t)


;; git frontend
(use-package magit
  :ensure-system-package git
  :ensure t
  :defer t)


(when (executable-find "julia")
  (use-package julia-mode
   :ensure t
   :config
   (use-package julia-repl
     :ensure t
     :hook ((julia-mode . julia-repl-mode)))
   (use-package ein
     :ensure t)))

(if (executable-find "rustup")
    (progn
      (ensure-package 'eglot
		      'flycheck)
      (unless (package-installed-p 'rust-mode)
	(package-install 'rust-mode)
	;; check and install components (i.e. rust-analyzer)
	(let ((buffer (generate-new-buffer "Rustup Output")))
	  (shell-command "rustup component list" buffer)
	  (when (with-current-buffer buffer
		  (goto-char (point-min))
		  (re-search-forward "^rust-analyzer-[^ ]* (installed)" nil t))
	    (shell-command "rustup component add rust-analyzer" buffer))
	  (kill-buffer buffer)))
      
      (add-hook 'rust-mode-hook 'flycheck-mode)
      (add-hook 'rust-mode-hook 'eglot-ensure))
  (when (package-installed-p 'rust-mode)
    (package-delete 'rust-mode))
  )


(when (executable-find "racket")
  (use-package racket-mode
   :ensure t
   :hook ((racket-mode . racket-unicode-input-method-enable)
	  (racket-mode . racket-xp-mode)
	  (racket-mode . smartparens-strict-mode))))


(when (executable-find "ghc")
  (use-package haskell-mode
   :ensure t
   :init
   (when (executable-find "hindent")
     (use-package hindent
      :ensure t
      :hook (haskell-mode . hindent-mode)))))

(when (executable-find "npm")
  (use-package js2-mode
    :ensure t
    :mode ("\\.jsx?\\'" . js-mode)
    :hook (js-mode . js2-minor-mode)))

(when (executable-find "tsserver")
  (use-package tide
    :ensure t
    :after (flycheck company)
    :functions (tide-hl-identifier-mode tide-setup)
    :init
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1)
      (company-mode +1))
    
    :config
    (setq company-tooltip-align-annotations t)
    (setq tide-format-options
	  '(:insertSpaceAfterFunctionKeywordForAnoymousFunctions t
	    :placeOpenBraceOnNewLineForFunctions nil)
	  )
    
    )
  
  (if (treesit-ready-p 'typescript)
      (progn
	(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
	(add-hook 'typescript-ts-mode-hook #'setup-tide-mode))
    (use-package typescript-mode
      :ensure t
      :mode "\\.ts\\'"
      :hook (typescript-mode . setup-tide-mode)
      )
    )
  (if (treesit-ready-p 'tsx)
      (progn
	(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
	(add-hook 'tsx-ts-mode-hook #'setup-tide-mode))
    (use-package web-mode
      :ensure t
      :after (flycheck)
      :mode "\\.tsx\\'"
      :hook (web-mode . (lambda ()
			  (let ((extension (file-name-extension buffer-file-name)))
			    (when (string-equal "tsx" extension)
			      (setup-tide-mode)))
			  )
		      )
      :config
      (flycheck-add-mode 'typescript-tslint 'web-mode)
      )
    )
  )



(when (executable-find "erl")
  (use-package erlang
    :ensure t
    :after (flycheck)
    :init
    ;; credit to https://www.lambdacat.com/post-modern-emacs-setup-for-erlang/
    (flycheck-define-checker erlang-otp
      "custom erlang checker"
      :command ("erlc" "-o" temporary-directory "-Wall"
		"-I" "../include" "-I" "../../include"
		"-I" "../../../include" source)
      :error-patterns
      ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
       (error line-start (file-name) ":" line ": " (message) line-end))
      :modes erlang-mode)
    :hook (erlang-mode . (lambda ()
			   (flycheck-select-checker 'erlang-otp)
			   (flycheck-mode)))
    :config
    (require 'erlang-start)
    )
  (if (require 'distel nil 'noerror)
      (progn
	(eval-when-compile
	  (declare-function distel-setup "distel")
	  (defvar inferor-erlang-machine-options)
	  (defvar erl-nodename-cache))
	;; credit to https://www.lambdacat.com/post-modern-emacs-setup-for-erlang/
	(defvar inferor-erlang-prompt-timeout t)
	(setq inferor-erlang-machine-options '("-sname" "emacs"))
	(when (executable-find "hostname")
	  (setq erl-nodename-cache
		(concat "emacs@"
			(car (split-string (shell-command-to-string "hostname"))))))
	(distel-setup)
        (use-package company-distel
	  :after (company)
	  :functions company-mode
	  :ensure t
	  :init (add-to-list 'company-backends 'company-distel)
	  :hook (erlang-mode . company-mode)))
    ;; fallback to `company-erlang'
    (use-package company-erlang
	:ensure t
	:hook (erlang-mode . company-erlang-init)))
  )

;;
;; Additional feature

;; Open URLs in browser
;; (when (executable-find "wslview")
;;   (setq browse-url-browser-function 'browse-url-generic
;;	browse-url-generic-program "wslview"))

