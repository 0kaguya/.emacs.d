;; -*- mode: emacs-lisp -*-
;; Additional Packages

(eval-when-compile
  (require 'use-package)
  (require 'flycheck))


(when (executable-find "julia")
  (use-package julia-mode
   :ensure t
   :config
   (use-package julia-repl
     :ensure t
     :hook ((julia-mode . julia-repl-mode)))
   (use-package ein
     :ensure t)))


(when (executable-find "rustup")
  (use-package rust-mode
   :ensure t
   :after (eglot)
   :ensure-system-package ((cargo . "rustup default stable")
			   (rust-analyzer . "rustup component add rust-analyzer"))
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
	  (rust-mode . eglot-ensure))))


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


(when (executable-find "tsc")
  (use-package typescript-mode
    :ensure t)
  (use-package tide
    :ensure t
    :functions (tide-hl-identifier-mode tide-setup)
    :init
    (defun setup-tide-mode ()
      (interactive)
      (tide-setup)
      (flycheck-mode +1)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1))
    :hook (typescript-mode . setup-tide-mode)))


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

