;; -*- lexical-binding: t -*-

(defun ensure-package (&rest package-list)
  "check if it is a installed package. if is not, install it."
  (dolist (name package-list)
    (unless (package-installed-p name)
      (package-install name))))

;; `use-package' itself need to be installed.
(ensure-package 'use-package)

(eval-when-compile
  (require 'package)
  (require 'use-package))

;; package load time
;;(setq use-package-verbose t
;;      use-package-compute-statistics t)

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

;; (if (string-search "TREE_SITTER" system-configuration-features)
;;     (require 'treesit)
;;   (use-package tree-sitter
;;     :ensure t)
;;   )

;; use `company' for completion
(use-package company
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package org
  :ensure t
  :defer t)

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
