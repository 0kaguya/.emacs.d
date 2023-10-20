;; -*- lexical-binding: t -*-

(let ((all-packages '())
      (active-packages '())
      (update-packages-functions '()))
  (defun update-packages ()
    (interactive)
    (run-hook-with-args
     'update-packages-functions
     (mapcan #'directory-files
	     (seq-filter #'file-exists-p exec-path)))
    (mapc (lambda (package)
	    (cond ((memq package active-packages)
		   (unless (package-installed-p package)
		     (package-install package)))
		  ('otherwise
		   (when (package-installed-p package)
		     (package-delete package)))))
	  all-packages)
    )
  (defun package-bind-executable (executables &rest packages)
    (add-hook
     'update-packages-functions
     (lambda (all-executables)
       (mapc (lambda (package)
		 (unless (memq package all-packages)
		   (setq all-packages (cons package all-packages))))
	     packages)
       (when (seq-every-p
	      (lambda (executable)
		(seq-some (lambda (s) (string-equal s executable))
			  all-executables))
	      (ensure-list executables))
	 (setq active-packages (append packages active-packages)))
       ))
    ))

(with-eval-after-load 'eglot
  (cond ((>= emacs-major-version 29)
	 (keymap-set eglot-mode-map "S-<f6>" #'eglot-rename))
	('else
	 (define-key eglot-mode-map (kbd "S-<f6>") #'eglot-rename)))
  )

(let
    ;; Typescript and TSX
    ((setup-typescript
      (lambda (&rest minor-modes)
	"major mode dispatcher"
	(cond ((and (>= emacs-major-version 29)
		    (treesit-available-p)
		    (treesit-language-available-p 'typescript))
	       (add-to-list 'auto-mode-alist
			    '("\\.ts\\'" . typescript-ts-mode))
	       (mapc (lambda (f) (add-hook 'typescript-ts-mode-hook f))
		     minor-modes)
	       (when (treesit-language-available-p 'tsx)
		 (add-to-list 'auto-mode-alist
			      '("\\.tsx\\'" . tsx-ts-mode))
		 (mapc (lambda (f) (add-hook 'tsx-ts-mode f))
		       minor-modes)))))))
  (package-bind-executable "tsserver" 'tide)
  (when (package-installed-p 'tide)
    (with-eval-after-load 'tide
      (setq
       tide-format-options
       '( :insertSpaceAfterFunctionKeywordForAnoymousFunctions t
	  :placeOpenBraceOnNewLineForFunctions nil
	  )))
    (funcall setup-typescript
	     #'tide-setup
	     #'flymake-mode
	     #'eldoc-mode
	     )))

(progn
  ;; Julia
  (package-bind-executable "julia" 'julia-mode 'julia-repl 'ein)
  (when (and (package-installed-p 'julia-mode)
	     (package-installed-p 'julia-repl))
    (add-hook 'julia-mode-hook #'julia-repl-mode)))

(progn
  ;; Racket
  (package-bind-executable "racket" 'racket-mode)
  (when (package-installed-p 'racket-mode)
    (add-hook 'racket-mode-hook #'racket-xp-mode)
    (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)
    (when (package-installed-p 'smartparens)
      (add-hook 'racket-mode-hook #'smartparens-strict-mode))
    ))

(progn
  ;; Rust
  (package-bind-executable '("cargo" "rust-analyzer") 'rust-mode)
  (when (package-installed-p 'rust-mode)
    (add-hook 'rust-mode-hook #'flymake-mode)
    (add-hook 'rust-mode-hook #'eglot-ensure))
  ;; (defun rustup-install-rust-analyzer ()
  ;;   (let ((buffer (generate-new-buffer "Rustup Output")))
  ;; 	  (shell-command "rustup component list" buffer)
  ;; 	  (when (with-current-buffer buffer
  ;; 		  (goto-char (point-min))
  ;; 		  (re-search-forward "^rust-analyzer-[^ ]* (installed)" nil t))
  ;; 	    (shell-command "rustup component add rust-analyzer" buffer))
  ;; 	  (kill-buffer buffer)))
  )

(progn
  ;; Haskell
  (package-bind-executable "ghc" 'haskell-mode 'hindent)
  (when (and (package-installed-p 'haskell-mode)
	     (package-installed-p 'hindent))
    (add-hook 'haskell-mode-hook #'hindent-mode)))

(progn
  ;; Javascript and JSX
  (package-bind-executable "npm" 'js2-mode)
  (when (package-installed-p 'js2-mode)
    (add-to-list 'auto-mode-alist
		 '("\\.jsx?\\'" . js-mode))
    (add-hook 'js-mode-hook #'js2-minor-mode))
  (with-eval-after-load 'js
    (setq js-indent-level 2)))
