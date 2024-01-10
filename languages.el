;; -*- lexical-binding: t -*-
(require 'cl-macs)

(let ((all-packages '())
      (active-packages '())
      (update-packages-functions '()))
  (defun update-packages ()
    "Alter packages according to executables found in PATH.
Run `update-packages' manually when the development toolchain for
some language is added or removed."
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
		     (package-delete (package-get-descriptor package))))))
	  all-packages)
    )
  (defun package-bind-executable (executables &rest packages)
    "Assign `packages' to track some `executables' in PATH.
This won't have effect until `update-packages' is called."
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
  (keymap-set eglot-mode-map "S-<f6>" #'eglot-rename))

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
	     #'electric-pair-local-mode
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
    (cond ((package-installed-p 'smartparens)
	   (add-hook 'racket-mode-hook #'smartparens-strict-mode))
	  ((add-hook 'racket-mode-hook #'electric-pair-local-mode))))
  )

(progn
  ;; Rust
  ;; On some distributions, an empty `rust-analyzer' will be created
  ;; when rustup is installed by system package manager.
  ;; This will cause an error at runtime. So make sure `rust-analyzer'
  ;; is marked with `installed' in the rustup component list.
  (package-bind-executable '("cargo" "rust-analyzer") 'rustic)
  (when (package-installed-p 'rust-mode)
    (add-hook 'rust-mode-hook #'flymake-mode)
    (add-hook 'rust-mode-hook #'eglot-ensure)
    (add-hook 'rust-mode-hook #'electric-pair-local-mode))
  ;; (defun rustup-install-rust-analyzer ()
  ;;   (let ((buffer (generate-new-buffer "Rustup Output")))
  ;;	  (shell-command "rustup component list" buffer)
  ;;	  (when (with-current-buffer buffer
  ;;		  (goto-char (point-min))
  ;;		  (re-search-forward "^rust-analyzer-[^ ]* (installed)" nil t))
  ;;	    (shell-command "rustup component add rust-analyzer" buffer))
  ;;	  (kill-buffer buffer)))
  (when (package-installed-p 'rustic)
    (setq rustic-lsp-client 'eglot)
    (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
    )
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

(with-eval-after-load 'mixal-mode
  ;; Knuth's MIXAL
  (load "/usr/share/mdk/mixvm.el" t)
  )

(progn
  (defun term-run (command)
    "run a shell command in a new window."
    (when (eq (next-window) (selected-window))
      (split-window))
    (with-selected-window (next-window)
      (term command)))
  (let ((compiler nil))
    (defvar-local compiler-options ""
      "file local compiler options")
    (put 'compiler-options 'safe-local-variable #'stringp)
    (defun c++-compile ()
      (interactive)
      (unless (consp compiler)
	(setq compiler (seq-filter #'executable-find
				   '("g++" "clang++" "cl"))))
      (let ((command (cond
		      ((not (consp compiler))
		       "echo no compiler found")
		      ((null (buffer-file-name))
		       "echo file have not saved yet")
		      ((concat
			(car compiler) " "
			compiler-options " "
			(buffer-file-name))))))
	(compile command))))
  (defun c++-run ()
    (interactive)
    ;; there could be a `in.txt' file in the same directory.
    (let ((input (concat (file-name-directory (buffer-file-name))
			 "in.txt")))
      ;; when `in.txt' exists, redirect input from it.
      (term-run (concat "./a.out"
			(when (file-exists-p input)
			  (concat " < " input))))))
  (let ((major-mode-hook
	   ;; select major mode
	   (cond ((and (treesit-available-p)
		       (treesit-language-available-p 'c++))
		  ;; use tree sitter mode when available
		  (add-to-list 'major-mode-remap-alist
			       '(c++-mode . c++-ts-mode))
		  'c++-ts-mode-hook)
		 ('c++-mode-hook))))
      ;; add hook to major mode
      (add-hook major-mode-hook #'electric-pair-local-mode)
      (add-hook major-mode-hook (lambda ()
				  (keymap-local-set "C-c C-c" #'c++-compile)
				  (keymap-local-set "C-c C-r" #'c++-run))))
  (add-hook 'text-mode-hook
	      (lambda ()
		(when (string= (file-name-nondirectory (buffer-file-name))
			       "in.txt")
		  ;; also bind keys for the `in.txt' file.
		  (keymap-local-set "C-c C-c" #'c++-compile)
		  (keymap-local-set "C-c C-r" #'c++-run)
				  ))))

(progn
  ;; C
  (add-hook 'c-mode-hook (lambda ()
			   (electric-pair-local-mode)
			   (keymap-local-set "C-c C-c" #'compile)))
  (when (and (treesit-available-p)
	     (treesit-language-available-p 'c))
    (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
    (add-hook 'c-ts-mode-hook (lambda () (run-hooks 'c-mode-hook)))))

(unless (package-installed-p 'yaml-mode)
  ;; YAML
  (package-install 'yaml-mode))

;; Coq
(package-bind-executable '("coqtop" "coqc" "coqdep") 'proof-general)

;; LaTeX
;  ... how about enable global electric pair mode?
;      or, globally smartparens mode?
(add-hook 'latex-mode-hook #'electric-pair-local-mode)

;; Swift
(package-bind-executable "swift" 'swift-mode)

;; Clojure
(package-bind-executable "clojure" 'clojure-mode 'cider)
(when (package-installed-p 'clojure-mode)
  (when (package-installed-p 'smartparens)
    (add-hook 'clojure-mode-hook #'smartparens-strict-mode)))

;; Scala
(package-bind-executable "scala" 'scala-mode)
