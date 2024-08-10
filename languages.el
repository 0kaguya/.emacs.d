;; -*- lexical-binding: t -*-
(require 'cl-macs)
(load (concat user-emacs-directory "package-management"))

;; Emacs Lisp (elisp)
(package-bind-executable "emacs" 'smartparens 'macrostep)
(when (package-installed-p 'macrostep)
  (keymap-set emacs-lisp-mode-map "C-c e" #'macrostep-expand))
(when (package-installed-p 'smartparens)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode))

;; Typescript

;; fetch and install tree-sitter grammar if needed.
(unless (treesit-language-available-p 'typescript)
  (add-to-list 'treesit-language-source-alist
	       '(typescript
		 "https://github.com/tree-sitter/tree-sitter-typescript"
		 nil
		 "typescript/src"))
  (treesit-install-language-grammar 'typescript))
(unless (treesit-language-available-p 'tsx)
  (add-to-list 'treesit-language-source-alist
	       '(tsx
		 "https://github.com/tree-sitter/tree-sitter-typescript"
		 nil
		 "tsx/src"))
  (treesit-install-language-grammar 'tsx))
;; setup tree-sitter major-mode.
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(dolist (hook '(typescript-ts-mode-hook
		tsx-ts-mode-hook))
  (dolist (f '(electric-pair-local-mode
	       eglot-ensure))
    (add-hook hook f)))

;; (when (package-installed-p 'tide)
;;   (add-hook 'typescript-ts-mode-hook #'tide-setup)
;;   (add-hook 'tsx-ts-mode-hook #'tide-setup)
;;   (add-hook 'tide-mode-hook #'electric-pair-local-mode))

;; Julia
(package-bind-executable "julia" 'julia-mode 'julia-repl 'ein)
(when (package-installed-p 'julia-mode)
  (add-hook 'julia-mode-hook #'julia-repl-mode))

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

  (with-eval-after-load 'treesit
    (add-to-list
     'treesit-language-source-alist
     '(rust . ("https://github.com/tree-sitter/tree-sitter-rust.git"))))

  (with-eval-after-load 'rust-ts-mode
    ;; use short tab width
    (setq rust-ts-mode-indent-offset 2))

  (dolist (f '(eglot-ensure
	       smartparens-mode
	       ))
    (add-hook 'rust-ts-mode-hook f))

  ;; hotfix
  (with-eval-after-load 'smartparens-rust
    (dolist (open '("<" "{" "[" "("))
      (sp-local-pair '(rust-mode rust-ts-mode rustic-mode)
		     open nil :post-handlers '(:add ("||\n[i]" "RET")))))

  (defun rust-install-components ()
    ;; Install required rust components.
    (let ((components '("rust-analyzer" "rustfmt" "clippy")))
      (apply #'start-process "Rustup" "Rustup Output"
	     "rustup" "component" "add" components)))

  (defun rust-ts-install-grammar ()
    ;; Install tree sitter grammar if not exist.
    ;; To install a tree sitter grammar Git and C/C++ compiler is needed.
    (unless (treesit-language-available-p 'rust)
      (treesit-install-language-grammar 'rust)))

  (package-bind-executable
   '("rustup" "cargo")
   '(rust-components . rust-install-components)
   '(rust-ts-grammar . rust-ts-install-grammar)
   'cargo)

  (package-bind-executable "__blacklisted"
			   'rust-mode
			   'rustic)
  
  (when (package-installed-p 'cargo)
    (add-hook 'rust-ts-mode-hook #'cargo-minor-mode))
  (with-eval-after-load 'cargo
    (keymap-set cargo-mode-map "C-c" cargo-minor-mode-command-map))

  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

  (defun rust-beginning-of-defun ()
    ;; Used for seeking test function's name.
    ;; Let's do it simple: we just search for the `#[test]' tag; it's user's
    ;; mistake when they invoke this test from elsewhere anyway.
    (re-search-backward "^ *#\\[\\([a-z_]+::\\)?test]")        
    (forward-word 2))

  ) ;; Rust config

(progn
  ;; Haskell
  (dolist (exec '("haskell-language-server"
		  "haskell-language-server-wrapper"))
    (package-bind-executable exec 'haskell-mode))

  (when (package-installed-p 'haskell-mode)
    (dolist (f '(eglot-ensure
		 electric-pair-local-mode))
      (add-hook 'haskell-mode-hook f))))

(progn
  ;; Javascript and JSX
  (package-bind-executable "npm" 'js2-mode)
  (when (package-installed-p 'js2-mode)
    (add-to-list 'auto-mode-alist
		 '("\\.jsx?\\'" . js-mode))
    (add-hook 'js-mode-hook #'js2-minor-mode))
  (with-eval-after-load 'js
    (setq js-indent-level 2)))

(progn
  ;; JSON
  
  (add-to-list 'auto-mode-alist
	     '("\\.json\\'" . json-ts-mode))
  
  (with-eval-after-load 'treesit
    (add-to-list
     'treesit-language-source-alist
     '(json . ("https://github.com/tree-sitter/tree-sitter-json"))))
  
  (with-eval-after-load 'json-ts-mode
    (unless (treesit-language-available-p 'json)
      (treesit-install-language-grammar 'json)))

  (defun json-set-indent ()
    (setq-local js-indent-level 4))

  (dolist (f '(json-set-indent
	       electric-pair-local-mode))
    (add-hook 'json-ts-mode-hook f)))

;; HTML/XML/CSS/etc
(unless (package-installed-p 'web-mode)
  (package-install 'web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

(with-eval-after-load 'mixal-mode
  ;; Knuth's MIXAL
  (load "/usr/share/mdk/mixvm.el" t)
  )

(progn
  ;; competitive programming
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


;; Java
(when (treesit-language-available-p 'java)
  (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode)))


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
(package-bind-executable "scala" 'scala-ts-mode)
(when (package-installed-p 'scala-ts-mode)
  (unless (treesit-language-available-p 'scala)
    (add-to-list
     'treesit-language-source-alist
     '(scala . ("https://github.com/tree-sitter/tree-sitter-scala.git")))
    (treesit-install-language-grammar 'scala))
  (add-hook 'scala-ts-mode-hook #'electric-pair-local-mode))

;; F♯ (fsharp, F#)
(package-bind-executable "dotnet"
			 'fsharp-mode
			 'eglot-fsharp
			 'highlight-indentation)
(when (package-installed-p 'fsharp-mode)
  (add-hook 'fsharp-mode-hook #'eglot-ensure)
  (add-hook 'fsharp-mode-hook #'highlight-indentation-mode)
  (add-hook 'fsharp-mode-hook #'electric-pair-local-mode)
  (add-to-list 'auto-mode-alist '("\\.fsproj\\'" . xml-mode) 'append)
  (with-eval-after-load 'fsharp-mode
    (require 'eglot-fsharp)
    (setq inferior-fsharp-program "dotnet fsi --readline-"))
  )

;; Lean
(package-bind-executable
 "lean"
 '(lean4-mode . (lambda ()
		  (package-vc-install
		   '(lean4-mode
		     :url "https://github.com/leanprover/lean4-mode.git")))))
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories
	       "[/\\\\]\\.lake\\'"))
(add-hook 'lean4-mode-hook #'electric-pair-local-mode)

;; gPRC's Protobuf
(package-bind-executable "protoc" 'protobuf-mode)

;; erlang
(progn
  (package-bind-executable "__blocklisted" 'edts)
  
  (when (and
	 ;; `erlang.el' is bundled with erlang toolchain.
	 (package-installed-p 'erlang)
	 ;; `eglot' is default configured to `erlang_ls'.
	 (executable-exists-p "erlang_ls"))
    (add-hook 'erlang-mode-hook #'eglot-ensure))
  )
