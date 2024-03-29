;; -- Emacs Config -*- lexical-binding: t -*-

(progn
  ;; -- Commonly used options.
  (unless (eq system-type 'darwin)
    ;; show menu bar on macOS, hide menu bar otherwise.
    (menu-bar-mode -1))
  ;; Prohibit some widgets for a clean appearance.
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; Don't show welcome page and welcome message.
  (setq inhibit-startup-screen t
	inhibit-startup-echo-area-message t)
  ;; Turn off beep
  (setq visible-bell 1)
  ;; Set backup file directory
  (setq backup-directory-alist
	`(("." . ,(concat user-emacs-directory ".backups"))))
  ;; Also display column number.
  (column-number-mode)
  ;; Automatic generated settings are at somewhere else.
  (setq custom-file (concat user-emacs-directory "custom.el"))
  )

(progn
  ;; -- Other personalize options
  (defvar use-melpa-mirror 'tuna
    "a non-nil value to use a mirror replacing original source.")
  (defvar input-method-enabled nil
    "non-nil to use a builtin input method")
  ;; I don't like blinking cursor.
  (blink-cursor-mode -1)
  ;; show startup time
  (add-hook 'emacs-startup-hook
	    (lambda () (message (emacs-init-time))))
  )

(setq
 ;; -- Toggle some options.
 ;; quoted insert use hex number
 read-quoted-char-radix 16
 ;; don't want a pop-up window.
 use-dialog-box nil
 )

;; Define functions that might not be defined in earlier Emacs versions.
(load (concat user-emacs-directory "compatibility"))

(when (display-graphic-p)
  ;; Defines and binds `toggle-transparency' feature
  (let ((alpha
	 (cond ((and (>= emacs-major-version 29)
		     (eq (framep (selected-frame)) 'pgtk))
		'alpha-background)
	       ('alpha)))
	;; change value of `low' for a comfortable opacity
	(low 0.7)
	(high 1.0))
    (defun toggle-transparency ()
      (interactive)
      (letrec ((current (frame-parameter nil alpha))
	       (next (cond ((null current) low)
			   ((eql high current) low)
			   ((eql low current) high)
			   ('otherwise high))))
	(set-frame-parameter nil alpha next))))
  ;; binds it to F12
  (keymap-global-set "<f12>" #'toggle-transparency))

(let ((uname nil))
  (defun wslp ()
    "Returns non-nil when Emacs is running on WSL."
    (and (eq system-type 'gnu/linux)
	 (string-match
	  "-[Mm]icrosoft"
	  (cond ((stringp uname)
		 uname)
		((setq uname (shell-command-to-string "uname -a"))))
	  ))))

(when (or (eq system-type 'windows-nt)
	  (wslp))
  ;; Define a second key for `just-one-space' because the default keybinding
  ;; get interrupt with other hotkeys on Windows.
  (keymap-global-set "M-<f1>" #'just-one-space))

(when (and (display-graphic-p)
	   (eq system-type 'darwin))
  ;; Replace ring bell with custom bell on macOS
  (defvar ring-bell-lock nil)
  (setq visible-bell nil)
  (setq ring-bell-function
	(lambda ()
	  (unless ring-bell-lock
	    (setq ring-bell-lock t)
	    (let ((current (face-attribute 'mode-line :background))
		  (flash "#000000")) ;; High contrast color
	      (set-face-attribute 'mode-line nil :background flash)
	      (run-with-idle-timer
	       0.1
	       nil
	       (lambda ()
		 (set-face-attribute 'mode-line nil
				     :background current)
		 (setq ring-bell-lock nil))))))))

(when (display-graphic-p)
  ;; Set Font
  ;; TODO: adjust font size by DPI
  (cond ((eq system-type 'darwin)
	 (set-face-attribute 'default nil
			     :family "Monaco" :foundry "outline"
			     :slant 'normal :weight 'normal
			     :height 150 :width 'normal))
	((eq system-type 'windows-nt)
	 (set-face-attribute 'default nil
			     :family "Sarasa Mono SC" :foundry "outline"
			     :slant 'normal :weight 'normal
			     :height 110 :width 'normal))

	('otherwise
	 ;; I don't know why, but monospacing only works at size=20.
	 (set-face-font 'default "Sarasa Mono SC:size=20"))
	))

(when (display-graphic-p)
  "Set frame size"
  ;; (set-frame-position (selected-frame) 100 100)
  ;; (set-frame-size (selected-frame) 100 30)
  (when (and (eq system-type 'gnu/linux)
	     (eq (window-system) 'pgtk))
    ;; sometimes they aren't smart enough.
    (set-frame-size (selected-frame) 80 25))
  )


;; Emulate the title bar's right-click menu
;; (define-prefix-command 'keymap-titlebar-menu)
;; (global-set-key (kbd "M-SPC") 'keymap-titlebar-menu)
;; (define-key keymap-titlebar-menu (kbd "x") 'toggle-frame-maximized)
;; (define-key keymap-titlebar-menu (kbd "c") 'save-buffers-kill-terminal)

(progn
  ;; Package Manager Settings.
  (when (< emacs-major-version 27)
    ;; load package manager for version beforce Emacs 27.
    (require 'package))
  (with-eval-after-load 'package
    (add-to-list
     'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (add-to-list
     'package-archives '("org"   . "https://orgmode.org/elpa/") t)
    (cond
     ((eq use-melpa-mirror 'tuna)
      ;; https://mirrors.tuna.tsinghua.edu.cn
      (setq
       package-archives
       '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	 ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
	 ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
	 ;; got problem with this one.
	 ;("org"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
	 )))
     )
    )
  (when (< emacs-major-version 27)
    ;; initialize package manager for version before Emacs 27.
    (package-initialize))
  (cond ((file-exists-p package-user-dir)
	 ;; refresh package list when Emacs is idle.
	 (run-with-idle-timer
	  30
	  nil
	  #'package-refresh-contents))
	;; fetch package list immediately when it's not present.
	((package-refresh-contents)))
  )

;; set no proxy for elpa mirror.
;; (with-eval-after-load 'url-vars
;;   (add-to-list 'url-proxy-services
;;	       '("no_proxy" . "[^.]*\\.tuna\\.tsinghua\\.edu\\.cn")))

(unless (memq system-type '(ms-dos windows-nt))
  ;; Use `exec-path-from-shell' to load shell PATH
  (unless (package-installed-p 'exec-path-from-shell)
    (package-install 'exec-path-from-shell))
  (with-eval-after-load 'exec-path-from-shell
    ;; `exec-path-from-shell' use interactive shell by default. On proper
    ;; configured shells starting a non-interactive shell should be ok.
    (setq exec-path-from-shell-arguments nil))
  ;; Load path from shell on every init. This takes considerable time.
  (exec-path-from-shell-initialize)
  )

(when input-method-enabled
  ;; Enable Input Method on demand.
  (unless (package-installed-p 'pyim)
    (package-install 'pyim))
  (unless (package-installed-p 'pyim-basedict)
    (package-install 'pyim-basedict))
  (with-eval-after-load 'pyim
    (setq-default pyim-punctuation-translate-p '(yes))
    (pyim-default-scheme 'quanpin)
    (setq pyim-page-length 5)
    (pyim-basedict-enable))
  (setq default-input-method "pyim"))


(progn
  ;; Config for editing Emacs Lisp.
  ;; smartparens for semi structure editing.
  (unless (package-installed-p 'smartparens)
    (package-install 'smartparens))
  (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)

  ;; macro expansion feature provided by `macrostep'.
  (unless (package-installed-p 'macrostep)
    (package-install 'macrostep))
  (keymap-set emacs-lisp-mode-map "C-c e" #'macrostep-expand)
  )

(unless (package-installed-p 'magit)
  ;; Git frontend
  (package-install 'magit))

(unless (display-graphic-p)
  ;; Use evil mode on text ui.
  ;; `evil-mode' configurations
  (unless (package-installed-p 'evil)
    (package-install 'evil))
  (add-hook 'after-init-hook #'evil-mode)
  )

(progn
  ;; -- LOAD OTHER CONFIG FILES
  ;; per-package config
  (load (concat user-emacs-directory "external"))
  ;; per-language config
  (load (concat user-emacs-directory "languages"))
  ;; legacy config
  (load (concat user-emacs-directory "conditional"))
  ;; Auto-generated config
  (load custom-file t)
  )
