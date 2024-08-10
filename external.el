;; -- Emacs config for individual packages -*- lexical-binding: t -*-

(with-eval-after-load 'elec-pair
  (setq electric-pair-delete-adjacent-pairs nil))

(with-eval-after-load 'eglot  
  (add-hook 'eglot-managed-mode-hook #'flymake-mode)
  (dolist (pair '(("C-c h" eldoc-doc-buffer)
		  ("C-c u" eglot-rename)
		  ("C-c b" eglot-code-actions)
		  ("C-c n" flymake-show-buffer-diagnostics)))
    (apply #'keymap-set eglot-mode-map pair))
  )

(with-eval-after-load 'evil
  ;; use `M-m' as a second escape key,
  ;; because Esc key can be error-prone when emacs runs at text mode.
  (keymap-global-set "M-m" "<escape>")
  ;; extra package is needed in order to cooperate with smartparens.
  (with-eval-after-load 'smartparens    
    (unless (package-installed-p 'evil-smartparens)
      (package-install 'evil-smartparens))
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
  ;; evil settings
  (setq evil-undo-system 'undo-redo))

(with-eval-after-load 'smartparens
  ;; enable smartparens' default config.
  (require 'smartparens-config))

(with-eval-after-load 'indent
  (setq tab-always-indent 'complete))

(with-eval-after-load 'desktop
  (let ((found (dir-locals-find-file (buffer-file-name))))
    (cond ((consp found) (add-to-list 'desktop-path (car found)))
	  ((stringp found) (add-to-list 'desktop-path found)))))
