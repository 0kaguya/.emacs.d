;; -- Emacs config for individual packages -*- lexical-binding: t -*-

(with-eval-after-load 'eglot
  (keymap-set eglot-mode-map "S-<f6>" #'eglot-rename)
  (keymap-set eglot-mode-map "<f1>" #'eldoc-doc-buffer))

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
