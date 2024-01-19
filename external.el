;; -- Emacs config for individual packages -*- lexical-binding: t -*-
(with-eval-after-load 'evil
  ;; use M-m as escape key to avoid that big problem.
  (keymap-global-set "M-m" "<escape>")
  ;; when evil enabled, use `evil-smartparens' with smartparens.
  (with-eval-after-load 'smartparens    
    (unless (package-installed-p 'evil-smartparens)
      (package-install 'evil-smartparens))
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))
  ;; evil settings
  (setq evil-undo-system 'undo-redo))

(with-eval-after-load 'smartparens
  ;; enable smartparens' default config.
  (require 'smartparens-config))
