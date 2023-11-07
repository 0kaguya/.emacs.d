;; --  -*- lexical-binding: t -*-
(with-eval-after-load 'evil
  ;; when evil enabled, use `evil-smartparens' with smartparens.
  (with-eval-after-load 'smartparens    
    (unless (package-installed-p 'evil-smartparens)
      (package-install 'evil-smartparens))
    (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)))

(with-eval-after-load 'smartparens
  ;; enable smartparens' default config.
  (require 'smartparens-config))
