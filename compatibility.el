;; -*- lexical-binding: t -*-
;; Define functions that might not be defined in earlier Emacs versions.

(unless (fboundp 'keymap-global-set)
  (defun keymap-global-set (key command)
    (global-set-key
     (kbd KEY)
     (cond ((stringp command)
	    (kbd command))
	   (command)))))

(unless (fboundp 'keymap-set)
  (defun keymap-set (keymap key definition)
    (define-key
     keymap
     (kbd key)
     (cond ((stringp definition)
	    (kbd definition))
	   (definition)))))

(unless (fboundp 'treesit-available-p)
  (defun treesit-available-p ()
    (package-installed-p 'tree-sitter)))
