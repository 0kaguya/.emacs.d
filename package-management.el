;; -*- lexical-binding: t -*-
(require 'cl-macs)
(defvar binds nil)

(defun package-bind-executable (exe &rest pkgs)
  (let ((elt (cons exe pkgs)))
    (setq binds (cons elt binds))))

(let ((bins nil))
  (defun get-bins ()
    (cond ((null bins)
	   (setq bins
		 (mapcan (lambda (path)
			   (when (file-exists-p path)
			     (directory-files path nil nil t)))
			 exec-path)))
	  ('else bins))))

(defun executable-exists-p (bin)
  (seq-contains (get-bins) bin #'string=))

(defun update-packages ()
  (interactive)
  (package-refresh-contents)
  (let ((all-packages nil)
	(active-packages nil))
      (dolist (bind binds)
	(let ((exe (car bind))
	      (pkgs (cdr bind)))
	  (let ((satisfied
		 (seq-every-p #'executable-exists-p (ensure-list exe))))
	    (dolist (pkg pkgs)
	      (let* ((form (ensure-list pkg))
		     (name (car form)))
		(unless (assq name all-packages)
		  (setq all-packages (cons form all-packages)))
		(when satisfied
		  (unless (assq name active-packages)
		    (setq active-packages (cons form active-packages)))))))))
      (dolist (package all-packages)
	(let ((name (car package))
	      (recipe (cdr package)))
	  (let ((satisfied
		 (assq name active-packages)))
	    (cond ((package-installed-p name)
		   (unless satisfied
		     (package-delete (package-get-descriptor name))))
		  ('else
		   (when satisfied
		     (cond ((null recipe)
			    (package-install name))
			   ('else
			    (funcall recipe)))))))))))
