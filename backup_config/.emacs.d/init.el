(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell t)
(set-face-attribute 'default nil :font "Ricty Diminished" :height 140)
(require 'exwm)
(require 'exwm-config)
(exwm-config-default)
(load-theme 'gruvbox-dark-medium t)
(set-face-foreground 'font-lock-string-face "red")
(set-face-foreground 'font-lock-comment-face "light pink")
(server-start)
(ac-config-default)
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

(package-initialize)
(unless package-archives-contents
  (package-refresh-contents))

(use-package smart-mode-line)
(defun package-autoremove ()
  "Remove packages that are no more needed.
	  Packages that are no more needed by other packages in
	  `package-selected-packages' and their dependencies
	  will be deleted."
  (interactive)
  ;; If `package-selected-packages' is nil, it would make no sense to
  ;; try to populate it here, because then `package-autoremove' will
  ;; do absolutely nothing.
  (when (or package-selected-packages
	      (yes-or-no-p
	      (format-message
	      "`package-selected-packages' is empty! Really remove ALL packages? ")))
      (let ((removable (package--removable-packages)))
      (if removable
	  (when (y-or-n-p
		  (format "%s packages will be deleted:\n%s, proceed? "
			  (length removable)
			  (mapconcat #'symbol-name removable ", ")))
	      (mapc (lambda (p)
		      (package-delete (cadr (assq p package-alist)) t))
		  removable))
	  (message "Nothing to autoremove")))))

  (defun package--removable-packages ()
    "Return a list of names of packages no longer needed.
  These are packages which are neither contained in
  `package-selected-packages' nor a dependency of one that is."
    (let ((needed (cl-loop for p in package-selected-packages
			   if (assq p package-alist)
			   ;; `p' and its dependencies are needed.
			   append (cons p (package--get-deps p)))))
      (cl-loop for p in (mapcar #'car package-alist)
	       unless (memq p needed)
	       collect p)))

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq delet-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
	'(kill-ring
	  search-ring
	  regexp-search-ring))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'custom-theme-load-path "/home/andou/.emacs.d/emacs-color-theme-solarized/")

(require 'xah-fly-keys)
(xah-fly-keys-set-layout "dvorak")
(xah-fly-keys 1)

(require 'which-key)
(which-key-mode 1)

(use-package winner
    :defer t)
