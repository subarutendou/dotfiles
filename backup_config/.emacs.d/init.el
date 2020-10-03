(setq inhibit-startup-message t)

; Setting for GUI emacs
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
;; (require 'exwm)
;; (require 'exwm-config)
;; (exwm-config-default)
; LINE NUMBE
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(menu-bar-mode -1)
(setq visible-bell t)
(set-face-attribute 'default nil :font "Ricty Diminished" :height 140)
(load-theme 'gruvbox-dark-medium t)
;;(set-face-foreground 'font-lock-string-face "red")
;;(set-face-foreground 'font-lock-comment-face "light pink")
(server-start)
(ac-config-default)
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

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

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

(use-package smart-mode-line)

(require 'xah-fly-keys)
(xah-fly-keys-set-layout "dvorak")
(xah-fly-keys 1)

(require 'which-key)
(which-key-mode 1)

(use-package winner
    :defer t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-delimiters xah-fly-keys which-key wakatime-mode use-package undo-tree solarized-theme smex smart-mode-line shrink-path rust-mode monokai-theme magit-popup magit guide-key gruvbox-theme ghub fill-column-indicator exwm exotica-theme auto-complete all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
