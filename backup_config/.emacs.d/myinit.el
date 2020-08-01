(package-initialize)

(defun my/reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'custom-theme-load-path "/home/andou/.emacs.d/emacs-color-theme-solarized/")

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

(defun my-inhibit-startup-screen-always ()
  "Startup screen inhibitor for `command-line-functions`.
Inhibits startup screen on the first unrecognised option."
  (ignore (setq inhibit-startup-screen t)))

(add-hook 'command-line-functions #'my-inhibit-startup-screen-always)

(setq-default fill-column 100)

(ac-config-default)

(server-start)

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(display-time-mode 1)

(require 'nyan-mode)
(nyan-mode)
(nyan-start-animation)
(nyan-toggle-wavy-trail)

(load-theme 'solarized t)

(use-package smart-mode-line)

(fset 'yes-or-no-p 'y-or-n-p)

(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

(global-wakatime-mode)

(use-package undo-tree
	     :diminish undo-tree-mode
	     :config
	     (progn
	       (global-undo-tree-mode)
	       (setq undo-tree-visualizer-timestamps t)
	       (setq undo-tree-visualizer-diff t)))

(use-package guide-key
	     :defer t
	     :diminish guide-key-mode
	     :config
	     (progn
	       (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
	       (guide-key-mode 1)))	; Enable guide-key-mode

(use-package winner
  :defer t)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

(global-set-key (kbd "C-x g") 'magit-status)

(require 'exwm-systemtray)
(exwm-systemtray-enable)
(require 'exwm)
(require 'exwm-config)
(exwm-config-default)