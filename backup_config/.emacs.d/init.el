(package-initialize)

(defun my/reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
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

(put 'upcase-region 'disabled nil)

(defun my-inhibit-startup-screen-always ()
  "Startup screen inhibitor for `command-line-functions`.
Inhibits startup screen on the first unrecognised option."
  (ignore (setq inhibit-startup-screen t)))

(add-hook 'command-line-functions #'my-inhibit-startup-screen-always)

(setq-default fill-column 100)

(ac-config-default)

(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package smart-mode-line)

(fset 'yes-or-no-p 'y-or-n-p)

(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

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

(load-theme 'solarized-dark t)
;; (load-theme 'gruvbox-dark-hard t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(electric-pair-mode t)
 '(frame-background-mode 'dark)
 '(global-display-line-numbers-mode t)
 '(global-wakatime-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(solarized-theme gruvbox-theme monokai-theme exotica-theme guide-key undo-tree smart-mode-line magit wakatime-mode smex rust-mode fill-column-indicator autothemer auto-complete))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#282828"))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(wakatime-cli-path "/usr/bin/wakatime")
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 174 :width normal :foundry "PfEd" :family "Ricty Diminished")))))
