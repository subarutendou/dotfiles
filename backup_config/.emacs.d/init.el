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
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(which-key xah-fly-keys gruvbox-theme monokai-theme exotica-theme guide-key undo-tree smart-mode-line magit wakatime-mode smex rust-mode fill-column-indicator autothemer auto-complete))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#282828"))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 174 :width normal :foundry "PfEd" :family "Ricty Diminished")))))

(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
(put 'upcase-region 'disabled nil)
