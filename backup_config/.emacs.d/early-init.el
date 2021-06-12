;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; Setting for GUI emacs
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(electric-pair-mode 1)
(server-start)
(fset 'yes-or-no-p 'y-or-n-p)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(setq visible-bell t)

(set-face-attribute 'default nil :height 120)

;; user interface
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(column-number-mode 1)
(setq use-dialog-box nil)
