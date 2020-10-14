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

;; user interface

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(column-number-mode 1)
(blink-cursor-mode 0)
(setq use-dialog-box nil)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(progn
  ;; no need to warn
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
)

;; set default font
(set-frame-font
 (cond
  ((string-equal system-type "windows-nt") ; Microsoft Windows
   (if (member "Consolas" (font-family-list))
       "Consolas"
     nil
     ))
  ((string-equal system-type "darwin") ; macOS
   (if (member "Menlo" (font-family-list))
       "Menlo-14"
     nil
     ))
  ((string-equal system-type "gnu/linux") ; linux
   (if (member "Ricty Diminished" (font-family-list))
       "Ricty Diminished"
     nil
     ))
  (t nil))
 t t)

;; set font for emoji
(set-fontset-font
 t
 '(#x1f300 . #x1fad0)
 (cond
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
  ((member "Symbola" (font-family-list)) "Symbola")
  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji"))
 ;; Apple Color Emoji should be before Symbola, but Richard Stallman skum disabled it.
 ;; GNU Emacs Removes Color Emoji Support on the Mac
 ;; http://ergoemacs.org/misc/emacs_macos_emoji.html
 ;;
 )

(cond
 ;; specify font for chinese characters
 ((string-equal system-type "windows-nt")
  (set-fontset-font
   t
   '(#x4e00 . #x9fff)
   (cond
    ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
    ((member "PMingLiU" (font-family-list)) "PMingLiU")
    ((member "SimHei" (font-family-list)) "SimHei")
    ((member "Microsoft YaHei UI" (font-family-list)) "Microsoft YaHei UI")
    ((member "MingLiU" (font-family-list)) "MingLiU")
    ((member "SimHei" (font-family-list)) "SimHei")
    ((member "DengXian" (font-family-list)) "DengXian")
    ((member "KaiTi" (font-family-list)) "KaiTi")
    ((member "SimSun" (font-family-list)) "SimSun"))))
 ((string-equal system-type "darwin")
  (cond
   ((member "Heiti SC" (font-family-list)) "Heiti SC")
   ((member "Heiti TC" (font-family-list)) "Heiti TC")
   ((member "Songti SC" (font-family-list)) "Songti SC")
   ((member "Songti TC" (font-family-list)) "Songti TC")
   ((member "Kaiti SC" (font-family-list)) "Kaiti SC")
   ((member "BiauKai" (font-family-list)) "BiauKai")))
 ((string-equal system-type "gnu/linux")
  (cond
   ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei"))))

(progn
  ;; use variable-width font for some modes
  (defun xah-set-proportial-font ()
    "Set current buffer to use variable-width font."
    (variable-pitch-mode 1)
    (text-scale-increase 1 ))

  (add-hook 'html-mode-hook 'xah-set-proportial-font)
  (add-hook 'nxml-mode-hook 'xah-set-proportial-font)
  (add-hook 'emacs-lisp-mode-hook 'xah-set-proportial-font)
  (add-hook 'js-mode-hook 'xah-set-proportial-font)
  (add-hook 'css-mode-hook 'xah-set-proportial-font)

  (add-hook 'xah-elisp-mode-hook 'xah-set-proportial-font)
  (add-hook 'xah-html-mode-hook 'xah-set-proportial-font)
  (add-hook 'xah-css-mode-hook 'xah-set-proportial-font)
  (add-hook 'xah-js-mode-hook 'xah-set-proportial-font)
  ;;
  )

(set-face-attribute 'default nil :height 120)

;; LINE NUMBE
;; (global-display-line-numbers-mode t)

;; Display Time
(display-time-mode 1)

;; File Setting
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
