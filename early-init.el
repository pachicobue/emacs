;;; early-init.el --- Emacs config before GUI load -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;
;; Emacs 27+ introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;; - Define build option
;; - System settings
;; - Speedup for startup process
;;
;;; Code:

;; [Option] Switch init mode.
(defconst opt/debug-p nil
  "If non-nil, make starup verbose.")
(defconst opt/enable-profile-p nil
  "If non-nil, enable profile.")

;; [Setting] Set output directory.
(setq user-emacs-directory (expand-file-name "local/" user-emacs-directory))
(custom-set-variables '(custom-file (expand-file-name "custom.el" user-emacs-directory)))

;; Turn on debug info.
(when opt/debug-p
  (setq init-file-debug t)
  (setq debug-on-error t))

;; Suppress GC during startup process.
;; Make sure to reset this.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 1024 1024 16))))

;; Supress `Magic File Name'
;; Make sure to reset this.
(let ((backup/file-name-handler-alist file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'after-init-hook
	    (lambda ()
	      (setq file-name-handler-alist backup/file-name-handler-alist))))

;; Disable automatic package load.
(setq package-enable-at-startup nil)

;; Newer .el is prior to old .elc
(setq load-prefer-newer t)

;; Disable frame content here
(push '(cursor-type . bar) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

(provide 'early-init.el)
;;; early-init.el ends here
