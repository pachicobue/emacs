;;; early-init.el --- Emacs config before GUI load -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;
;; Emacs 27+ introduces early-init.el, which is run before init.el,
;;
;;; Code:

;; File Path
(defconst my/base-directory user-emacs-directory)
(defconst my/early-init-file (expand-file-name "early-init.el" my/base-directory))
(defconst my/init-file (expand-file-name "init.el" my/base-directory))
(defconst my/leaf-db-file (expand-file-name "leaf-db.el" my/base-directory))
(defconst my/local-file-directory (expand-file-name "local/" my/base-directory))

;; To keep .config/emacs clean
(setq auto-save-list-file-prefix nil)
(setq user-emacs-directory my/local-file-directory)

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

(provide 'early-init.el)
;;; early-init.el ends here
