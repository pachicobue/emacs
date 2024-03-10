;;; init.el --- Emacs config before GUI load -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;
;; Emacs configurations
;;
;;; Code:

;; Turn on performance profiling.
(when opt/enable-profile-p
  (require 'profiler)
  (profiler-start 'cpu)
  (add-hook 'after-init-hook
	    (lambda ()
	      (profiler-report)
	      (profiler-stop))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)
    :config
    (leaf-keywords-init))

  (leaf general :ensure t))


(leaf *basic-config
  :config
  (leaf system
    :custom ((ring-bell-function . 'ignore)
	     (set-language-environment . "japanese")
	     (prefer-coding-system . 'utf-8))
    :config
    (defalias 'yes-or-no-p 'y-or-n-p))
  (leaf user
    :custom ((user-full-name . "pachicobue")
             (user-mail-address . "tigerssho@gmail.com")
             (user-login-name . "pachicobue")))
  (leaf history
    :custom ((history-length . 1000)
             (history-delete-duplicates . t)))
  (leaf autofile
    :custom ((create-lockfiles . nil)
             (make-backup-files . nil)
             (savehist-autosave-interval . 60)
             (auto-save-default . nil)))
  (leaf startup
    :custom ((inhibit-startup-screen . t)
             (inhibit-startup-message . t)
             (inhibit-startup-echo-area-message . t)))
  (leaf recentf
    :custom ((recentf-exclude . '("recentf" "COMMIT_EDITMSG")))
    :global-minor-mode t)
  (leaf autorevert
    :custom ((auto-revert-interval . 0.1))
    :global-minor-mode auto-revert-mode))


;;-------------------------------------------------------------------------

(leaf evil
  :ensure t
  :custom ((evil-normal-state-cursor . 'box))
  :bind ((evil-normal-state-map
	  ("H" . mwim-beginning-of-code-or-line)
	  ("L" . mwim-end-of-code-or-line))
	 (evil-insert-state-map
	  ("C-g" . evil-normal-state)
	  ("ESC" . evil-normal-state)))
  :global-minor-mode evil-mode)

(leaf vundo
  :ensure t
  :bind ((vundo-mode-map
	  ("h" . vundo-backward)
	  ("j" . vundo-next)
	  ("k" . vundo-previous)
	  ("l" . vundo-forward)))
  :config
  (leaf evil-bind
    :after evil
    :bind ((evil-normal-state-map
	    ("C-u" . vundo)))))

(leaf smartparens
  :ensure t
  :require smartparens-config
  :hook
  (prog-mode-hook . turn-on-smartparens-mode)
  :blackout t
  :config
  (show-smartparens-global-mode t))

(leaf which-key
  :ensure t
  :blackout t
  :global-minor-mode which-key-mode)

(leaf mwim :ensure t)

(leaf rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(leaf volatile-highlights
  :ensure t
  :blackout t
  :global-minor-mode volatile-highlights-mode)

(leaf centaur-tabs
  :ensure t
  :global-minor-mode centaur-tabs-mode
  :config
  (leaf evil-bind
    :after evil
    :bind ((evil-normal-state-map
	    ("g t" . centaur-tabs-forward)
	    ("g T" . centaur-tabs-backward)))))

(leaf avy
  :ensure t
  :config
  (leaf evil-bind
    :after evil
    :bind ((evil-normal-state-map
	    ("f" . avy-goto-word-1)))))

(leaf format-all
  :ensure t
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  )

(leaf *completion
  :config
  (leaf orderless
    :ensure t
    :custom ((completion-styles . '(orderless basic))
	     (completion-category-defaults . nil)
	     (completion-category-overrides . nil)))
  (leaf vertico
    :ensure t
    :custom ((vertico-preselect . 'prompt))
    :global-minor-mode vertico-mode)
  (leaf consult
    :ensure t)
  (leaf corfu
    :ensure t
    :bind ((corfu-map
	    ("TAB" . corfu-insert)
	    ("RET" . nil)))
    :custom ((corfu-cycle . t)
	     (corfu-auto . t)
	     (corfu-auto-delay . 0)
	     (corfu-auto-prefix . 3)
	     (corfu-on-exact-match . nil)
	     (tab-always-indent . 'complete))
    :global-minor-mode global-corfu-mode)
  (leaf kind-icon
    :ensure-system-package
    :after corfu
    :custom (kind-icon-default-face 'corfu-default)
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(leaf pangu-spacing
  :ensure t
  :hook ((markdown-mode-hook text-mode-hook org-mode-hook) . pangu-spacing-mode)
  :custom ((pangu-spacing-real-insert-separator . t))
  :blackout t)

(leaf projectile
  :ensure t
  :blackout t
  :global-minor-mode projectile-mode)

(leaf copilot
  :el-get (copilot
	   :type github
	   :pkgname "copilot-emacs/copilot.el")
  :init
  (leaf s :ensure t)
  (leaf dash :ensure t)
  (leaf editorconfig :ensure t)
  (leaf jsonrpc :ensure t)
  )

(leaf exec-path-from-shell
  :when (daemonp)
  :ensure t
  :config
  (exec-path-from-shell-initialize))
