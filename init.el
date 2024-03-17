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

;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; leaf.el
(eval-and-compile
  (straight-use-package 'leaf)
  (straight-use-package 'leaf-keywords)
  (leaf-keywords-init))

;;=========================================================

(leaf *basic-config
  :config
  (leaf system
    :custom
    (ring-bell-function . 'ignore)
    (set-language-environment . "japanese")
    (prefer-coding-system . 'utf-8)
    :config
    (defalias 'yes-or-no-p 'y-or-n-p))
  (leaf user
    :custom
    (user-full-name . "pachicobue")
    (user-mail-address . "tigerssho@gmail.com")
    (user-login-name . "pachicobue"))
  (leaf history
    :custom
    (history-length . 1000)
    (history-delete-duplicates . t))
  (leaf autofile
    :custom
    (create-lockfiles . nil)
    (make-backup-files . nil)
    (savehist-autosave-interval . 60)
    (auto-save-default . nil))
  (leaf startup
    :custom
    (inhibit-startup-screen . t)
    (inhibit-startup-message . t)
    (inhibit-startup-echo-area-message . t))
  (leaf recentf
    :custom
    (recentf-max-saved-items . 1000)
    (recentf-auto-cleanup . 'never)
    :global-minor-mode recentf-mode)
  (leaf autorevert
    :custom
    (auto-revert-interval . 1)
    (auto-revert-check-vc-info . t)
    :global-minor-mode auto-revert-mode)
  (leaf server
    :commands (server-running-p)
    :unless (server-running-p)
    :config
    (server-start)))

;;=========================================================

(leaf *environment
  :config
  (leaf exec-path-from-shell
    :straight t
    :custom
    (exec-path-from-shell-arguments . nil)
    (exec-path-from-shell-warn-duration-millis . 2000)
    :config
    (exec-path-from-shell-copy-envs '("PATH" "CXX_COMPILER" "CXX_COMMON_OPTIONS" "CXX_DEBUG_OPTIONS" "CXX_RELEASE_OPTIONS"))))

(leaf *appearance
  :config
  (leaf fontaine
    :straight t
    :custom
    (fontaine-presets . '((regular
			   :default-familly "Noto Sans Mono CJK JP"
			   :default-height 105
			   :default-weight semi-bold
			   :fixed-pitch-family "Noto Sans Mono CJK JP"
			   :variable-pitch-family "Noto Sans CJK JP"
			   :italic-family "Noto Sans Mono CJK JP"
			   :line-spacing 1)))
    :config
    (fontaine-set-preset 'regular))
  (leaf *theme
    :custom
    (modus-italic-constructs . t)
    (modus-bold-constructs . t)
    (modus-themes-no-mixed-fonts . nil)
    (modus-themes-subtle-line-numbers . t)
    (modus-themes-success-deuteranopia . t)
    (modus-themes-inhibit-reload . t)
    (modus-themes-fringes . nil)
    (modus-themes-lang-checkers . nil)
    (modus-themes-mode-line . '(moody borderless))
    (modus-themes-syntax . nil)
    (modus-themes-hl-line . '(underline accented))
    (modus-themes-paren-match . '(bold intense))
    (modus-themes-links . '(neutral-underline background))
    (modus-themes-prompts . '(intense bold))
    (modus-thems-completions . 'moderate)
    (modus-themes-mail-citations . nil)
    (modus-themes-region . '(bg-only no-extend))
    (modus-themes-diffs . 'fg-only-deuteranopia)
    (modus-themes-org-blocks . 'gray-background)
    (modus-themes-org-agenda . '((header-block . (variable-pitch scale-title))
				 (header-date . (grayscale workaholic bnold-today))
				 (scheduled . uniiform)
				 (habit . traffic-light-deuteranopia)))
    (modus-theme-headings '((1 . (overline background))
			    (2 . (rainbow overline))
			    (t . (no-bold))))
    (modus-themes-variable-pitch-ui . t)
    (modus-themes-variable-pitch-headings . t)
    (modus-themes-scale-headings . t)
    (modus-themes-scale-1 . 1.1)
    (modus-themes-scale-2 . 1.15)
    (modus-themes-scale-3 . 1.21)
    (modus-themes-scale-4 . 1.27)
    (modus-themes-scale-title . 1.33)
    :config
    (require-theme 'modus-themes)
    (modus-themes-load-themes)
    (modus-themes-load-vivendi))
  (leaf minions
    :straight t
    :custom
    (minions-mode-line-lighter . "[+]")
    :global-minor-mode minions-mode)
  (leaf rainbow-delimiters
    :straight t
    :init
    (defun rainbow-delimiters-using-stronger-colors ()
      (interactive)
      (cl-loop
       for index from 1 to rainbow-delimiters-max-face-count
       do
       (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
	 (cl-callf color-saturate-name (face-foreground face) 100))))
    :hook (prog-mode-hook . rainbow-delimiters-using-stronger-colors)
    :config
    (rainbow-delimiters-mode-enable))
  (leaf display-line-numbers
    :custom
    (display-line-numbers-width . 4)
    :global-minor-mode global-display-line-numbers-mode)
  (leaf centaur-tabs
    :straight t
    :custom
    (centaur-tabs-set-modified-marker . t)
    (centaur-tabs-cycle-scope . 'tabs)
    :global-minor-mode centaur-tabs-mode)
  (leaf volatile-highlights
    :straight t
    :global-minor-mode volatile-highlights-mode))

(leaf *view
  :config
  (leaf mwim :straight t)
  (leaf avy :straight t)
  (leaf expand-region :straight t)
  (leaf multiple-cursors :straight t)
  (leaf smooth-scroll
    :straight t
    :global-minor-mode smooth-scroll-mode)
  (leaf projectile
    :straight t))

(leaf *edit
  :custom
  (delete-selection-mode . t)
  :config
  (setq-default indent-tabs-mode nil)
  (leaf delete-selection-mode
    :global-minor-mode delete-selection-mode)
  (leaf electric-pair-mode
    :global-minor-mode electric-pair-mode)
  (leaf vundo :straight t)
  (leaf evil
    :straight t
    :init
    (leaf which-key
      :straight t
      :global-minor-mode which-key)
    (leaf general :straight t)
    :custom
    (evil-move-beyond-eol . t)
    (evil-cross-lines . t)
    (evil-respect-visual-line-mode . t)
    (evil-want-fine-undo . t)
    (evil-normal-state-cursor . 'box)
    (evil-disable-insert-state-bindings . t)
    :global-minor-mode evil-mode
    :config
    (leaf evil-surround
      :straight t
      :global-minor-mode global-evil-surround-mode)))

(leaf *shell
  :config
  (leaf vterm
    :straight t
    :custom
    (vterm-max-scrollback . 10000)
    (vterm-buffer-name-string . "vterm %s")
    :config
    (leaf vterm-toggle
      :straight t
      :custom
      (vterm-toggle-scope . 'project)
      (vterm-toggle-fullscreen-p . nil)
      :config
      (add-to-list 'display-buffer-alist
             '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                (display-buffer-reuse-window display-buffer-in-direction)
                (direction . bottom)
                (dedicated . t)
                (reusable-frames . visible)
                (window-height . 0.3))))))

(leaf *completion
  :config
  (leaf marginalia
    :straight t
    :init (marginalia-mode))
  (leaf vertico
    :straight t
    :custom
    (vertico-preselect . 'prompt)
    :global-minor-mode vertico-mode)
  (leaf orderless
    :straight t
    :custom
    (completion-styles . '(orderless basic))
    (completion-category-defaults . nil)
    (completion-category-overrides . nil))
  (leaf consult :straight t)
  (leaf corfu
    :straight t
    :custom
    (corfu-cycle . t)
    (corfu-auto . t)
    (corfu-auto-delay . 0)
    (corfu-auto-prefix . 3)
    (corfu-on-exact-match . nil)
    (tab-always-indent. 'complete)
    :global-minor-mode global-corfu-mode)
  (leaf kind-icon
    :straight t
    :after corfu
    :custom
    (kind-icon-default-face 'corfu-default)
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  (leaf copilot
    :straight (copilot
	       :type git
	       :host github
	       :repo "copilot-emacs/copilot.el")
    :hook (prog-mode-hook . copilot-mode)
    :custom
    (copilot-indent-offset-warning-disable . t)))

(leaf *programming
  :config
  (leaf eglot
    :hook
    (c-common-mode-hook . eglot-ensure)))

(leaf *keybind
  :init
  (defun my/newline-below ()
    (interactive)
    (end-of-line)
    (newline-and-indent))
  (defun my/newline-above ()
    (interactive)
    (beginning-of-line)
    (newline-and-indent)
    (forward-line -1))
  :config
  (general-define-key
   :states '(normal visual)
   :prefix "SPC"
   "SPC" '(execute-extended-command :which-key "M-x")
   
   "f" '(:ignore t :which-key "File")
   "fs" '(save-buffer :which-key "save buffer")
   "fd" '(kill-buffer :which-key "kill buffer")
   "ff" '(find-file :which-key "open file")
   "fr" '(consult-recent-file :which-key "recent file")
   "fj" '(dired-jump :which-key "dired jump")
   "fR" '(rename-file :which-key "rename file")

   "b" '(:ignore t :which-key "Buffer")
   "bb" '(consult-buffer :which-key "switch buffer")
   "bd" '(kill-buffer :which-key "kill buffer")
   "bs" '(save-buffer :which-key "save buffer")
   
   "h" '(:ignore t :which-key "Help")
   "hk" '(describe-key :which-key "describe key")
   "hf" '(describe-function :which-key "describe functino")

   "p" '(:ignore t :which-key "Project")
   "pp" '(projectile-switch-project :which-key "switch project")
   "pf" '(projectile-find-file :which-key "find file in project")
   "pr" '(projectile-recentf :which-key "recent file in project")

   "q" '(:ignore t :which-key "Quit")
   "qr" '(restart-emacs :which-key "restart emacs")
   "qq" '(evil-quit :which-key "quit")
   
   "s" '(:ignore t :which-key "Search")
   "ss" '(consult-line :which-key "search line")
   "sg" '(consult-ripgrep :which-key "search ripgrep")
   "so" '(consult-outline :which-key "search outline")

   "t" '(:ignore t :which-key "Terminal")
   "tt" '(vterm-toggle :which-key "toggle vterm")

   "w" '(:ignore t :which-key "Window")
   "ww" '(other-window :which-key "other window") 
   "wd" '(delete-window :which-key "delete window")
   )
  

  )

