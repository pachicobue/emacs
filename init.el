;;; init.el --- Emacs config before GUI load -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;
;; Emacs configurations
;;
;;; Code:

;; leaf.el
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf)))
(leaf leaf-keywords
:ensure t
    :config
    (leaf el-get :ensure t
      :custom
      (el-get-git-shallow-clone . t))
    (leaf-keywords-init))

(leaf cus-edit
  :custom
  `(custom-file . null-device))

(leaf cus-start
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (leaf user
    :custom
    (user-full-name . "pachicobue")
    (user-mail-address . "tigerssho@gmail.com")
    (user-login-name . "pachicobue"))
  (leaf encoding
    (set-language-environment . "japanese")
    (prefer-coding-system . 'utf-8))
  (leaf history
    (history-length . 1000)
    (histroy-delete-duplicates . t))
  (leaf autofile
    (create-lockfiles . nil)
    (make-backup-files . nil)
    (auto-save-default . nil))
  (leaf frame
    (scroll-bar-mode . nil)
    (menu-bar-mode . nil)
    (tool-bar-mode . nil)
    (inhibit-startup-screen . t)
    (inhibit-startup-message . t)
    (inhibit-startup-echo-area-message . t))
  (leaf edit
    :custom
    (indent-tabs-mode . nil)
    (delete-selection-mode . t)))

;; パッケージ設定

(leaf whitespace
  :custom ((show-trailing-whitespace . t)))

(leaf autorevert
  :custom ((auto-revert-interval . 1)
           (auto-revert-check-vc-info . t))
  :global-minor-mode global-auto-revert-mode)

(leaf avy :ensure t)

(leaf centaur-tabs
  :ensure t
  :init
  (defun my/centaur-tabs-buffer-groups-function nil
    (list
     (cond
      ((string-equal "*"
                     (substring
                      (buffer-name)
                      0 1))
       "Emacs")
      ((derived-mode-p 'vterm-mode)
       "Terminal")
      ((derived-mode-p 'dired-mode)
       "Directory")
      (t "Editing"))))

  :custom ((centaur-tabs-style . "alternate")
           (centaur-tabs-height . 30)
           (centaur-tabs-set-icons . t)
           (centaur-tabs-set-bar quote under)
           (x-underline-at-descent-line . t)
           (centaur-tabs-set-modified-marker . t)
           (centaur-tabs-cycle-scope quote tabs)
           (centaur-tabs-buffer-groups-function function my/centaur-tabs-buffer-groups-function))
  :global-minor-mode t)

(leaf consult :ensure t)

;; (leaf copilot
;;   :disabled
;;   :require t
;;   :init
;;   (defun my/copilot-tab nil
;;     (interactive)
;;     (or
;;      (copilot-accept-completion)
;;      (indent-for-tab-command)))
;;   :ensure (copilot :type git :host github :repo "copilot-emacs/copilot.el")
;;   :hook (prog-mode-hook . copilot-mode)
;;   :custom (copilot-indent-offset-warning-disable . t))

(leaf display-line-numbers
  :custom (display-line-numbers-width . 4)
  :global-minor-mode global-display-line-numbers-mode)

(leaf evil
  :ensure t
  :custom ((evil-cross-lines . t)
           (evil-respect-visual-line-mode . t)
           (evil-want-fine-undo . t)
           (evil-normal-state-cursor quote box)
           (evil-motion-state-cursor quote box)
           (evil-disable-insert-state-bindings . t))
  :global-minor-mode t)

(leaf evil-goggles
  :after evil
  :ensure t
  :config
  (evil-goggles-use-diff-faces)
  :global-minor-mode t)

(leaf evil-numbers
  :after evil
  :ensure t)

(leaf evil-surround
  :after evil
  :ensure t
  :global-minor-mode global-evil-surround-mode)

(leaf exec-path-from-shell
  :ensure t
  :custom ((exec-path-from-shell-arguments)
           (exec-path-from-shell-warn-duration-millis . 2000))
  :config
  (exec-path-from-shell-copy-envs
   '("PATH" "CXX_COMPILER" "CXX_COMMON_OPTIONS" "CXX_DEBUG_OPTIONS" "CXX_RELEASE_OPTIONS"))
  :require t)

(leaf fontaine
  :ensure t
  :custom (fontaine-presets quote
                            ((regular :default-familly "Source Code Pro" :default-height 120 :default-weight regular)))
  :config
  (fontaine-set-preset 'regular)
  :global-minor-mode t)

(leaf general
  :require t
  :ensure t
  :config
  (general-evil-setup))

(leaf key-chord
  :require t
  :ensure t
  :custom (key-chord-two-keys-delay . 0.2)
  :global-minor-mode t)

;; (leaf lsp-bridge
;;   :disabled
;;   :ensure (lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge" :files
;;                       (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;                       :build
;;                       (:not compile))
;;   :after yasnippet
;;   :init
;;   (global-lsp-bridge-mode)
;;   :custom ((lsp-bridge-python-multi-lsp-server . "pyright_ruff")
;;            (lsp-bridge-enable-completion-in-string . t)
;;            (lsp-bridge-enable-inlay-hint . t)
;;            (lsp-bridge-enable-org-babel . t)
;;            (acm-enable-icon . t)
;;            (acm-enable-copilot)
;;            (acm-enable-preview . t)
;;            (acm-enable-yas . t)
;;            (acm-backend-search-file-words)
;;            (acm-backend-lsp-enable-auto-import)))

(leaf marginalia
  :ensure t
  :global-minor-mode t)

(leaf markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :custom ((markdown-command quote
                             ("pandoc" "--from=markdown" "--to=html39"))
           (markdown-fontify-code-blocks-natively . t)
           (markdown-enable-math . t)
           (markdown-header-scaling . t)
           (markdown-indent-on-enter quote indent-and-new-item)))

(leaf minions
  :require t
  :ensure t
  :custom (minions-mode-line-lighter . "[+]")
  :global-minor-mode minions-mode)

(leaf modus-themes
  :ensure t
  :commands modus-themes-load-theme
  :custom ((modus-themes-custom-auto-reload . t)
           (modus-themes-disable-other-themes . t))
  :init
  (modus-themes-load-theme 'modus-vivendi-tinted))

(leaf mwim
  :require t
  :ensure t)

(leaf nerd-icons
  :require t
  :ensure t
  :custom ((centaur-tabs-icon-type quote nerd-icons)))

(leaf orderless
  :ensure t
  :custom ((completion-styles quote
                              (orderless basic))
           (completion-category-defaults)
           (completion-category-overrides)))

(leaf recentf
  :custom (recentf-max-saved-items . 1000) (recentf-auto-cleanup quote never)
  :global-minor-mode t)

(leaf saveplace
  :global-minor-mode save-place-mode)

(leaf server
  :commands (server-running-p)
  :unless (server-running-p)
  :config
  (server-start))

(leaf smooth-scroll
  :require t
  :ensure t
  :custom (smooth-scroll/vscroll-step-size . 10)
  :global-minor-mode smooth-scroll-mode)

(leaf treesit :custom ((treesit-font-lock-level . 4)))

(leaf treesit-auto
  :ensure t
  :custom ((treesit-auto-install quote prompt)
           (treesit-auto-add-to-auto-mode-alist quote all))
  :global-minor-mode global-treesit-auto-mode)

(leaf vertico
  :ensure t
  :custom (vertico-preselect quote prompt)
  :global-minor-mode t)

(leaf vundo :ensure t)

(leaf which-key
  :ensure t
  :global-minor-mode t)

(leaf yasnippet
  :ensure t
  :require t
  :global-minor-mode yas-global-mode)

(leaf keybindings
  :config
  (leaf SPACE-keymap
    :init
    (defun my/open-init-el ()
      (interactive)
      (find-file my/init-file))
    (defun my/open-scratch ()
      (interactive)
      (switch-to-buffer "*scratch*"))
    :config
    (general-unbind 'motion "SPC")
    (general-mmap
      :prefix "SPC"
      "SPC" '(execute-extended-command :which-key "M-x")
      "f" '(:ignore t :which-key "File")
      "ff" '(find-file :which-key "open file")
      "fs" '(save-buffer :which-key "save buffer")
      "fd" '(kill-this-buffer :which-key "kill buffer")
      "fr" '(consult-recent-file :which-key "recent file")
      "fj" '(dired-jump :which-key "dired jump")
      "fR" '(rename-file :which-key "rename file")
      "b" '(:ignore t :which-key "Buffer")
      "bb" '(consult-buffer :which-key "switch buffer")
      "bs" '(save-buffer :which-key "save buffer")
      "bd" '(kill-this-buffer :which-key "kill buffer")
      "h" '(:ignore t :which-key "Help")
      "hk" '(describe-key :which-key "describe key")
      "hf" '(describe-function :which-key "describe functino")
      "hv" '(describe-variable :which-key "describe variable")
      "hm" '(describe-keymap :which-key "describe map")
      "p" '(:ignore t :which-key "Project")
      "pp" '(project-switch-project :which-key "switch project")
      "pf" '(project-find-file :which-key "find file in project")
      "q" '(:ignore t :which-key "Quit")
      "qr" '(restart-emacs :which-key "restart emacs")
      "qq" '(evil-quit :which-key "quit")
      "s" '(:ignore t :which-key "Search")
      "ss" '(consult-line :which-key "search line")
      "sg" '(consult-ripgrep :which-key "search ripgrep")
      "so" '(consult-outline :which-key "search outline")
      "t" '(:ignore t :which-key "Toggle window")
      "tt" '(vterm-toggle :which-key "toggle vterm")
      "td" '(treemacs :which-key "toggle treemacs")
      "w" '(:ignore t :which-key "Window")
      "wo" '(delete-other-windows :which-key "delete other windows")
      "ww" '(other-window :which-key "other window")
      "wd" '(delete-window :which-key "delete window")
      "o" '(:ignore t :which-key "Open File")
      "oi" '(my/open-init-el :which-key "open init.el")
      "ol" '(leaf-manager :which-key "open leaf-manager")
      "os" '(my/open-scratch :which-key "open scratch")
      "+" '(text-scale-adjust :which-key "text zoom-in")
      "=" '(text-scale-adjust :which-ey "text zoom-in")
      "-" '(text-scale-adjust :which-key "text zoom-out")
      "0" '(text-scale-adjust :which-key "text zoom-reset")))
  (leaf motion-keymap
    :config
    (general-mmap
      "f" 'avy-goto-word-0
      ";" 'evil-ex
      "j" 'evil-next-visual-line
      "k" 'evil-previous-visual-line
      "H" 'mwim-beginning-of-code-or-line
      "L" 'mwim-end-of-code-or-line
      "U" 'vundo
      "Y" (general-simulate-key "y$")
      "gt" 'centaur-tabs-forward
      "gT" 'centaur-tabs-backward))

  (leaf normal-keymap
    :config
    (general-nmap
      "+" 'evil-numbers/inc-at-pt-incremental
      "-" 'evil-numbers/dec-at-pt-incremental))

  (leaf visual-keymap
    :config
    (general-vmap
      "+" 'evil-numbers/inc-at-pt-incremental
      "-" 'evil-numbers/dec-at-pt-incremental))

  (leaf operator-keymap
    :config
    (general-omap
      "." 'evil-avy-goto-word-or-subword-0
      "l" 'evil-avy-goto-line))

  (leaf insert-keymap
    :config
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
    (general-imap
      "C-S-k" 'kill-whole-line
      "C-e" 'mwim-end-of-code-or-line
      "C-a" 'mwim-beginning-of-code-or-line
      "C-<tab>" 'my/copilot-tab
      "C-TAB" 'my/copilot-tab)
    (general-imap
      :keymaps 'vterm-mode-map
      "TAB" 'vterm-send-tab
      "<tab>" 'vterm-send-tab))

  (leaf global-keymap
    :config
    (general-define-key
     "M-x" 'execute-extended-command
     "C-x C-c" 'server-edit)
    (general-define-key
     :keymaps 'lsp-bridge-mode-map
     "M-t" 'lsp-bridge-find-def
     "M-h" 'lsp-bridge-find-def-return
     "<f2>" 'lsp-bridge-rename
     "M-r" 'lsp-bridge-rename)
    (general-define-key
     :keymaps 'dired-mode-map
     "SPC" nil)
    (general-define-key
     :keymaps 'vundo-mode-map
     "h" 'vundo-backward
     "j" 'vundo-next
     "k" 'vundo-previous
     "l" 'vundo-forward)
    (general-define-key
     :keymaps 'acm-mode-map
     "C-j" 'acm-select-next
     "C-k" 'acm-select-prev
     "TAB" 'acm-select-next
     "<tab>" 'acm-select-next
     "S-TAB" 'acm-select-prev
     "<backtab>" 'acm-select-prev
     "<iso-lefttab>" 'acm-select-prev)
    (general-define-key
     :keymaps 'vertico-map
     "C-j" 'vertico-next
     "C-k" 'vertico-previous)))
