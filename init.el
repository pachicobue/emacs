;;; init.el --- Emacs config before GUI load -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;
;; Emacs configurations
;;
;;; Code:

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

;;-------------------------------------------------------------------------------

;;======================
;; Before setup package
;;======================
(leaf BeforePackage
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (leaf user
    :custom
    (user-full-name . "pachicobue")
    (user-mail-address . "tigerssho@gmail.com")
    (user-login-name . "pachicobue"))
  (leaf encode
    :custom
    (set-language-environment . "japanese")
    (prefer-coding-system quote utf-8))
  (leaf history
    :custom
    (history-length . 1000)
    (history-delete-duplicates . t))
  (leaf autofile
    :custom
    (create-lockfiles . nil)
    (make-backup-files . nil)
    (auto-save-default . nil))
  (leaf apperance
    :custom
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

;;================
;; Setup packages
;;================
(defun my/load-leaf-db ()
  (interactive)
  (load-file my/leaf-db-file))
(my/load-leaf-db)

;;=====================
;; After setup package
;;=====================
(leaf AfterPackage
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
    :init
    (defun my/copilot-tab ()
      (interactive)
      (or (copilot-accept-completion)
          (indent-for-tab-command)))
    :config
    (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
    (general-imap
      "C-S-k" 'kill-whole-line
      "C-e" 'mwim-end-of-code-or-line
      "C-a" 'mwim-beginning-of-code-or-line
      "<tab>" 'my/copilot-tab
      "TAB" 'my/copilot-tab)
    (general-imap
      :keymaps 'vterm-mode-map
      "TAB" 'vterm-send-tab
      "<tab>" 'vterm-send-tab))
  
  (leaf global-keymap
    :config 
    (general-define-key
     "M-x" 'execute-extended-command
     "M-t" 'evil-goto-definition
     "M-a" 'embark-act
     "M-e" 'embark-dwim
     "C-x C-c" 'server-edit
     "C-<tab>" 'centaur-tabs-forward
     "C-TAB" 'centaur-tabs-forward
     "C-S-<tab>" 'centaur-tabs-backward
     "C-S-TAB" 'centaur-tabs-backward)
    (general-define-key
     :keymaps 'corfu-mode-map
     "C-j" 'corfu-next
     "C-k" 'corfu-previous)
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
     :keymaps 'vertico-map
     "C-j" 'vertico-next
     "C-k" 'vertico-previous)))
