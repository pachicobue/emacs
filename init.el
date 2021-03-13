(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

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
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf leaf-tools
  :doc "tools for customizing Emacs with leaf"
  :tag "leaf"
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))


;; ====================== Builtin Config Below ==========================

(leaf *builtin
  :config
  (eval-and-compile
    (leaf bytecomp
      :doc "compilation of Lisp code into byte code"
      :tag "builtin" "lisp"
      :custom (byte-compile-warnings . '(cl-functions))))
  
  (leaf cus-edit
    :doc "tools for customizing Emacs and Lisp packages"
    :tag "builtin" "faces" "help"
    :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

  (leaf cus-start
    :doc "define customization properties of builtins"
    :tag "builtin" "internal"
    :custom ((user-full-name . "Sho Yasui")
             (user-mail-address . "tigerssho@gmail.com")
             (user-login-name . "pachicobue")
             (debug-on-error . nil)
             (init-file-debug . t)
             (frame-resize-pixelwise . t)
             (enable-recursive-minibuffers . t)
             (history-length . 1000)
             (history-delete-duplicates . t)
             (scroll-preserve-screen-position . t)
             (scroll-conservatively . 100)
             (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
             (ring-bell-function . 'ignore)
             (text-quoting-style . 'straight)
             (transient-mark-mode . t)
             (create-lockfiles . nil)
             (make-backup-files . nil)
             (savehist-autosave-interval . 60)
             (auto-save-default . nil)
             (require-final-newfile . t)
             (inhibit-startup-screen . t)
             (inhibit-startup-message . t)
             (inhibit-startup-echo-area-message . t)
             (use-dialog-box . nil)
             (use-file-dialog . nil)
             (tool-bar-mode . nil)
             (menu-bar-mode . nil)
             (indent-tabs-mode . nil))
    :config
    (add-to-list 'default-frame-alist '(cursor-type . bar))
    (defalias 'yes-or-no-p 'y-or-n-p)
    (keyboard-translate ?\C-h ?\C-?)
    (set-face-attribute 'default nil :height 100))

  (leaf undo
    :doc "undo/redo keybinding"
    :tag "builtin"
    :bind (("C-z" . undo)))

  (leaf zoom
    :doc "zoom in/out editor"
    :tag "builtin"
    :hydra (hydra-zoom
            (global-map "C-+")
            "zoom"
            ("+" text-scale-increase "in")
            ("-" text-scale-decrease "out")))  

  (leaf autorevert
    :doc "revert buffers when files on disk change"
    :tag "builtin"
    :added "2020-08-30"
    :custom ((auto-revert-interval . 0.1)
             (auto-revert-check-vc-info . t))
    :global-minor-mode auto-revert-mode)
  
  (leaf paren
    :doc "highlight matching paren"
    :tag "builtin"
    :added "2020-08-30"
    :custom ((show-paren-delay . 0.1))
    :global-minor-mode show-paren-mode)
  
  
  (leaf server
    :doc "Lisp code for GNU Emacs running as server process"
    :tag "builtin"
    :added "2020-08-30"
    :bind (("C-x C-c" . server-edit))
    :global-minor-mode t)
  
  (leaf recentf
    :doc "setup a menu of recently opened files"
    :tag "builtin"
    :added "2020-08-30"
    :custom ((recentf-exclude . '("recentf" "COMMIT_EDITMSG")))
    :global-minor-mode t)
  
  (leaf dired
    :doc "directory-browsing commands"
    :tag "builtin" "files"
    :added "2020-08-30"
    :bind (:dired-mode-map
           ("r" . wdired-change-to-wdired-mode)))  
  
  (leaf save-place-mode
    :doc "automatically save place in files"
    :custom ((save-place-mode . t)))

  (leaf ansi-color
    :doc "translate ANSI escape sequences into faces"
    :tag "builtin"
    :added "2020-08-30"
    :preface
    (defun c/ansi-color-init()
      (ansi-color-apply-on-region (point-min) (point-max)))
    :defun ansi-color-apply-on-region
    :hook ((compilation-filter-hook . c/ansi-color-init)))

  (leaf display-line-numbers
    :doc "interface for display-line-numbers"
    :tag "builtin"
    :added "2020-08-30"
    :global-minor-mode global-display-line-numbers-mode)
  )

;; ====================== Package Config Below ==========================

(leaf *package
  :config
  (leaf iflipb
    :doc "Interactively flip between recently visited buffers"
    :added "2020-08-27"
    :url "https://github.com/jrosdahl/iflipb"
    :ensure t
    :bind (("C-<tab>" . iflipb-next-buffer)
           ("<C-iso-lefttab>" . iflipb-previous-buffer))
    :custom ((iflipb-wrap-around . t)))

  (leaf doom-modeline
    :doc "A minimal and modern mode-line"
    :req "emacs-25.1" "all-the-icons-2.2.0" "shrink-path-0.2.0" "dash-2.11.0"
    :tag "mode-line" "faces" "emacs>=25.1"
    :added "2020-08-29"
    :url "https://github.com/seagle0128/doom-modeline"
    :emacs>= 25.1
    :ensure t
    ;; :after all-the-icons shrink-path
    :custom ((doom-modeline-buffer-file-name-style . 'relative-from-project)
    	     (doom-modeline-icon . t))
    :config
    (line-number-mode nil)
    (column-number-mode nil)
    :global-minor-mode t)

  (leaf doom-themes
    :doc "an opinionated pack of modern color-themes"
    :req "emacs-25.1" "cl-lib-0.5"
    :tag "nova" "faces" "icons" "neotree" "theme" "one" "atom" "blue" "light" "dark" "emacs>=25.1"
    :added "2020-08-30"
    :url "https://github.com/hlissner/emacs-doom-theme"
    :emacs>= 25.1
    :ensure t
    :custom ((doom-theme-enable-italic . t)
             (doom-theme-enable-bold . t))
    :config
    (load-theme 'doom-dark+ t)
    (doom-themes-neotree-config)
    (doom-themes-org-config))
  
  (leaf dashboard
    :doc "A startup screen extracted from Spacemacs"
    :req "emacs-25.3" "page-break-lines-0.11"
    :tag "dashboard" "tools" "screen" "startup" "emacs>=25.3"
    :added "2020-08-27"
    :url "https://github.com/emacs-dashboard/emacs-dashboard"
    :emacs>= 25.3
    :ensure t
    ;; :after page-break-lines
    :hook (after-init-hook . dashboard-setup-startup-hook))

  (leaf undo-tree
    :doc "Treat undo history as a tree"
    :tag "tree" "history" "redo" "undo" "files" "convenience"
    :added "2020-08-27"
    :url "http://www.dr-qubit.org/emacs.php"
    :ensure t
    :blackout t
    :bind (("C-x u" . undo-tree-visualize))
    :global-minor-mode global-undo-tree-mode)

  (leaf which-key
    :doc "Display available keybindings in popup"
    :req "emacs-24.4"
    :tag "emacs>=24.4"
    :added "2020-08-27"
    :url "https://github.com/justbur/emacs-which-key"
    :emacs>= 24.4
    :ensure t
    :global-minor-mode t)

  (leaf smartparens
    :doc "Automatic insertion, wrapping and paredit-like navigation with user defined pairs."
    :req "dash-2.13.0" "cl-lib-0.3"
    :added "2020-08-29"
    :ensure t
    :preface
    (defun sp-init()
      (require 'smartparens-config)
      (smartparens-global-mode t))
    :hook ((after-init-hook . sp-init))
    :config
    (sp-pair "\"" nil :actions '(:rem escape))
    (sp-pair "\'" nil :actions '(:rem escape))
    (sp-with-modes '(lisp-mode emacs-lisp-mode)
                   (sp-local-pair "'" nil :actions))
    (sp-with-modes '(org-mode)
                   (sp-local-pair "+" "+")
                   (sp-local-pair "=" "="))
    (sp-with-modes '(markdown-mode)
                   (sp-local-pair "`" "`"))
    :global-minor-mode t)

  (leaf expand-region
    :doc "Increase selected region by semantic units."
    :added "2020-08-29"
    :ensure t
    :bind (("C-@" . er/expand-region)
           ("C-M-@" . er/contract-region)))

  (leaf anzu
    :doc "Show number of matches in mode-line while searching"
    :req "emacs-24.3"
    :tag "emacs>=24.3"
    :added "2020-08-29"
    :url "https://github.com/emacsorphanage/anzu"
    :emacs>= 24.3
    :ensure t
    :bind (("M-%" . anzu-query-replace)
	   ("C-M-%" . anzu-query-replace-regexp))
    :global-minor-mode global-anzu-mode)
  
  (leaf ivy
    :doc "Incremental Vertical completion"
    :req "emacs-24.5"
    :tag "matching" "emacs>=24.5"
    :added "2020-08-27"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :blackout t
    :ensure t
    :global-minor-mode t)

  (leaf ivy-rich
    :doc "More friendly display transformer for ivy"
    :req "emacs-25.1" "ivy-0.13.0"
    :tag "ivy" "convenience" "emacs>=25.1"
    :added "2020-08-29"
    :url "https://github.com/Yevgnen/ivy-rich"
    :emacs>= 25.1
    :ensure t
    :after ivy
    :global-minor-mode t)
  
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :added "2020-08-27"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    ;; :after ivy
    :bind (("C-s" . swiper))) 

  (leaf all-the-icons
    :doc "A library for inserting Developer icons"
    :req "emacs-24.3" "memoize-1.0.1"
    :tag "lisp" "convenient" "emacs>=24.3"
    :added "2020-08-29"
    :url "https://github.com/domtronn/all-the-icons.el"
    :emacs>= 24.3
    :ensure t)
  
  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :added "2020-08-27"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    ;; :after swiper
    :bind ((("C-c b" . counsel-switch-buffer)
  	    ("C-c g" . counsel-rg)
  	    ("C-c r" . counsel-recentf)
            ("C-S-s" . counsel-imenu)))
    :custom ((counsel-yank-pop-separator . "\n----------\n"))
    :global-minor-mode t)

  (leaf flycheck
    :doc "On-the-fly syntax checking"
    :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
    :tag "tools" "languages" "convenience" "emacs>=24.3"
    :added "2020-08-27"
    :url "http://www.flycheck.org"
    :emacs>= 24.3
    :ensure t)

  (leaf company
    :doc "Modular text completion framework"
    :req "emacs-24.3"
    :tag "matching" "convenience" "abbrev" "emacs>=24.3"
    :added "2020-08-27"
    :url "http://company-mode.github.io/"
    :emacs>= 24.3
    :ensure t
    :blackout t
    :custom ((company-idle-delay . 0)
             (company-echo-delay . 0)
             (company-minimum-prefix-length . 2))
    :bind ((:company-active-map
            ("<tab>" . company-complete-common-or-cycle)))
    :global-minor-mode global-company-mode)

  (leaf company-box
    :doc "Company front-end with icons"
    :req "emacs-26.0.91" "dash-2.13" "dash-functional-1.2.0" "company-0.9.6"
    :tag "convenience" "front-end" "completion" "company" "emacs>=26.0.91"
    :added "2020-08-29"
    :url "https://github.com/sebastiencs/company-box"
    :emacs>= 26.0
    :ensure t
    :after company
    :hook ((company-mode . company-box-mode))
    :custom ((company-box-icons-alist . "company-box-icons-all-the-icons")
             (company-box-doc-enable . t)))

  (leaf yasnippet
    :doc "Yet another snippet extension for Emacs"
    :req "cl-lib-0.5"
    :tag "emulation" "convenience"
    :added "2020-08-29"
    :url "http://github.com/joaotavora/yasnippet"
    :ensure t
    :global-minor-mode yas-global-mode)

  (leaf lsp-mode
    :doc "LSP mode"
    :req "emacs-26.1" "dash-2.14.1" "dash-functional-2.14.1" "f-0.20.0" "ht-2.0" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
    :tag "languages" "emacs>=26.1"
    :added "2020-08-27"
    :url "https://github.com/emacs-lsp/lsp-mode"
    :emacs>= 26.1
    :ensure t
    ;; :after spinner markdown-mode lv
    :preface
    (defun format-and-save nil
      (interactive)
      (lsp-format-buffer)
      (save-buffer))
    :hook ((c-mode-common-hook . lsp))
    :bind (:lsp-mode-map
	   ("<f2>" . lsp-rename)
           ("C-x C-s" . format-and-save))
    :custom ((lsp-prefer-capf . t)
             (lsp-response-timeout . 5)
             (lsp-enable-completion-at-point . t)
             (lsp-enable-indentation . t)))

  (leaf lsp-ui
    :doc "UI modules for lsp-mode"
    :req "emacs-26.1" "dash-2.14" "dash-functional-1.2.0" "lsp-mode-6.0" "markdown-mode-2.3"
    :tag "tools" "languages" "emacs>=26.1"
    :added "2020-08-27"
    :url "https://github.com/emacs-lsp/lsp-ui"
    :emacs>= 26.1
    :ensure t
    :after lsp-mode ;; markdown-mode
    :hook ((lsp-mode-hook . lsp-ui-mode))
    :custom ((lsp-ui-doc-enable . t)
	     (lsp-ui-doc-header . t)
             (lsp-ui-doc-include-signature . t)
    	     (lsp-ui-doc-use-childframe . t)
	     (lsp-ui-doc-use-webkit . t)
             (lsp-ui-flycheck-enable . t)
             (lsp-ui-sideline-enable . t)
             (lsp-ui-imenu-enable . t)
             (lsp-ui-peek-enable . t))
    :bind (:lsp-mode-map
           ("<f5>" . lsp-ui-peek-find-definitions)))

  (leaf ccls
    :doc "ccls client for lsp-mode"
    :req "emacs-25.1" "lsp-mode-6.3.1" "dash-2.14.1"
    :tag "c++" "lsp" "languages" "emacs>=25.1"
    :added "2020-08-27"
    :url "https://github.com/MaskRay/emacs-ccls"
    :emacs>= 25.1
    :ensure t
    ;; :after lsp-mode
    :custom ((ccls-executable . "/usr/bin/ccls")))

  (leaf multiple-cursors
    :doc "Multiple cursors for Emacs."
    :req "cl-lib-0.5"
    :added "2020-08-29"
    :ensure t
    :hydra (hydra-mutiplecursors
            (global-map "M-u")
            "zoom"
            ("u" mc/edit-lines :exit t)
            (">" mc/mark-next-like-this "mark next")
            ("." mc/skip-to-next-like-this "skip to next")
            ("<" c/mark-prev-like-this "mark prev")
            ("," mc/skip-to-prev-like-this "skip to next")))

  (leaf magit
    :doc "A Git porcelain inside Emacs."
    :req "emacs-25.1" "async-20200113" "dash-20200524" "git-commit-20200516" "transient-20200601" "with-editor-20200522"
    :tag "vc" "tools" "git" "emacs>=25.1"
    :added "2020-08-29"
    :emacs>= 25.1
    :ensure t
    :bind (("C-M-g" . magit-status)
           ("<f10>" . hydra-magit/body))
    :hydra ((hydra-magit
             (:hint nil)
             "
_m_agit  _t_imemachine  |  hunk: _p_revious  _n_ext  _s_tage  _r_evert  _SPC_:toggle"
             ("m" magit-status :exit t)
             ("t" git-timemachine :exit t)
             ("p" git-gutter:previous-hunk)
             ("n" git-gutter:next-hunk)
             ("s" git-gutter:stage-hunk)
             ("r" git-gutter:revert-hunk)
             ("SPC" c/git-gutter:toggle-popup-hunk)))
    )
  
  (leaf bury-successful-compilation
    :doc "Bury the *compilation* buffer after successful compilation"
    :tag "compilation"
    :added "2020-08-30"
    :ensure t
    :config
    (bury-successful-compilation 1))
)

;; ====================== Custom Functions Below ==========================
(leaf *custom-functions
  :doc "custom interactive functions"
  :tag "def funcs"
  :after cc-mode
  :config
  (setq algolib-root-path "/home/sho/programming/algolib")
  (setq algolib-source-path (concat algolib-root-path "/src"))
  (setq algolib-script-path (concat algolib-root-path "/scripts"))
  (setq acl-source-path "/home/sho/programming/ac-library")
  (setq oj-executable "/home/sho/.local/bin/oj")
  (setq export-executable (concat algolib-script-path "/expand_by_gcc.py"))
  (setq export-executable2 (concat algolib-script-path "/expand_by_dfs.py"))
  (setq local-macro "HOGEPACHI")

  (defun procon/download-testcase(url)
    (interactive "sURL:")
    (compile (concat
              (format "rm -rf %s_case" (file-name-sans-extension buffer-file-name))
              " && "
              (format "%s dl \"%s\" --directory=\"%s_case\"" oj-executable url (file-name-sans-extension buffer-file-name)))))
  (defun procon/expand()
    (interactive)
    (compile
     (format "python3 %s %s -I %s %s -C"
             export-executable
             buffer-file-name
             algolib-source-path
             acl-source-path)))
  (defun procon/expand2()
    (interactive)
    (compile
     (format "python3 %s %s -I %s %s -C"
             export-executable2
             buffer-file-name
             algolib-source-path
             acl-source-path)))
  (defun procon/test()
    (interactive)
    (compile
     (concat
      (format "g++ -std=c++17 -Wall -Wextra -O0 -g3 -fsanitize=undefined -D_GLIBCXX_DEBUG  -I%s -I%s -D%s %s -o %s.exe"
              algolib-source-path
              acl-source-path
              local-macro
              buffer-file-name
              (file-name-sans-extension buffer-file-name))      
      " && "
      (format "%s test --directory=\"%s_case\" --command=\"%s.exe\""
              oj-executable
              (file-name-sans-extension buffer-file-name)
              (file-name-sans-extension buffer-file-name)))))

  (leaf-keys (:c-mode-base-map
	      ("C-S-t" . procon/test)
	      ("C-S-e" . procon/expand)
	      ("C-S-d" . procon/download-testcase))))

