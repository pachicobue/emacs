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
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

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

  (leaf paren
    :doc "highlight matching paren"
    :tag "builtin"
    :added "2020-08-30"
    :custom ((show-paren-delay . 0.1))
    :global-minor-mode show-paren-mode)
  
  (leaf recentf
    :doc "setup a menu of recently opened files"
    :tag "builtin"
    :added "2020-08-30"
    :custom ((recentf-exclude . '("recentf" "COMMIT_EDITMSG")))
    :global-minor-mode t)
  
  (leaf display-line-numbers
    :doc "interface for display-line-numbers"
    :tag "builtin"
    :added "2020-08-30"
    :global-minor-mode global-display-line-numbers-mode))
