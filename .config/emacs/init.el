;;
;; EMACS CONFIG by usernamesystems
;;
;; CREDITS/SEE ALSO:
;; https://github.com/daviwil/emacs-from-scratch; Emacs from scratch by daviwil
;;
(setq inhibit-startup-message t) ; Stop displaying stupid emacs startpage

(set-default-coding-systems 'utf-8)
(menu-bar-mode -1)               ; Disable menubar
(scroll-bar-mode -1)             ; Disable scrollbar
(tool-bar-mode -1)               ; Disable toolbar 
(tooltip-mode -1)                ; Disable tooltips
(set-fringe-mode 10)             ; margins 
(global-visual-line-mode)
;; display line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

(setq scroll-conservatively 101) ;; value greater than 100 gets rid of half page jumping
(setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; how many lines at a time
(setq mouse-wheel-progressive-speed t) ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-margin 5) ;; scroll off 5

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		treemacs-mode-hook
		shell-mode-hook
		markdown-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(delete-selection-mode t)
;; make ESC quit prompts like C-g
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;
;; FAST STARTUP
;;

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;;
;; PACKAGE MANAGEMENT
;;
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(package-selected-packages
   '(writeroom-mode visual-fill-column gdscript-mode c-mode company-box company lsp-ivy lsp-treemacs lsp-ui lsp-mode markdown-mode dired-hide-dotfiles dired-open all-the-icons-dired dired-single dashboard counsel-projectile projectile emojify org-bullets evil-nerd-commenter evil-magit magit hydra evil-collection evil general helpful doom-themes counsel ivy-rich which-key rainbow-delimiters doom-modeline ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; AUTO UPDATE PACKAGES
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;;
;; ALL THE ICONS
;;
;; M-x all-the-icons-install-fonts to install all fonts
(use-package all-the-icons)

;;
;; EMOJIS
;;
(use-package emojify
  :hook (after-init . global-emojify-mode))

;;
;; DASHBOARD
;;
(use-package dashboard
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;;(setq dashboard-startup-banner "~/.config/emacs/dashboard.png")
  (setq dashboard-center-content nil)
  (setq dashboard-items '((recents . 3)
			  (bookmarks . 5)
			  (projects . 5)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
				    (bookmarks . "book"))))

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;;
;; IVY + COUNSEL
;;
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;;
;; SMEX (M-x command history)
;;
(use-package smex)
(smex-initialize)

;;
;; DOOM MODELINE
;; 
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(setq display-time-format "%a %b %d, %-l:%M"
      display-time-default-load-average nil)
(display-time-mode)

;;
;; DOOM THEMES
;;
(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;
;; RAINBOW DELIMITERS
;;
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;
;; WHICH KEY
;;
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;;
;; HELPFUL
;;
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package general
  :config
  (general-create-definer user/leader-keys
			  :keymaps '(normal insert visual emacs)
			  :prefix "SPC"
			  :global-prefix "C-SPC")
  (user/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

;;
;; EVIL MODE (VI/M EMULATION)
;;
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))
(use-package hydra)

;;
;; HYDRA
;;
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(user/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;;
;; PROJECTILE
;;
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Repos")
    (setq projectile-project-search-path '("~/Repos")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;;
;; MAGIT
;;
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;
;; ORG MODE
;;

;; org bullets 
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;;
;; DIRED
;;
;;    n / j - next line
;;    p / k - previous line
;;    j / J - jump to file in buffer
;;    RET - select file or directory
;;    ^ - go to parent directory
;;    S-RET / g O - Open file in “other” window
;;    M-RET - Show file in other window without focusing (previewing files)
;;    g o (dired-view-file) - Open file but in a “preview” mode, close with q
;;    g / g r Refresh the buffer with revert-buffer after changing configuration (and after filesystem changes!)
;;
;;    m - Marks a file
;;    u - Unmarks a file
;;    U - Unmarks all files in buffer
;;    * t / t - Inverts marked files in buffer
;;    % m - Mark files in buffer using regular expression
;;    * - Lots of other auto-marking functions
;;    k / K - “Kill” marked items (refresh buffer with g / g r to get them back)
;;    Many operations can be done on a single file if there are no active marks!
;;
;;    C - Copy marked files (or if no files are marked, the current file)
;;    Copying single and multiple files
;;    U - Unmark all files in buffer
;;    R - Rename marked files, renaming multiple is a move!
;;    % R - Rename based on regular expression: ^test , old-\&
;;Power command:
;;    C-x C-q (dired-toggle-read-only) - Makes all file
;;      names in the buffer editable directly to rename them! Press Z Z to
;;      confirm renaming or Z Q to abort.
;;
;;    D - Delete marked file
;;    d - Mark file for deletion
;;    x - Execute deletion for marks
;;    delete-by-moving-to-trash - Move to trash instead of deleting permanently
;;
;;    Z - Compress or uncompress a file or folder to (.tar.gz)
;;    c - Compress selection to a specific file
;;    dired-compress-files-alist - Bind compression commands to file extension
;;
;;    T - Touch (change timestamp)
;;    M - Change file mode
;;    O - Change file owner
;;    G - Change file group
;;    S - Create a symbolic link to this file
;;    L - Load an Emacs Lisp file into Emacs

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer
    "f" 'dired-create-empty-file
    "n" 'dired-create-directory))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;;
;; LSP
;;
(defun user/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symboles))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . user/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :after lsp)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package gdscript-mode
  :hook (gdscript-mode . lsp-deferred))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

;;
;; LANGUAGE SUPPORT
;;

(use-package markdown-mode)

;;
;; WRITEROOM
;;
(use-package visual-fill-column)
(use-package writeroom-mode)

(user/leader-keys
  "tb" '(writeroom-mode :which-key "writeroom"))
