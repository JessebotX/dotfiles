;;
;; STARTUP
;;
(setq gc-cons-threshold (* 50 1000 1000)) ; make emacs faster
(setq inhibit-startup-message t) ; disable emacs start page

;;
;; STARTUP DASHBOARD
;;
(use-package dashboard
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs")
  (setq dashboard-startup-banner 'logo)  ;; use custom image as banner
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 3)
                          (projects . 3)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))

;; required for emacsclient
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;;
;; AUTO UPDATE PACKAGES
;;
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;;
;; STOP LITTERING
;;
;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;
;; ALL THE ICONS
;;
(use-package all-the-icons)
(use-package emojify
  :hook (after-init . global-emojify-mode))

;;
;; USER INTERFACE
;;

;; disable any audio/visible bells
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; make interface more minimal
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(set-fringe-mode 10); margins

;; improve scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq scroll-margin 5)
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(use-package visual-fill-column)
;; WRITEROOM
(use-package writeroom-mode
  :bind (("C-x t b" . writeroom-mode)))

;; THEMING
(use-package modus-themes)
(use-package doom-themes)

(load-theme 'modus-operandi t)

;; MODELINE
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(display-time-mode 1)

;;
;; GENERAL EDITING
;;
(delete-selection-mode t) ; delete selected text when typingn

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;;
;; KEYBOARD
;;
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x t e") 'eval-buffer)

;;
;; IVY AND COUNSEL MINIBUFFER THINGS
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
  (ivy-mode 1)
  ;; remove ^ prefix so it can start looking anywhere in the string
  (setq ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
	 ("C-x t t" . 'counsel-load-theme)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

;;
;; HELPFUL
;;
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;
;; DIRED
;;
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)
	 :map dired-mode-map
	 ("f" . dired-create-empty-file)
	 ("n" . dired-create-directory))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png"  . "imv")
				("jpg"  . "imv")
				("jpeg" . "imv")
				("webp" . "imv")
				("mp4"  . "mpv")
                                ("mkv"  . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
	      ("." . dired-hide-dotfiles-mode)))

;;
;; LANGUAGE SUPPORT
;;
(defun user/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(defun user/markdown-mode-setup ()
  (visual-line-mode 1))

(use-package markdown-mode
  :hook (markdown-mode . user/markdown-mode-setup))

(use-package org
  :pin org
  :hook (org-mode . user/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell")))

;;
;; ELFEED
;;
(use-package elfeed
  :config
  (setq elfeed-search-feed-face ":foreground #fff :weight bold"
        elfeed-feeds (quote
                       (("https://lukesmith.xyz/rss.xml" blog lukesmith)
	                ("https://videos.lukesmith.xyz/feeds/videos.xml?sort=-publishedAt" videos lukesmith)
	                ("https://archlinux.org/feeds/news/" news linux arch)
                        ("https://www.debian.org/News/news" news linux debian)
                        ("http://suckless.org/atom.xml" news suckless linux)
                        ("https://bugswriter.com/atom.xml" blog bugswriter)))))

;;
;; MAGIT
;;
(use-package magit
  :commands magit-status)

;;
;; PROJECTILE
;;
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-x p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Repos")
    (setq projectile-project-search-path '("~/Repos")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;;
;; TERMINALS
;;
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

;; windows people, just use shell-mode
(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "bash"))

;; note, vterm has dependencies: https://github.com/akermu/emacs-libvterm/#requirements
(use-package vterm
  :commands vterm
  :config
  ;;(setq vterm-shell "sh")
  (setq vterm-max-scrollback 100000))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))


;;
;; WHICH KEY
;;
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))
