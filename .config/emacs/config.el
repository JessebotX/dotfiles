;; +------------------------+
;; | MY EMACS CONFIGURATION |
;; +------------------------+
;; This file is loaded in init.el.
;; -------------------------------

;; This loads in some helper functions i/somebody else wrote
(load "~/.config/emacs/helper")

;; -------------------
;; QUICK CONFIGURATION
;; -------------------
(defvar user/transparency '(100 . 100))
(defvar user/default-font-size 130)
(defvar user/windows-font-size 100)
(defvar user/mac-font-size 140)

;; ------------
;; PACKAGE LIST
;; ------------
(defvar user/pkg-list
  '(all-the-icons
    auto-package-update
    company
    company-box
    consult
    dashboard
    dired-single
    doom-modeline
    doom-themes
    elfeed
    elpher
    evil
    evil-collection
    lsp-mode
    lsp-ui
    magit
    markdown-mode
    modus-themes
    no-littering
    orderless
    org
    projectile
    rainbow-delimiters
    savehist
    vertico
    visual-fill-column
    which-key)
  "List of packages to be installed")

;; run package installation
(user/install-pkgs user/pkg-list)

(require 'auto-package-update)

(with-eval-after-load 'auto-package-update
  (setq auto-package-update-interval 7)
  (setq auto-package-update-prompt-before-update t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; --------------------
;; EMACS CONFIG STARTUP
;; --------------------
;; disable ugly startpage
(setq inhibit-startup-message t)

;; modify garbage collection to possibly make emacs faster
(setq gc-cons-threshold (* 50 1000 1000))

;; cleanup shit with no-littering package and move autosave files somewhere else to stop emacs pollution
;; - note: make sure user-emacs-directory variable is set correctly (see my init.el)
(require 'no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; replace ugly startpage with dashboard
(require 'dashboard)
(with-eval-after-load 'dashboard
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs")
  (setq dashboard-startup-banner 'logo)  ;; use custom image as banner
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects . 5))))

(dashboard-setup-startup-hook)
(dashboard-modify-heading-icons '((recents . "file-text")
                                  (bookmarks . "book")))

;; --------------
;; USER INTERFACE
;; --------------
;; disable visible/audible bells
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; make interface more minimal
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10) ; add some margins

;; frame
;; transparency
(set-frame-parameter (selected-frame) 'alpha user/transparency)
(add-to-list 'default-frame-alist `(alpha . ,user/transparency))
;; maximize by default
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; fonts
;; Set the font face based on os
(pcase system-type
  ((or 'windows-nt 'cygwin)
   (set-face-attribute
    'default nil
    :font "JetBrainsMono NF"
    :height user/windows-font-size))
  ('gnu/linux
   (set-face-attribute
    'default nil
    :font "JetBrainsMono Nerd Font"
    :height user/default-font-size))
  ('darwin
   (set-face-attribute
    'default nil
    :font "JetBrainsMono Nerd Font"
    :height user/mac-font-size)))

;; scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
(setq scroll-margin 5)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;; theming
(load-theme 'doom-gruvbox t)

;; modeline
(require 'doom-modeline)
(with-eval-after-load 'doom-modeline
  (doom-modeline-mode 1))

(setq doom-modeline-height 20)
(display-time-mode 1)

;; --------------------
;; COMPLETION FRAMEWORK
;; --------------------
;; Currently, i'm using vertico
(require 'vertico)
(with-eval-after-load 'vertico
  (vertico-mode 1)
  (setq vertico-scroll-margin 3)
  (setq vertico-cycle t))

;; vertico sort by history
(require 'savehist)
(with-eval-after-load 'savehist
  (savehist-mode 1))

;; better completion with vertico
(require 'orderless)
(setq completion-styles '(orderless))

;; provide good commands for vertico. See KEYBOARD section
(require 'consult)

;; ---------------
;; GENERAL EDITING
;; ---------------
(delete-selection-mode t)

;; setting tab width function
(defun user/set-tab-width (width)
  (setq tab-width width)
  ;; for EVIL mode
  (setq-default evil-shift-width width))

;; default tab width
(setq-default tab-width 8)
(user/set-tab-width 8)

(setq backward-delete-char-untabify-method 'hungry)
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode) ; Enable whitespace mode everywhere
; END TABS CONFIG

;; ---------
;; LANGUAGES
;; ---------
;; for writing in general
(defun user/writeroom-mode-visual-fill ()
  "Centers text and restricts max column width like a writeroom
mode using the visual-fill-column package"
  (setq visual-fill-column-width 80)
  (setq visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; for programming in general
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; html/css
(defun user/web-stuff-modes-setup ()
  (setq indent-tabs-mode nil)
  (user/set-tab-width 2))

(add-hook 'html-mode-hook #'user/web-stuff-modes-setup)
(add-hook 'mhtml-mode-hook #'user/web-stuff-modes-setup)
(add-hook 'sgml-mode-hook #'user/web-stuff-modes-setup)
(add-hook 'css-mode-hook #'user/web-stuff-modes-setup)

;; markdown
(defun user/markdown-mode-setup ()
  (dolist (face '((markdown-header-face-1 . 1.9)
		  (markdown-header-face-2 . 1.5)
		  (markdown-header-face-3 . 1.2)
		  (markdown-header-face-4 . 1.1)
		  (markdown-header-face-5 . 1.0)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))
  (user/writeroom-mode-visual-fill)
  (visual-line-mode 1)
  (setq indent-tabs-mode nil)
  (set-tab-width 2))

(add-hook 'markdown-mode-hook #'user/markdown-mode-setup)

;; org
(defun user/org-mode-setup ()
  (dolist (face '((org-level-1 . 1.9)
		  (org-level-2 . 1.5)
		  (org-level-3 . 1.25)
		  (org-level-4 . 1.1)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.0)
		  (org-level-7 . 1.0)
		  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))
  (user/writeroom-mode-visual-fill)
  (visual-line-mode 1))

(add-hook 'org-mode-hook #'user/org-mode-setup)
(setq org-ellipsis " ▾")

(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell")))

;; ----------------
;; LANGUAGE SERVERS
;; ----------------
(defun user/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  (lsp-ui-mode t))

(defun user/company-mode-setup ()
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key lsp-mode-map (kbd "<tab>") 'company-indent-or-complete-common)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0))

(require 'lsp-mode)
(add-hook 'lsp-mode-hook #'user/lsp-mode-setup)
(with-eval-after-load 'lsp-mode
  (setq lsp-keymap-prefix "M-s l"))
(setq lsp-enable-which-key-integration t)

(setq lsp-ui-doc-position 'bottom)

;; enable lsp-mode on certain languages
(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)

(add-hook 'lsp-mode-hook #'company-mode)

(add-hook 'company-mode-hook #'user/company-mode-setup)
(add-hook 'company-mode-hook #'company-box-mode)

;; -----
;; MAGIT
;; -----
(unless (fboundp 'magit-status)
  (autoload #'magit-status "magit" nil t))

;; ------
;; ELFEED
;; ------
(setq elfeed-search-feed-face ":weight bold"
      elfeed-feeds (quote
		     (("https://lukesmith.xyz/rss.xml" blog lukesmith)
                      ("https://videos.lukesmith.xyz/feeds/videos.xml?sort=-publishedAt" videos lukesmith)
                      ("https://archlinux.org/feeds/news/" news linux arch)
                      ("https://www.debian.org/News/news" news linux debian)
                      ("http://suckless.org/atom.xml" news suckless linux)
                      ("https://bugswriter.com/atom.xml" blog bugswriter))))

;; ----------
;; PROJECTILE
;; ----------
(with-eval-after-load 'projectile
  (when (file-directory-p "~/Repos")
    (setq projectile-project-search-path '("~/Repos")))
  (setq projectile-switch-project-action #'projectile-dired))


;; ---------
;; WHICH KEY
;; ---------
(run-with-idle-timer 0 nil #'require 'which-key nil t)
(with-eval-after-load 'which-key
  (which-key-mode)
  (setq which-key-idle-delay 0.2))

;; --------
;; KEYBOARD
;; --------
;; EVIL vi/m keybindings
(setq evil-want-keybinding nil)
(with-eval-after-load 'evil
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil))
(evil-mode 1)
(evil-collection-init)
;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)
(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)
(evil-collection-define-key 'normal 'dired-mode-map
		"h" 'dired-single-up-directory
		"l" 'dired-single-buffer)

(with-eval-after-load 'vertico
  (define-key minibuffer-local-map (kbd "C-j") 'next-line)
  (define-key minibuffer-local-map (kbd "C-k") 'previous-line))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-s t t") 'consult-theme)
(global-set-key (kbd "M-s l") 'elfeed)
(global-set-key (kbd "M-s t e") 'eval-buffer)
(global-set-key (kbd "M-s j") 'text-scale-decrease)
(global-set-key (kbd "M-s k") 'text-scale-increase)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "M-s b") 'consult-buffer)
(global-set-key (kbd "M-s g") 'magit)
;; dired keys
(global-set-key (kbd "M-s d") 'dired)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "n") 'dired-create-directory)
  (define-key dired-mode-map (kbd "f") 'dired-create-empty-file)
	(define-key dired-mode-map (kbd "h") 'dired-single-up-directory))
