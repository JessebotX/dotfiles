;;
;; STARTUP
;;
(setq gc-cons-threshold (* 50 1000 1000)) ; make emacs faster
(setq inhibit-startup-message t) ; disable emacs start page

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

(set-fringe-mode 10) ; margins

;;
;; THEMING
;;
(use-package doom-themes
  :init (load-theme 'doom-one t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;;
;; GENERAL EDITING
;;
(delete-selection-mode t) ; delete selected text when typingn

;;
;; IVY AND COUNSEL
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
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
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
;; ORG MODE
;;
(defun user/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(use-package org
  :pin org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;;
;; WHICH KEY
;;
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))
