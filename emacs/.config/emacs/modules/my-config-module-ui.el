;;; my-config-module-ui.el -*- lexical-binding: t; -*-

;; Copyright (c) 2025

;;; Commentary:
;;
;; Themes, fonts, and other UI packages

;;; Code:

(defcustom my/ui-font-size-default 14
  "The default font size.

NOTE: value must be scaled for Emacs font height fields.")

;; Icons from nerd fonts
(use-package nerd-icons
  :ensure t)

;; Manage multiple Emacs fonts
(when (display-graphic-p)
  (defvar my/font-size--scaled-default (* my/ui-font-size-default 10))

  (use-package fontaine
    :ensure t
    :init
    (defun my/hook--fontaine-set-preset ()
      (set-fontset-font
       t
       (if (version< emacs-version "28.1")
           '(#x1f300 . #x1fad0)
         'emoji)
       (cond
        ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
        ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
        ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
        ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
        ((member "Symbola" (font-family-list)) "Symbola")))
      ;; Cascadia Code symbols: ●
      (set-fontset-font t '(#x25e6 . #x25e6) "Cascadia Code")
      (set-fontset-font t '(#x25ce . #x25ce) "Cascadia Code")
      (set-fontset-font t '(#x25cf . #x25cf) "Cascadia Code")
      (set-fontset-font t '(#x25c9 . #x25c9) "Cascadia Code")
      (set-fontset-font t '(#x25cb . #x25cb) "Cascadia Code")
      (set-fontset-font t '(#x25cf . #x25cf) "Cascadia Code"))
    (add-hook 'fontaine-set-preset-hook #'my/hook--fontaine-set-preset)
    (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
    :custom
    (fontaine-presets
     `((regular)
       (cascadia-code
        :default-family "Cascadia Code"
        :fixed-pitch-family "Cascadia Code")
       (tengwar-cursive-variable-pitch
        :variable-pitch-family "Tengwar Cursive")
       (julia-mono
        :default-family "JuliaMono")
       (jetbrains-mono
        :default-family "JetBrains Mono")
       (ubuntu-mono
        :default-family "Ubuntu Mono"
        :default-height ,(+ my/font-size--scaled-default 20)
        :variable-pitch-family "Ubuntu")
       (writing
        :default-family "iMWritingMono Nerd Font"
        :variable-pitch-family "iMWritingMono Nerd Font")
       (t
        :default-family "Maple Mono"
        :default-weight regular
        :default-height ,my/font-size--scaled-default
        :fixed-pitch-family "JetBrains Mono"
        :fixed-pitch-weight regular
        :variable-pitch-height ,(+ my/font-size--scaled-default 30)
        :variable-pitch-family "Inter")))
    :config
    (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)))

;;; Themes

(use-package adwaita-dark-theme
  :ensure t
  :defer t
  :custom
  (adwaita-dark-theme-pad-mode-line t))

;; NOTE: no defer
(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-to-toggle '(modus-operandi modus-vivendi))
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-common-palette-overrides
   '((fg-line-number-inactive "gray50")
     (fg-line-number-active red-cooler)
     (bg-line-number-inactive unspecified)
     (bg-line-number-active unspecified)
     (border-mode-line-active bg-dim)
     (bg-mode-line-active bg)
     (bg-mode-line-inactive bg))))

;; NOTE: no defer
(use-package lambda-themes
  :ensure (:type git :host github :repo "lambda-emacs/lambda-themes")
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t))

(use-package standard-themes
  :ensure t
  :defer t)

;;; Mode line configuration
(setopt display-time-default-load-average nil)

(display-time-mode 1)
(column-number-mode)

;; only display battery when using portable battery
(use-package battery
  :config
  (when
      (and battery-status-function
           (not (string-match-p "N/A"(battery-format "%B" (funcall battery-status-function)))))
    (display-battery-mode 1)))

(use-package doom-modeline
  :ensure t
  :hook (emacs-startup . doom-modeline-mode)
  :custom
  (doom-modeline-height 35)
  (doom-modeline-enable-word-count t))

;;; End
(provide 'my-config-module-ui)
