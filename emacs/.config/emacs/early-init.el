;;; early-init.el -*- lexical-binding: t -*-

;; Copyright (c) 2025

;; Permission to use, copy, modify, and/or distribute this software
;; for any purpose with or without fee is hereby granted.
;;
;; THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
;; OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
;; OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:
;;
;; Configurations that happen before the startup of Emacs, including
;; basic frame settings and garbage collection/speedup hacks.

;;; Code:

;;; Preface
(defvar my/early-init--gc-cons-threshold (* 16 1024 1024))
(defvar my/early-init--gc-cons-percentage 0.1)
(defvar my/early-init--file-name-handler-alist file-name-handler-alist)
(defvar my/early-init--vc-handled-backends vc-handled-backends)

;;; Startup hacks
;; Temporary configurations at startup to improve initial startup
;; speed. Reset settings afterwards.

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
(setq file-name-handler-alist nil)
(setq vc-handled-backends nil)

(defun my/early-init--hook-emacs-startup ()
  "Clear temporary speedup-startup settings."
  (setq gc-cons-threshold my/early-init--gc-cons-threshold)
  (setq gc-cons-percentage my/early-init--gc-cons-percentage)
  (setq file-name-handler-alist my/early-init--file-name-handler-alist)
  (setq vc-handled-backends my/early-init--vc-handled-backends))

(add-hook 'emacs-startup-hook #'my/early-init--hook-emacs-startup)

;;; General early init Emacs settings

(setq package-enable-at-startup nil) ; use different package manager
(setq vc-follow-symlinks nil)
(setq inhibit-x-resources t)
(setq ring-bell-function 'ignore)
(setq use-dialog-box t)
(setq use-file-dialog nil)

;;; Basic frame/startup buffer settings

(push '(font . "Maple Mono 12") default-frame-alist) ; fallback font for frames

(setq inhibit-startup-screen t)
;(setq initial-scratch-message nil)
(setq inhibit-startup-buffer-menu t)
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)
(setq frame-title-format '("%b"))

;;; Startup echo area message

(setq inhibit-startup-echo-area-message user-login-name)
(defun display-startup-echo-area-message ()
  "HACK: Don't show startup message in echo area by overriding the function"
  (message nil))

;;; User interface

;;;; Disable misc. UI bars
;; I don't use the scroll bar or the menu/tool bars
(setq scroll-bar-mode nil)
(setq menu-bar-mode nil)
(setq tool-bar-mode nil)

(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

;;;; Frame padding
;; Credit: `spacious-padding' package by Protesilaos
(push '(internal-border-width . 24) default-frame-alist)
(push '(right-divider-width   . 30) default-frame-alist)
(push '(scroll-bar-width      . 8)  default-frame-alist)

(defun my/early-init--set-invisible-window-dividers (_theme)
  "Make window dividers for THEME invisible."
  (let ((bg (face-background 'default)))
    (custom-set-faces
     `(fringe ((t :background ,bg)))
     `(olivetti-fringe ((t :background ,bg)))
     `(window-divider ((t :background ,bg :foreground ,bg)))
     `(window-divider-first-pixel ((t :background ,bg :foreground ,bg)))
     `(window-divider-last-pixel ((t :background ,bg :foreground ,bg))))))

(add-hook 'enable-theme-functions #'my/early-init--set-invisible-window-dividers)

;;; Final steps

;; Load post-early-init.el, machine-early-init.el if it exists
;; NOTE: machine-early-init.el should be ignored in version control systems
(load (locate-user-emacs-file "post-early-init.el") :noerror :nomessage)
(load (locate-user-emacs-file "machine-early-init.el") :noerror :nomessage)

;; End of early-init.el
