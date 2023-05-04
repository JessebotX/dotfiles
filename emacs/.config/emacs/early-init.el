(defvar my/file-name-handler-alist file-name-handler-alist)

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
(setq file-name-handler-alist nil)

(defun my/emacs-startup-hook-setup ()
  (setq gc-cons-threshold 16777216)
  (setq gc-cons-percentage 0.1)
  (setq file-name-handler-alist my/file-name-handler-alist))

(add-hook 'emacs-startup-hook #'my/emacs-startup-hook-setup)

(setq vc-follow-symlinks nil)
(setq package-enable-at-startup nil)
(setq site-run-file nil)
(setq inhibit-startup-message t)
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
