;;; my-config-module-apps.el -*- lexical-binding: t; -*-
;; Copyright (c) 2025.

;;; Commentary:
;; Other useful emacs packages

;;; Code:

(use-package ace-window
  :ensure t)

(use-package magit
  :ensure t
  :commands (magit-status))

;; End
(provide 'my-config-module-apps)
