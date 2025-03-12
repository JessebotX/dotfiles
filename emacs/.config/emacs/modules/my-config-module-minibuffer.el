;;; my-config-module-minibuffer.el -*- lexical-binding: t; -*-

;; Copyright (c) 2025.

;;; Commentary:
;; Add minibuffer commands

;;; Code:

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode 1))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :ensure t)

(use-package marginalia
  :ensure t
  :hook (vertico-mode . marginalia-mode))

;; End
(provide 'my-config-module-minibuffer)
