;;; my-config-module-minibuffer.el -*- lexical-binding: t; -*-

;; Copyright (c) 2025.

;;; Commentary:
;; Add minibuffer commands

;;; Code:

;;; General settings

;; Delete up to parent folder when in a minibuffer completing a file name
(defun my/ui-minibuffer--backward-kill (arg)
  "When minibuffer is completing a file name, delete up to parent
folder, otherwise delete a word."
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (kill-word (- arg))))

(let ((map minibuffer-local-map))
  (define-key map (kbd "C-<backspace>") #'my/ui-minibuffer--backward-kill)
  (define-key map (kbd "M-<backspace>") #'my/ui-minibuffer--backward-kill))

;;; Packages

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
  :config
  (marginalia-mode 1))

;; End
(provide 'my-config-module-minibuffer)
