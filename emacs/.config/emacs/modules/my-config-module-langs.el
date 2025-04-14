;;; my-config-module-langs.el -*- lexical-binding: t; -*-

;; Copyright (c) 2025.

;;; Commentary:
;; Configure text and programming languages.

;;; Code:

(defun my/indent-with-spaces (width)
  "Set the local buffer's line indent size to WIDTH and insert as
spaces.

The value of WIDTH should be a positive integer."
  (interactive "nIndent space width: ")
  (setq-local evil-shift-width width)
  (setq-local tab-width width)
  (setq-local indent-tabs-mode nil))

(defun my/indent-with-tabs (width)
  "Set the local buffer's line indent size to be WIDTH and insert as
a tab character.

The value of WIDTH should be a positive integer."
  (interactive "nIndent tab width: ")
  (setq-local evil-shift-width width)
  (setq-local tab-width width)
  (setq-local indent-tabs-mode t))

;;;; HTML

(defun my/lang-hook--html-mode ()
  (setopt sgml-basic-offset 4)
  (my/indent-with-spaces 4))
(add-hook 'html-mode-hook #'my/lang-hook--html-mode)

;;;; CSS

(defun my/lang-hook--css-mode ()
  (setopt css-indent-offset 4)
  (my/indent-with-spaces 4))
(add-hook 'css-mode-hook #'my/lang-hook--css-mode)

;;;; Go

(use-package go-mode
  :ensure t
  :mode "\\.\\(?:go\\)\\'"
  :preface
  (defun my/lang-hook--go-mode ()
    (my/indent-with-tabs 8))
  :config
  (add-hook 'go-mode-hook #'my/lang-hook--go-mode))

;;;; JavaScript

(defun my/lang-hook--js-mode ()
  (setopt js-indent-level 4)
  (my/indent-with-spaces 4))
(add-hook 'js-mode-hook #'my/lang-hook--js-mode)

;;; Markdown

(use-package markdown-mode
  :ensure t
  :mode "\\.\\(?:md\\|txt\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'"
  :preface
  (defun my/lang-hook--markdown-mode ()
    "Settings for `markdown-mode-hook'."
    (my/indent-with-tabs 4))
  :custom
  (markdown-max-image-size '(512 . 512))
  :config
  (add-hook 'markdown-mode-hook #'my/lang-hook--markdown-mode))

;;; Org mode

(use-package org
  :ensure nil
  :preface
  (defun my/lang-hook--org-mode ()
    "Settings for `org-mode-hook'"
    (my/indent-with-spaces 8)
    (setq paragraph-start "\\|[    ]*$")
    (setq paragraph-separate "[     ]*$")
    (visual-line-mode 1)
    (display-line-numbers-mode -1)
    ;(my/large-headings-mode 1)
    )
  :hook (org-mode . my/lang-hook--org-mode)
  :custom
  (org-hide-leading-stars t)
  (org-id-link-to-org-use-id 'use-existing)
  (org-ellipsis " +")
  (org-auto-align-tags nil)
  (org-src-preserve-indentation t)
  (org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                          (vm-imap . vm-visit-imap-folder-other-frame)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file)
                          (wl . wl-other-frame)))
  (org-link-file-path-type 'relative)
  (org-hide-emphasis-markers t)
  (org-startup-folded 'showall)
  (org-todo-keywords '((sequence
                        "TODO(t)"
                        "WIP(w)"
                        "HOLD(h)"
                        "EDIT(e)"
                        "REDO(r)"
                        "APPOINTMENT(a)"
                        "|" ; separate active and inactive states
                        "ARCHIVED(s)"
                        "DONE(d)"
                        "CANCELLED(w)")))
  (org-agenda-span 5)
  (org-agenda-start-day "+0d")
  (org-agenda-skip-timestamp-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-scheduled-if-deadline-is-shown t)
  (org-agenda-skip-timestamp-if-deadline-is-shown t)
  (org-startup-with-inline-images t)
  (org-tags-column 0)
  :config
  (add-to-list 'org-export-backends 'md)

  ;; EVIL MODE: let j,k move around `org-agenda-mode'
  (with-eval-after-load 'evil
    (evil-define-key '(normal emacs) org-agenda-mode-map
      (kbd "j") 'org-agenda-next-item
      (kbd "k") 'org-agenda-previous-item
      (kbd "N") 'org-agenda-goto-date
      (kbd "P") 'org-agenda-capture))

  ;; Commands/functions/macros
  (defun my/org-toggle-markup ()
    "Toggle hiding/showing `org-mode' emphasis markers."
    (interactive)
    (if org-hide-emphasis-markers
        (setopt org-hide-emphasis-markers nil)
      (setopt org-hide-emphasis-markers t))
    (org-mode-restart))

  ;; Binds
  (let ((map org-mode-map))
    (keymap-set map "C-'" nil) ; remove org agenda cycle list
    (keymap-set map "C-," nil) ; remove org agenda cycle list
    (keymap-set map "C-c m i" 'org-id-store-link)
    (keymap-set map "C-c m l" 'org-store-link)
    (keymap-set map "C-c C-x RET" #'my/org-toggle-markup)))

(use-package org-modern
  :ensure t
  :hook ((org-mode . global-org-modern-mode)
         (org-agenda-mode . global-org-modern-mode))
  :custom
  (org-modern-keyword "» ")
  (org-modern-star '("◉" "●" "○" "◈" "◇"))
  (org-modern-block-fringe nil)
  (org-modern-hide-stars t)
  :config
  (defun my/org-modern-hide-star-headings ()
    "Remove all stars from a heading, increase heading sizes and disable `org-indent-mode'."
    (interactive)
    (setopt org-modern-hide-stars t)
    (setopt org-hide-leading-stars t)
    (org-mode-restart)
    (my/large-headings-mode 1)
    (org-indent-mode -1))

  (defun my/org-modern-star-headings ()
    "Enable stars and `org-indent-mode'."
    (interactive)
    (setopt org-modern-hide-stars nil)
    (setopt org-hide-leading-stars nil)
    (org-mode-restart)
    (my/large-headings-mode -1)
    (org-indent-mode 1)))

(use-package org-wc
  :ensure t
  :commands org-wc-display)

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))

(use-package ox-epub
  :ensure t
  :commands (org-export-dispatch))

;; End
(provide 'my-config-module-langs)
