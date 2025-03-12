;;; my-config-module-editing.el -*- lexical-binding: t -*-

;;; Commentary:
;; Configure packages related to text editing functionality.

;;; Code:

;; Jump to definition (without using LSP)
(use-package dumb-jump
  :ensure t
  :config
  (setopt xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Quick code commenting
(use-package evil-nerd-commenter
  :ensure t)

;; Colors to color names and values (e.g. #23ffaa, blue)
(use-package rainbow-mode
  :ensure t
  :hook (prog-mode))

;; Unique colors for delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode))

;; Better whitespace deletion
(use-package hungry-delete
  :ensure t
  :custom
  ;; delete the following on backspace: SPC, TAB, ^M, ^L, ^K
  (hungry-delete-chars-to-skip " 	")
  :config
  (global-hungry-delete-mode 1))

;; Margin/write-room/focus/zen modes
(use-package olivetti
  :ensure t
  :hook (text-mode . olivetti-mode))

(use-package writeroom-mode
  :ensure t)

;; Highlight TODO keywords in comments and strings
(use-package hl-todo
  :ensure t
  :hook ((prog-mode . hl-todo-mode)
         (text-mode . hl-todo-mode))
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
   `(("TODO" . (warning italic bold))
     ("FIXME" . (error italic bold))
     ("DEBUG" . (error italic bold))
     ("REVIEW" . (font-lock-doc-face italic bold))
     ("NOTE" . (warning italic bold))
     ("DEPRECATED" . (font-lock-doc-face italic bold)))))

;;; End
(provide 'my-config-module-editing)
