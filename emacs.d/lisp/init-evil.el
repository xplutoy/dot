;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package evil
  :custom
  (evil-undo-system 'undo-redo)
  :init
  (setq evil-default-state 'emacs
        evil-search-module 'evil-search
        evil-respect-visual-line-mode t
        evil-want-C-u-scroll t
        evil-want-C-w-delete t
        evil-want-Y-yank-to-eol t
        evil-move-cursor-back t
        evil-lookup-func #'helpful-at-point)
  ;; necessary for evil-collection
  (setq evil-want-integration t
        evil-want-keybinding nil)

  :config
  (evil-mode 1)

  ;; mode with normal states
  (defvar my-initial-evil-state-setup
    '((messages-buffer-mode . normal)
      (prog-mode . normal)
      (emacs-lisp-mode . normal)
      (org-mode . normal)
      (text-mode . normal))
    "Default evil state per major mode.")
  (dolist (p my-initial-evil-state-setup)
    (evil-set-initial-state (car p) (cdr p)))

  ;; evil re-assign "M-." to `evil-repeat-pop-next' which I don't use actually.
  ;; Restore "M-." to original binding command
  (define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
  :general
  (:states 'normal
           "M-." 'xref-find-definitions)
  (:states 'insert
           "C-a" 'crux-move-beginning-of-line
           "C-e" 'move-end-of-line
           "C-k" 'kill-line
           "C-y" 'yank)
  (evil-ex-completion-map "C-a" 'move-beginning-of-line
                          "C-b" 'backward-char
                          "M-n" 'next-complete-history-element
                          "M-p" 'previous-complete-history-element)
  )

(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :after evil
  :init
  (setq evil-collection-mode-list
        '(corfu vertico vterm
                consult bookmark diff-hl
                dired eglot ediff
                doc-view eldoc elfeed
                elisp-mode embark emoji
                eshell flymake gnus ibuffer
                imenu info lua-mode org
                org-roam python simple w3m
                which-key women xref))
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  (defun evil-surround-prog-mode-hook-setup ()
    "Set up surround shortcuts."
    (when (memq major-mode '(c++-mode))
      (push '(?< . ("<" . ">")) evil-surround-pairs-alist))
    (when (memq major-mode '(sh-mode))
      (push '(?$ . ("${" . "}")) evil-surround-pairs-alist))
    (when (memq major-mode '(org-mode))
      (push '(?\[ . ("[[" . "]]")) evil-surround-pairs-alist) ; [
      (push '(?= . ("=" . "=")) evil-surround-pairs-alist))
    (when (memq major-mode '(emacs-lisp-mode))
      (push '(?\( . ("( " . ")")) evil-surround-pairs-alist)
      (push '(?` . ("`" . "'")) evil-surround-pairs-alist))
    ;; generic
    (push '(?/ . ("/" . "/")) evil-surround-pairs-alist))
  (add-hook 'prog-mode-hook 'evil-surround-prog-mode-hook-setup)
  )

(use-package evil-escape
  :init
  (setq-default evil-escape-delay 0.3
                evil-escape-key-sequence "jk")
  (setq evil-escape-unordered-key-sequence t)
  :config
  (evil-escape-mode 1)
  :bind ("C-c C-g" . evil-escape)
  )

(use-package evil-matchit
  :hook (prog-mode . turn-on-evil-matchit-mode)
  :general
  (yx-comma-leader-def   "di" 'evilmi-delete-items
                         "si" 'evilmi-select-items))

(use-package evil-mark-replace
  :general
  (yx-comma-leader-def    "rb" 'evilmr-replace-in-buffer
                          "ts" 'evilmr-tag-selected-region))

(use-package evil-nerd-commenter
  :config
  (evilnc-default-hotkeys nil t))


;; end evil
(provide 'init-evil)
