;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package eglot
  :ensure nil
  :init
  (dolist (m '(python-mode-hook c-mode-hook c++-mode-hook))
    (add-hook m #'eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '(python-mode "pyls"))
  )

(use-package yasnippet-snippets)
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  )

;; vterm ;;;;;;;;;;;;;;;
(use-package vterm
  :init
  (setq vterm-kill-buffer-on-exit t)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*vterm\\*" (display-buffer-reuse-mode-window
                                display-buffer-in-side-window)
                 (side . bottom)
                 (dedicated . t)
                 (window-height . 0.4)
                 (mode vterm-mode vterm-copy-mode)))
  :bind (:map vterm-mode-map
         ("C-q" . vterm-send-next-key))
  )
(use-package vterm-toggle
  :after vterm
  :init
  (setq vterm-toggle-hide-method nil)
  :general
  (yx-space-leader-def "tv" 'vterm-toggle-cd))

;; project
(with-eval-after-load "project"
  (define-key project-prefix-map "m" 'magit))

;; diff-hl
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-dired-mode))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook ((prog-mode emacs-lisp-mode) . rainbow-delimiters-mode)
  )

;; aggressive-indent
(use-package aggressive-indent
  :diminish
  :hook ((emacs-lisp-mode c-mode python-mode) . aggressive-indent-mode)
  )

;; hl-todo
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :general
  (yx-space-leader-def "tdp" 'hl-todo-previous
                       "tdn" 'hl-todo-next
                       "tdo" 'hl-todo-occur
                       "tdi" 'hl-todo-insert))

;; indent-guide
(use-package indent-guide
  :diminish
  :init
  (setq indent-guide-recursive nil)
  :hook ((prog-mode emacs-lisp-mode) . indent-guide-mode)
  )

;; magit
(use-package magit
  :defer t
  )


(provide 'init-ide)
