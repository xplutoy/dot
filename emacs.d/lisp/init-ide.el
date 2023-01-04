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
  :hook (prog-mode-hook . yas-minor-mode)
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

;; project
(with-eval-after-load "project"
    (define-key project-prefix-map "m" 'magit))

(provide 'init-ide)
