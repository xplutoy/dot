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
  :bind ("C-;" . vterm-toggle-cd)
  )

;; project
(with-eval-after-load "project"
  (define-key project-prefix-map "m" 'magit))

;; diff-hl
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-dired-mode)
  )

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook ((prog-mode emacs-lisp-mode) . rainbow-delimiters-mode)
  )

;; aggressive-indent
(use-package aggressive-indent
  :hook ((emacs-lisp-mode c-mode python-mode) . aggressive-indent-mode)
  )

;; magit
(use-package magit
  :defer t
  )


(provide 'init-ide)
