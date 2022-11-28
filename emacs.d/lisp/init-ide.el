;; -*- coding: utf-8; lexical-binding: t; -*-
(yx-require-package 'eglot)
(dolist (m '(python-mode-hook
             c-mode-hook
             c++-mode-hook))
  (add-hook m #'eglot-ensure))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '(python-mode "pyls"))
  )

(yx-require-package 'yasnippet-snippets)
(yx-require-package 'yasnippet)
(add-hook 'prog-mode-hook 'yas-minor-mode)
(with-eval-after-load 'yasnippet
  ;; unbind <TAB> completion
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "<tab>")  nil)
  ;; bind
  (define-key yas-minor-mode-map (kbd "S-<tab>") 'yas-expand)
  )

;; vterm ;;;;;;;;;;;;;;;
(yx-require-package 'vterm)
(with-eval-after-load 'vterm
  (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
  (setq vterm-kill-buffer-on-exit t)
  (add-to-list 'display-buffer-alist
               '("\\*vterm\\*" (display-buffer-reuse-mode-window
                                display-buffer-in-side-window)
                 (side . bottom)
                 (dedicated . t)
                 (window-height . 0.4)
                 (mode vterm-mode vterm-copy-mode)))
  )

;; project
(with-eval-after-load "project"
    (define-key project-prefix-map "m" 'magit))

(provide 'init-ide)
