;; -*- coding: utf-8; lexical-binding: t; -*-
(yx-require-package 'eglot)
(add-hook 'python-mode-hook 'eglot-ensure)

(yx-require-package 'yasnippet-snippets)
(yx-require-package 'yasnippet)
(add-hook 'prog-mode-hook (lambda ()
			    (run-with-idle-timer 2 nil 'yas-minor-mode)))
(with-eval-after-load 'yasnippet
  ;; unbind <TAB> completion
  (define-key yas-minor-mode-map [(tab)]        nil)
  (define-key yas-minor-mode-map (kbd "TAB")    nil)
  (define-key yas-minor-mode-map (kbd "<tab>")  nil)
  ;; bind 
  (define-key yas-minor-mode-map (kbd "S-<tab>") 'yas-expand)
  )

(provide 'init-ide)
