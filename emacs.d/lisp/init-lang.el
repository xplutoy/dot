;; -*- coding: utf-8; lexical-binding: t; -*-
;; python
(add-hook 'python-mode-hook
          #'(lambda()
              (setq tab-width 4)
              (setq imenu-create-index-function #'python-imenu-create-flat-index)
              ))

(use-package pyvenv)
(use-package pyvenv-auto)
(add-hook 'python-mode-hook #'pyvenv-auto-mode)

;; c/c++

(provide 'init-lang)
