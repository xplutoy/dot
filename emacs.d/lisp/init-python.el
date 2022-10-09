;; -*- coding: utf-8; lexical-binding: t; -*-
(add-hook 'python-mode-hook
          #'(lambda()
              (setq imenu-create-index-function #'python-imenu-create-flat-index)
              ))

(yx/require-package 'pyvenv)
(yx/require-package 'pyvenv-auto)
(add-hook 'python-mode-hook #'pyvenv-auto-mode)

(provide 'init-python)
