;; -*- coding: utf-8; lexical-binding: t; -*-

(defun python-imenu-use-flat-index ()
  (setq imenu-create-index-function
        #'python-imenu-create-flat-index))
(add-hook 'python-mode-hook
          #'python-imenu-use-flat-index)

(yx-require-package 'pyvenv)
(yx-require-package 'pyvenv-auto)
(add-hook 'python-mode-hook #'pyvenv-auto-mode)

(provide 'init-python)
