;; -*- coding: utf-8; lexical-binding: t; -*-
(yx-require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load 'company
  
  (setq company-idle-delay
	(lambda () (if (company-in-string-or-comment) nil 0.2)))
  (setq company-global-modes
	'(not
	  erc-mode
	  message-mode
	  eshell-mode))
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-limit 6)
  (setq company-tooltip-flip-when-above t)
  (setq company-tooltip-margin 3)
  (setq company-show-quick-access 'left))

(provide 'init-company)
