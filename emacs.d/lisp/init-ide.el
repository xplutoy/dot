;; -*- coding: utf-8; lexical-binding: t; -*-
(yx-require-package 'eglot)
(dolist (m '(python-mode-hook
             c-mode-hook
             c++-mode-hook))
  (add-hook m #'eglot-ensure))
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
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
  )

;; aweshell
(add-to-list 'load-path (concat user-emacs-directory "nonelpa/aweshell"))
;; (setq aweshell-auto-suggestion-p nil) ;;auto suggestion depend on company ;before require
(setq exec-path-from-shell-check-startup-files nil)
(require 'aweshell)
(setq eshell-highlight-prompt nil)
(setq eshell-prompt-function 'epe-theme-lambda)
(setq aweshell-dedicated-window-height 15)
(define-key eshell-mode-map (kbd "C-l") #'aweshell-clear-buffer )
(define-key eshell-mode-map (kbd "C-n") #'aweshell-next )
(define-key eshell-mode-map (kbd "C-p") #'aweshell-prev)
(define-key eshell-mode-map (kbd "C-t") #'aweshell-new)
(define-key eshell-mode-map (kbd "C-p") #'aweshell-prev)
(define-key eshell-mode-map (kbd "C-o") #'aweshell-switch-buffer)
(define-key eshell-mode-map (kbd "C-r") #'aweshell-search-history)
(define-key eshell-mode-map (kbd "C-s") #'aweshell-sudo-toggle)
(global-set-key (kbd "C-,") #'aweshell-dedicated-toggle)

;; project
(with-eval-after-load "project"
    (define-key project-prefix-map "m" 'magit))

(provide 'init-ide)
