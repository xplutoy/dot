;; -*- coding: utf-8; lexical-binding: t; -*-
;; ui theme
(yx-require-package 'gruvbox-theme)
(load-theme 'gruvbox-light-medium t)

;; ui modeline
(when (display-graphic-p)
  (require 'awesome-tray)
  (with-eval-after-load 'awesome-tray
    (setq awesome-tray-git-show-status t)
    (setq awesome-tray-buffer-name-buffer-changed t)
    ;; (setq awesome-tray-file-path-show-filename t)
    ;; (setq awesome-tray-file-path-full-dirname-levels 0) ;; only file name
    (setq awesome-tray-active-modules '("location" "git" "buffer-name" "mode-name"))
    
    ;; awesome-tray-module-alist
    (awesome-tray-mode 1)
    ))

(yx-require-package 'which-key)
(run-with-idle-timer 1 nil 'which-key-mode)
(with-eval-after-load 'which-key
  ;; (setq which-key-popup-type 'minibuffer)
  )

(provide 'init-misc)
