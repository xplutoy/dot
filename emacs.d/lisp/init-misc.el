;; -*- coding: utf-8; lexical-binding: t; -*-
;; ui theme
;; 1, monokai-theme
(yx/require-package 'gruvbox-theme)
(load-theme 'gruvbox-light-medium t)

;; ui modeline
(when (display-graphic-p)
  (add-to-list 'load-path (concat user-emacs-directory "nonelpa/awesome-tray"))
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

(yx/require-package 'which-key)
(add-hook 'after-init-hook
	  #'(lambda ()
	      (setq which-key-idle-delay 1.5) ;; whick-key文挡上说必须在load之前设置
	      (setq which-key-idle-secondary-delay 0.05)
	      (setq which-key-popup-type 'minibuffer)
	      (which-key-mode 1)
	      ))

;; buffer-move
(yx/require-package 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; restart-emacs
(yx/require-package 'restart-emacs)

(yx/require-package 'posframe)  ;; sdcv dep

;; sdcv @https://github.com/manateelazycat/sdcv
(yx/run-with-idle-timer 2 #'(lambda ()
                              (add-to-list 'load-path (concat user-emacs-directory "nonelpa/sdcv"))
                              (require 'sdcv)
                              (setq sdcv-dictionary-simple-list (list "朗道英汉字典5.0")
                                    sdcv-dictionary-complete-list (list "朗道英汉字典5.0")
                                    sdcv-dictionary-data-dir "/Users/yx/.config/stardict/dic") ; set local sdcv dict dir
                              (global-set-key (kbd "M-s s") 'sdcv-search-pointer+)
                              ))

(provide 'init-misc)
