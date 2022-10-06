;; -*- coding: utf-8; lexical-binding: t; -*-

(set-default-coding-systems 'utf-8)

(setq ring-bell-function 'ignore)
(setq blink-cursor-mode nil)
(setq use-short-answers t)
;; (setq mode-line-compact t)
(setq apropos-sort-by-scores t)
(setq auto-save-default t)
(setq auto-save-no-message t)
(setq backup-directory-alist
          `(("." . ,(concat user-emacs-directory "backups"))))
(setq blink-cursor-mode nil)
(setq kill-do-not-save-duplicates t)
(setq delete-auto-save-files t)  ;; when save or kill buffer
(setq isearch-allow-motion t)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'imenu)

(defun yx-inbuilt-modes-global-toggle ()
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1)
  (electric-pair-mode 1)
  (windmove-default-keybindings)
  (recentf-mode 1)
  (setq recentf-max-saved-items 80)
  ;; (global-set-key (kbd "C-x C-r") 'recentf-open-files)
  (savehist-mode 1)
  (delete-selection-mode 1)
  (global-superword-mode 1)
  (repeat-mode 1)
  (winner-mode 1)
  (global-so-long-mode 1)
  (save-place-mode 1)
  (auto-save-visited-mode 1)
  (setq auto-save-visited-interval 10)
  
  )

(add-hook 'after-init-hook 'yx-inbuilt-modes-global-toggle)
(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)

(provide 'init-default)
