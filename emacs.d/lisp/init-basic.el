;; -*- coding: utf-8; lexical-binding: t; -*-
(setq-default major-mode 'text-mode
              fill-column 100
              tab-width 4
              indent-tabs-mode nil
	          cursor-type 'box)

(fringe-mode 4) ;;default 8

(setq inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash  t)  ; Deleting files go to OS's trash folder

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (global-display-line-numbers-mode 1)
(setq-default abbrev-mode t)
(setq kill-buffer-delete-auto-save-files 1)
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(setq use-dialog-box nil)
;; (setq mode-line-compact t)
(setq apropos-sort-by-scores t)
(setq auto-save-default t)
(setq auto-save-no-message t)
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq kill-do-not-save-duplicates t)
(setq delete-auto-save-files t)  ;; when save or kill buffer
(setq isearch-allow-motion t)
(setq browse-url-browser-function 'eww-browse-url)  ;; in emacs use eww as web browser
(setq tab-bar-show t)
(setq create-lockfiles nil)
(setq require-final-newline t)
(setq frame-resize-pixelwise t)
;; Better scroll behavior
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-yank-at-point t)

;; ui
(when (display-graphic-p)
  (tool-bar-mode -1)
  )
(menu-bar-mode -1)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-window-scroll-bars
             (minibuffer-window frame) 0 nil 0 nil t)))
(setq tab-bar-new-button-show nil
      tab-bar-close-button-show nil)


(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'imenu)

(defun yx/inbuilt-modes-global-toggle ()
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode 1)
  (add-hook 'prog-mode-hook 'electric-pair-local-mode)
  (add-hook 'conf-mode-hook 'electric-pair-local-mode)
  ;; (electric-pair-mode 1)
  (windmove-default-keybindings)
  (recentf-mode 1)
  (setq recentf-max-saved-items 100)
  (setq recentf-exclude '(
                "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                "^/tmp/"))
  (setq recentf-auto-cleanup 10)
  (savehist-mode 1)
  (delete-selection-mode 1)
  (global-superword-mode 1)
  (repeat-mode 1)
  (winner-mode 1)
  (global-so-long-mode 1)
  (setq desktop-save t)
  (setq sesktop-load-locked-desktop t)
  (setq desktop-path (list user-emacs-directory))
  (setq desktop-dirname user-emacs-directory)
  (desktop-save-mode 1)
  (save-place-mode 1)
  (auto-save-visited-mode 1)
  (setq auto-save-visited-interval 10)
  (global-hl-line-mode 1)
  (blink-cursor-mode -1)
  (tab-bar-history-mode 1)


  ;; 一些跟后面其他插件有冲突的基础配置
  (when yx/basic-mode-p
    (require 'ido)
    (setq ido-enable-flex-matching t)
    (setq ido-use-filename-at-point 'guess)
    (setq ido-creat-new-buffer 'always)
    (setq ido-use-url-at-point t)
    (setq ido-ignore-extensions t)
    (setq ido-case-fold t)
    (setq ido-everywhere t)
    (ido-mode t)
    (fido-mode t)
    (global-set-key (kbd "C-x C-r") 'recentf-open-files)
    (load-theme 'wombat))

  )

;; eww
;; (with-eval-after-load 'eww
;;   ;; eww-auto-rename-buffe
;;   )

(add-hook 'after-init-hook 'yx/inbuilt-modes-global-toggle)
(add-hook 'prog-mode-hook #'show-paren-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)
;;delete whitespace after line
(add-hook 'prog-mode-hook
          #'(lambda ()
              (add-hook 'before-save-hook #'delete-trailing-whitespace 0 t)))
(add-hook 'emacs-startup-hook #'desktop-read)


(provide 'init-basic)
