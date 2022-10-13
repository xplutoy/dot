;; -*- coding: utf-8; lexical-binding: t; -*-

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.
(setq package-enable-at-startup nil)
(set-default 'cursor-type 'box)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; no menu bar, toolbar, scroll bar
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (horizontal-scroll-bars)
        (vertical-scroll-bars)
        (fullscreen . maximized)))

(setq inhibit-splash-screen t
      inhibit-startup-message t
      frame-inhibit-implied-resize t)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      file-name-handler-alist nil)
(add-hook 'emacs-startup-hook (lambda ()
				(setq gc-cons-threshold (* 20 1024 1024)
				      gc-cons-percentage 0.1)))
