;; -*- coding: utf-8; lexical-binding: t; -*-
(setq package-enable-at-startup nil)
(set-default 'cursor-type 'box)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; no menu bar, toolbar, scroll bar
(dolist (var '(default-frame-alist initial-frame-alist))
  (add-to-list var '(menu-bar-lines . 0))
  (add-to-list var '(tool-bar-lines . 0))
  (add-to-list var '(vertical-scroll-bars))
  (add-to-list var '(horizontal-scroll-bars))
  (add-to-list var '(fullscreen . maximized)))

(setq inhibit-splash-screen t
      inhibit-startup-message t
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t)
;; (setq native-comp-async-report-warnings-errors 'silent)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      file-name-handler-alist nil)
(add-hook 'emacs-startup-hook (lambda ()
				(setq gc-cons-threshold (* 20 1024 1024)
				      gc-cons-percentage 0.1)))
