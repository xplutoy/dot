;; -*- coding: utf-8; lexical-binding: t; -*-
;; no menu bar, toolbar, scroll bar
(when (display-graphic-p)
  (fringe-mode 4)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(menu-bar-mode -1)

(dolist (var '(default-frame-alist initial-frame-alist))
  (add-to-list var '(menu-bar-lines . 0))
  (add-to-list var '(tool-bar-lines . 0))
  (add-to-list var '(vertical-scroll-bars))
  (add-to-list var '(horizontal-scroll-bars))
  (add-to-list var '(undecorated-round . t))
  (add-to-list var '(fullscreen . maximized)))

(setq package-enable-at-startup nil)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook #'(lambda ()
				                  (setq gc-cons-threshold (* 20 1024 1024)
				                        gc-cons-percentage 0.1)
                                                  ))

