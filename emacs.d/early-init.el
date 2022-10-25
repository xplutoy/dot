;; -*- coding: utf-8; lexical-binding: t; -*-
;; no menu bar, toolbar, scroll bar
(dolist (var '(default-frame-alist initial-frame-alist))
  (add-to-list var '(menu-bar-lines . 0))
  (add-to-list var '(tool-bar-lines . 0))
  (add-to-list var '(vertical-scroll-bars))
  (add-to-list var '(horizontal-scroll-bars))
  (add-to-list var '(fullscreen . maximized)))

(setq package-enable-at-startup nil)

(let ((alist-bk file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook #'(lambda ()
                                    (dolist (handler file-name-handler-alist)
                                      (add-to-list 'alist-bk handler))
                                    (setq file-name-handler-alist alist-bk)))
  )

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      file-name-handler-alist nil)
(add-hook 'emacs-startup-hook #'(lambda ()
				                  (setq gc-cons-threshold (* 20 1024 1024)
				                        gc-cons-percentage 0.1)
                                  ))
