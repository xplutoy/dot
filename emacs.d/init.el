;; -*- coding: utf-8; lexical-binding: t; -*-
;; yx custom
(setq user-full-name "yangxue")
(setq user-mail-address "yangxue.cs@foxmail.com")
(defvar yx-org-root "~/personal/org/")
(defvar yx-share-data-path "~/personal/local.d/share/")

;; const
(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; custom.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; elpa-init
(require 'package)
(setq package-archives
      '(
	("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ;; ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ;; ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
        ))
(setq package-quickstart nil)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-ui)
(require 'init-basic)
(require 'init-misc)
(require 'init-completion)
(require 'init-evil)
(require 'init-eshell)
(require 'init-org)
(require 'init-mail)
(require 'init-elfeed)
(require 'init-ide)
(require 'init-lang)
