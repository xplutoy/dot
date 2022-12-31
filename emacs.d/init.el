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
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

(setq inhibit-splash-screen t
      inhibit-startup-message t
      ;; initial-scratch-message nil
      inhibit-compacting-font-caches t
      frame-title-format '("%b")
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t)
;; (setq native-comp-async-report-warnings-errors 'silent)

(setq use-short-answers t
      echo-keystrokes 0.25
      visible-bell t
      ring-bell-function 'ignore
      use-dialog-box nil
      load-prefer-newer nil
      create-lockfiles nil)

(setq delete-by-moving-to-trash  ON-MAC
      kill-do-not-save-duplicates t)

;; elpa-init
(require 'package)
(setq package-archives
      '(
	("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ;; ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ;; ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
        ))
(setq package-quickstart t)
(add-hook 'kill-emacs-hook 'package-quickstart-refresh)

;;help function
(defmacro yx-require-package (package)
  "Only install the package if it is not already installed."
  `(progn
     (unless (package-installed-p ,package) (package-install ,package))
     (package-activate ,package)))

(defun yx-add-to-load-path-r (dir)
  (let ((default-directory  dir))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))
  )

(defun yx-delay-run (seconds func)
  "After SECONDS, run function FUNC once."
  (run-with-idle-timer seconds nil func))

(yx-add-to-load-path-r (concat user-emacs-directory "lisp"))

(require 'init-ui)
(add-hook 'emacs-startup-hook  #'(lambda ()
                                   (message "Emacs loaded in %s." (emacs-init-time))
                                   (require 'subr-x)
                                   (require 'init-basic)
                                   (require 'init-misc)
                                   (require 'init-completion)
                                   (require 'init-eshell)
                                   (require 'init-org)
                                   (require 'init-mail)
                                   (require 'init-elfeed)
                                   (require 'init-ide)
                                   (require 'init-python)
                                   ;; (require 'init-company)
                                   ))
