;; -*- coding: utf-8; lexical-binding: t; -*-
;; const
(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; custom.el
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

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

(add-hook 'after-init-hook  #'(lambda ()
                                (require 'server)
                                (unless (or (bound-and-true-p server-process)
                                            (server-running-p))
                                  (server-start))))

(add-hook 'emacs-startup-hook  #'(lambda ()
                                   (message "Emacs loaded in %s."
                                            (emacs-init-time))))

(require 'init-font)
(require 'init-basic)
(require 'init-org)
(require 'init-elfeed)
(require 'init-misc)
(require 'init-completion)
(require 'init-ide)
(require 'init-python)
;; (require 'init-company)
