;; -*- coding: utf-8; lexical-binding: t; -*-

;; Check the system used
(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; in terminal, use basic mode
(defvar yx/basic-mode-p t)
(when (display-graphic-p)
  (setq yx/basic-mode-p nil) ;; if t dont use other package unless inbuilt
  )

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "Emacs loaded in %s."
                       (emacs-init-time))))

(defun yx/add-to-load-path-r (dir)
   (let ((default-directory  dir))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))
   )

(defun yx/run-with-idle-timer (seconds func)
  "After SECONDS, run function FUNC once."
  (run-with-idle-timer seconds nil func))

(yx/add-to-load-path-r (concat user-emacs-directory "lisp"))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)


(require 'init-basic)
(require 'init-font)
(require 'init-elpa)
(require 'init-org)
(require 'init-elfeed)
(unless yx/basic-mode-p
  (require 'init-misc)
  (require 'init-completion)
  (require 'init-ide)
  (require 'init-python))

;; (require 'init-company)
