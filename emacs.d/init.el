;; -*- coding: utf-8; lexical-binding: t; -*-

;; Check the system used
(defconst ON-LINUX   (eq system-type 'gnu/linux))
(defconst ON-MAC     (eq system-type 'darwin))
(defconst ON-BSD     (or ON-MAC (eq system-type 'berkeley-unix)))
(defconst ON-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s."
                     (emacs-init-time))))

(defun yx-add-to-load-path-r (dirs)
  (when dirs
    (let ((default-directory  (car dirs)))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))
    (yx-add-to-load-path-r (cdr dirs))))

(yx-add-to-load-path-r (list
			(concat user-emacs-directory "lisp")
			(concat user-emacs-directory "nonelpa")))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)


(require 'init-default)
(require 'init-elpa)
(require 'init-misc)
(require 'init-completion)
(require 'init-ide)
(require 'init-python)
;; (require 'init-company)

