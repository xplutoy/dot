;;; -*- lexical-binding: t no-byte-compile: t -*-

(setq mail-yank-prefix "> ")
(setq mail-user-agent 'gnus-user-agent)

(setq message-directory "~/personal/mail/")
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/local/bin/msmtp" 
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)

(setq send-mail-function 'smtpmail-send-it
      smtpmail-queue-mail t
      smtpmail-smtp-user "yangxue.cs@foxmail.com"
      smtpmail-smtp-server "smtp.qq.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)

(setq mml-default-sign-method "pgpmime")
(setq mml-secure-openpgp-sign-with-sender t)
(setq mml-secure-openpgp-encrypt-to-self t)

;; gnus
(setq gnus-inhibit-startup-message t
      gnus-init-file (concat user-emacs-directory "gnus.el")
      gnus-startup-file (concat user-emacs-directory "newsrc")
      gnus-directory (concat user-emacs-directory "News"))

;; notmuch
(use-package notmuch
  :init
  (setq notmuch-show-logo nil
        notmuch-show-all-tags-list  t
        notmuch-hello-auto-refresh t
        notmuch-hello-recent-searches-max 20
        notmuch-address-command 'internal
        notmuch-crypto-process-mime t))


(provide 'init-mail)
