;;; -*- lexical-binding: t no-byte-compile: t -*-

(setq mail-yank-prefix "> ")
(setq mail-user-agent 'gnus-user-agent)

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

;; notmuch
(yx-require-package 'notmuch)
(setq notmuch-address-command 'internal)
(setq notmuch-crypto-process-mime t)


(provide 'init-mail)
