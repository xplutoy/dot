;;; -*- lexical-binding: t no-byte-compile: t -*-
;; rmail
;; (setq rmail-file-name   "~/mail/rmail/RMAIL"
;;       rmail-secondary-file-directory "~/mail/rmail"
;;       rmail-primary-inbox-list nil
;;       rmail-spool-directory ""
;;       rmail-preserve-inbox t
;;       rmail-mail-new-frame t
;;       rmail-delete-after-output t
;;       rmail-mime-prefer-html nil
;;       )

(setq mail-yank-prefix "> ")
(setq mail-user-agent 'message-user-agent)

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/local/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
(setq smtpmail-queue-mail t
      smtpmail-smtp-user "yangxue.cs@foxmail.com"
      smtpmail-smtp-server "smtp.qq.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)


;; notmuch
(yx-require-package 'notmuch)


(provide 'init-mail)
