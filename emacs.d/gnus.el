;; (setq gnus-verbose 10)
(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods
      '(
        (nntp "news.gwene.org")
        (nnml "qqmail"
              (nnir-search-engine notmuch))
        (nnimap "outlook"
                (nnimap-address "outlook.office365.com")
                (nnimap-server-port 993)
                (nnimap-inbox "INBOX")
                (nnimap-stream ssl)
                (nnimap-split-methods default)
                (nnimap-search-engine imap))
        )
      )

(setq nntp-connection-timeout 5
      nntp-maximum-request 1)

(setq mail-sources
      '((imap :server "imap.qq.com"
              :port 993
              :user "yangxue.cs@foxmail.com"
              :mailbox "INBOX"
              :fetchflag "\\Seen"
              :stream tls
              :dontexpunge t)
        ))

(setq nnmail-split-fancy
      '(| (any ".*help-gnu-emacs@.*" "INBOX.help-gnu-emacs")
          (any ".*lua-l@.*" "INBOX.lua-l")
          (any ".*arch-general@.*" "INBOX.arch-general")
          (: nnmail-split-fancy-with-parent)
          "INBOX.misc"
          ))

(setq gnus-message-archive-group '((format-time-string "sent.%Y")))
(setq gnus-large-newsgroup 500
      gnus-auto-select-first nil
      gnus-newsgroup-maximum-articles 200)

(setq nnmail-treat-duplicates 'delete
      nnmail-split-fancy-match-partial-words t
      nnmail-cache-accepted-message-ids t
      nnmail-message-id-cache-length 5000
      nnmail-split-methods 'nnmail-split-fancy
      nnmail-use-long-file-names t)


(setq gnus-user-date-format-alist '((t . "%m-%d %H:%M"))
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
      gnus-summary-display-arrow nil
      gnus-summary-gather-subject-limit 'fuzzy
      )

(setq gnus-thread-sort-functions '(gnus-thread-sort-by-subject
                                   gnus-thread-sort-by-most-recent-number)
      gnus-thread-hide-subtree t
      gnus-thread-ignore-subject t
      gnus-fetch-old-headers t
      )

(setq gnus-always-read-dribble-file t)

(setq gnus-suppress-duplicates t
      gnus-save-duplicate-list t
      gnus-duplicate-list-length 50000)

;; @see https://mail.gnu.org/archive/html/info-gnus-english/2009-01/msg00053.html
(setq nnrss-ignore-article-fields '(description guid pubData dc:creator link))

(setq mm-sign-option nil
      mm-verify-option 'always)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
(add-hook 'kill-emacs-hook 'gnus-group-save-newsrc)
;; (gnus-demon-add-handler 'gnus-demon-scan-news 10 10)
