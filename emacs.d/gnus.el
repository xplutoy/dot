(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods
      '(
        ;; (nnmaildir "qqmail" (directory "~/mail/qqmail"))  ;; isync
        (nntp "news.gwene.org")
        (nnimap "qqmail"
                 (nnimap-address "imap.qq.com")
                 (nnimap-inbox "INBOX")
                 (nnimap-expunge t)
                 (nnimap-server-port 993)
                 (nnimap-expunge-on-close 'always)
                 (nnimap-stream ssl))
        )
      )

(setq nnmail-expiry-wait 'never
      nnmail-split-methods 'gnus-group-split)

(setq gnus-message-archive-group nil)

(setq gnus-user-date-format-alist '((t . "%m-%d %H:%M"))
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
      gnus-summary-display-arrow t
      gnus-summary-same-subject ""
      gnus-summary-gather-subject-limit 'fuzzy
      )
(setq gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)
      gnus-thread-hide-subtree t
      gnus-thread-ignore-subject t
      gnus-fetch-old-headers t
      )
