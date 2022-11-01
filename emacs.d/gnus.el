(setq gnus-select-method '(nnnil ""))
(setq gnus-secondary-select-methods
      '(
        (nntp "news.gwene.org")
        ;; (nnmaildir "qqmail" (directory "~/mail/qqmail"))  ;; isync
        (nnimap "qqmail"
                (nnimap-address "imap.qq.com")
                (nnimap-server-port 993)
                (nnimap-inbox "INBOX")
                (nnmail-expiry-wait 14)
                (nnimap-stream ssl)
                (nnimap-search-engine imap)
                (nnimap-expunge 'never)
                (nnimap-expunge-on-close 'never))
        (nnimap "outlook"
                (nnimap-address "outlook.office365.com")
                (nnimap-server-port 993)
                (nnimap-inbox ("INBOX" "SENT"))
                (nnimap-stream ssl)
                (nnimap-search-engine imap))
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
(setq gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date
                                   (not gnus-thread-sort-by-number))
      gnus-thread-hide-subtree t
      gnus-thread-ignore-subject t
      gnus-fetch-old-headers t
      )
