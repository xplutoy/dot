;; -*- coding: utf-8; lexical-binding: t; -*-

(yx-require-package 'elfeed)
(with-eval-after-load 'elfeed
  (setq-default elfeed-search-filter "@1-year-ago +unread ")
  (define-key elfeed-search-mode-map (kbd "j") #'next-line)
  (define-key elfeed-search-mode-map (kbd "k") #'previous-line)
  (define-key elfeed-search-mode-map (kbd "l")
    (lambda ()
      (interactive)
      (switch-to-buffer (elfeed-log-buffer))))
  
  (setq elfeed-feeds
        '(
          ("http://pragmaticemacs.com/feed/" emacs)
          ("https://karthinks.com/index.xml" hack tool)
          ("zhihu.com/rss" zhihu)
          ("https://blog.evjang.com/feeds/posts/default" tech)
          ("https://ruder.io/rss/index.rss" ai nlp)
          ("https://www.inference.vc/rss" ai)
          ("https://karpathy.github.io/feed.xml" ai)
          ("https://lilianweng.github.io/index.xml" ai)
          ))
  )

(provide 'init-elfeed)
