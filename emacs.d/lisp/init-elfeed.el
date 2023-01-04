;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package elfeed
  :init
  ;; (setq elfeed-db-directory (concat yx-share-data-path "/elfeed-db"))
  (setq-default elfeed-search-filter "@1-year-ago +unread")
  (setq elfeed-feeds
        '(
          ("https://liujiacai.net/atom.xml" emacs)
          ("https://protesilaos.com/codelog.xml" emacs)
          ("http://pragmaticemacs.com/feed/" emacs)
          ("https://karthinks.com/index.xml" hack tool)
          ("https://blog.evjang.com/feeds/posts/default" tech)
          ("https://ruder.io/rss/index.rss" ai nlp)
          ("https://www.inference.vc/rss" ai)
          ("https://karpathy.github.io/feed.xml" ai)
          ("https://lilianweng.github.io/index.xml" ai)
          ("https://lobste.rs/t/programming.rss" programming)
          ("https://lobste.rs/t/ai.rss" ai)
          ("https://lobste.rs/t/python.rss" python)
          ("https://lobste.rs/t/emacs.rss" emacs)
          ("https://emacstalk.github.io/podcast/index.xml" emacs)
          ("http://xahlee.info/emacs/emacs/blog.xml" emacs)
          ("https://lucidmanager.org/categories/productivity//index.xml" emacs)
          ("https://emacsredux.com/atom.xml" emacs)
          ("https://sachachua.com/blog/category/emacs/feed/" emacs)
          ("https://planet.emacslife.com/atom.xml" emacs)
          ("https://takeonrules.com/index.atom" tech)
          ("http://blog.lujun9972.win/emacs-document/rss.xml" emacs)
          ("https://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-only.xml" tech)
          ("https://linuxtoy.org/feeds/all.atom.xml" linux tool)
          ("http://www.howardism.org/index.xml" emacs)
          ("https://manateelazycat.github.io/feed.xml" emacs)

          ("https://tboox.org/feed.xml" c/c++)
          ("https://blog.codingnow.com/atom.xml" tech c/c++)

          ("https://www.agwa.name/blog/feed" crypto)

          ("https://news.ycombinator.com/rss" hacker)
          ("https://www.reddit.com/r/programming.rss" programming)
          ("https://www.reddit.com/r/emacs.rss" emacs)
          ("https://www.reddit.com/r/python.rss" python)

          ("https://wangyurui.com/feed.xml" 思考 人生 社会)
          ("http://www.4sbooks.com/feed" 人文 四季书评)
          ("http://feeds.initium.news/theinitium" 新闻)

          ("https://linuxhandbook.com/rss/" linux)
          ("http://www.ruanyifeng.com/blog/atom.xml" 技术 软件))
        )
  )

(provide 'init-elfeed)
