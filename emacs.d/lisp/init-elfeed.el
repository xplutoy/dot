;; -*- coding: utf-8; lexical-binding: t; -*-
(yx-require-package 'elfeed)

(with-eval-after-load 'elfeed
  (define-key elfeed-search-mode-map (kbd "j") #'next-line)
  (define-key elfeed-search-mode-map (kbd "k") #'previous-line)
  (define-key elfeed-search-mode-map (kbd "l") #'(lambda ()
                                                   (interactive)
                                                   (switch-to-buffer (elfeed-log-buffer))))
  )

(setq elfeed-db-directory (concat user-emacs-directory "elfeed_db"))
(setq-default elfeed-search-filter "@1-year-ago +unread ")
(setq elfeed-feeds
      '(
        ("https://liujiacai.net/atom.xml" emacs)
        ("https://protesilaos.com/codelog.xml" emacs)
        ("http://pragmaticemacs.com/feed/" emacs)
        ("https://karthinks.com/index.xml" hack tool)
        ("https://www.zhihu.com/rss")
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

        ("https://wangyurui.com/feed.xml" 思考 人生 社会)
        ("http://www.4sbooks.com/feed" 人文 四季书评)

        ("https://www.tmtpost.com/feed" 科技 商业)
        ("https://36kr.com/feed" 商业)
        ("http://feeds.initium.news/theinitium" 新闻)
        ("https://wanqu.co/feed" 创业 技术)
        ("https://feedx.fun/rss/wsj.xml" 新闻)

        ("https://linux.cn/rss.xml" linux 技术)
        ("http://www.ruanyifeng.com/blog/atom.xml" 技术 软件)

        ("https://api.feeddd.org/feeds/613381f91269c358aa0eac99" 机器之心 ai)
        ("https://api.feeddd.org/feeds/621321d4dca58a380c66b2ef" 每周论文 ai)
        )
      )

(provide 'init-elfeed)
