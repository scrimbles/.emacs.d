;;; news.el --- newsreader configuration -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Author: M Cooper Healy
;;
;;; Code:

(use-package newsticker
  :bind
  ("C-c C-r" . newsticker-show-news))
(setq newsticker-url-list-defaults nil)
(setq newsticker-url-list
      '(("XKCD"
         "https://xkcd.com/rss.xml"
         nil
         3600)
        ("Hacker News"
         "https://hnrss.org/newest?q=lisp+OR+emacs"
         nil
         3600)
        ("Analog Nowhere"
         "https://analognowhere.com/feed/rss.xml"
         nil
         3600)))


(provide 'news)
;;; news.el ends here
