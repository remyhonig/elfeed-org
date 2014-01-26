rmh-elfeed-org
==============

  Configure the Elfeed RSS reader with an Orgmode file

# Problem

Maintaining tags for all my rss feeds is cumbersome using the regular flat list
where there is no hierarchy and tag names are duplicated a lot. For example
this is how elfeed users typically configure their subscriptions.

    (defvar elfeed-feeds-alist
      '(("http://threesixty360.wordpress.com/feed/" blog math)
        ("http://www.50ply.com/atom.xml" blog dev)
        ("http://blog.cryptographyengineering.com/feeds/posts/default" blog)
        ("http://abstrusegoose.com/feed.xml" comic)
        ("http://accidental-art.tumblr.com/rss" image math)
        ("http://english.bouletcorp.com/feed/" comic)
        ("http://curiousprogrammer.wordpress.com/feed/" blog dev)
        ("http://feeds.feedburner.com/amazingsuperpowers" comic)
        ("http://amitp.blogspot.com/feeds/posts/default" blog dev)
        ("http://pages.cs.wisc.edu/~psilord/blog/rssfeed.rss" blog)
        ("http://www.anticscomic.com/?feed=rss2" comic)
        ("http://feeds.feedburner.com/blogspot/TPQSS" blog dev)))

# Solution

Org-mode makes the book keeping of tags and feeds much easier. Tags get
inherited from parent headlines so there is no need to specify tags for each
and every feed.

    * Technolog                                                             :tech:
     :PROPERTIES:
     :ID: elfeed
     :END:
      * entry-title: \(linux\|linus\|ubuntu\|kde\|gnome\)                  :linux:
      * http://git-annex.branchable.com/design/assistant/blog/index.rss :mustread:
      * http://feeds.feedburner.com/InformationIsBeautiful
      * Software Development                                                 :dev:
        * Emacs                                                   :emacs:mustread:
          * http://www.terminally-incoherent.com/blog/feed
          * http://nullprogram.com/feed
          * entry-title: \(emacs\|org-mode\)
          * http://planet.emacsen.org/atom.xml
        * Web Development                                                    :web:
          * http://planet.phpunit.de/atom.xml
          * http://feeds.feedburner.com/symfony/blog
          * http://feeds.feedburner.com/qooxdoo/blog/content
        * Eclipse                                                        :eclipse:
          * http://blog.eclipse-tips.com/feeds/posts/default?alt=rss
          * http://ed-merks.blogspot.com/feeds/posts/default
          * http://feeds.feedburner.com/eclipselive
          * http://www.fosslc.org/drupal/rss.xml                           :video:


A few tips for the org-mode feed configuration:

-   The tree must have an ID property

-   Feeds must start with "http"

-   Tag rules must start with "entry-title: "

-   Everything else in the tree is ignored.

# Usage

In your initialization script add the following:

    ;; Load this extension.
    (load "~/Development/rmh-elfeed-org/rmh-elfeed-org.el")
    
    ;; Elfeed.org contains the feed subscriptions.
    (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
    
    ;; Use an advice to load the configuration.
    (defadvice elfeed (before configure-elfeed activate)
      "Load all feed settings before elfeed is started."
      (rmh-elfeed-org-configure))
