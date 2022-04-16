elfeed-org
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

If you want to add a custom title to a feed, that is even more cumbersome...

# Solution

Org-mode makes the book keeping of tags and feeds much easier. Tags get
inherited from parent headlines so there is no need to specify tags for each
and every feed.

    * Blogs                                                              :elfeed:
    ** entry-title: \(linux\|linus\|ubuntu\|kde\|gnome\)                  :linux:
    ** http://git-annex.branchable.com/design/assistant/blog/index.rss :mustread:
    ** http://feeds.feedburner.com/InformationIsBeautiful
    ** [[http://orgmode.org][Org Mode Links supported as well]]
    ** Software Development                                                 :dev:
    *** Emacs                                                    :emacs:mustread:
    **** http://www.terminally-incoherent.com/blog/feed
    **** http://nullprogram.com/feed
    **** entry-title: \(emacs\|org-mode\)
    **** http://planet.emacsen.org/atom.xml
    **** [[toobnix:154][EmacsConf]]
    *** Web Development                                                     :web:
    **** http://planet.phpunit.de/atom.xml
    **** http://feeds.feedburner.com/symfony/blog
    **** http://feeds.feedburner.com/qooxdoo/blog/content
    *** Eclipse                                                         :eclipse:
    **** http://blog.eclipse-tips.com/feeds/posts/default?alt=rss
    **** http://ed-merks.blogspot.com/feeds/posts/default
         A description of a feed can be written under any headline.
         The text will be ignored by elfeed.
    **** http://feeds.feedburner.com/eclipselive                         :ignore:
    **** http://www.fosslc.org/drupal/rss.xml                             :video:
    #+link toobnix https://toobnix.org/feeds/videos.xml?videoChannelId=


Some highlights:

* `[[http://orgmode.org][Org Mode Links supported as well]]` If you choose to use org-mode links, the link description will be used as the feed's title in Elfeed. This is useful for feeds with long titles.
* `entry-title: \(emacs\|org-mode\)` Tags any blog post with "emacs" or "org-mode" in the title according to it's given and inherited tags (in the example above "emacs", "mustread" and "dev").
* `http://feeds.feedburner.com/eclipselive :ignore:` The "ignore" excludes the feed completely from your checked subscriptions. You can use another name for "ignore" by customizing `rmh-elfeed-org-ignore-tag`. These "ignore" tags are set by elfeed-org automatically on feed errors (wrong urls or renamed feeds for example) if `rmh-elfeed-org-auto-ignore-invalid-feeds` is set to `t`.

## Import/Export OPML

Use `elfeed-org-export-opml` to export the tree structure of feeds to OPML format to a temporary buffer. This will allow you to make edits before saving it. You will probabably want to do this because most readers don't allow more than 2 levels while elfeed-org has no limit.

Use `elfeed-org-import-opml` to import an OPML file to an elfeed-org structured tree.

## The Configuration Tree

You can use multiple trees. Those trees can be in same file or spread
across multiple files specified in the list
`rmh-elfeed-org-files`. Why would I want to use multiple trees? You
can have a tree in a file that you may want to share with your
`emacs.d` on GitHub and keep your personal feeds in a separate private
repository.

In the initial version of this package the tree is identified by
setting the ID property to to the value of `rmh-elfeed-org-tree-id`
("elfeed" by default) but that property turns out to be special (see
the
[org mode manual](http://orgmode.org/manual/Special-properties.html#Special-properties))
and I should not have used it. To not break any existing users'
configurations this is still allowed but I recommend to tag the tree
now with the value of `rmh-elfeed-org-tree-id`.

So instead of

    * Blogs
     :PROPERTIES:
     :ID: elfeed
     :END:

I recommend to use

    * Blogs                                                              :elfeed:

# Tips
A few tips for the org-mode feed configuration:

* The tree should have a tag matching the string specified in the
  variable `rmh-elfeed-org-tree-id`.
* Feeds must start with `http`, or be in
  [org-mode link format](http://orgmode.org/org.html#Link-format) (the
  URL should still start with `http`).
* Tag rules must start with `entry-title: ` and end with a regular expression.
* A tag rule tags all the posts that match the regular expression in the title
  using a "tag hook" with `elfeed-make-tagger`. For more info see https://github.com/skeeto/elfeed
* You may add text below the headline with the url to describe the
  feed or to add notes. They will be ignored.
* Headlines not starting with `http` or `entry-title: ` will be ignored.

# Installation

## Requirements

Runs on emacs versions specified in `.github/workflows/ci.yml`

This package needs `org-mode 8.2.7` to run properly if you use the `emacs-24` distribution. The reason that that org-mode version is needed is because `org-mode 8.2.6` - at least in combination with `GNU Emacs 24.4.50.1` - causes the error `(error "recenter'ing a window that does not display current-buffer.")`.

The minimum version of org-mode that is minimally needed to even run is `org-mode 8.1` as that is the first release that defined the function `org-element-map` which is critical for this package.

## Spacemacs
[![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)

This package is part of the excellent (Spacemacs)[http://spacemacs.org/]. See [Elfeed Layer Documentation](http://spacemacs.org/layers/+web-services/elfeed/README.html) for installation instructions.

## Through MELPA
[![MELPA](http://melpa.org/packages/elfeed-org-badge.svg)](http://melpa.org/#/elfeed-org)

    ;; Install through package manager
    M-x package-install <ENTER>
    elfeed-org <ENTER>

## Manual

Download elfeed-org

    cd ~/.emacs.d/lisp
    wget https://github.com/remyhonig/elfeed-org/blob/master/elfeed-org.el

Install the package in Emacs

    C-x C-f ~/.emacs.d/lisp/elfeed-org.el <ENTER>
    M-x package-install-from-buffer <ENTER>

# Initialization

In your initialization script add the following:

    ;; Load elfeed-org
    (require 'elfeed-org)

    ;; Initialize elfeed-org
    ;; This hooks up elfeed-org to read the configuration when elfeed
    ;; is started with =M-x elfeed=
    (elfeed-org)

    ;; Optionally specify a number of files containing elfeed
    ;; configuration. If not set then the location below is used.
    ;; Note: The customize interface is also supported.
    (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))
    
# Feature requests and bug fixes

I appreciate feature requests and code snippets provided to fix things, but I really don't have much spare time nowadays. If you'd like to add a feature or fix a bug, please post a pull requests and I will merge it right away if has a corresponding test and an update of the README if applicable. I just migrated the CI from Travis - which was not functional anymore - to Github Action so this should be a faster process now. And to close off: a big thank you to all PR authors and issue submitters!
