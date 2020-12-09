
(xt-deftest rmh-elfeed-org-remove-elfeed-tag
  (xt-note "The elfeed tag should not be assigned to the feeds")
  (xt-should (equal
              (rmh-elfeed-org-cleanup-headlines '(("" elfeed tag1) ("" elfeed tag2)) 'elfeed)
              '(("" tag1) ("" tag2)))))

(xt-deftest rmh-elfeed-org-convert-headline-to-tagger-params
  (xt-note "Get paramemeters to create elfeed tagger")
  (xt-should (equal
              (rmh-elfeed-org-convert-headline-to-tagger-params '("entry-title:   hoi " tag0 tag1))
              '("hoi" (tag0 tag1)))))

(xt-deftest rmh-elfeed-org-trees-with-tag
  (xt-note "Use any number of trees tagged with \"elfeed\"")
  (xtd-should 'xt-trees-with-id-length
              ("test/fixture-no-ids-or-tags.org" 0)
              ("test/fixture-one-tag.org" 1)
              ("test/fixture-two-tags.org" 2)))

(xt-deftest rmh-elfeed-org-trees-with-id
  (xt-note "Use any number of trees with the id property \"elfeed\"")
  (xtd-should 'xt-trees-with-id-length
              ("test/fixture-no-ids-or-tags.org" 0)
              ("test/fixture-one-id2.org" 1)
              ("test/fixture-two-ids.org" 2)))

(xt-deftest rmh-elfeed-org-convert-tree-to-headlines
  (xt-note "Recusively include all feeds in a tree with their tags inherited from their parents")
  (xtd-should 'xt-feeds
              ("test/fixture-no-ids-or-tags.org" nil)
              ("test/fixture-one-tag.org"
               (("http1" elfeed tag1) ("http2" elfeed)))
              ("test/fixture-two-tags.org"
               (("http1" elfeed) ("http2" elfeed)))))

(xt-deftest rmh-elfeed-org-filter-taggers
  (xt-note "Make sure tagging rules are recognized")
  (xt-should (equal
              (rmh-elfeed-org-filter-taggers
               (rmh-elfeed-org-import-headlines-from-files '("test/fixture-feed-formats.org") "elfeed"))
              '(("entry-title: \\(linux\\|gnome\\)" tag3)
                ("entry-title: \\(apple\\)" tag3)))))

(xt-deftest rmh-elfeed-org-convert-headline-to-tagger-params
  (xt-note "Entry titles are converted to elfeed structures")
  (xt-should (equal
              (rmh-elfeed-org-convert-headline-to-tagger-params '("entry-title: \\(apple\\)" tag3 tag4))
              '("\\(apple\\)" (tag3 tag4)))))

(xt-deftest rmh-elfeed-org-import-headlines-from-files
  (xt-note "Make sure all types of headlines are recognized")
  (xt-should (equal
              (rmh-elfeed-org-import-headlines-from-files '("test/fixture-feed-formats.org") "elfeed")
              '(("http://url" tag3 tag1)
                ("http://namedorgmodelink" tag3 "namedorgmodelink")
                ("http://unnamedorgmodelink" tag3)
                ("http://abbreviatedlink" tag3 "abbreviatedlink")))))

(xt-deftest rmh-elfeed-org-import-headlines-from-files
  (xt-note "Use all feeds in a multiple trees tagged with the \"elfeed\" tag and inherited their parent's tags")
  (xt-should (equal
              (rmh-elfeed-org-import-headlines-from-files '("test/fixture-one-tag.org" "test/fixture-two-ids.org") "elfeed")
              '(("http1" tag1) ("http2") ("http1" tag0 tag1) ("http2" tag2)))))

(xt-deftest rmh-elfeed-org-headlines-and-entrytitles-from-files
  (xt-note "Use all feeds in multiple trees tagged with the \"elfeed\" tag and inherited their parent's tags")
  (xt-should (equal
              (rmh-elfeed-org-import-headlines-from-files '("test/fixture-one-tag.org" "test/fixture-entry-title.org") "elfeed")
              '(("http1" tag1) ("http2") ("entry-title 1" tag1)))))

(xt-deftest rmh-elfeed-org-unique-headlines-and-entrytitles-from-files
  (xt-note "Should not return duplicate feeds, in this case two \"http2\" entries")
  (xt-should (equal
              (rmh-elfeed-org-import-headlines-from-files '("test/fixture-one-tag.org" "test/fixture-entry-title.org") "elfeed")
              '(("http1" tag1) ("http2") ("entry-title 1" tag1)))))

(xt-deftest rmh-elfeed-org-feeds-get-from-with-none-found
  (xt-note "Make sure no nil values instead of feeds are returned")
  (xt-should (equal
              (rmh-elfeed-org-import-headlines-from-files '("test/fixture-one-tag.org" "test/fixture-one-id-no-feeds.org") "elfeed")
              '(("http1" tag1) ("http2")))))

(xt-deftest rmh-elfeed-org-import-headlines-and-ignore-tags
  (xt-note "Use all feeds in a multiple trees tagged with the \"elfeed\" tag and not match the \"ignore\" tag and inherited their parent's tags")
  (xt-should (equal
              (rmh-elfeed-org-import-headlines-from-files '("test/fixture-ignore-tag.org") "elfeed")
              '(("http1" tag1) ("http3" tag3)))))

(xt-deftest rmh-elfeed-org-mark-feed-ignore
  (xt-note "Mark special feed with tag ignore.")
  (xt-should (equal
              (let* ((rmh-elfeed-org-files '("test/fixture-mark-feed-ignore.org")))
                (rmh-elfeed-org-mark-feed-ignore "http://invalidurl")
                (with-current-buffer (get-buffer "fixture-mark-feed-ignore.org")
                  (buffer-string)))

              "* elfeed tree :elfeed:
** http://invalidurl :tag1:ignore:
** [[http://namedorgmodelink][namedorgmodelink]]
** [[http://invalidurl]] :ignore:
* other tree
** http://invalidurl
")))

(xt-deftest rmh-elfeed-org-gets-inherited-tags2
  (xt-note "Get all headlines with inherited tags")
  (xtd-return= (lambda (_) (progn (org-mode)
                             (rmh-elfeed-org-convert-tree-to-headlines
                              (org-element-parse-buffer 'headline))))
               ("
* tree1 :elfeed:
** http1 :tag1:
** tree2 :tag2:
*** http2 :tag3:
** http4 :tag5:
* tree3 :elfeed:
** http3 :tag4:"
                '(("tree1" elfeed)
                  ("http1" elfeed tag1)
                  ("tree2" elfeed tag2)
                  ("http2" elfeed tag2 tag3)
                  ("http4" elfeed tag5)
                  ("tree3" elfeed)
                  ("http3" elfeed tag4)))))

(xt-deftest rmh-elfeed-org-test-filter
  (xt-note "Get headlines filtered")
  (xtd-return= (lambda (_) (progn (org-mode)
                             (rmh-elfeed-org-filter-relevant
                              (rmh-elfeed-org-convert-tree-to-headlines
                               (org-element-parse-buffer 'headline)
                               ))))
               ("
* tree1 :elfeed:
** http1 :tag1:
** entry-title :tag2:
*** http2 :tag3:
** http4 :tag5:
* tree3 :elfeed:
** http3 :tag4:"
                '(("http1" elfeed tag1)
                  ("entry-title" elfeed tag2)
                  ("http2" elfeed tag2 tag3)
                  ("http4" elfeed tag5)
                  ("http3" elfeed tag4)))))

(xt-deftest rmh-elfeed-org-test-cleanup
  (xt-note "The tag of the root tree node should not be included.")
  (xtd-return= (lambda (_) (progn (org-mode)
                             (rmh-elfeed-org-cleanup-headlines
                              (rmh-elfeed-org-convert-tree-to-headlines
                               (org-element-parse-buffer 'headline)
                               ) 'elfeed)))
               ("
* tree1 :elfeed:
** http1 :tag1:
** tree2 :tag2:
*** http2 :tag3:
** http4 :tag5:
* tree3 :elfeed:
** http3 :tag4:"
                '(("tree1")
                  ("http1" tag1)
                  ("tree2" tag2)
                  ("http2" tag2 tag3)
                  ("http4" tag5)
                  ("tree3")
                  ("http3" tag4)))))

(xt-deftest rmh-elfeed-org-test-flagging
  (xt-note "Wrongly formatted headlines are tagged to be ignored during import.")
  (xtd-setup= (lambda (_)
                (org-mode)
                (let ((parsed-org (org-element-parse-buffer 'headline)))
                  (delete-region (point-min) (point-max))
                  (insert (org-element-interpret-data
                           (rmh-elfeed-org-flag-headlines parsed-org)))))
              ("* tree1 :elfeed:\n-!-"
               "* tree1 :_flag_:elfeed:\n-!-"
               )))

(xt-deftest rmh-elfeed-org-export-opml
  (xt-note "Export to OPML.")
  (xt-should (equal
              (let* ((rmh-elfeed-org-files '("test/fixture-export.org")))
                (elfeed-org-export-opml)
                (with-current-buffer "*Exported OPML Feeds*" (buffer-string)))
              "<?xml version=\"1.0\"?>
<opml version=\"1.0\">
  <head>
    <title>Elfeed-Org Export</title>
  </head>
  <body>
      <outline title=\"Unknown\" xmlUrl=\"http://url\"/>
      <outline title=\"namedorgmodelink\" xmlUrl=\"http://namedorgmodelink\"/>
      <outline title=\"linkwithspecialcode\" xmlUrl=\"http://linkwith&amp;specialcode\"/>
      <outline title=\"folder1\">
        <outline title=\"Unknown\" xmlUrl=\"http://unnamedorgmodelink\"/>
      </outline>
      <outline title=\"folder2\">
        <outline title=\"Unknown\" xmlUrl=\"http://url\"/>
      </outline>
  </body>
</opml>
")))

(xt-deftest rmh-elfeed-org-import-opml
  (xt-note "Import from OPML.")
  (xt-should (equal
              (progn
                (elfeed-org-import-opml "test/fixture-import.opml")
                (with-current-buffer "*Imported Org Feeds*" (buffer-string)))
              "* Imported Feeds            :elfeed:
** [[http://url][url]]
** [[http://namedorgmodelink][namedorgmodelink]]
** folder1
*** [[http://unnamedorgmodelink][url1]]
** folder2
*** [[http://url][url2]]
")))
