;;; elfeed subscriptions
(setq elfeed-feeds
      `((,(with-temp-buffer
            (insert-file-contents
             (concat (getenv "HOME")
                     "/.config/secrets/github-private-atom"))
            (buffer-string))
         dashboard)
        ("https://repology.org/maintainer/sternenseemann%40systemli.org/feed-for-repo/nix_unstable/atom" dashboard releases)
        ("http://hundimbuero.blogspot.com/feeds/posts/default?alt=rss" blog cool-and-nice)
        ("gopher://text.causal.agency/0feed.atom" blog)
        ("http://xsteadfastx.org/feed/" blog cool-and-nice)
        ("https://hannes.robur.coop/atom" blog)
        ("https://stevelosh.com/rss.xml" blog)
        ("https://blog.benjojo.co.uk/rss.xml" blog)
        ("https://leahneukirchen.org/blog/index.atom" blog cool-and-nice)
        ("https://leahneukirchen.org/trivium/index.atom" blog links cool-and-nice)
        ("https://tazj.in/feed.atom" blog cool-and-nice)
        ("https://alyssa.is/feed.xml" blog cool-and-nice)
        ("https://eta.st/feed.xml" blog cool-and-nice)
        ("https://spectrum-os.org/git/www/atom/bibliography.html" links blog)
        ("https://rachelbythebay.com/w/atom.xml" blog)
        ("http://evrl.com/feed.xml" blog)
        ("https://vulns.xyz/feed.xml" blog)
        ("https://linuxrocks.online/@dnkl.rss" releases)
        ("https://www.german-foreign-policy.com/?type=9818" news)
        ("https://niedzejkob.p4.team/rss.xml" blog)
        ("https://grahamc.com/feed/" blog)
        ("https://michael.stapelberg.ch/feed.xml" blog)
        ("https://kazu-yamamoto.hatenablog.jp/feed" blog)
        ("https://bodil.lol/rss.xml" blog)
        ("http://blog.nullspace.io/feed.xml" blog)
        ("https://blog.kingcons.io/rss.xml" blog)
        ("http://jaspervdj.be/rss.xml" blog)
        ("https://christine.website/blog.rss" blog)
        ("https://drewdevault.com/feed.xml" blog)
        ("https://www.imperialviolet.org/iv-rss.xml" blog)
        ("https://latacora.micro.blog/feed.xml" blog)
        ("https://22gato.tumblr.com/rss" pictures cool-and-nice)
        ("https://theprofoundprogrammer.com/rss" blog)
        ("https://wiki.openlab-augsburg.de/_feed" openlab)
        ("http://shitopenlabsays.tumblr.com/rss" openlab)
        ("http://suckless.org/atom.xml" releases)
        ("https://kristaps.bsd.lv/lowdown/atom.xml" releases)
        ("https://www.tweag.io/rss.xml" blog)
        ("http://planet.haskell.org/atom.xml" planet blog)
        ("http://0pointer.net/blog/index.atom" blog)
        ("https://emacsninja.com/feed.atom" blog)
        ("https://emacshorrors.com/feed.atom" blog)
        ("http://therealmntmn.tumblr.com/rss" blog)
        ("http://blog.duangle.com/feeds/posts/default" blog)
        ("http://blog.johl.io/atom.xml" blog)
        ("http://blog.z3bra.org/rss/feed.xml" blog)
        ("http://ccc.de/de/rss/updates.xml" news)
        ;; ("http://fabienne.us/feed/" blog) ; database error
        ("http://feeds.feedburner.com/baschtcom" blog)
        ("http://ffaaaffaffaffaa.tumblr.com/rss" pictures)
        ("http://fnordig.de/feed.xml" blog)
        ("http://fotografiona.tumblr.com/rss" pictures)
        ("https://grandhotel-cosmopolis.org/de/feed" news)
        ("http://guteaussicht.org/rss" pictures)
        ("http://konvergenzfehler.de/feed/" blog)
        ("https://markuscisler.com/feed.xml" blog)
        ("http://n00bcore.de/feed/" podcast)
        ("http://spacethatneverwas.tumblr.com/rss" pictures)
        ("http://theresa.someserver.de/blog/?feed=rss2" blog)
        ("http://www.frumble.de/blog/feed/" blog)
        ("http://www.plomlompom.de/PlomRogue/plomwiki.php?action=Blog_Atom" blog)
        ("http://www.whvrt.de/rss" pictures)
        ("http://www.windytan.com/feeds/posts/default" blog)
        ("https://echtsuppe.wordpress.com/feed/" blog defunct)
        ("https://mgsloan.com/feed.xml" blog)
        ("https://notes.sterni.lv/atom.xml" me)
        ("http://arduina.net/feed/" defunct blog)
        ("https://anchor.fm/s/94bb000/podcast/rss" podcast)))
        ;; http://www.wollenzin.de/feed/ ;_;

(provide 'subscriptions)
