;; -*- no-byte-compile: t; -*-

;; TODO Infer :host gitlab as well as :host nil
(defmacro pkg (url &rest keywords)
  "Expand into `package!' with pre-filled arguments.

If you were not going to specify any keywords, you can just use
`package!', but otherwise this macro has a simpler calling
convention and lets you skip boilerplate.

First argument URL expands into the boilerplate
\"package-name :recipe (:host github :repo \"partial-url\")\".

KEYWORDS can be any of :name, :type, :files, :depth, :branch.

- See documentation for most of those at
  https://github.com/radian-software/straight.el
- The keyword :type comes from Doom's `package!'
- The keyword :name is unique to this macro, for setting a
  package name that differs from the GitHub repo name

This macro does not implicitly quote any arguments."
  (let* ((type (plist-get keywords :type))
         (files (plist-get keywords :files))
         (depth (plist-get keywords :depth))
         (branch (plist-get keywords :branch)))
    `(package! ,(or (plist-get keywords :name)
                    (intern (replace-regexp-in-string
                             "^.*?github.com/.*?/" "" url)))
       ;; ,@(if (plist-get keywords :type) (list :type (plist-get keywords :type)))
       ,@(if type (list :type type))
       :recipe (:host github
                :repo ,(replace-regexp-in-string "^.*?github.com/" "" url)
                ;; Don't implicitly quote args (confusing)
                ,@(if branch (list :branch (doom-unquote branch)))
                ,@(if depth (list :depth (doom-unquote depth)))
                ,@(if files (list :files (doom-unquote files)))))))

;; ;; Macroexpand this
;; (package "https://github.com/minad/org-modern")
;; (pkg "https://github.com/minad/org-modern" :files '("*.texi") :type 'local)

;; ;; Testing
;; (package! org-modern :recipe (:host github :repo "minad/org-modern"))
(pkg "https://github.com/minad/org-modern")

;; Need during init
(package! named-timer)
;; (package! l) ;; fn literal so i can write (l'rainbow-mode 0)
;; (package! llama)
(package! defrepeater)
(package! el-patch)
(package! compat)
(package! dash)

;; My own packages
(package! asyncloop   :type 'local :recipe (:host github :repo "meedstrom/asyncloop" :depth full))
(package! massmapper  :type 'local :recipe (:host github :repo "meedstrom/massmapper" :depth full))
(package! deianira    :type 'local :recipe (:host github :repo "meedstrom/deianira" :depth full))
(package! eager-state :type 'local :recipe (:host github :repo "meedstrom/eager-state" :depth full))
(package! quickroam   :type 'local :recipe (:host github :repo "meedstrom/quickroam" :depth full))
(package! inline-anki :type 'local :recipe (:host github :repo "meedstrom/inline-anki" :depth full))
(package! org-node    :type 'local :recipe (:host github :repo "meedstrom/org-node" :depth full))
(package! eva         :type 'local :recipe (:host github :repo "meedstrom/eva" :depth full :files (:defaults "assets" "renv" "*.R" "*.gnuplot")))

;; Disable Doom-installed stuff
(package! org-crypt :disable t) ;; slowwww
(package! helpful :disable t) ;; slowwww
(package! which-key :disable t) ;; slowwww
(package! doom-snippets :disable t) ;; doom's yasnippets

(pkg "https://github.com/minad/org-modern")
(pkg "https://github.com/chen-chao/consult-ffdata")
(pkg "https://github.com/oantolin/math-delimiters")
(pkg "https://github.com/toshism/org-super-links")
(pkg "https://github.com/rtrppl/orgrr")
(pkg "https://github.com/SebastienWae/app-launcher")
(pkg "https://github.com/karthink/dired-hist")
(pkg "https://github.com/chainsawriot/ess-rproj")
(pkg "https://github.com/kickingvegas/casual-avy")
(pkg "https://github.com/kickingvegas/casual-dired")
(pkg "https://github.com/kickingvegas/casual")
(pkg "https://github.com/tangxinfa/firefox-bookmarks")
(pkg "https://github.com/kmonad/kbd-mode")
(pkg "https://github.com/manateelazycat/awesome-tray")
(pkg "https://github.com/Idorobots/gamify-el")


;; The rest
(package! academic-phrases)
(package! apheleia)
(package! artbollocks-mode)
(package! backup-walker)
(package! beginend)
(package! kv)
(package! bm)
;; (package! chatgpt-shell)
(package! bui)
(package! buttercup)
(package! calibredb)
(package! cape)
(package! circadian)
(package! consult)
(package! copy-as-format)
(package! ctrlf)
(package! corfu)
(package! crux)
(package! cycle-buffer) ;; last updated 1997, but more useful commands than iflipb
(package! director)
(package! dired-git-info)
(package! dired-hacks)
(package! disable-mouse)
(package! disk-usage)
(package! dmenu)
(package! doom-themes)
(package! elisp-format)
(package! elisp-autofmt)
(package! embark)
(package! transducers)
(package! esup)
(package! flycheck-package)
(package! form-feed)
(package! gif-screencast)
(package! git-messenger)
(package! git-timemachine)
(package! goggles)
(package! hacker-typer)
(package! helm-firefox)
(package! help-find)
;; (package! copilot)
(package! consult-org-roam)
(package! pocket-reader)
(package! hyperbole)
(package! format-all)
(package! iedit)
(package! iflipb) ;; vs cycle-buffer?
(package! iscroll)
(package! keymap-utils) ;; prefix kmu-*
(package! mastodon)
(package! org-ql)
(package! mediawiki)
(package! memoize)
(package! mw-thesaurus)
(package! nameless)
(package! objed) ;; for objed-ipipe
(package! package-lint)
(package! peep-dired)
(package! persist)
(package! pfuture)
(package! pinboard)
(package! pinboard-popular)
(package! dogears)
(package! prism)
(package! screencast)
(package! shelldon)
(package! ef-themes)
(package! ement)
(package! snitch)
(package! snow)
(package! tempel)
(package! elfeed)
(package! elfeed-org)
(package! tempel-collection)
(package! ts)
(package! vc-msg)
(package! vimgolf)
(package! visual-regexp)
(package! wgrep)
(package! xr)

;; Org
(package! org-anki)
(package! org-transclusion)
(package! org-roam)
(package! org-roam-ui)
(package! org-download)
(package! htmlize)
;; (package! org-recent-headings)
;; (package! org-roam-bibtex) ;; yes still relevant for org 9.5
;; (package! org-tanglesync)
;; (package! ox-rss)
;; (package! ox-rss)

;; Copypasta from Doom module (the complexity permits downloading a
;; shallow clone, normally you can't build Org from a shallow clone).
(package! org
  :recipe (:host github
           :repo "emacs-straight/org-mode"
           :files (:defaults "etc")
           :depth 1
           :build t
           :pre-build
           (progn
             (with-temp-file "org-loaddefs.el")
             (with-temp-file "org-version.el"
               (let ((version
                      (with-temp-buffer
                        (insert-file-contents (doom-path "lisp/org.el") nil 0 1024)
                        (if (re-search-forward "^;; Version: \\([^\n-]+\\)" nil t)
                            (match-string-no-properties 1)
                          "Unknown"))))
                 (insert (format "(defun org-release () %S)\n" version)
                         (format "(defun org-git-version (&rest _) \"%s-??-%s\")\n"
                                 version (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
                         "(provide 'org-version)\n"))))))
(package! org-contrib
  :recipe (:host github
           :repo "emacsmirror/org-contrib"))


;; (package! maple-minibuffer :recipe (:host github :repo "honmaple/emacs-maple-minibuffer"))
;; (package! sway)
;; (package! eot :recipe (:host github :repo ""))
;; (package! affe)
;; (package! exwm)
;; (package! exwm-edit)
;; (package! exwm-firefox :recipe (:host github :repo "ieure/exwm-firefox"))
;; (package! golden-ratio)
;; (package! helm-selector)
;; (package! key-assist)
;; (package! nov)
;; (package! popper)
;; (package! beancount :recipe (:host github :repo "cnsunyour/beancount.el"))
;; (package! forge)
;; (package! magit)
;; (package! repeaters :recipe (:host github :repo "mmarshall540/repeaters"))
;; (package! xah-elisp-mode) ;; try xah-elisp-prettify-root-sexp
;; (package! subed :recipe (:host github :repo "rndusr/subed"))
;; (package! cycle-region)
;; (package! jammer)
;; (package! frames-only-mode)
;; (package! frame-mode)
;; (package! fullframe)
;; (package! mini-frame)
;; (package! cdlatex)
;; (package! docker-tramp)
;; (package! emms)
;; (package! good-scroll)
;; (package! helm-bibtex)
;; (package! helm-navi)
;; (package! helm-org-recent-headings)
;; (package! literate-calc-mode)
;; (package! prodigy)
;; (package! weechat)

;;(package! taxy
;;  :recipe (:host github
;;           :repo "alphapapa/taxy.el"
;;           :files (:defaults "deffy.el" "*.el")))

;; Not tried out

;; (package! org-timeline)
;; (package! embrace)
;; (package! pulseaudio-control)
;; (package! suggest)
;; (package! osm)
;; (package! transmission)
;; (package! trashed)
;; (package! eshell-prompt-extras)
;; (package! navi-mode)
;; (package! sx)
