
;; (package! vterm :ignore t) ;; let guix install it
(package! org-roam)
(package! org-roam-server)
(package! org-roam-bibtex)
(package! org-ref)
(package! fish-mode)
(package! org-noter)
(package! bm)
(package! crux)
(package! el-patch)
;; (package! auto-compile)
(package! org-tanglesync)
(package! org-download)
(package! helm-bibtex)
(package! helm)
(package! ivy-bibtex)
(package! guix)
(package! stan-mode)
(package! academic-phrases)
(package! rainbow-blocks)
(package! company-org-roam)
(package! beancount :recipe (:host github :repo "cnsunyour/beancount.el"))
(package! artbollocks-mode)
(package! sicp)
(package! hyperbole)
(package! disable-mouse)
(package! eshell-z)
;; (package! exwm-mff)
(package! key-assist)
(package! backup-walker)
(package! cdlatex)
(package! literate-calc-mode)
(package! elcontext)
(package! prodigy)
(package! synosaurus)
(package! emacs-piper :recipe (:host gitlab :repo "howardabrams/emacs-piper"))
(package! vc-msg)
(package! git-messenger)
(package! feebleline)
(package! mini-modeline)
(package! unfill)
(package! hacker-typer)
(package! emms)
(package! doom-snippets :ignore t) ;; disable doom's yasnippets
;; (package! github-review :ignore t) ;; causes errors
(package! winner :ignore t)
(package! ts)
(package! org-pomodoro)
;; (package! unpackaged :recipe (:host github :repo "alphapapa/unpackaged.el"))
(package! navi-mode)
(package! helm-navi)
;; (package! delve)
(package! org-recent-headings)
;; (package! helm-org-recent-headings)
(package! org-drill)
(package! mediawiki)
(package! ctrlf)
(package! selectrum)
;(package! magit) ;; because magit-todos is B R O K E N
;(package! forge)
(package! deferred)
(package! consult)
(package! marginalia)
(package! embark)
;; (package! mini-frame)
(package! wgrep)
(package! exwm)
(package! exwm-edit)
(package! exwm-firefox  :recipe (:host github :repo "ieure/exwm-firefox"))
;; (package! snow)
(package! sx)
(package! suggest)
(package! nameless)
(package! package-lint)
(package! named-timer)
(package! key-chord)
(package! fullframe)
(package! form-feed)
(package! org-journal)
(package! esup)
(package! rainbow-mode)
(package! hercules)
(package! objed)
(package! org-timeline)
(package! embrace)
(package! orderless)
(package! anki-editor)
(package! llama)
(package! subed :recipe (:host github :repo "rndusr/subed"))
(package! matrix-client)
(package! weechat)
(package! dmenu)

;; Copypasta from https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/packages.el
(package! org-mode
  :recipe (:host github
           ;; Use a mirror because code.orgmode.org runs on a potato.
           :repo "emacs-straight/org-mode"
           :files ("*.el" "lisp/*.el" "contrib/lisp/*.el" "contrib/scripts")
           ;; HACK You need either this or :depth full. 
           ;; https://github.com/hlissner/doom-emacs/issues/4248#issuecomment-725194428
           :pre-build
           (with-temp-file (doom-path (straight--repos-dir "org-mode") "org-version.el")
             (insert "(fset 'org-release (lambda () \"9.5\"))\n"
                     "(fset 'org-git-version #'ignore)\n"
                     "(provide 'org-version)\n"))
           ;; Prevent built-in Org from sneaking into the byte-compilation of
           ;; `org-plus-contrib', and inform other packages that `org-mode'
           ;; satisfies the `org' dependency: raxod502/straight.el#352
           :includes (org org-plus-contrib)))
