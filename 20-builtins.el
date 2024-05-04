;; -*- lexical-binding: t; -*-

(setopt auth-sources '("~/.authinfo")) ;; https://magit.vc/manual/ghub/Storing-a-Token.html
(setopt shr-max-image-proportion 0.5)
(setopt gnus-select-method '(nntp "news.eternal-september.org"))
(setopt mouse-yank-at-point t)
(setopt save-interprogram-paste-before-kill t)
(setopt select-enable-primary t)
(setopt enable-local-variables :all)
(setopt custom-safe-themes t)
(setopt message-log-max 8000)
(setopt kill-read-only-ok t)
(setopt kill-ring-max 600)
(setopt view-read-only t)
(setopt indent-tabs-mode nil)
(setopt debug-on-error t)
;; (delq 'user-error debug-ignored-errors) ;; Also enter debugger for `user-error'
(setopt vc-msg-copy-id-to-kill-ring nil)
(setopt shift-select-mode nil)
(setopt mouse-drag-and-drop-region-cross-program t) ;; no effect on wayland?
(setopt show-paren-context-when-offscreen t)
(setopt help-enable-variable-value-editing t)
(setopt proced-enable-color-flag t)
(setopt abbrev-suggest t)
(setopt use-short-answers t)
(setopt backtrace-on-redisplay-error nil)
(setq byte-compile-warnings '(not free-vars))
(setopt eval-expression-print-length 64)
(setopt eval-expression-print-level 16)

;; Don't clear my echo area
(setopt garbage-collection-messages nil)
(setopt auto-save-no-message t)
(setopt suggest-key-bindings nil) ;; prefer to show command's return value

;; Browse with Firefox or EWW depending on the link
(setopt browse-url-generic-program "firefox")
(setopt browse-url-handlers
        '(("github.com" . browse-url-generic)
          ("melpa.org" . browse-url-generic)
          ("fanfiction.net" . browse-url-generic)
          ;; Default
          ("." . eww-browse-url)))

;; No limit on recentf
;; I wonder if that slows it down?  (length recentf-list) is 2751 atm
(after! recentf
  (setopt recentf-max-saved-items nil))



;;; Calendar...
;; Modern phones are our calendars now, but it's less aggravating to add and
;; remove many events at once here.  (If only iOS or Android were user-friendly
;; enough to expose a simple YAML/TOML file for all system and app settings,
;; that'd be another story).  Emacs provides a neat solution: since org-agenda
;; integrates holiday.el info, and the iOS app Beorg can sync all agenda stuff
;; into the iOS calendar, adding it here adds it there.  Magic.

(setopt holiday-bahai-holidays nil)
(setopt holiday-hebrew-holidays nil)
(setopt holiday-islamic-holidays nil)
(setopt holiday-oriental-holidays nil)
(setopt calendar-view-holidays-initially-flag t)

;; Swedish holidays
(setopt holiday-general-holidays
        '((holiday-fixed 1 1 "New Year's Day")
          (holiday-fixed 3 8 "International Women's Day")
          (holiday-easter-etc) ;; Surprisingly complex calculations!
          (holiday-fixed 4 1 "April Fools' Day")
          (holiday-fixed 4 30 "Walpurgis Night")
          (holiday-float 5 0 -1 "Mother's Day")
          (holiday-fixed 6 24 "Midsummer")
          (holiday-fixed 10 31 "Halloween")
          (holiday-float 11 0 2 "Father's Day")
          (holiday-fixed 12 13 "Lucia")
          (holiday-fixed 12 24 "Christmas Eve")
          (holiday-fixed 12 31 "New Year's Eve")))

;; Personal holidays
(setopt holiday-other-holidays
        ;; Birthdays
        '((holiday-fixed 1 25 "Joel's birthday")
          (holiday-fixed 2 25 "Ann-Julie's birthday")
          (holiday-fixed 3 8 "Clarence's birthday")
          (holiday-fixed 4 1 "Karin's birthday")
          (holiday-fixed 4 11 "Griselda's birthday")
          (holiday-fixed 4 25 "Lena Duske's birthday")
          (holiday-fixed 4 29 "Thor's birthday")
          (holiday-fixed 5 5 "Jesus' birthday")
          (holiday-fixed 6 18 "Rickard's birthday")
          (holiday-fixed 6 27 "Yang Yu Ting's birthday")
          (holiday-fixed 7 5 "Nath's birthday")
          (holiday-fixed 9 13 "Tuyana's birthday")
          (holiday-fixed 9 24 "Lena A's birthday")
          (holiday-fixed 11 6 "Lore's birthday")
          (holiday-fixed 12 10 "Simon's birthday")

          ;; Other things
          (holiday-fixed 7 1 "Ignaz Semmelweis Day")
          (holiday-fixed 9 26 "Petrov Day")
          (holiday-fixed 10 27 "Arkhipov Day")))

;; This shouldn't be necessary, but somehow `calendar-holidays' is being
;; pre-set in my Emacs (and its defcustom has no :set-after).
(setopt calendar-holidays (append holiday-general-holidays
                                  holiday-other-holidays))

;; add these birthdays
;; - Cristina
;; - Laia
;; - Marc-Antoine
;; - Mirela
;; - Sammer
;; - Seda
;; - Tim
;; - Thor & Emil


;;; Fixes for buffer-cycling

;; Don't filter the buffer list when cycling.  How do people actually find the
;; filtered buffers when they want them?
(assoc-delete-all 'buffer-predicate default-frame-alist) ;; undoom
(set-frame-parameter nil 'buffer-predicate nil) ;; undoom
(setopt iflipb-ignore-buffers (lambda (&rest _) t))

;; Never bury buffers, so the buffer list is truly chronological and
;; unsurprising to cycle thru.  FWIW, might be worth knowing the command
;; `unbury-buffer' and using that instead -- but I would prefer if there was a
;; visual effect when a buffer gets buried, if I'm gonna have to keep track of
;; what got buried as opposed to just switched out.
(fset 'bury-buffer #'ignore)
(fset 'bury-buffer-internal #'ignore)
