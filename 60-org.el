;; -*- lexical-binding: t; -*-

(require 'my-lib-external)

(add-hook 'delve-mode-hook #'delve-compact-view-mode)
(add-hook 'lister-mode-hook #'View-exit)
(after! delve
  ;; It normally inherits from org-roam-title, which I find too big
  (set-face-attribute 'delve-title-face () :inherit 'org-document-title))

;; undoom
(after! org
  (remove-hook 'read-only-mode-hook 'doom-docs--toggle-read-only-h))
;; Crude but guaranteed to work
(fset 'doom-docs-org-mode #'ignore)
(fset 'doom-docs--toggle-read-only-h #'ignore)

(after! org
  (org-recent-headings-mode)
  (setopt org-startup-folded 'fold)
  ;; Undoom. Having exactly two states makes for comfy toggling.
  (setopt org-todo-keywords '((sequence "TODO" "DONE"))))

(after! org-roam-node
  ;; Use my own slug style
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (my-slugify (org-roam-node-title node))))

(add-hook 'org-roam-buffer-postrender-functions #'magit-section-show-level-2)
(setopt org-roam-directory "/home/kept/roam/")
(setopt org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%H:%M>\n%?" :if-new
         (file+head "%<%Y-%m-%d>.org" "#+title: [%<%Y-%m-%d>]\n#+filetags: :personal:\n")
         :immediate-finish t
         :jump-to-captured t)))

;; See also C-h d m :tools biblio
(setopt citar-bibliography '("/home/kept/roam/refs/library_biblatex.bib"))

(add-hook 'org-noter-notes-mode-hook #'abbrev-mode)
(add-hook 'org-noter-notes-mode-hook (l'rainbow-delimiters-mode 0))
(add-hook 'org-mode-hook #'my-org-prettify)
;;(add-hook 'org-mode-hook #'org-resolve-clocks 95)
;;(add-hook 'org-mode-hook #'rainbow-delimiters-mode)
;;(add-hook 'org-mode-hook #'org-clock-persistence-insinuate)
(add-hook 'org-clock-in-hook #'org-clock-save)
(add-hook 'text-mode-hook (defun my-kill-smartparens () (smartparens-mode 0)))

;; see Org-roam UI in Chromium's kiosk mode
(setopt browse-url-chromium-arguments '("--app=http://localhost:35901"))
(setopt org-roam-ui-browser-function #'browse-url-chromium)
(when guix
  (add-to-list 'browse-url-chromium-arguments "--no-sandbox"))

;; Modified version of `org-roam-node-slug'
(defun my-slugify (title)
    (let ((slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768 ; U+0300 COMBINING GRAVE ACCENT
                             769 ; U+0301 COMBINING ACUTE ACCENT
                             770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771 ; U+0303 COMBINING TILDE
                             772 ; U+0304 COMBINING MACRON
                             774 ; U+0306 COMBINING BREVE
                             775 ; U+0307 COMBINING DOT ABOVE
                             776 ; U+0308 COMBINING DIAERESIS
                             777 ; U+0309 COMBINING HOOK ABOVE
                             778 ; U+030A COMBINING RING ABOVE
                             779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                             780 ; U+030C COMBINING CARON
                             795 ; U+031B COMBINING HORN
                             803 ; U+0323 COMBINING DOT BELOW
                             804 ; U+0324 COMBINING DIAERESIS BELOW
                             805 ; U+0325 COMBINING RING BELOW
                             807 ; U+0327 COMBINING CEDILLA
                             813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814 ; U+032E COMBINING BREVE BELOW
                             816 ; U+0330 COMBINING TILDE BELOW
                             817 ; U+0331 COMBINING MACRON BELOW
                             )))
      (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s) (string-glyph-compose
                                              (apply #'string
                                                     (seq-remove #'nonspacing-mark-p
                                                                 (string-glyph-decompose s)))))
                 (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(
                        ("[[:space:]]+" . "-")
                        ("[^[:alnum:][:digit:]+=-]" . "")
                        ("--*" . "-")                  
                        ("^-" . "")
                        ("-$" . "")
                        ;; ("-a-" . "-")
                        ;; ("-the-" . "-")
                        ;; ("-i-" . "-")
                        ;; ("-in-" . "-")
                        ;; ("-of-" . "-")
                        ;; ("-is-" . "-")
                        ;; ("-to-" . "-")
                        ;; ("-as-" . "-")
                        ;; ("-that-" . "-")
                        ;; ("-are-" . "-")
                        ;; ("-you-" . "-")
                        ("-\\+-" . "+")
                        ("-=-" . "=")
                        ))
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          (downcase slug)))))

;; (my-slugify "A/B testing")
;; (my-slugify "Someday/Maybe whale carcass")
;; (my-slugify "No one can feel a probability that small")
;; (my-slugify "\"But there's still a chance, right?\"")
;; (my-slugify "Löb's Theorem")
;; (my-slugify "How to convince me that 2 + 2 = 3")
;; (my-slugify "C. S. Peirce")
;; (my-slugify "Do one thing at a time")
;; (my-slugify "Are you losing items in recentf, bookmarks, org-id-locations? Solution: Run kill-emacs-hook periodically.")

(defun my-rename-roam-file-by-title (&optional path title)
  (interactive)
  (unless path
    (setq path (buffer-file-name)))
  (unless (equal "org" (file-name-extension path))
    (error "Unexpected that file doesn't end in .org, halting on: %s" path))
  (unless title
    (with-temp-buffer
      (insert-file-contents path)
      (let ((case-fold-search t))
        (setq title (save-excursion
                      (goto-char (point-min))
                      (when (search-forward "#+title: " nil t)
                        (buffer-substring (point) (line-end-position))))))))
  (let* ((filename-preamble
          (when (string-match-p (rx (= 4 digit) "-" (= 2 digit) "-" (= 2 digit))
                                (file-name-nondirectory path))
            (substring (file-name-nondirectory path) 0 10)))
         (slugified-path (concat (file-name-directory path)
                                 filename-preamble
                                 "-"
                                 (my-slugify title)
                                 ".org"))
         (visiting (find-buffer-visiting path)))
    (unless (equal slugified-path path)
      (if (and visiting (buffer-modified-p visiting))
          (message "Unsaved file, letting it be: %s" path)
        (when visiting
          (kill-buffer visiting))
        (and (file-writable-p path)
             (file-writable-p slugified-path)
             (rename-file path slugified-path))
        (when visiting
          (find-file slugified-path))))))

(add-hook 'org-export-before-parsing-functions #'my-add-backlinks-if-roam)

(defun my-add-backlinks-if-roam (&rest _)
  (when (ignore-errors (org-roam-node-at-point))
    (let ((backlinks nil))
      (dolist (obj (org-roam-backlinks-get (org-roam-node-at-point)
                                           :unique t))
        (let ((node (org-roam-backlink-source-node obj)))
          (cl-pushnew (cons (org-roam-node-id node) (org-roam-node-title node)) backlinks)))
      (when backlinks
        (if (bobp)
            (progn
              (goto-char (point-max))
              (insert "* What links here"))
          (org-insert-subheading nil)
          (insert "What links here"))
        (dolist (backlink backlinks)
          (newline)
          (insert "- [[id:" (car backlink) "][" (cdr backlink) "]]"))))))


(defun my-publish-to-blog (plist filename pub-dir)
  ;; (my-rename-roam-file-by-title filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (let* ((case-fold-search t)
           (title (save-excursion
                    (goto-char (point-min))
                    (when (search-forward "#+title: " nil t)
                      (buffer-substring (point) (line-end-position)))))
           (planted-date (save-excursion
                   (goto-char (point-min))
                   (when (search-forward "#+date: " nil t)
                     (buffer-substring (1+ (point)) (1- (line-end-position))))))
           (org-html-extension ""))

      ;; Black box
      (org-publish-org-to 'html filename org-html-extension plist pub-dir)

      ;; Some logic borrowed from `org-publish-org-to' 2023-02-02
      (let* ((org-inhibit-startup t)
             (visiting (find-buffer-visiting filename))
             (work-buffer (or visiting (find-file-noselect filename))))
        (unwind-protect
            (with-current-buffer work-buffer
              (let* ((output (org-export-output-file-name org-html-extension nil pub-dir))
                     (output-buf (find-buffer-visiting output))
                     (was-opened nil)
                     (relative-slug (replace-regexp-in-string "^.*/posts/" "" output))
                     (data `((slug . ,relative-slug)
                             (title . ,title)
                             (date . ,planted-date)
                             (content . nil))))
                (unless (and title planted-date)
                  (delete-file output)
                  (message "FILE LACKING TITLE OR DATE: %s" output))
                ;; This file has no body because it met :exclude-tags, idk why it gets created
                (if (= 0 (doom-file-size output))
                    (progn
                      (delete-file output)
                      (message "File deleted because empty: %s" output))
                  (when (and title planted-date)
                    (when output-buf
                      (setq was-opened t)
                      (unless (buffer-modified-p output-buf)
                        (kill-buffer output-buf)))
                    (with-temp-file output
                      (insert
                       (with-temp-buffer
                         (insert "\n<h1>" title "</h1>")
                         (insert "\n<p>Planted " planted-date "</p>")
                         (insert-file-contents output)

                         ;; Adjust the result from `my-add-backlinks-if-roam'
                         (goto-char (point-max))
                         (when (search-backward "What links here<" nil t)
                           (goto-char (line-beginning-position))
                           (while (search-forward "h2" (line-end-position) t)
                             (replace-match "h1" nil t)))

                         (setf (alist-get 'content data) (buffer-string))
                         (json-encode data))))
                    (when was-opened
                      (find-file-noselect output))))))
          (unless visiting (kill-buffer work-buffer))))
      )))


;; Struggled so long looking for a hook that would work like the old
;; before-export-hook.  Let this be a lesson.  We never actually need there to
;; exist before-hooks or after-hooks, since it is always possible to use
;; add-function or write a wrapper like this.  The hook system exists to let you
;; subtly modify a function in the middle of its body.
(defun my-publish-to-blog (plist filename pub-dir)
  ;; (my-rename-roam-file-by-title filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (let* ((case-fold-search t)
           (title (save-excursion
                    (goto-char (point-min))
                    (when (search-forward "#+title: " nil t)
                      (buffer-substring (point) (line-end-position)))))
           (date (save-excursion
                   (goto-char (point-min))
                   (when (search-forward "#+date: " nil t)
                     (buffer-substring (1+ (point)) (1- (line-end-position))))))
           (org-html-extension ""))

      (org-publish-org-to 'html filename org-html-extension plist pub-dir)

      ;; Some logic borrowed from `org-publish-org-to' 2023-02-02
      (let* ((org-inhibit-startup t)
             (visiting (find-buffer-visiting filename))
             (work-buffer (or visiting (find-file-noselect filename))))
        (unwind-protect
            (with-current-buffer work-buffer
              (let* ((output (org-export-output-file-name org-html-extension nil pub-dir))
                     (output-buf (find-buffer-visiting output))
                     (was-opened nil)
                     (relative-slug (replace-regexp-in-string "^.*/posts/" "" output)))
                (unless (and title date)
                  (delete-file output)
                  (message "FILE LACKING TITLE OR DATE: %s" output))
                ;; This file has no body because it met :exclude-tags, idk why it gets created
                (if (= 0 (doom-file-size output))
                    (progn
                      (delete-file output)
                      (message "File deleted because empty: %s" output))
                  (when (and title date)
                    (when output-buf
                      (setq was-opened t)
                      (unless (buffer-modified-p output-buf)
                        (kill-buffer output-buf)))
                    ;; Hand-craft a JSON object.  Apparently, Elisp's
                    ;; `json-encode' doesn't escape quotes enough for
                    ;; javascript's JSON.parse() to understand (the latter
                    ;; seems shitty overall with shitty error messages).
                    (with-temp-file output
                      ;; JSON
                      (insert "  {")
                      (insert "\n    \"slug\": \"" relative-slug "\",")
                      (insert "\n    \"title\": \"" (string-replace "\"" "\\\"" title) "\",")
                      (insert "\n    \"date\": \"" date "\",")
                      (insert "\n    \"content\": \"")
                      (setq content-start-pos (point))

                      ;; Content
                      (insert "\n<h1>" title "</h1>")
                      (insert "\n<p>Planted " date "</p>")
                      (insert-file-contents output)
                      
                      ;; Adjust the result from `my-add-backlinks-if-roam'
                      (goto-char (point-max))
                      (when (search-backward "What links here<" nil t)
                        (goto-char (line-beginning-position))
                        (while (search-forward "h2" (line-end-position) t)
                          (replace-match "h1" nil t)))

                      ;; JSON again
                      (goto-char content-start-pos)
                      (while (search-forward "
" nil t)
                        (replace-match "\\n" nil t))
                      (goto-char content-start-pos)
                      (while (search-forward "\"" nil t)
                        (replace-match "\\\\\\\"" nil t))
                      (goto-char (point-max))
                      (insert "\"\n  }"))
                    (when was-opened
                      (find-file-noselect output))))))
          (unless visiting (kill-buffer work-buffer))))
      )))

(setopt org-html-checkbox-type 'html)
(setopt org-publish-project-alist
        ;; TODO: Rename the exported file as a Jekyll-compatible slug, so I don't need
        ;; the original filename to be any particular way.
        '(("blag"
           :base-directory "/home/kept/roam/blog/"
           :publishing-directory "/home/kept/blog/meedstrom.github.io/_posts/"
           :publishing-function org-md-publish-to-md
           )
          ("react-blog"
           :base-directory "/home/kept/roam/"
           :publishing-directory "/home/kept/blog/baz/baz-backend/posts/"
           :publishing-function my-publish-to-blog
           :recursive t
           :preparation-function (lambda (_) (setopt org-export-use-babel nil))
           :completion-function (lambda (_) (setopt org-export-use-babel t))
           :with-toc nil
           :section-numbers nil
           :body-only t
           :exclude "daily/"
           ;; this does not work! because filetag?
           :exclude-tags ("noexport" "private" "personal" "censor")
           )))

;; (setopt org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
;;                                    (todo . " %i %-32b") ;; have breadcrumbs
;;                                    (tags . " %i %-12:c")
;;                                    (search . " %i %-12:c")))
;; (setopt org-agenda-custom-commands '(("b" todo "NEXT")
;;                                      ("w" todo "WAITING")
;;                                      ("p" todo "PROCRASTINATING")
;;                                      ("c" tags-todo "+active")))
(setopt org-agenda-tag-filter-preset '("-exclude"))
(setopt org-agenda-todo-list-sublevels nil)
(setopt org-agenda-todo-ignore-scheduled t)
(setopt org-agenda-files '("/home/kept/archive/journal/diary.org"
                           ;; to always cache the org-id locations
                           "/home/kept/emacs/conf-doom/elfeed.org"
                           "/home/kept/roam/"
                           "/home/sync-phone/beorg/"))

;; (setopt org-archive-location "/home/kept/archive/journal/diary.org::datetree/")
(setopt org-archive-save-context-info '(time file itags olpath))
(setopt org-pomodoro-play-sounds nil)
(setopt org-clock-out-remove-zero-time-clocks t)
(setopt org-clock-persist t)
(setopt org-clock-idle-time 5)
(setopt org-hide-leading-stars nil)
(setopt org-clock-mode-line-total 'today)
(setopt org-clock-auto-clock-resolution 'always)
(setopt org-clock-in-resume t)
(setopt org-catch-invisible-edits 'smart)
(setopt org-ctrl-k-protect-subtree t)
(setopt org-agenda-include-diary t)
(setopt org-cycle-separator-lines 3)
(setopt org-datetree-add-timestamp t)
(setopt org-edit-src-content-indentation 0)
(setopt org-ellipsis "⤵")
(setopt org-hide-emphasis-markers t) ; hide the *, =, and / markers
(setopt org-image-actual-width '(200)) ; use #ATTR if available, else 200 px
(setopt org-insert-heading-respect-content t)
;; (setopt org-latex-compiler "xelatex") ; allow unicode (åäö) in VERBATIM blocks
(setopt org-log-done 'time)
(setopt org-log-into-drawer t) ; hide spam
(setopt org-modules '(org-id ol-info ol-eww)) ;;org-eww-copy-for-org-mode
(setopt org-pretty-entities t)
(setopt org-use-speed-commands t)
(setopt org-clock-x11idle-program-name (or (executable-find "xprintidle") "x11idle"))
(setopt org-replace-disputed-keys t)

;; Improve org performance
(global-auto-composition-mode 0)
(setopt bidi-display-reordering nil)

(setopt my-org-prettify-alist '(
                              ;; Nice for writing/reading equations in plaintext
                              ;; ("[" . "［")
                              ;; ("]" . "］")
                              ;; ("-" . "－")
                              ;; ("=" . "＝")
                              ;; ("+" . "＋")
                              ("\\vdots" . "⋮")
                              ("\\implies" . "⟹")
                              ("\\sqrt" . "√")
                              ("\\ldots" . "…")))

(defun my-org-prettify ()
  (setq prettify-symbols-alist
        (cl-union prettify-symbols-alist my-org-prettify-alist))
  ;; (mapc (lambda (x) (pushnew! prettify-symbols-alist x))
  ;; my-org-prettify-alist)
  )

(after! org
  (require 'org-protocol) ;; for org capture from firefox

  (require 'named-timer) ;; an indispensable 70-line library
  (named-timer-run :my-clock-reminder nil 600
                   (defun my-clock-remind ()
                     (when (org-clock-is-active)
                       (message (concat "Currently working on: "
                                        org-clock-current-task))))))

;; WIP
;; I don't think this will work. I think the code that looks up values entered by user, has to run after, not in the template function.
;;
;;To go with a capture template that goes like this
;; ("p" "Person" plain "%?" :if-new
;;    (file+head "%<%Y-%m-%d>-${slug}.org")
;;    (function my-person-template)
;;    :unnarrowed t
;;    :immediate-finish t
;;    :jump-to-captured t)
(defun my-person-template ()
  "Do a capture to two places: an usual new note and a link
to the new note in the \"timeline\" note."
  ;; untested
  (with-current-buffer (org-id-open "3r342-id-for-the-timeline-note")
    (insert "- 1991-2022 Person Name"))
  ;; untested
  (org-set-property "ROAM_ALIASES"
                    (concat (ndk/org-current-buffer-get-title)
                            " ("
                            (read-string "Years of birth and death (YYYY--YYYY)")
                            ")"))
  "#+title: ${title}\n#+filetags: :person:stub:\n#+date: \[%<%Y-%m-%d>\]\n")

(after! org-roam
  (add-hook 'doom-load-theme-hook
            (defun my-theme-mod-org ()
              (set-face-attribute 'org-roam-title nil :height 1.5)))
  (my-theme-mod-org)
  (setopt org-roam-extract-new-file-path "%<%Y-%m-%d>-${slug}.org")
  (setopt org-roam-capture-templates
        `(("d" "default" plain "%?" :if-new
           (file+head "%<%Y-%m-%d>-${slug}.org"
                      "#+title: ${title}\n#+filetags: :stub:\n#+date: \[%<%Y-%m-%d>\]\n")
           :unnarrowed t
           :immediate-finish t
           :jump-to-captured t)

          ("i" "instantly create this node" plain "%?" :if-new
           (file+head "%<%Y-%m-%d>-${slug}.org"
                      "#+title: ${title}\n#+filetags: :stub:\n#+date: \[%<%Y-%m-%d>\]\n")
           :unnarrowed t
           :immediate-finish t)
          
          )))

;; has to happen after load bc doom sets capture templates at load time.
;; incidentally also means we cannot use custom-file to config them.
;; to remove the offender, do
;; (remove-hook 'org-load-hook #'+org-init-capture-defaults-h)
(after! org
  (setopt org-capture-templates
        `(
          ("p" "Person of history" entry
           (file "/home/kept/roam/2021-08-27-historical-people.org")
           ,(lines "* %^{Name} :stub:"
                   ":PROPERTIES:"
                   ":ID:       %(org-id-uuid)"
                   ":ROAM_ALIASES: \"%\\1 (%\\2)\""
                   ":END:"
                   "%^{Years of birth and death (YYYY--YYYY)}"
                   "%?"))

          ;; TODO: Number it by a counter
          ("u" "untitled note" plain
           (file "/home/kept/roam/untitled-notes.org")
           "* \[%<%Y-%m-%d %T>\]
:PROPERTIES:
:ID:  %(org-id-uuid)
:END:
:DATE: \[%<%Y-%m-%d>\]
%i%?
%a")
          (";" "firefox capture" plain
           (file "/tmp/captures.org")
           "* %a
:PROPERTIES:
:ID:  %(org-id-uuid)
:END:
:DATE: \[%<%Y-%m-%d>\]
%i%?")

          ("e" "Emacs idea" entry (file+headline "/home/kept/roam/2021-08-27-someday_maybe.org" "Ideas"))
          ("q" "Statistics question" entry (file+headline "/home/kept/roam/stats.org" "Questions"))
          ("t" "Statistics header" entry (file+headline "/home/kept/roam/stats.org" "Statistics"))
          ("s" "Someday/Maybe" entry (file+headline "/home/kept/roam/2021-08-27-someday_maybe.org" "Unsorted"))
          ("m" "A Virtual Assistant function")
          ("mw" "weight" plain (function eva-session-new) :immediate-finish t)
          ("mf" "visit Ledger file" plain (function eva-present-ledger-file) :immediate-finish t)

          ("ln" "From Nordea" plain (file "/home/kept/self-data/clean_start.ledger")
           ,(lines "%<%Y-%m-%d> * \"\""
                   "    Expenses   %?"
                   "    Assets:Nordea:Personkonto")
           :empty-lines 1
           :jump-to-captured t)

          ("li" "InvestNotSpend" plain (file "/home/kept/self-data/clean_start.ledger")
           ,(lines "%<%Y-%m-%d> ! \"\""
                   "    [Assets:Lysa:InvestNotSpend]   %?"
                   "    Assets:Nordea:Personkonto")
           :empty-lines 1
           :jump-to-captured t)

          ("r" "Retroactive clock" entry (file+olp+datetree "/home/kept/archive/journal/diary.org")
           ,(lines "* %^{Activity|School|Piano|Signing|Coding}"
                   "CLOCK: %^{Time at start}U--%^{Time at finish}U => %^{Rough time spent (sorry, the program is dumb), H:MM}")
           :immediate-finish t)

          )))

;; Because capfs don't do what I want in Roam
(add-hook 'org-mode-hook #'my-corfu-turn-off)

(after! org
  (unless after-init-time
    (warn "Org loaded during init, I don't want this"))
  (setopt org-babel-load-languages '((R . t)
                                     (emacs-lisp . t)
                                     (calc . t)
                                     (ditaa . t)
                                     (sqlite . t)
                                     (dot . t)))
  ;; Stuff to do if I'm not using Doom's Org
  (unless (fboundp '+org-init-org-directory-h)
    ;; Upscale the LaTeX preview.
    (my-change-latex-scale)
    ;; Adapt LaTeX preview scale to the font zoom.
    (add-hook 'text-scale-mode-hook #'my-change-latex-scale)))

;; Prettify code-snippets in exported pdf.
(after! ox-latex
  (setopt org-latex-listings t)
  (setopt org-latex-listings-options '(("basicstyle" "\\small"))) ; small code font
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "booktabs")))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 90)