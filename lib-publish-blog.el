;;; lib-publish-blog.el -- Publishing to edstrom.dev -*- lexical-binding: t; -*-

(setq org-modules '(org-id))
(require 'org-roam)
(require 'org-agenda)
(require 'ox-publish)
(require 'dom)
(require 'prism)
(require 'dash)
(require 'kv)
(require 'lintorg)

(defvar my-tags-to-avoid-uploading '("noexport" "archive" "private" "censor"))
(defvar my-tags-for-hiding '("gri" "shrink" "privy" "lover" "fren"))
(defvar my-tags-for-publishing (cons "pub" my-tags-for-hiding))


;; TODO: Upstream
;; Override the info: link type so it won't get exported into an empty <a href>
;; with not even a link description.  What nasty default behavior!  See
;; `org-link-parameters' if I need the same treatment for more link types.
(require 'ol-info)
(defun org-info-export (path desc _format)
  (or desc path))

;; TODO: Upstream
;; Give each h2...h6 heading (and its container div) an ID attribute that
;; matches the source Org heading ID property, if it has one, instead of
;; e.g. "org953031".  That way, hash-links such as
;; #ID-e10bbdfe-1ffb-4c54-9228-2818afdfc5ba will make the web browser jump to
;; that heading.  Thank org-roam for including this code!  Note that I convert
;; these IDs later away from UUID form using `my-uuid-to-short'.
(after! ox
  (require 'org-roam-export))

;; This function is where I can make destructive env changes that I
;; don't want in my main Emacs.
(defun my-publish-begin ()
  "All-in-one command for use in a child emacs.

With C-u, first rebuild the org-roam-db that's in /tmp, else reuse
it from some past run (makes sense if you only changed the export
code, but not the notes).

With C-u C-u, also run `my-validate-org-buffer' on each file
scanned."
  (interactive)
  (view-echo-area-messages) ;; for watching it work
  (setq org-export-use-babel nil)
  (setq org-export-with-broken-links nil) ;; links would disappear quietly!
  (setq org-html-extension "")
  (setq org-html-checkbox-type 'unicode)
  (setq org-html-html5-fancy t)
  (setq org-html-with-latex 'html) ;; Use `org-latex-to-html-convert-command'
  ;; TODO: upstream as an option for `org-html-with-latex'. this is very fast
  (setq org-latex-to-html-convert-command "node ~/.config/emacs/texToMathML.js %i")
  (setq save-silently t)
  (setq debug-on-error t)
  (setq-default tab-width 8)
  (my-remove-all-advice 'org-roam-db-update-file) ;; disable my hacks
  ;; ignore backlinks
  (setq org-roam-db-extra-links-exclude-keys
        '((node-property "ROAM_REFS" "BACKLINKS")
          (keyword "transclude")))

  ;; Speed up publishing
  (setq gc-cons-threshold (* 5 1000 1000 1000))
  (fset 'org-publish-write-cache-file #'ignore) ;; mega speedup!
  (setq file-name-handler-alist nil)
  (setq coding-system-for-read 'utf-8-unix)
  (setq coding-system-for-write 'utf-8-unix)
  (setq org-inhibit-startup t) ;; from `org-publish-org-to'
  (setq org-mode-hook nil)

  ;; Attempt to speed up publishing, not sure these help much
  (advice-remove 'after-find-file #'doom--shut-up-autosave-a)
  (remove-hook 'org-export-before-parsing-functions #'org-attach-expand-links)
  (setq whitespace-global-modes nil)
  (my-disable-modes-if-present '(apheleia-global-mode
                                 auto-encryption-mode
                                 auto-save-mode
                                 auto-save-visited-mode
                                 awesome-tray-mode
                                 beginend-global-mode
                                 better-jumper-mode
                                 column-number-mode
                                 context-menu-mode
                                 delete-selection-mode
                                 diff-hl-flydiff-mode
                                 dired-async-mode
                                 dired-hist-mode
                                 display-battery-mode
                                 editorconfig-mode
                                 electric-indent-mode
                                 elpaca-use-package-mode
                                 eva-mode
                                 file-name-shadow-mode
                                 gcmh-mode
                                 global-corfu-mode
                                 global-diff-hl-mode
                                 global-eldoc-mode
                                 global-emojify-mode
                                 global-font-lock-mode
                                 global-form-feed-mode
                                 global-ligature-mode
                                 global-prettify-symbols-mode
                                 magit-auto-revert-mode
                                 marginalia-mode
                                 minibuffer-regexp-mode
                                 my-auto-commit-mode
                                 nerd-icons-completion-mode
                                 org-node-backlink-mode
                                 org-node-cache-mode
                                 org-node-complete-at-point-mode
                                 org-node-fakeroam-db-feed-mode
                                 org-node-fakeroam-fast-render-mode
                                 org-node-fakeroam-jit-backlinks-mode
                                 org-node-fakeroam-redisplay-mode
                                 pixel-scroll-precision-mode
                                 projectile-mode
                                 recentf-mode
                                 repeat-mode
                                 save-place-mode
                                 savehist-mode
                                 show-paren-mode
                                 smartparens-global-mode
                                 solaire-global-mode
                                 tooltip-mode
                                 transient-mark-mode
                                 window-divider-mode
                                 winner-mode
                                 ws-butler-global-mode))
  ;; See `org-node--with-quick-file-buffer'
  (setq find-file-hook nil)
  (setq kill-buffer-hook nil)
  (setq org-element-use-cache nil)
  (setq before-save-hook nil)
  (setq after-save-hook nil)
  (setq kill-buffer-query-functions nil)
  (setq buffer-list-update-hook nil)

  ;; Lintorg is an unpublished linter, made before I knew about org-lint
  (setq lintorg-on-front-matter-hook
        '(lintorg-local-entry/assert-id
          lintorg-local-entry/assert-created-if-id
          lintorg-local-entry/assert-created-is-proper-timestamp
          lintorg-local-entry/assert-roam-refs-have-no-quotes
          lintorg-front-matter/assert-title
          lintorg-front-matter/assert-tags-are-lowercase
          lintorg-front-matter/seek-broken-filetags
          ))
  (setq lintorg-across-buffer-hook
        '(lintorg-region/seek-wrong-link-brackets
          lintorg-region/assert-org-ids-are-uuid
          lintorg-region/assert-structures-needing-own-line-are-on-own-line
          lintorg-region/assert-only-permitted-link-types
          ))
  (setq lintorg-on-subtree-hook
        '(lintorg-subtree/seek-broken-heading-tags
          lintorg-subtree/assert-tags-are-lowercase
          lintorg-local-entry/assert-created-if-id
          lintorg-local-entry/assert-created-is-proper-timestamp
          lintorg-local-entry/assert-roam-refs-have-no-quotes
          lintorg-subtree/seek-broken-heading-tags
          ))

  ;; For hygiene, ensure that this subordinate emacs syncs nothing to disk
  (persist-state-mode 0)
  (setq kill-emacs-hook nil)

  ;; Switch theme for 2 reasons
  ;; 1. Remind me that this is not my normal Emacs
  ;; 2. The theme carries over to code blocks, so ensure it's a theme that
  ;;    looks okay in both light and dark mode
  (let ((theme 'doom-monokai-machine))
    ;; (let ((theme 'ef-rosa))
    ;; (let ((theme 'doom-zenburn))
    ;; (let ((theme 'doom-rouge))
    (unless (member theme custom-enabled-themes)
      (me/load-theme theme)))

  ;; Copy the files to /tmp to work from there
  (mkdir "/tmp/posts" t)
  (mkdir "/tmp/posts/org" t)
  (shell-command "rm -rfv /tmp/posts/org/*")
  (shell-command "cp -a ~/org/* /tmp/posts/org/")
  ;; HACK: Subfolders currently get their own img folders (not intentional),
  ;;       merge.
  (shell-command "mv -t /tmp/posts/org/img/ /tmp/posts/org/*/img/*")

  ;; Pretty-print a post of recent completed todos
  (let ((org-agenda-files '("/tmp/posts/org/noagenda/archive.org")))
    (my-generate-todo-log "/tmp/posts/org/todo-log.org" "/tmp/posts/"))

  ;; Ensure that each post URL will contain a unique page ID by now placing
  ;; them in subdirectories named by that ID, since org-export will translate
  ;; all org-id links into these relative filesystem paths.
  (cl-loop for path in (directory-files-recursively "/tmp/posts/org/" "\\.org$")
           do (if (not (or (string-search ".sync-conflict-" path)
                           (string-search "/logseq/" path)))
                  (if-let* ((uuid (my-org-file-id path))
                            (newdir (concat "/tmp/posts/org/"
                                            (my-uuid-to-short uuid)
                                            "/")))
                      (progn
                        (mkdir newdir t)
                        (rename-file path newdir))
                    (warn "No file ID in %s" path)
                    (delete-file path))
                (delete-file path)))

  ;; Tell `org-id-locations' and the org-roam DB about the new work directory
  (setq org-id-locations-file "/tmp/posts/org-id-locations")
  (setq org-roam-db-location "/tmp/posts/org-roam.db")
  (setq org-roam-directory "/tmp/posts/org/")
  (setq org-node-extra-id-dirs '("/tmp/posts/org/"))
  (setq org-agenda-files '("/tmp/posts/org/"))

  ;; With C-u, wipe databases so they get rebuilt
  (when current-prefix-arg
    (shell-command "rm /tmp/posts/org-roam.db")
    (me/wipe-org-id)
    ;; With C-u C-u, run my custom linter too
    (if (equal current-prefix-arg '(16))
        (add-hook 'my-org-roam-pre-scan-hook #'lintorg-lint)
      (remove-hook 'my-org-roam-pre-scan-hook #'lintorg-lint)))

  (unless (file-exists-p org-roam-db-location)
    ;; (org-node-cache-ensure t t)
    ;; (org-node-fakeroam-db-rebuild)
    (org-id-update-id-locations) ;; find files with ROAM_EXCLUDE too
    (org-roam-update-org-id-locations)
    (org-roam-db-sync 'force)
    )

  ;; Wipe previous work output
  (shell-command "rm -rf /tmp/posts/{html,json,atom}/")
  (shell-command "mkdir -p /tmp/posts/{html,json,atom}")
  (clrhash my-ids)

  ;; Change some things about the Org files, before org-export does its thing.
  (add-hook 'org-export-before-parsing-functions #'my-add-refs-as-paragraphs)
  (add-hook 'org-export-before-parsing-functions #'my-replace-datestamps-with-links)
  (add-hook 'org-export-before-parsing-functions #'my-strip-inline-anki-ids)
  (add-hook 'org-export-before-parsing-functions #'org-transclusion-mode 1)
  (add-hook 'org-export-before-parsing-functions
            ;; Allow later hooks to manipulate the org-transclusion areas
            ;; (which are usually read-only)
            (lambda (&rest _) (setq buffer-read-only nil)) 2)
  (add-hook 'org-export-before-parsing-functions #'my-add-backlinks 10)
  (add-hook 'org-export-before-parsing-functions #'my-ensure-section-containers 20)

  (org-publish "my-slipbox-blog" t) ;; "Main"; basically run the rest of this file.
  (my-check-id-collisions)
  (my-compile-atom-feed "/tmp/posts/posts.atom" "/tmp/posts/atom/")
  (find-file "/home/kept/pub/")
  (async-shell-command "./encrypt-rebuild.sh")
  (start-process "firefox" nil "firefox" "http://localhost:5173"))

(setopt org-publish-project-alist
        `(("my-slipbox-blog"
           :base-directory "/tmp/posts/org/"
           :publishing-directory "/tmp/posts/html/"
           :publishing-function my-publish-to-blog
           :recursive t
           :body-only t
           :section-numbers nil
           :headline-levels 5  ;; Go all the way to <h6> before making <li>
           :with-toc nil
           :with-tags nil
           :with-todo-keywords nil
           :with-smart-quotes nil
           :with-drawers '(not "logbook" "noexport") ;; (case-insensitive FYI)
           ;; NOTE: This only excludes subtrees, so we also check file-level
           ;; tag in `my-publish-to-blog'.  Maybe upstream a patch?
           :exclude-tags ,my-tags-to-avoid-uploading)))

(defvar my-publish-ctr 0)
(defun my-publish-to-blog (plist filename pub-dir)
  "Take org file FILENAME and make html file in PUB-DIR.
Then postprocess that same html into json and atom files.

Designed to be called by `org-publish'.  All arguments pass
through to `org-html-publish-to-html'."
  (redisplay) ;; Let me watch it work
  (if (or (not (-intersection (my-org-file-tags filename) my-tags-for-publishing))
          (-intersection (my-org-file-tags filename) my-tags-to-avoid-uploading))
      ;; If we already know we won't publish it, don't export the file at all.
      ;; Saves so much time.  Some other issues can also disqualify the file,
      ;; but I take care of them in `my-validate-org-buffer'.
      (message "Found exclude-tag, excluding: %s" filename)
    (when-let ((open (find-buffer-visiting filename)))
      (cl-assert (not (buffer-modified-p open)))
      (cl-assert (memq (buffer-local-value 'buffer-undo-list open) '(t nil)))
      (kill-buffer open))
    (when (= 0 (% (cl-incf my-publish-ctr) 200))
      ;; Reap open file handles (max 1024 on many OSes and in Emacs)
      (garbage-collect))
    (with-current-buffer (find-file-noselect filename)
      (goto-char (point-min))
      (let* ((html-path (org-html-publish-to-html plist filename pub-dir))
             (keywords (org-collect-keywords '("DATE" "SUBTITLE")))
             (tags (org-get-tags))
             (created (substring (org-entry-get nil "CREATED") 1 11))
             (updated (let ((value (map-elt keywords "DATE")))
                        (when (and value (not (string-blank-p (car value))))
                          (substring (car value) 1 11))))
             (created-fancy
              (format-time-string (car org-time-stamp-custom-formats)
                                  (date-to-time created)))
             (updated-fancy
              (when updated
                (format-time-string (car org-time-stamp-custom-formats)
                                    (date-to-time updated))))
             (pageid (-last-item (split-string pub-dir "/" t)))
             (hidden (not (null (-intersection my-tags-for-hiding tags))))
             (metadata
              (list :pageid pageid
                    :tags tags
                    :hidden hidden
                    :created created
                    :createdFancy created-fancy
                    :updated updated
                    :updatedFancy updated-fancy
                    :slug (string-replace pub-dir "" html-path)
                    :description (car (map-elt keywords "SUBTITLE"))
                    :title (if (member "daily" tags)
                               created-fancy
                             (->> (org-get-title)
                                  (string-replace "---" "—")
                                  (string-replace "--" "–")))
                    :wordcount
                    (save-excursion
                      (cl-loop
                       while (re-search-forward my-org-text-line-re nil t)
                       if (and (eq (preceding-char) ?*)
                               (member "noexport" (org-get-tags)))
                       ;; Don't count words under hidden subtrees
                       do (org-next-visible-heading 1)
                       else sum (count-words (point) (line-end-position))))
                    :linkcount
                    (save-excursion
                      (cl-loop
                       while (re-search-forward org-link-bracket-re nil t)
                       if (member "noexport" (org-get-tags))
                       do (org-next-visible-heading 1)
                       else count t))))
             ;; Final "post object" for use by blog
             (post (append metadata
                           (list :content (my-customize-the-html
                                           html-path metadata))))
             (uuid (org-id-get)))
        (let ((save-silently t))
          ;; Write JSON object to be read by my Svelte app
          (with-temp-file (concat "/tmp/posts/json/" pageid)
            (insert (json-encode post)))
          ;; Write Atom entry if the post is acceptable for the feed
          (when (and (not hidden)
                     (not (-intersection tags '("tag" "daily" "stub" "unwashed")))
                     (string-lessp "2023" (or updated created)))
            (with-temp-file (concat "/tmp/posts/atom/" pageid)
              (insert (my-make-atom-entry post uuid))))))
      (kill-buffer (current-buffer)))))

(defun my-customize-the-html (html-path metadata)
  "Take contents of HTML-PATH and return customized content.
METADATA is most of the corresponding post's metadata."
  (let ((dom (with-temp-buffer
               (insert-file-contents html-path)
               ;; Give the ToC div a class and remove its pointless inner div
               (when (re-search-forward "^<div id===\"table-of-contents\".*?>" nil t)
                 (replace-match "<nav class=\"toc\" role=\"doc-toc\">")
                 (re-search-forward "^<div id=\"text-table-of-contents\".*?>")
                 (replace-match "")
                 (search-forward "</div>\n</div>")
                 (replace-match "</nav>"))
               ;; Add role="doc-endnotes"
               (goto-char (point-min))
               (when (re-search-forward "^<h2 id.*>What links here" nil t)
                 (forward-line -1)
                 (search-forward " class=\"outline-2\"" (line-end-position))
                 (insert " role=\"doc-endnotes\""))

               (libxml-parse-html-region))))
    (if (null dom)
        ""

      ;; Declutter unused classes
      ;; (Actually remove the entire class="..." attribute, but that's OK here)
      (cl-loop for node in (--mapcat (dom-by-class dom it)
                                     '("org-ul" "org-ol"))
               do (dom-remove-attribute node 'class))

      ;; Edit the .outline-2, .outline-3... divs that Org generated to enable
      ;; the CSS selectors "section.even" and "section.odd".
      ;; (Already converted div->section in `my-ensure-section-containers').
      (cl-loop
       for section in (dom-by-tag dom 'section)
       as class = (dom-attr section 'class)
       as parity = (if (string-match-p "[246]" class) "even" "odd")
       do (dom-set-attribute section 'class (concat parity " " class)))

      ;; Mess with internal links
      (cl-loop
       for anchor in (dom-by-tag dom 'a)
       as href = (dom-attr anchor 'href)
       as hash = (when href (cadr (split-string href "#")))
       as uuid? = (when hash (->> hash
                                  (string-replace "ID-" "")
                                  (string-replace "id:" "")))
       when (and uuid? (org-uuidgen-p uuid?))
       do (let ((shortid (my-uuid-to-short uuid?))
                (target-tags (-flatten (org-roam-db-query
                                        `[ :select [tag]
                                           :from tags
                                           :where (= node-id ,uuid?)]))))
            ;; 2024-07-25 I'm no longer adding :noexport: everywhere.  My
            ;; system was made for opt-out publishing, so hack it to be opt-in
            ;; by adding "noexport" if :pub: or its siblings are not present.
            (when (or (not (-intersection target-tags my-tags-for-publishing))
                      (null target-tags))
              (push "noexport" target-tags))
            ;; Replace the UUIDs with my shortened ID form,
            ;; and strip the #HEADING-ID if it is the same as /PAGE-ID/
            ;; (typical for links that don't go to a specific subheading).
            (dom-set-attribute anchor 'href (my-strip-hash-if-matches-base
                                             (string-replace hash shortid href)))
            ;; Let the link be styled based on target document's tags
            (when target-tags
              (dom-set-attribute anchor 'class (string-join target-tags " ")))
            ;; https://www.w3.org/TR/dpub-aria-1.0/#doc-backlink
            (when (string-blank-p (car (split-string href "#")))
              (dom-set-attribute anchor 'role "doc-backlink"))
            ;; Remember the short-ID for a collision-check later on
            (push uuid? (gethash shortid my-ids))))

      ;; Format nondescript links (bare URLs) more nicely
      (cl-loop
       for anchor in (dom-by-tag dom 'a)
       as children = (dom-children anchor)
       as desc = (car children)
       as fixed-desc = (when (and desc (stringp desc))
                         (->> desc
                              (replace-regexp-in-string "^http.?://" "")
                              (string-replace "%20" " ")))
       when fixed-desc
       unless (equal fixed-desc desc)
       do (progn
            (dom-add-child-before anchor fixed-desc desc)
            (dom-remove-node anchor desc)))

      ;; Fix <section id="..."> attributes, and add self-links to the section
      (let-alist (kvplist->alist metadata)
        (cl-loop
         for section in (dom-by-tag dom 'section)
         as id = (string-replace "outline-container-" ""
                                 (dom-attr section 'id))
         as heading = (car (dom-non-text-children section))
         as uuid? = (string-replace "ID-" "" id)
         do (if (org-uuidgen-p uuid?)
                ;; The id= looked like:
                ;; "outline-container-ID-e9eaf8ff-b2ea-4891-8e93-bbb426f277c1"
                (let ((hashid (my-uuid-to-short uuid?)))
                  (dom-set-attribute section 'id hashid)
                  ;; Add a self-link.  Hardcode the canonical URL, because my
                  ;; blog may display the same post on several paths.
                  (dom-append-child heading
                                    (dom-node 'a
                                              `((href . ,(concat "/" .pageid
                                                                 "/" .slug
                                                                 "#" hashid))
                                                (class . "heading-permalink")
                                                (rel . "bookmark"))
                                              "🔗")))
              ;; The id= looked like:
              ;; "outline-container-org1234567"
              (dom-set-attribute section 'id id))))

      ;; Org-Export doesn't replace double/triple-dash in all situations (like
      ;; inside a heading, or when the triple-dash butts up against a link on a
      ;; newline), so force it.
      (cl-labels ((fix-recursively-unless-verbatim (dom)
                    (cl-loop
                     for child in (dom-children dom)
                     if (stringp child)
                     ;; Leaf-node that is a bare string, e.g. the string
                     ;; "Boca" within a node <p>Boca</p>.  Edit it.
                     do (let ((fixed-child (->> child
                                                (string-replace "---" "—")
                                                (string-replace "--" "–"))))
                          (unless (equal fixed-child child)
                            (dom-add-child-before dom fixed-child child)
                            (dom-remove-node dom child)))
                     ;; Not a bare string, go deeper.  But skip <code> blocks.
                     else if (not (member (dom-tag child) '(code pre kbd samp)))
                     do (progn
                          (unless (eq (dom-tag child) 'section)
                            ;; Bonus: strip unnecessary ID attributes
                            (dom-remove-attribute child 'id))
                          (fix-recursively-unless-verbatim child)))))
        (fix-recursively-unless-verbatim dom))

      ;; Wrap tables in divs that can be scrolled left-right
      (cl-loop for tbl in (dom-by-tag dom 'table)
               as parent = (dom-parent dom tbl)
               as wrapped-tbl = (dom-node 'div
                                          '((class . "table-container"))
                                          (copy-sequence tbl))
               do (progn
                    (dom-add-child-before parent wrapped-tbl tbl)
                    (dom-remove-node parent tbl)))

      ;; Fix img attributes
      (cl-loop for img in (dom-by-tag dom 'img)
               as src = (dom-attr img 'src)
               as alt = (dom-attr img 'alt)
               ;; All my Org files have image links that start with
               ;; "file:img/..." because the directory "img/" sits in the same folder
               ;; as the Org files, but my blog has more nested paths for each
               ;; note while the img path is absolute.  So prepend a slash.
               when (string-prefix-p "img" src)
               do (dom-set-attribute img 'src (concat "/" src))
               ;; Absent an #+ATTR_HTML: :alt attribute, Org exports an image
               ;; alt-text copied from the file basename.  Interesting idea,
               ;; since the alt-text becomes portable if that's where you put
               ;; the alt-text!  Go with it and just strip .jpg/.png extension.
               and when (string-search alt src)
               do (dom-set-attribute img 'alt (->> alt
                                                   (file-name-sans-extension)
                                                   (string-replace "_" " "))))

      ;; Let images that aren't already linkified become self-links.
      ;; Such links, to resources on the same domain, also need rel="external"
      ;; to prevent SvelteKit from handling the URL as a SPA route.
      ;; NOTE: Do not merge with the other loop on image elements, b/c this one
      ;;       will insert new elements.
      (cl-loop for img in (dom-by-tag dom 'img)
               as parent = (dom-parent dom img)
               unless (eq 'a (dom-tag parent))
               do (let ((linkified-img
                         (dom-node 'a `((href . ,(dom-attr img 'src))
                                        (rel . "external"))
                                   (copy-sequence img))))
                    (dom-add-child-before parent linkified-img img)
                    (dom-remove-node parent img)))

      ;; Much the same as above <img> changes, but for any regular <a>
      ;; element pointing to an image (i.e. there is an image resource that a
      ;; note simply links to, instead of embedding).
      (cl-loop for anchor in (dom-by-tag dom 'a)
               as href = (dom-attr anchor 'href)
               when (string-prefix-p "img" href)
               do
               (dom-set-attribute anchor 'href (concat "/" href))
               (dom-set-attribute anchor 'rel "external"))

      ;; Return final HTML.  Phew!
      ;; Note `dom-print' had a bug -- patched in self-contained/patches.el.
      (with-temp-buffer
        (dom-print dom)
        (search-backward "</body></html>")
        (replace-match "")
        (goto-char (point-min))
        (search-forward "<html><body>")
        (replace-match "")
        (buffer-string)))))


;;; roam db hacks

(after! org-roam-db
  (fset 'org-roam-db-update-file #'my-org-roam-db-update-file)
  (fset 'org-roam-db-sync #'my-org-roam-db-sync))

(defun my-org-roam-db-update-file (&optional file-path _)
  "Version of `org-roam-db-update-file' calling `my-org-roam-pre-scan-hook'."
  (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
  (let ((content-hash (org-roam-db--file-hash file-path))
        (db-hash (caar (org-roam-db-query [ :select hash :from files
                                            :where (= file $s1)]
                                          file-path)))
        info)
    (unless (string= content-hash db-hash)
      (require 'org-ref nil 'noerror)
      (require 'oc nil 'noerror)
      (org-roam-with-file file-path nil
        (org-with-wide-buffer (run-hooks 'my-org-roam-pre-scan-hook))
        (emacsql-with-transaction (org-roam-db)
          (org-with-wide-buffer
           ;; please comment why
           (org-set-regexps-and-options 'tags-only)
           ;; Maybe not necessary anymore
           ;; 2021 https://github.com/org-roam/org-roam/issues/1844
           ;; 2023 https://code.tecosaur.net/tec/org-mode/commit/5ed3e1dfc3e3bc6f88a4300a0bcb46d23cdb57fa
           (org-refresh-category-properties)
           (org-roam-db-clear-file)
           (org-roam-db-insert-file content-hash)
           (org-roam-db-insert-file-node)
           ;; please comment why
           (setq org-outline-path-cache nil)
           (goto-char (point-min))
           (let ((end (point-max)))
             (when (re-search-forward org-outline-regexp-bol nil t)
               (while (progn
                        (when (org-roam-db-node-p)
                          (org-roam-db-insert-node-data)
                          (org-roam-db-insert-aliases)
                          (org-roam-db-insert-tags)
                          (org-roam-db-insert-refs))
                        (outline-next-heading)
                        (< (point) end)))))
           (setq org-outline-path-cache nil)
           (setq info (org-element-parse-buffer))
           (org-roam-db-map-links
            (list #'org-roam-db-insert-link))
           (when (featurep 'oc)
             (org-roam-db-map-citations
              info
              (list #'org-roam-db-insert-citation)))
           ))
        ))))

;; Make the processing message more informative
(defun my-org-roam-db-sync (&optional force)
  "Synchronize the cache state with the current Org files on-disk.
If FORCE, force a rebuild of the cache from scratch."
  (interactive "P")
  (org-roam-db--close) ;; Force a reconnect
  (when force (delete-file org-roam-db-location))
  (org-roam-db) ;; To initialize the database, no-op if already initialized
  (org-roam-require '(org-ref oc))
  (let* ((gc-cons-threshold org-roam-db-gc-threshold)
         (org-agenda-files nil)
         (org-roam-files (org-roam-list-files))
         (current-files (org-roam-db--get-current-files))
         (modified-files nil))
    (dolist (file org-roam-files)
      (let ((contents-hash (org-roam-db--file-hash file)))
        (unless (string= (gethash file current-files)
                         contents-hash)
          (push file modified-files)))
      (remhash file current-files))
    (emacsql-with-transaction (org-roam-db)
      ;; Bruh. Just load compat.
      (org-roam-dolist-with-progress (file (hash-table-keys current-files))
          "Clearing removed files..."
        (org-roam-db-clear-file file))
      ;; Unfortunately it's good for debugging to show which file you got
      ;; stuck on. So we can't use the message log combination feature with
      ;; the dolist with ...
      ;; I've always felt that the "Processing modified files...38%" was too
      ;; little information.
      (let ((ctr 0))
        (dolist (file modified-files)
          (message "Processing modified files... (%d/%d) %s"
                   (cl-incf ctr)
                   (length modified-files)
                   (file-name-nondirectory file))
          (condition-case err
              (org-roam-db-update-file file)
            (error
             (org-roam-db-clear-file file)
             (lwarn 'org-roam :error "Failed to process %s with error %s, skipping..."
                    file (error-message-string err)))))))))

;; Fix: don't publish files that have a #+FILETAGS matching :exclude-tags
(after! ox-publish
  (defun org-publish-get-base-files (project)
    "Return a list of all files in PROJECT."
    (let* ((base-dir (file-name-as-directory
                      (org-publish-property :base-directory project)))
           (extension (or (org-publish-property :base-extension project) "org"))
           (match (if (eq extension 'any) ""
                    (format "^[^\\.].*\\.\\(%s\\)$" extension)))
           (base-files
            (cond ((not (file-exists-p base-dir)) nil)
                  ((not (org-publish-property :recursive project))
                   (cl-remove-if #'file-directory-p
                                 (directory-files base-dir t match t)))
                  (t
                   ;; Find all files recursively.  Unlike to
                   ;; `directory-files-recursively', we follow symlinks
                   ;; to other directories.
                   (letrec ((files nil)
                            (walk-tree
                             (lambda (dir depth)
                               (when (> depth 100)
                                 (error "Apparent cycle of symbolic links for %S"
                                        base-dir))
                               (dolist (f (file-name-all-completions "" dir))
                                 (pcase f
                                   ((or "./" "../") nil)
                                   ((pred directory-name-p)
                                    (funcall walk-tree
                                             (expand-file-name f dir)
                                             (1+ depth)))
                                   ((pred (string-match match))
                                    (push (expand-file-name f dir) files))
                                   (_ nil)))
                               files)))
                     (funcall walk-tree base-dir 0))))))

      (org-uniquify
       (append
        ;; Files from BASE-DIR.  Apply exclusion filter before adding
        ;; included files.
        (let* ((exclude-regexp (org-publish-property :exclude project))
               (exclude-tags (org-publish-property :exclude-tags project))
               (filtered-by-regexp
                (if exclude-regexp
                    (cl-remove-if
                     (lambda (f)
                       ;; Match against relative names, yet BASE-DIR file
                       ;; names are absolute.
                       (string-match exclude-regexp
                                     (file-relative-name f base-dir)))
                     base-files)
                  base-files)))
          (if exclude-tags
              (cl-remove-if
               (lambda (f)
                 (cl-intersection exclude-tags (my-org-file-tags f)
                                  :test #'string-equal-ignore-case))
               filtered-by-regexp)
            filtered-by-regexp))
        ;; Sitemap file.
        (and (org-publish-property :auto-sitemap project)
             (list (expand-file-name
                    (or (org-publish-property :sitemap-filename project)
                        "sitemap.org")
                    base-dir)))
        ;; Included files.
        (mapcar (lambda (f) (expand-file-name f base-dir))
                (org-publish-property :include project))))))

  (defun org-publish-get-project-from-filename (filename &optional up)
    "Return a project that FILENAME belongs to.
When UP is non-nil, return a meta-project (i.e., with a :components part)
publishing FILENAME."
    (let* ((filename (expand-file-name filename))
           (project
            (cl-some
             (lambda (p)
               ;; Ignore meta-projects.
               (unless (org-publish-property :components p)
                 (let ((base (expand-file-name
                              (org-publish-property :base-directory p))))
                   (cond
                    ;; Check if FILENAME is explicitly included in one
                    ;; project.
                    ((cl-some (lambda (f) (file-equal-p f filename))
                              (mapcar (lambda (f) (expand-file-name f base))
                                      (org-publish-property :include p)))
                     p)
                    ;; Exclude file names matching :exclude property.
                    ((let ((exclude-re (org-publish-property :exclude p)))
                       (and exclude-re
                            (string-match-p exclude-re
                                            (file-relative-name filename base))))
                     nil)
                    ;; Check :extension.  Handle special `any'
                    ;; extension.
                    ((let ((extension (org-publish-property :base-extension p)))
                       (not (or (eq extension 'any)
                                (string= (or extension "org")
                                         (file-name-extension filename)))))
                     nil)
                    ;; Check if FILENAME belong to project's base
                    ;; directory, or some of its sub-directories
                    ;; if :recursive in non-nil.
                    ((member filename (org-publish-get-base-files p)) p)
                    (t nil)))))
             org-publish-project-alist)))
      (cond
       ((not project) nil)
       ((not up) project)
       ;; When optional argument UP is non-nil, return the top-most
       ;; meta-project effectively publishing FILENAME.
       (t
        (letrec ((find-parent-project
                  (lambda (project)
                    (or (cl-some
                         (lambda (p)
                           (and (member (car project)
                                        (org-publish-property :components p))
                                (funcall find-parent-project p)))
                         org-publish-project-alist)
                        project))))
          (funcall find-parent-project project)))))))

;;; lib-publish-blog.el ends here
