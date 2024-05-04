;; Experiment zone -*- lexical-binding: t; -*-

;; TODO: actually let's set window fringes, so that the whitespace in the centre blends together
;; i.e. instead of fringes F-2F-F, let's have just F-F-F.
(defun fringe-from-fill-column ()
  "Return a cons cell where the `car' is the amount of windows
you would be able to fit side-by-side in a frame as wide as
`frame-width' while being sized to `fill-column', and the `cdr'
is the max possible pixel value you can set for the fringe-width
in this configuration.

Assumes that you want the windows to fit 1 more character than
the fill column i.e. for a `fill-column' of 80, that you want a
window at least 81 chars wide.

Assumes there are no window dividers."
  (let* ((pixels-per-char (/ (frame-pixel-width) (frame-width)))
         (needed-window-char-width (1+ (default-value 'fill-column)))
         (needed-window-px-width (* needed-window-char-width pixels-per-char))
         (n-windows-possible (/ (frame-pixel-width) needed-window-px-width))
         (leftover-px (mod (frame-pixel-width) needed-window-px-width))
         (leftover-px-per-window (/ leftover-px n-windows-possible)))
    (cons n-windows-possible (/ leftover-px-per-window 2))))
(set-fringe-mode (cdr (fringe-from-fill-column)))

(defun window-leftover-px-after-satisfying-fill-column ()
  (let* ((pixels-per-char (/ (window-pixel-width) (window-total-width)))
         (window-minimum-px (* (1+ fill-column) pixels-per-char)))
    (- (window-pixel-width) window-minimum-px)))

(setq backtrace-on-redisplay-error t)

;; (defvar pad-fringes-to-fill-column--state nil)
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
;; OK, I know what to do.  This function gonna run once for every live window,
;; right?  Then first it checks if the window is already insufficient for
;; fill-column, and minimizes the fringes if so.  Otherwise, it looks up the
;; parent to check if it is a horizontal group.  If the width of that group
;; matches the frame width, then it will try to do something that results in
;; F-F-F arrangement. otherwise it falls back to the dumb method that would
;; result in F-2F-F.  Don't really need code to cover that since it seems
;; extremely hard to end up in that situation and those ppl can live with it.
(defun pad-fringes-to-fill-column (&rest _)
  (interactive)
  (cond
   ;; In horizontal group
   ((window-combined-p (selected-window) t)
    (let ((and-siblings (cddar (window--subtree (window-parent)))))
      (if (>= fill-column (window-total-width))
          ;; Fallback if the window is already insufficient for fill-column.
          ;; Here we could do something clever like temporarily decrease
          ;; the font size...  But that's a stretch goal if anything.
          (dolist (win and-siblings)
            (with-selected-window win
              (set-window-fringes (selected-window) 0)
              (unless window-divider-mode (window-divider-mode))))
        (dolist (win and-siblings)
          (with-selected-window win
            (let ((max-px (window-leftover-px-after-satisfying-fill-column)))
              (set-window-fringes (selected-window) (/ max-px 2) (/ max-px 2) t)
              (when window-divider-mode (window-divider-mode 0))))))))
   ;; Vertical group
   ((window-combined-p (selected-window))
    )
   ;; No group; must be root window
   ((frame-root-window-p (selected-window))
    (let ((max-px (window-leftover-px-after-satisfying-fill-column)))
      (set-window-fringes (selected-window) (/ max-px 2) (/ max-px 2))))))


(remove-hook 'window-size-change-functions #'pad-fringes-to-fill-column)
(remove-hook 'window-buffer-change-functions #'pad-fringes-to-fill-column)


;; I used `auto-save-visited-mode' for years.  But many Emacs features are noisy
;; on save, and now I'm tired of the noise.  We can configure the classic
;; `auto-save-mode' to do for us largely what auto-save-visited-mode did:
(setq save-all-timer (run-with-idle-timer 60 t #'save-some-buffers t))
(setq auto-save-timeout 5)
(add-hook 'find-file-hook
          (defun my-auto-recover-this-file ()
            (when (file-newer-than-file-p (or buffer-auto-save-file-name
                                              (make-auto-save-file-name))
                                          buffer-file-name)
              (recover-file buffer-file-name))))


(hookgen doom-after-init-hook
  (setq my-stim-collection (my-stim-collection-generate)))

;; try to make delete-file fast again
(advice-remove 'delete-file #'delete-file-projectile-remove-from-cache)

(defun my-write-roam-graph-tsv ()
  (let ((tsv-string
         (concat "src\tdest\n"
                 (mapconcat (lambda (pair)
                              (concat (car pair) "\t" (cadr pair)))
                            (org-roam-db-query [:select [source dest]
                                                :from links
                                                :where (= type "id")])
                            "\n"))))
    (f-write tsv-string 'utf-8 "/tmp/org-roam-digraph.tsv")))

(when nil
  (my-write-roam-graph-tsv)
  (setq feedbacks (my-read-lisp (f-read "/tmp/feedback_arcs.el")))
  ;; Yup, already sorted by src
  ;; (equal feedbacks (cl-sort feedbacks #'string-lessp :key #'car))
  (prog1 nil
    (setq foo
          (cl-loop
           for (src . dest) in feedbacks
           concat (concat "\n" (org-roam-node-title (org-roam-node-from-id src))
                          " --> " (org-roam-node-title (org-roam-node-from-id dest)))
           ))))



;;; roam db

(after! org-roam-db
  (fset 'org-roam-db-update-file #'my-org-roam-db-update-file)
  (fset 'org-roam-db-sync #'my-org-roam-db-sync))

(defun my-org-roam-db-update-file (&optional file-path _)
  "Version of `org-roam-db-update-file' calling `my-org-roam-pre-scan-hook'."
  (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
  (let ((content-hash (org-roam-db--file-hash file-path))
        (db-hash (caar (org-roam-db-query [:select hash :from files
                                           :where (= file $s1)] file-path)))
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

;; ;; Fix
;; (defun helm-get-firefox-user-init-dir (directory)
;;   "Guess the default Firefox user directory name."
;;   (with-temp-buffer
;;     (insert-file-contents
;;      (expand-file-name "profiles.ini" directory))
;;     (goto-char (point-min))
;;     (search-forward "Default=1")
;;     (search-backward "Path=")
;;     (file-name-as-directory (expand-file-name
;;                              (buffer-substring-no-properties
;;                               (match-end 0) (point-at-eol))
;;                              directory))))


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
