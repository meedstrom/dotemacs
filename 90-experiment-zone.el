;; Experiment zone -*- lexical-binding: t; -*-

;; (find-olpath 20 )
;; (find-olpath 23 )
;; (find-olpath 24 )
;; (find-olpath 94 )
;; (find-olpath 96 )
;; (find-olpath 109 )
;; (find-olpath 150 )
;; (find-olpath 200 )
;; (find-olpath 220 )
;; (find-olpath 221 )



;; (construct-tree)



(defun find-olpath (line tree)
  "Given LINE number, return a list of line numbers corresponding to ancestor
subtrees."
  ;; car is line number
  ;; cdr is outline level (nubmer of heading stars) of the local subtree
  (let* ((relevant (cl-loop for row in tree
                            when (> line (car row))
                            collect row))
         (reversed (nreverse relevant))
         (curr-level (cdr (car reversed)))
         (res (list (car (car reversed)))))
    (cl-loop for row in reversed
             when (> curr-level (cdr row))
             do (setq curr-level (cdr row))
             (push (car row) res))
    res
    ))

;; (defun find-olpath (line tree)
;;   "Given LINE number, return a list of line numbers corresponding to ancestor
;; subtrees."
;;   (let* ((relevant (cl-loop for row in tree
;;                            when (> line (plist-get row :lnum))
;;                            collect row))
;;          (reversed (nreverse relevant))
;;          (curr-level (plist-get (car reversed) :lvl))
;;          res )
;;     (cl-loop for row in reversed
;;              when (>= curr-level (plist-get row :lvl))
;;              do (setq curr-level (plist-get row :lvl))
;;              and collect (plist-get row :lnum))
;;     ))

;; (defun find-olpath (line)
;;   "Given LINE number, return a list of line numbers corresponding to ancestor
;; subtrees."
;;   (let* ((relevant (cl-loop for row in tbl
;;                            when (> line (plist-get row :lnum))
;;                            collect row))
;;          (reversed (nreverse relevant))
;;          (curr-level (plist-get (car reversed) :lvl))
;;          res )
;;     (cl-loop for row in reversed
;;              when (>= curr-level (plist-get row :lvl))
;;              do (setq curr-level (plist-get row :lvl))
;;              (push (plist-get row :lnum) res))
;;     res))

;; (defun find-olpath (line)
;;   (let* ((relevant (cl-loop for row in tbl
;;                            when (> line (plist-get row :lnum))
;;                            collect row))
;;          (reversed (nreverse relevant))
;;          (curr-level (plist-get (car reversed) :lvl))
;;          (res (list (cons line curr-level))))
;;     (cl-loop for row in reversed
;;              when (> curr-level (plist-get row :lvl))
;;              do (push (cons (plist-get row :lnum)
;;                             (setq curr-level (plist-get row :lvl)))
;;                       res))
;;     res))


;; Reinvent the wheel.  Not so happy with `lwarn'...  A wrapper that hardcodes
;; arguments TYPE and LEVEL will indent more compactly like vanilla `error'.
;; Further, the level :error is not a real error and allows program flow to
;; continue!  And also not so happy with `error' as an user-facing warning,
;; because it's super easy to miss when `debug-on-error' is not t.
;;
;; If I have to write so much code every time to emit an error, I may not
;; bother to write them in the first place.  It needs to be easy.

(defun die (&rest args)
  "Like `error' but allow specifying keywords :level and :type anywhere in ARGS,
and display the error clearly using `display-warning'.

Unlike `lwarn', LEVEL is a proper non-keyword symbol, i.e. do not
write :level :warning, but :level 'warning."
  ;; (declare (indent defun))
  (let (level type true-args err-string keyword)
    (cl-labels
        ((eat (args)
           (if (not (keywordp (car args)))
               (push (pop args) true-args)
             (setq keyword (pop args))
             (cond
              ((eq ':level keyword)
               (setq level
                     (intern
                      (concat ":" (symbol-name (pop args))))))
              ((eq ':type keyword)
               (setq type (pop args)))))
           (unless (null args)
             (eat args))))
      (eat args)
      (setq args (reverse true-args))
      (setq err-string (format (car args) (cdr args))))
    (display-warning type err-string (or level :error))
    (error "%s" err-string)))


;;

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
