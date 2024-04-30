;; -*- lexical-binding: t; -*-

;; Improve org performance
(global-auto-composition-mode 0)
(setopt bidi-display-reordering nil)

(setopt org-timestamp-custom-formats '("%Y-%b-%d" . "%Y-%m-%d %a %H:%M"))
(setopt org-pretty-entities t)
(setopt org-archive-location "/home/kept/roam/noagenda/archive.org::datetree/")
(setopt org-clock-persist t)
(setopt org-clock-auto-clock-resolution t)
;; (setopt org-startup-folded 'nofold)
(setopt org-agenda-include-diary t)
(setopt citar-bibliography '("/home/kept/roam/refs/library_biblatex.bib"))
(setopt org-agenda-todo-list-sublevels nil)
(setopt org-agenda-todo-ignore-scheduled t)
(setopt org-agenda-dim-blocked-tasks nil) ;; perf
(setopt org-agenda-use-tag-inheritance '(todo search)) ;; perf
(setopt org-agenda-ignore-properties '(stats)) ;; perf
(setopt org-agenda-inhibit-startup t) ;; perf
(setopt org-archive-save-context-info '(time file itags olpath))
(setopt org-pomodoro-play-sounds nil)
(setopt org-export-backends '(html latex odt texinfo))
(setopt org-export-with-toc nil)
(setopt org-clock-out-remove-zero-time-clocks t)
(setopt org-clock-idle-time 5)
(setopt org-hide-leading-stars nil)
(setopt org-clock-mode-line-total 'today)
(setopt org-clock-in-resume t)
(setopt org-catch-invisible-edits 'smart)
(setopt org-ctrl-k-protect-subtree t)
(setopt org-M-RET-may-split-line '((headline . nil) (default . t)))
(setopt org-cycle-separator-lines 3)
(setopt org-datetree-add-timestamp nil)
(setopt org-edit-src-content-indentation 0)
;; (setopt org-ellipsis "⤵")
(setopt org-ellipsis "…")
(setopt org-hide-emphasis-markers t) ; hide the *, =, and / markers
(setopt org-image-actual-width '(200)) ; use #ATTR if available, else 200 px
;; (setopt org-latex-compiler "xelatex") ; allow unicode (åäö) in VERBATIM blocks
(setopt org-log-done 'time)
(setopt org-log-into-drawer t) ; hide spam
(setopt org-modules '(org-id ol-info ol-eww)) ;; `org-eww-copy-for-org-mode'
(setopt org-use-speed-commands t)
(setopt org-clock-x11idle-program-name (or (executable-find "xprintidle") "x11idle"))
(setopt org-replace-disputed-keys t)
(setopt org-tags-column 0)
(setopt org-download-heading-lvl nil)
(setopt org-download-image-dir "img/")
(setopt org-node-creation-fn #'org-capture)

(use-package org-node
  :hook ((org-mode . org-node-backlink-mode)
         (org-mode . org-node-cache-mode)))

(setq org-node-perf-assume-coding-system 'utf-8-unix)

(setopt org-node-format-candidate-fn
        (lambda (node title)
          (if-let ((olp (org-node-olp node)))
              (concat (string-join olp " > ") " > " title)
            title)))

(after! org-node
  ;; Make sure the extracted subtree inherits any CREATED property,
  ;; else creates one for today
  (advice-add 'org-node-extract-subtree :around
              (lambda (orig-fn &rest args)
                (let ((parent-creation
                       (save-excursion
                         (while (not (or (bobp) (org-entry-get nil "CREATED")))
                           (org-up-heading-or-point-min))
                         (org-entry-get nil "CREATED"))))
                  (apply orig-fn args)
                  (org-entry-put nil "CREATED"
                                 (or parent-creation (format-time-string "[%F]")))))))

(setq org-capture-templates
      '(("n" "ID node")
        ("nc" "Capture to ID node (maybe creating it)"
         plain (function org-node-capture-target) nil
         :empty-lines-after 1)
        ("nv" "Visit ID node (maybe creating it)"
         plain (function org-node-capture-target) nil
         :jump-to-captured t
         :immediate-finish t)
        ("ni" "Instantly create ID node without content & without visiting"
         plain (function org-node-capture-target) nil
         :immediate-finish t)))

;; (after! org-roam-mode
;;   ;; (advice-remove 'org-roam-backlinks-get #'org-node--fabricate-roam-backlinks)
;;   (advice-add 'org-roam-backlinks-get :override #'org-node--fabricate-roam-backlinks))

;; (setq-default org-display-custom-times t) ;; could it cause org-element bugs due to daily page titles?
(setopt org-agenda-files
        (-filter #'file-exists-p '(
                                   ;; "/home/kept/roam/"   ;; slowww
                                   ;; "/home/kept/roam/daily/" ;; sloww
                                   ;; "/home/kept/roam/refs/"
                                   ;; "/home/kept/roam/frozen/"
                                   "/home/kept/roam/grismartin/pages/"
                                   ;; to always cache the org-id locations
                                   "/home/me/.doom.d/elfeed.org")))

;; (setopt org-babel-load-languages '((R . t)
;;                                    (emacs-lisp . t)
;;                                    (calc . t)
;;                                    (ditaa . t)
;;                                    (sqlite . t)
;;                                    (dot . t)))

;; (setopt org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
;;                                    (todo . " %i %-32b") ;; have breadcrumbs
;;                                    (tags . " %i %-12:c")
;;                                    (search . " %i %-12:c")))
;; (setopt org-agenda-custom-commands '(("b" todo "NEXT")
;;                                      ("w" todo "WAITING")
;;                                      ("p" todo "PROCRASTINATING")
;;                                      ("c" tags-todo "+active")))
;; (setopt org-agenda-tag-filter-preset '("-exclude"))


;; For inline-anki: override Org's underlines to represent cloze deletions and
;; make them look appropriate for that.
;; (set-face-attribute 'underline () :box t)
(defface my-cloze '((t . (:box t))) "Cloze face")
(setq org-emphasis-alist '(("*" bold)
                           ("/" italic)
                           ("_" my-cloze)
                           ("=" org-verbatim verbatim)
                           ("~" org-code verbatim)
                           ("+" (:strike-through t))))

(add-hook 'org-noter-notes-mode-hook #'abbrev-mode)
(add-hook 'org-noter-notes-mode-hook (l'rainbow-delimiters-mode 0))
(add-hook 'org-mode-hook #'my-org-setup-prettify)
;; (add-hook 'org-mode-hook #'org-resolve-clocks 95)
;; (add-hook 'org-mode-hook #'org-clock-persistence-insinuate)
(add-hook 'org-clock-in-hook #'org-clock-save)
(add-hook 'org-clock-out-hook #'bh/clock-out-maybe 90)
(add-hook 'text-mode-hook #'turn-off-smartparens-mode)

(defun my-org-setup-prettify ()
  (setq prettify-symbols-alist
        (cl-union prettify-symbols-alist
                  '(;; Still waiting for the Year of AsciiMath on Org-mode...
                    ("\\vdots" . "⋮")
                    ("\\implies" . "⟹")
                    ("\\sqrt" . "√")
                    ("\\ldots" . "…")))))

(after! org
  (unless after-init-time
    (setq debug-on-error t)
    (error (message "Org loaded during init, I don't want this")))
  ;; Regenerate org-id-locations if it's gone
  (require 'org-id)
  (unless (and (file-exists-p org-id-locations-file)
               (org-id-locations-load)
               (not (hash-table-empty-p org-id-locations)))
    (org-id-update-id-locations
     (--filter (and (not (string-search "/logseq/bak/" it))
                    (not (string-search "/logseq/version-files/" it)))
               (--mapcat (directory-files-recursively it "\\.org$")
                         '("/home/kept/roam/"
                           "/home/kept/archive/"
                           "/home/me/.doom.d/")))))

  (require 'named-timer)
  (named-timer-run :my-clock-reminder nil 600
                   (defun my-clock-remind ()
                     (when (org-clock-is-active)
                       (message (concat "Currently working on: "
                                        org-clock-current-task)))))
  ;; If using Doom's Org
  (if (modulep! :lang org)
      ;; fix interference with org-transclusion
      (advice-remove 'org-link-search '+org--recenter-after-follow-link-a)
    ;; if not using Doom's org
    ;; (require 'org-indent)
    ;; (add-hook 'org-mode-hook #'org-indent-mode)
    (my-change-latex-scale) ;; Bigger LaTeX preview
    ;; Adapt LaTeX preview scale to the font zoom
    (add-hook 'text-scale-mode-hook #'my-change-latex-scale)))

(after! ox-latex
  ;; Prettify code-snippets in exported pdf.
  (setopt org-latex-listings t)
  (setopt org-latex-listings-options '(("basicstyle" "\\small"))) ; small code font
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (add-to-list 'org-latex-packages-alist '("" "booktabs"))
  ;; Add letter class so I can... write a cover letter. yup, my life
  (add-to-list 'org-latex-classes
               '("letter"
                 "\\documentclass[11pt]{letter}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(after! org-transclusion
  (set-face-background 'org-transclusion "#222")
  (setopt org-transclusion-exclude-elements '(property-drawer comment keyword)))


;;; Workaround the tide of org-element parser bugs since 9.5 rewrite

;; (setq org-element-use-cache nil)
