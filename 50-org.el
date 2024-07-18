;; -*- lexical-binding: t; -*-

(setopt org-timestamp-custom-formats '("%Y-%b-%d" . "%Y-%m-%d %a %H:%M"))
(setopt org-pretty-entities t)
(setopt org-archive-location "/home/kept/roam/noagenda/archive.org::datetree/")
(setopt org-clock-persist t)
(setopt org-clock-auto-clock-resolution t)
;; (setopt org-startup-folded 'nofold)
(setopt citar-bibliography '("/home/kept/roam/refs/library_biblatex.bib"))
;; (setopt org-agenda-todo-list-sublevels nil)
(setopt org-agenda-include-diary nil) ;; perf... :(
(setopt org-agenda-dim-blocked-tasks nil) ;; perf
(setopt org-agenda-use-tag-inheritance '(todo search)) ;; perf
(setopt org-agenda-ignore-properties '(stats)) ;; perf
(setopt org-agenda-inhibit-startup t) ;; perf
(setopt org-archive-save-context-info '(time file itags olpath))
(setopt org-export-backends '(html latex odt texinfo))
(setopt org-export-with-toc nil)
(setopt org-timestamp-formats (cons "%Y-%m-%d" "%Y-%m-%d %a %H:%M"))
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
(setopt org-image-max-width 300)
;; (setopt org-image-actual-width '(200)) ; use #ATTR if available, else 200 px
;; (setopt org-latex-compiler "xelatex") ; allow unicode (åäö) in VERBATIM blocks
(setopt org-log-done 'time)
(setopt org-log-into-drawer t) ; hide spam
(setopt org-modules '(org-id org-habit org-gamify ol-info ol-eww)) ;; `org-eww-copy-for-org-mode'
(setopt org-use-speed-commands t)
(setopt org-clock-x11idle-program-name (or (executable-find "xprintidle") "x11idle"))
(setopt org-replace-disputed-keys t)
(setopt org-tags-column 0)
(setopt org-startup-indented t)
(setopt org-download-heading-lvl nil)
(setopt org-download-image-dir "img/")
(setopt org-clock-kill-emacs-query nil) ;; fix bug

;; Make org faster
(global-auto-composition-mode 0)
(setopt bidi-display-reordering nil)

(setopt consult-fontify-max-size 100000) ;; 0.1x default (weak computer)

(setopt undo-limit (* 16 1000 1000)) ;; 16 MB, not default 160 kB
(setopt undo-strong-limit (* 24 1000 1000))

;; (setq org-node--debug nil)
(use-package org-node
  :hook ((org-mode . org-node-cache-mode))
  :init
  (setopt org-node-extra-id-dirs '("/home/kept/roam/" "/home/me/.doom.d/"))
  (setopt org-node-eagerly-update-link-tables t)
  (setopt org-node-perf-assume-coding-system 'utf-8-unix)
  :config
  (org-node-complete-at-point-mode)
  ;; (org-node-backlink-global-mode)
  ;; (org-node-roam-db-shim-mode)
  (org-node-fakeroam-nosql-mode)
  (org-node-fakeroam-redisplay-mode)


  ;; Make sure the extracted subtree inherits any CREATED property,
  ;; else creates one for today
  (advice-add 'org-node-extract-subtree :around
              (defun my-inherit-creation-date (orig-fn &rest args)
                (let ((parent-creation
                       (save-excursion
                         (without-restriction
                           (while (not (or (org-entry-get nil "CREATED")
                                           (bobp)))
                             (org-up-heading-or-point-min)))
                         (org-entry-get nil "CREATED"))))
                  (apply orig-fn args)
                  ;; Now in the new buffer
                  (org-entry-put nil "CREATED"
                                 (or parent-creation
                                     (format-time-string "[%F %a]")))))))

;; Different sets of settings to test

;; (setopt org-node-ask-directory "/home/kept/roam")
;; (setopt org-node-make-file-level-nodes nil)
;; (setq org-node-slug-fn #'org-node-slugify-like-roam)
;; (setq org-node-slug-fn #'org-node-slugify-as-url)
;; (setq org-node-creation-fn #'org-node-new-by-roam-capture)
;; (setq org-node-creation-fn #'org-node-new-file)
(setq org-node-creation-fn #'org-capture)

;; (setq org-node-alter-candidates t) ;; More completions than if nil
;; (setq org-node-alter-candidates nil)

(setq org-node-filter-fn
      (lambda (node)
        (not (or (string-search "archive/" (org-node-get-file-path node))
                 (string-search "noagenda/" (org-node-get-file-path node))))))

;; (require 'org-roam)

(add-hook 'doom-load-theme-hook
          (defun my-theme-mod-org ()
            (after! org-roam
              ;; for backlinks buffer
              (set-face-attribute 'org-roam-title nil :height 1.5))))

;; (setq-default org-display-custom-times t) ;; could it cause org-element bugs due to daily page titles?


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
(add-hook 'org-noter-notes-mode-hook (lambda () rainbow-delimiters-mode 0))
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
    (message "Org loaded during init, I don't want this"))


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

(setq org-element-use-cache nil)
