;; -*- lexical-binding: t; -*-

(setopt org-agenda-todo-list-sublevels nil)
(setopt org-agenda-todo-ignore-scheduled t)
(setopt org-agenda-dim-blocked-tasks nil) ;; Speed up the agenda
(setopt org-agenda-use-tag-inheritance '(todo search)) ;; Speed up the agenda
(setopt org-agenda-ignore-properties '(stats)) ;; Speed up the agenda
(setopt org-agenda-inhibit-startup t) ;; Speed up the agenda
(setopt org-archive-location "/home/kept/roam/noagenda/archive.org::datetree/")
(setopt org-archive-save-context-info '(time file itags olpath))
(setopt org-attach-id-dir "static/")
(setopt org-pomodoro-play-sounds nil)
(setopt org-clock-out-remove-zero-time-clocks t)
(setopt org-clock-persist t)
(setopt org-clock-idle-time 5)
(setopt org-hide-leading-stars nil)
(setopt org-clock-mode-line-total 'today)
(setopt org-clock-auto-clock-resolution t)
(setopt org-clock-in-resume t)
(setopt org-catch-invisible-edits 'smart)
(setopt org-ctrl-k-protect-subtree t)
(setopt org-agenda-include-diary t)
(setopt org-cycle-separator-lines 3)
(setopt org-datetree-add-timestamp nil)
(setopt org-edit-src-content-indentation 0)
;; (setopt org-ellipsis "⤵")
;; (setopt org-ellipsis "…")
(setopt org-hide-emphasis-markers t) ; hide the *, =, and / markers
(setopt org-image-actual-width '(200)) ; use #ATTR if available, else 200 px
(setopt org-insert-heading-respect-content t)
;; (setopt org-latex-compiler "xelatex") ; allow unicode (åäö) in VERBATIM blocks
(setopt org-log-done 'time)
(setopt org-log-into-drawer t) ; hide spam
(setopt org-modules '(org-id ol-info ol-eww)) ;; `org-eww-copy-for-org-mode'
(setopt org-pretty-entities t)
(setopt org-use-speed-commands t)
(setopt org-clock-x11idle-program-name (or (executable-find "xprintidle") "x11idle"))
(setopt org-replace-disputed-keys t)
(setopt org-timestamp-custom-formats '("%Y-%b-%d" . "%Y-%m-%d %a %H:%M"))
(setq-default org-display-custom-times t)
(setopt org-agenda-files
        (-filter #'file-exists-p '(
                                   ;; "/home/kept/roam/"   ;; slowww
                                   ;; "/home/kept/roam/daily/" ;; sloww
                                   ;; "/home/kept/roam/refs/"
                                   ;; "/home/kept/roam/frozen/"
                                   "/home/kept/roam/grismartin/pages/"
                                   ;; to always cache the org-id locations
                                   "/home/me/.doom.d/elfeed.org")))
;; (setopt org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
;;                                    (todo . " %i %-32b") ;; have breadcrumbs
;;                                    (tags . " %i %-12:c")
;;                                    (search . " %i %-12:c")))
;; (setopt org-agenda-custom-commands '(("b" todo "NEXT")
;;                                      ("w" todo "WAITING")
;;                                      ("p" todo "PROCRASTINATING")
;;                                      ("c" tags-todo "+active")))
;; (setopt org-agenda-tag-filter-preset '("-exclude"))


;; For inline-anki: override underlines to represent cloze deletions, as I
;; never use underlines anyway
(defface my-cloze '((t . (:box t))) "Cloze face")
(setq org-emphasis-alist '(("*" bold)
                           ("/" italic)
                           ("_" my-cloze)
                           ("=" org-verbatim verbatim)
                           ("~" org-code verbatim)
                           ("+" (:strike-through t))))

(after! ox-latex
  (add-to-list 'org-latex-classes
               '("letter"
                 "\\documentclass[11pt]{letter}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(add-hook 'delve-mode-hook #'delve-compact-view-mode)
(after! delve
  ;; It normally inherits from org-roam-title, which I find too big
  (set-face-attribute 'delve-title-face () :inherit 'org-document-title))

;; See also the doom module (C-h d m :tools biblio)
(setopt citar-bibliography '("/home/kept/roam/refs/library_biblatex.bib"))

(add-hook 'org-noter-notes-mode-hook #'abbrev-mode)
(add-hook 'org-noter-notes-mode-hook (l'rainbow-delimiters-mode 0))
(add-hook 'org-mode-hook #'my-org-prettify)
;; (add-hook 'org-mode-hook #'org-resolve-clocks 95)
;; (add-hook 'org-mode-hook #'org-clock-persistence-insinuate)
(add-hook 'org-clock-in-hook #'org-clock-save)
(add-hook 'text-mode-hook (defun my-kill-smartparens () (smartparens-mode 0)))

;; Improve org performance
(global-auto-composition-mode 0)
(setopt bidi-display-reordering nil)

(setopt my-org-prettify-alist
        '(;; Nice for writing/reading equations in plaintext.
          ;; Still waiting for the year of AsciiMath on Org-mode... 2015 I
          ;; called it...
          ("[" . "［")
          ("]" . "］")
          ("-" . "－")
          ("=" . "＝")
          ("+" . "＋")
          ("\\vdots" . "⋮")
          ("\\implies" . "⟹")
          ("\\sqrt" . "√")
          ("\\ldots" . "…")))

(defun my-org-prettify ()
  (setq prettify-symbols-alist
        (cl-union prettify-symbols-alist my-org-prettify-alist)))

(after! org
  (require 'named-timer) ;; an indispensable 50-line library
  (named-timer-run :my-clock-reminder nil 600
                   (defun my-clock-remind ()
                     (when (org-clock-is-active)
                       (message (concat "Currently working on: "
                                        org-clock-current-task))))))


;; (add-hook 'org-mode-hook
;;           (defun my-warn-org-init ()
;;             (warn (message "Org loaded during init, I don't want this"))))

;; (add-hook 'after-init-hook
;;           (defun my-warn-org-init-remove ()
;;             (remove-hook 'org-mode-hook #'my-warn-org-init)))

(after! org
  (unless after-init-time
    (toggle-debug-on-error)
    (error (message "Org loaded during init, I don't want this")))
  (setopt org-babel-load-languages '((R . t)
                                     (emacs-lisp . t)
                                     (calc . t)
                                     (ditaa . t)
                                     (sqlite . t)
                                     (dot . t)))
  ;; Stuff to do if I'm not using Doom's Org
  (unless (fboundp '+org-init-org-directory-h)
    (use-package! org-indent
      :config
      (add-hook 'org-mode-hook #'org-indent-mode))
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


;;; Workaround the floodwave of org-element parser bugs since 9.5 rewrite

;; Workaround a startup bug that appeared on update 2024-01-15 (did doom change
;; default tab-width? i don't think so, but something strange's going on with
;; the org element parser)
(setq-default tab-width 8)
(hookgen org-mode-hook (setq tab-width 8))
(setopt org-element-use-cache nil) ;; heavily bugged, cba2debug
(after! org
  (require 'org-element) ;; org-element-at-point not found ???
  (require 'org-archive) ;; `org-add-archive-files' ???
  (require 'org-macs) ;; invalid-function org-element-with-disabled-cache ???
  ;; (require 'ox-html)  ;; htmlize not found , maybe this helps
  ;; (org-require-package 'htmlize) ;; cannot be found!!! have to install it in packages.el
  )