;; Reverse some Doom Emacs defaults -*- lexical-binding: t; -*-

;; Yep... give me a long slow init please!
(add-hook 'emacs-startup-hook
          (defun my-eager-startup ()
            (run-hooks 'doom-first-input-hook)
            (run-hooks 'doom-first-buffer-hook)
            (run-hooks 'doom-first-file-hook)))

;; Gimme readable backup names, because I rename files and dirs all the time.
(advice-remove #'make-backup-file-name-1 #'doom-make-hashed-backup-file-name-a)

;; I find customize a handy exploration tool, so gimme access
(put 'customize-themes 'disabled nil)
(put 'customize-group 'disabled nil)
(put 'customize-changed 'disabled nil)
(put 'customize-face 'disabled nil)
(put 'customize-variable 'disabled nil)

;; Doom puts eww-bookmarks in doomemacs/.local/cache, which I find dangerous
;; since I may unthinkingly wipe that entire folder.  Put it where I won't
;; delete it: my own .doom.d.  Do same for abbrev.  This stuff is NOT mere
;; "cache".
(setopt eww-bookmarks-directory doom-user-dir)
(setopt abbrev-file-name (expand-file-name "abbrevs" doom-user-dir))

;; I'll do M-x dlnm RET when I want it (couple of occasions per year)
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  #'display-line-numbers-mode)

(after! vertico
  (keymap-unset vertico-map "<backspace>" t))

(after! org
  ;; Having exactly two states makes for comfy toggling.
  (setopt org-todo-keywords '((sequence "TODO" "DONE"))))

;; This "doom-docs-mode" was a nice idea, but I find it mainly gets in my way.
(fset 'doom-docs-org-mode #'ignore)
(fset 'doom-docs--toggle-read-only-h #'ignore)

;; I lost work !!!  Why wouldn't it ask to save unsaved buffers?  Some Vim norm?
(after! doom-keybinds
  (keymap-set doom-leader-map "q q" #'save-buffers-kill-emacs))

(after! eshell
  (setopt eshell-input-filter #'eshell-input-filter-default)
  (setopt eshell-scroll-to-bottom-on-input nil)
  ;; Give me access to emacs --help
  (fmakunbound #'eshell/emacs))

(after! esh-mode
  (keymap-set eshell-mode-map "C-l" #'recenter-top-bottom))

(after! dired
  (keymap-set dired-mode-map "q" #'kill-current-buffer))

(after! ws-butler
  ;; A nil setting jibes very badly with `auto-save-visited-mode'. PR:
  ;; https://github.com/doomemacs/doomemacs/pull/7843
  (setopt ws-butler-keep-whitespace-before-point t))

(remove-hook 'dired-mode-hook #'dired-omit-mode) ;; Don't hide any files
(remove-hook 'term-mode-hook #'hide-mode-line-mode)
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; FIXME: This snippet makes the setting t before and after init but not during
;; init, which is where I want it to be t.  Guess I'll just have to make a
;; habit of launching emacs every time with "doom sync && emacs" while I'm
;; developing a package.  Which is, you know... every time I relaunch emacs.
;; Maybe a fix would be to load-prefer-newer your local packages?
(setopt load-prefer-newer t) ;; don't spend another minute confused by this
(general-after-init
  (setopt load-prefer-newer t))

;; "Because an 80 char wide Emacs window starts wrapping at 79."
;; --Guido van Rossum, on why Python style mandates 79 columns
;; https://www.reddit.com/r/learnpython/comments/1h2eug
;;
;; Even in other editors, a fill-column of exactly 80 can't ever play well with
;; an editor window that's exactly 80 wide.  There's a reason many traditional
;; styles actually mandate 72 or something.  79 is the absolute upper limit.
;; 80 is for people who don't realize how bad their code looks on real 80-wide
;; windows, because they always happen to have some margin i.e. splitting their
;; screen gives them 2*87 or some such, not 2*80.
;;
;; So why don't I just set my frames to a multiple of at least 81?  My current
;; hardware.  My monitor+font fits exactly 2*80, yay!  Fully optimized screen
;; estate!  But it's a cursed windfall...  Shrinking the font one notch is not
;; an option since that would take it all the way down to 2x110 or so (wasn't
;; hiDPI supposed to give us more granular font sizes?).  I want the text as
;; big as will fit.
;;
;; BUG None of these settings work for my repos in
;; doomemacs/.local/straight!  Only adding an .editorconfig does.
(after! doom-editor
  (setq-default fill-column 79))
(general-after-init
  (setq-default fill-column 79))
(setq-default fill-column 79)
(add-hook! '(emacs-lisp-mode-hook prog-mode-hook) :depth 99
  (lambda () (setq fill-column 79)))
