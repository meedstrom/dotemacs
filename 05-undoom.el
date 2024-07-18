;; Reverse some Doom Emacs defaults -*- lexical-binding: t; -*-

;; Yep... give me a longer init please!  Each of these hooks is slow enough
;; on my machine to have a perceptible delay, so this design actually annoys
;; more than just preloading everything so it's done when I start using Emacs.
(add-hook 'emacs-startup-hook
          (defun my-eager-startup ()
            (run-hooks 'doom-first-input-hook)
            (run-hooks 'doom-first-buffer-hook)
            (run-hooks 'doom-first-file-hook)))

;; Gimme readable backup names.  I rename files and dirs A LOT, so I tend to
;; have to manually look in the backups folder when I seek an old version,
;; remembering what the file used to be called.
(advice-remove #'make-backup-file-name-1
               #'doom-make-hashed-backup-file-name-a)

;; The prompt "Recover file #20w9i023rif394fmw9d3#?" makes me uncertain which
;; file it's asking about recovering, since it's not a given that recover-file
;; is being called for the current buffer (it's very likely of course for a
;; file that I just now opened, but sometimes I come back to Emacs after a
;; while and there's that prompt waiting for me, which must have been triggered
;; by an idle timer or something so I really would like the prompt to tell me
;; which file it's talking about)
(advice-remove #'make-auto-save-file-name
               #'doom-make-hashed-auto-save-file-name-a)

;; I find customize a handy exploration tool, so gimme access
(put 'customize-themes 'disabled nil)
(put 'customize-group 'disabled nil)
(put 'customize-changed 'disabled nil)
(put 'customize-face 'disabled nil)
(put 'customize-variable 'disabled nil)

;; Doom puts eww-bookmarks in doomemacs/.local/cache, which I find dangerous
;; since I often wipe that entire folder.  Put it where I won't delete it: my
;; own .doom.d.  Do same for abbrev.  This stuff is NOT mere "cache".
(setopt eww-bookmarks-directory doom-user-dir)
(setopt abbrev-file-name (expand-file-name "abbrevs" doom-user-dir))

;; I do M-x dlnm RET when I want it (couple of occasions per year), so it's not
;; an option to do as the manual suggests and set `display-line-numbers-type'
;; nil since then M-x dlnm RET won't do anything.
(remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
  #'display-line-numbers-mode)

(after! vertico
  (keymap-unset vertico-map "<backspace>" t))

(when (modulep! :lang org)
  (after! org
    ;; Having exactly two states makes for comfy toggling.
    (setopt org-todo-keywords '((sequence "TODO" "DONE")))))

;; This "doom-docs-mode" was a nice idea, but I find it mainly gets in my way.
(fset 'doom-docs-org-mode #'ignore)
(fset 'doom-docs--toggle-read-only-h #'ignore)

;; I lost work!!!  Why would it NOT ask to save unsaved buffers?  Some Vim
;; norm?  In retrospect, it seems that auto-save should have saved me, but I
;; suspect that I had the file open in several emacsen, which makes all such
;; things unpredictable.
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

(remove-hook 'dired-mode-hook #'dired-omit-mode) ;; Don't hide any files
(remove-hook 'term-mode-hook #'hide-mode-line-mode)
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; FIXME: This snippet makes the setting t before and after init but not during
;; init, which is where I want it to be t.  Guess I'll just have to make a
;; habit of launching emacs every time with "doom sync && emacs" while I'm
;; developing a package.  Which is, you know... every time I relaunch emacs.
;; Maybe a fix would be to load-prefer-newer your 'local packages?
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
;; hiDPI supposed to give us more granular font sizes?).
;;
;; BUG None of these settings apply to my own package repos in
;; ~/doomemacs/.local/straight/REPO/!  Only adding an .editorconfig does.
(after! doom-editor
  (setq-default fill-column 79))
(general-after-init
  (setq-default fill-column 79))
(setq-default fill-column 79)
(add-hook! '(emacs-lisp-mode-hook prog-mode-hook) :depth 99
  (lambda () (setq fill-column 79)))

;; Backups saved my skin in 2015, 2016, 2018, and 2020.
;; So I should not stop using them until 2030 or so, given no more incidents.
;; And I'll probably keep them forever.  The root issue with "GiT cAn RePlAcE
;; bAcKuPs!" is I don't commit regularly in every project nor do I even have a
;; git project everywhere.
(setq make-backup-files t)
