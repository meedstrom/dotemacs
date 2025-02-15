;; -*- lexical-binding: t; -*-

;;; Init Elpaca, a package manager (using snippet from their README)
(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
;; CUSTOM
(defvar elpaca-builds-directory
  (expand-file-name (concat "builds" emacs-version "/") elpaca-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; (when (>= emacs-major-version 30)
;;   ;; Reduce to minimum set of ignores, to upgrade all upgradables on demand
;;   (setq elpaca-ignored-dependencies
;;         (append
;;          ;; Downgrades-in-disguise
;;          '(cl-lib cl-generic nadvice use-package bind-key)
;;          ;; Need special upgrade steps for now (#216)
;;          '(seq map tramp)
;;          ;; No repo, or repo is the GNU Emacs repo (250 MB download)
;;          (cl-loop
;;           for dep in elpaca-ignored-dependencies
;;           when (let ((repo (plist-get (elpaca-recipe dep) :repo)))
;;                  (or (not repo)
;;                      (equal repo "https://github.com/emacs-mirror/emacs")))
;;           collect dep)))
;;   (delq 'eglot elpaca-ignored-dependencies))

;; Setup use-package
(elpaca elpaca-use-package
  (setopt use-package-always-ensure t)
  (setopt use-package-enable-imenu-support t)
  (setopt use-package-compute-statistics t)
  (if init-file-debug
      (setopt use-package-verbose t)
    (setopt use-package-expand-minimally t))
  ;; Make the :ensure keyword call Elpaca
  (elpaca-use-package-mode))

;; Require packages I need during init
(use-package no-littering)
(elpaca-wait)
;; (use-package on :ensure (:fetcher gitlab :repo "ajgrf/on.el"))
(use-package defrepeater)
(use-package crux)
(use-package dash)
(elpaca-wait)

;; Load some separated-out initfiles
(let ((default-directory user-emacs-directory))
  (dolist (file (directory-files "self-contained/" t "^\\w.*\\.el"))
    (load-file file)))
(load-file (setq custom-file (locate-user-emacs-file "custom.el")))


;;;; Unconfigured packages

;; Pkg dev
(elpaca (unpackaged :repo "https://github.com/alphapapa/unpackaged.el"))
(elpaca help-find)
(elpaca htmlize)
(elpaca keymap-utils) ;; prefix kmu-*
(elpaca kv)
(elpaca package-lint)
;; (elpaca persist)
(elpaca ts)
;; (elpaca sisyphus)

;; Untried

;; (when (>= emacs-major-version 30)
;;   (elpaca (casual-avy :repo "https://github.com/kickingvegas/casual-avy"))
;;   (elpaca (casual-dired :repo "https://github.com/kickingvegas/casual-dired")))
(elpaca dogears)
;; (elpaca tree-sitter)
;; (elpaca tree-sitter-langs)
(elpaca ranger)

;; The rest
(elpaca (ess-rproj :repo "https://github.com/chainsawriot/ess-rproj"))
(elpaca (kbd-mode :repo "https://github.com/kmonad/kbd-mode"))
(elpaca (org-gamify :repo "https://bitbucket.org/eeeickythump/org-gamify"))
(elpaca academic-phrases)
(elpaca backup-walker)
(elpaca doom-themes)
(elpaca drag-stuff)
(elpaca ef-themes)
(elpaca git-timemachine)
(elpaca hacker-typer)
(elpaca multiple-cursors)
(elpaca mw-thesaurus)
(elpaca peep-dired)
(elpaca pinboard-popular)
(elpaca shelldon)
(elpaca vc-msg)
(elpaca wgrep)
(elpaca xr)
;; (elpaca (app-launcher :repo "https://github.com/SebastienWae/app-launcher"))
;; (elpaca (awesome-tray :repo "https://github.com/manateelazycat/awesome-tray"))
;; (elpaca (casual :repo "https://github.com/kickingvegas/casual"))
;; (elpaca (consult-ffdata :repo "https://github.com/chen-chao/consult-ffdata"))
;; (elpaca (define-repeat-map :repo "https://tildegit.org/acdw/define-repeat-map.el"))
;; (elpaca (dired-hist :repo "https://github.com/karthink/dired-hist"))
;; (elpaca (emacs-piper :repo "https://gitlab,com/howardabrams/emacs-piper"))
;; (elpaca (firefox-bookmarks :repo "https://github.com/tangxinfa/firefox-bookmarks"))
;; (elpaca (math-delimiters :repo "https://github.com/oantolin/math-delimiters"))
(elpaca (org-super-links :repo "https://github.com/toshism/org-super-links"))
;; (elpaca bm)
;; (elpaca bui)
;; (elpaca buttercup)
;; (elpaca chatgpt-shell)
;; (elpaca consult-org-roam)
;; (elpaca copilot)
;; (elpaca copy-as-format)
;; (elpaca ctrlf)
;; (elpaca cycle-buffer) ;; last updated 1997, but more useful commands than iflipb
;; (elpaca director)
;; (elpaca dired-git-info)
;; (elpaca dired-hacks)
;; (elpaca disable-mouse)
;; (elpaca disk-usage)
;; (elpaca dmenu)
;; (elpaca elisp-autofmt)
;; (elpaca elisp-format)
;; (elpaca ement)
;; (elpaca esup)
;; (elpaca format-all)
;; (elpaca gif-screencast)
;; (elpaca git-messenger)
;; (elpaca helm-firefox)
;; (elpaca mastodon)
;; (elpaca mediawiki)
;; (elpaca memoize)
;; (elpaca org)
;; (elpaca org-anki)
;; (elpaca org-contrib)
;; (elpaca org-ql)
;; (elpaca org-recent-headings)
;; (elpaca org-roam-bibtex) ;; yes still relevant for org 9.5
;; (elpaca org-roam-ui)
;; (elpaca org-tanglesync)
;; (elpaca ox-rss)
;; (elpaca ox-rss)
;; (elpaca pfuture)
;; (elpaca pinboard)
;; (elpaca pocket-reader)
;; (elpaca screencast)
;; (elpaca snitch)
;; (elpaca snow)
;; (elpaca tempel)
;; (elpaca tempel-collection)
;; (elpaca transducers)
;; (elpaca vimgolf)
;; (elpaca visual-regexp)

;; TODO: Crib doom/escape to avoid "Not in most nested command loop"


;;;; Use-package forms

(use-package apheleia
  :config
  ;; (setopt apheleia-log-debug-info t)
  (apheleia-global-mode))

(use-package async
  :after dired
  :config
  (dired-async-mode))

(use-package asyncloop
  :ensure (:fetcher github :repo "meedstrom/asyncloop"))

;; TODO Get Doom's autorevert behavior for dired too
(use-package autorevert
  :ensure nil
  :init
  (add-hook 'focus-in-hook #'me/maybe-revert-visible-buffers)
  (add-hook 'after-save-hook #'me/maybe-revert-visible-buffers)
  (add-hook 'doom-switch-buffer-hook #'me/maybe-revert-buffer)
  (add-hook 'doom-switch-window-hook #'me/maybe-revert-buffer)
  (setq auto-revert-verbose t)
  (setq auto-revert-use-notify nil)
  (setq auto-revert-stop-on-user-input nil)
  ;; Confirm before reverting buffer iff it's unsaved
  (setq revert-without-query (list ".")))

(use-package beginend
  :config
  (beginend-global-mode))

(use-package calibredb
  :defer
  :config
  (setopt calibredb-root-dir "~/Calibre Library/")
  (setopt calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setopt calibredb-format-width 8))

(use-package copilot :disabled
  :defer
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package cape :disabled
  :after corfu
  :config
  ;; https://github.com/minad/corfu/wiki#configuring-corfu-for-eglot
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-super-capf
                       #'eglot-completion-at-point
                       ;; #'tempel-expand
                       #'cape-file))))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf))

(use-package combobulate
  :ensure (:fetcher github :repo "mickeynp/combobulate")
  :defer)

(use-package consult-dir
  :defer
  :config
  (keymap-set global-map "C-x d" #'consult-dir))

(use-package consult
  :config
  ;; (setq consult-org-roam-buffer-after-buffers t)
  ;; hella weak computer
  (setq consult-fontify-max-size 100000)
  (setq consult-preview-partial-chunk 10000)
  (setq consult-preview-partial-size 10000)
  ;; Make narrowing help available in the minibuffer.  Try it one day.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  (setopt consult-narrow-key "<")

  (after! vertico
    (define-key vertico-map [S-up] #'vertico-previous)
    (define-key vertico-map [S-down] #'vertico-next)
    (consult-customize consult-recent-file :preview-key '("S-<up>" "S-<down>")))
  (add-to-list 'consult-preview-variables '(org-inhibit-startup . t))
  (define-key global-map [remap switch-to-buffer] #'consult-buffer)
  ;; Don't ignore according to .gitignore, only according to .ignore
  (setopt consult-ripgrep-args
          (concat (custom--standard-value 'consult-ripgrep-args)
                  " --no-ignore-vcs")))

;; completion-at-point, TAB -> corfu-complete
;; RET -> corfu-insert
;; M-g -> corfu-show-location
;; M-h -> corfu-show-documentation
(use-package corfu
  :config
  (setopt corfu-quit-at-boundary nil)
  (setopt tab-always-indent 'complete)
  ;; (setopt corfu-auto t)
  ;; (setopt corfu-auto-delay 0.35)
  ;; (setopt tab-always-indent t) ;; Just bind `completion-at-point' to M-q
  ;; (setopt completion-cycle-threshold 3)
  ;; invoke corfu for dabbrev instead of its own dabbrev-expand
  ;; (keymap-set [remap hippie-expand])
  ;; (global-corfu-mode)

  (advice-add #'corfu-insert :after #'me/corfu-send-shell)
  (add-hook 'minibuffer-setup-hook #'me/corfu-enable-always-in-minibuffer 1)
  (keymap-set corfu-map "SPC" #'me/complete-on-double-space)

  ;; Complete on punctuation
  ;; https://github.com/minad/corfu/wiki#tab-and-go-completion
  (dolist (c (list (cons "." ".")
                   (cons "," ",")
                   (cons ":" ":")
                   (cons ")" ")")
                   (cons "}" "}")
                   (cons "]" "]")))
    (define-key corfu-map (kbd (car c)) `(lambda ()
                                           (interactive)
                                           (corfu-insert)
                                           (insert ,(cdr c)))))
  (global-corfu-mode))

(use-package delve :disabled
  :after org-roam
  :config
  (add-hook 'delve-mode-hook #'delve-compact-view-mode)
  ;; It normally inherits from org-roam-title, which I find too big
  (set-face-attribute 'delve-title-face () :inherit 'org-document-title))

(use-package deianira
  :ensure (:repo "https://github.com/meedstrom/deianira")
  :defer
  :config
  (when (featurep 'which-key)
    (which-key-mode 0)
    (fset 'which-key-mode #'ignore))
  (after! hydra
    (define-key hydra-base-map (kbd "<f5>") #'hydra-repeat))
  (setq dei-ignore "C-")

  (setq dei-invisible-leafs
        (seq-difference dei-invisible-leafs '("<menu>" "SPC"))))

(use-package dired
  :ensure nil
  :defer
  :init
  (add-hook 'dired-mode-hook #'dired-hide-details-mode) ;; press ( to toggle
  (setq dired-auto-revert-buffer #'dired-buffer-stale-p)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq wdired-allow-to-change-permissions 'advanced)
  (setq dired-listing-switches "-ahl -v --group-directories-first")
  (setq global-auto-revert-non-file-buffers t)
  :config
  (require 'dired-x)
  (setq dired-omit-verbose nil)
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (add-to-list 'dired-omit-extensions ".eshell-command-history")
  (add-to-list 'dired-omit-extensions ".eshell-scrollback")
  (require 'dired-aux)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; show folder sizes (requires fs indexing with duc to be fast)
(use-package dired-du :disabled
  :when (and (executable-find "duc")
             (not (string-match-p "Error" (my-process-output-to-string "duc" "info"))))
  :config
  (run-with-timer 60 3600
                  (defun me/index-duc ()
                    (start-process "duc" nil "duc" "index" "/home")))
  (setopt dired-du-size-format t)  ;; human-readable
  (setopt dired-du-used-space-program '("duc" "ls -bD")))

(use-package dired-hacks :disabled
  :init (add-hook 'dired-mode-hook #'dired-collapse-mode))

(use-package eglot
  :ensure nil
  :defer
  :config
  ;; Use locally-installed NPM packages, not global
  (cl-loop for item in-ref eglot-server-programs
           when (equal (car-safe (cdr-safe item)) "typescript-language-server")
           return (push "npx" (cdr item)))
  (setf (alist-get 'svelte-mode eglot-server-programs)
        (list "npx" "svelte-language-server" "--stdio")))

(use-package flycheck
  :defer
  :init
  (setq flycheck-idle-change-delay 50)
  (setq flycheck-check-syntax-automatically
        '(save idle-change mode-enabled))
  (add-hook 'emacs-lisp-mode-hook #'flycheck-mode))

(use-package flycheck-package
  :after flycheck
  :config (flycheck-package-setup))

(use-package persist-state
  :requires asyncloop
  :config
  (defvar persist-state--remainder nil)
  (defun persist-state--consume (loop)
    (require 'asyncloop)
    (if (null persist-state--remainder)
        "Done"
      (push t (asyncloop-remainder loop))
      (let ((inhibit-message t)
            (save-silently t)
            (standard-output (lambda (&rest _)))
            (real-write-region (symbol-function #'write-region))
            (fn (pop persist-state--remainder)))
        (cl-letf (((symbol-function #'write-region)
                   (lambda (start end filename &optional append visit lockname mustbenew)
                     (unless visit (setq visit 'no-message))
                     (funcall real-write-region start end filename append visit lockname mustbenew))))
          (funcall fn)
          fn))))

  (defun persist-state--regularly-run-on-idle (f &rest _)
    (run-with-timer persist-state-save-interval
                    persist-state-save-interval
                    f))
  (defun persist-state--save-state ()
    (require 'asyncloop)
    (asyncloop-run
      '((lambda (_)
          (setq persist-state--remainder persist-state-saving-functions))
        persist-state--consume)
      :log-buffer-name " *persist-state*"))
  ;; Always act like we crash (good battle test, and fast restarts!)
  (advice-add #'kill-emacs :before
              (lambda (&rest _) (setq kill-emacs-hook nil)))
  ;; Save more often due to the nulled kill-emacs-hook
  (setq persist-state-save-interval 100)
  (after! transient
    (add-hook 'persist-state-saving-functions #'transient-maybe-save-history))
  (after! persist
    (add-hook 'persist-state-saving-functions #'persist--save-all))
  ;; Remove a function that sometimes prompts for input, especially with
  ;; several emacs instances active
  (remove-hook 'persist-state-saving-functions #'persist-state--maybe-save-bookmarks)
  (persist-state-mode))

(use-package editorconfig
  :config
  (editorconfig-mode))

;; (use-package elfeed
;;   :defer
;;   :config
;;   (setq elfeed-db-directory (concat user-emacs-directory "elfeed/db/")
;;         elfeed-enclosure-default-dir (concat user-emacs-directory "elfeed/enclosures/"))
;;   (make-directory elfeed-db-directory t)
;;   (add-hook 'elfeed-new-entry-hook
;;             (elfeed-make-tagger :entry-title (rx (or "MCMXXX"
;;                                                      "A&R"))
;;                                 :add 'junk))
;;   (setopt elfeed-curl-max-connections 1)
;;   (setopt elfeed-search-filter "@2-months-ago -junk +unread"))

;; (use-package elfeed-org
;;   :after elfeed
;;   :config
;;   (setopt rmh-elfeed-org-files
;;           '("/home/kept/roam/contemporaries.org"))
;;   (elfeed-org))

(use-package embark
  :defer
  :config
  (setq embark-quit-after-action
        '((me/load-theme . nil)
          (t . t)))
  (keymap-set embark-general-map "M-r" #'embark-isearch-backward)
  (keymap-set embark-general-map "M-s" #'embark-isearch-forward))

(use-package embark-consult
  :defer)

(use-package eshell
  :ensure nil
  :defer
  :config
  (require 'esh-module)
  ;; Try some extra modules, see C-h P esh-groups
  ;; (add-to-list 'eshell-modules-list 'eshell-smart)
  (add-to-list 'eshell-modules-list 'eshell-xtra)

  ;;   (require 'esh-mode)
  ;;   (require 'em-hist))

  (setopt eshell-show-lisp-completions t)
  (setopt eshell-scroll-show-maximum-output nil) ;; ??
  (setopt eshell-scroll-to-bottom-on-output 'this)

  ;; TODO I prefer it pick a recent buffer
  ;; (setopt +eshell-enable-new-shell-on-split nil)

  ;; TODO: give the different parts of the string different text properties
  (setopt eshell-banner-message '(funcall #'me/esh-banner))

  ;; (add-hook 'eshell-post-command-hook #'end-of-buffer)

  (add-hook 'eshell-mode-hook
            (defun my-esh-add-local-post-command-hook ()
              (add-hook 'post-command-hook #'scroll-right nil t)))

  ;; Sync history on every command, in case I powercycle the computer

  (add-hook 'my-real-eshell-post-command-hook #'eshell-write-history)
  (add-hook 'eshell-before-prompt-hook #'my-esh-save-scrollback)

  ;; Name the buffer so I can see the directory on the modeline.
  (add-hook 'eshell-directory-change-hook #'my-esh-rename)
  (add-hook 'eshell-mode-hook #'my-esh-rename)

  ;; Misc
  ;; (add-hook 'my-real-eshell-post-command-hook #'my-esh-narrow-to-output 95)

  ;; TODO: try the "smart" thing for a while
  ;; (use-package em-smart
  ;;   :custom ((eshell-review-quick-commands nil)
  ;;            (eshell-smart-space-goes-to-end t)
  ;;            (eshell-where-to-jump 'begin)))

  (after! em-hist
    (setopt eshell-hist-ignoredups t)
    (define-key eshell-hist-mode-map [remap consult-history] #'my-esh-consult-history))

  ;; (after! esh-mode
  ;; Automatically narrow/widen to output on point motion.  Damn, it's weird
  ;; and often not what I want, but that's me abusing point motion.
  ;; (define-key eshell-mode-map [remap next-line] #'my-esh-next-line)
  ;; (define-key eshell-mode-map [remap previous-line] #'my-esh-prev-line)
  ;; (define-key eshell-mode-map [remap eshell-next-prompt] #'my-esh-next-prompt)
  ;; (define-key eshell-mode-map [remap eshell-previous-prompt] #'my-esh-previous-prompt)
  ;; )
  )

(use-package ess
  :defer
  :init (add-hook 'inferior-ess-mode-hook
                  (lambda () (setq-local comint-scroll-to-bottom-on-input t)))
  :config
  (defun my-append-to-rhistory (input)
    (with-temp-buffer
      (insert (concat (format-time-string "《%FT%T%z》") input))
      (kill-matching-buffers "^.Rhistory" nil t)
      (quiet! (append-to-file (point-min) (point-max)
                              (expand-file-name ".Rhistory" default-directory))))
    input)

  ;; ;; Make command `previous-buffer' not skip the R console
  ;; (el-patch-defun doom-buffer-frame-predicate (buf)
  ;;   "To be used as the default frame buffer-predicate parameter. Returns nil if
  ;; BUF should be skipped over by functions like `next-buffer' and `other-buffer'."
  ;;   (or (string-prefix-p "*R:" (buffer-name buf))
  ;;       (doom-real-buffer-p buf)
  ;;       (eq buf (doom-fallback-buffer))))

  ;; http://chainsawriot.com/mannheim/2020/07/19/elisp.html

  ;; (setopt ess-directory-function
  ;;       (lambda ()
  ;;         (or (ignore-errors (car (project-roots (project-current))))
  ;;             nil)))

  ;; (add-hook 'ess-presend-filter-functions #'my-append-to-rhistory)

  ;; (defun my-set-rhistory (&rest r)
  ;; (setq! ess-history-directory default-directory)
  ;; (setq! ess-history-file ".Rhistory"))

  ;; (advice-add #'ess-set-working-directory :after #'my-set-rhistory)

  (setopt inferior-R-args "--no-save --no-restore")
  (setopt ess-ask-for-ess-directory nil) ;; Muffle annoying ESS startup prompt
  (setopt ess-use-ido nil)
  (setopt ess-use-flymake nil)
  (setopt ess-use-tracebug nil) ;; Sidestep a bug that destroys performance
  (setopt ess-use-auto-complete nil)
  (setopt ess-indent-with-fancy-comments nil)
  (setopt ess-history-file nil)
  (setopt ess-ask-for-ess-directory nil)
  (setopt ess-eval-visibly 'nowait)
  (me/unihook ess-r-mode-hook (ess-set-style 'RStudio)))

(use-package eva :disabled
  :ensure (:repo "/home/kept/emacs/eva" :branch "dev"
                 :files (:defaults "assets" "renv" "*.R" "*.gnuplot"))
  :init
  :config
  ;; (setopt eva-debug t)
  (setopt eva-fallback-to-emacs-idle t)
  (setopt eva-init-r nil)
  (setopt eva-user-birthday "1991-12-07")
  (setopt eva-idle-log-path         "/home/kept/self-data/idle.tsv")
  (setopt eva-buffer-focus-log-path "/home/kept/self-data/buffer-focus.tsv")
  (setopt eva-buffer-info-path      "/home/kept/self-data/buffer-info.tsv")
  (setopt eva-main-datetree-path nil)
  (setopt eva-past-sample-function #'eva-past-sample-casual)
  (setopt ess-ask-for-ess-directory nil) ;; Prevent annoying ESS startup prompt
  (require 'eva-builtin)
  ;; Looked up by `eva-present-diary', but org-journal not needed
  (setq org-journal-dir "/home/kept/roam/daily/")
  (setq org-journal-file-format "%F")
  ;; (setq org-journal-file-format "%F.org")
  (add-hook 'eva-after-load-vars-hook #'eva-check-dangling-clock)
  (add-hook 'eva-after-load-vars-hook #'eva-check-org-vars)
  (eva-defun me/eva-present-outcomes-or-agenda ()
             (require 'org-agenda)
             (require 'org-id)
             (message (eva-emit "Here are your Org'd thoughts."))
             (sit-for eva-sit-short)
             (if (> (ts-hour (ts-now)) 18)
                 ;; Late in the day is more of a review-time, so show agenda
                 (progn
                   (org-agenda-list)
                   (push (current-buffer) eva-excursion-buffers))
               ;; Still early, so show desired outcomes and task ideas
               (org-id-goto "3ec7f712-2437-4222-8905-72d39ba6188a")
               (push (current-buffer) eva-excursion-buffers)
               (if (one-window-p) (split-window))
               (other-window 1)
               (org-id-goto "c55ab064-0db2-4556-aa24-0c3c8dce9e76")
               (push (current-buffer) eva-excursion-buffers))
             (eva-stop-queue))
  (setq eva-items
        (list
         (eva-item-create :fn #'eva-greet
                          :min-hours-wait 1)

         (eva-item-create :fn #'eva-present-diary
                          :max-successes-per-day 1)

         (eva-item-create :fn #'me/eva-present-outcomes-or-agenda)

         ;; you can inline define the functions too
         (eva-item-create
          :fn (eva-defun my-bye ()
                         (message (eva-emit "All done for now."))
                         (bury-buffer (eva-buffer-chat)))
          :min-hours-wait 0)))
  (transient-replace-suffix 'eva-dispatch '(0)
    '["General actions"
      ("q" "Quit the chat" bury-buffer)
      ("l" "View Ledger report" eva-present-ledger-report)
      ("f" "View Ledger file" eva-present-ledger-file)
      ("a" "View Org agenda" org-agenda-list)])
  (define-key eva-chat-mode-map (kbd "l") #'eva-present-ledger-report)
  (define-key eva-chat-mode-map (kbd "f") #'eva-present-ledger-file)
  (define-key eva-chat-mode-map (kbd "a") #'org-agenda-list)
  (eva-mode))

(use-package exwm :disabled
  :init
  (setq exwm-input-simulation-keys
        '(([?\s-a] . [home])
          ([?\s-b] . [left])
          ([?\s-d] . [delete])
          ([?\s-e] . [end])
          ([?\s-f] . [right])
          ([?\s-g] . [escape])
          ([?\s-k] . [S-end delete])
          ([?\s-m] . [return])
          ([?\s-n] . [down])
          ([?\s-p] . [up])
          ([?\s-s] . [C-f])
          ([?\s-t] . [S-right C-x left C-v])
          ([?\s-v] . [next])
          ([?\s-w] . [C-x])
          ([?\s-y] . [C-v])
          ([?\s-/] . [C-z])
          ([?\M-w] . [C-c])
          ([?\M-d] . [C-S-right delete])
          ([?\M-t] . [C-S-right C-x C-left C-v])
          ([f8] . [menu])
          ([XF86Back] . [prior])
          ([XF86Forward] . [next])))
  (setq exwm-input-prefix-keys
        '(?\s-1 ?\s-2 ?\s-x ?\s-c menu f1 f2 f3 f5 f7 f10 f11 f12 katakana henkan))
  (setq exwm-input-global-keys
        `((,(kbd "C-M-<delete>") . exwm-reset)
          (,(kbd "M-<f4>") . kill-current-buffer) ;; y u no work?
          ;; (,(kbd "A-<f4>") . kill-current-buffer)
          (,(kbd "<XF86MonBrightnessDown>") . my-backlight-dec)
          (,(kbd "<XF86MonBrightnessUp>") . my-backlight-inc)))
  (after! exwm-core
    ;; Move C-c prefix to M-q so to clear C-c for copy
    (keymap-set exwm-mode-map "M-q" (keymap-lookup exwm-mode-map "C-c"))
    (keymap-unset exwm-mode-map "C-c"))
  (add-hook 'exwm-update-class-hook #'my-exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook #'my-exwm-rename-buffer))

(use-package fd-dired
  :when (or (executable-find "fd") (executable-find "fdfind"))
  :defer
  :init
  (global-set-key [remap find-dired] #'fd-dired))

(use-package forge
  :defer
  :config
  ;; I really need my CAPFs to be instant
  (advice-add forge-bug-reference-setup :after
              (lambda ()
                (remove-hook 'completion-at-point-functions
                             #'forge-topic-completion-at-point t))))

(use-package form-feed
  :config
  (global-form-feed-mode)
  (add-hook 'emacs-lisp-compilation-mode-hook #'form-feed-mode))

(use-package gcmh
  :config
  (setq gcmh-high-cons-threshold (* 15 1000 1000))
  (gcmh-mode))

;; (use-package goggles
;;   :hook ((prog-mode text-mode) . goggles-mode))

(use-package gif-screencast
  :defer
  :init
  ;; Support KDE on Wayland
  ;; (setq gif-screencast-program "spectacle")
  ;; (setq gif-screencast-args (list "-anbo"))
  )

(use-package go-mode
  :defer)

(use-package helm :disabled
  :defer
  :config
  ;; wishlist: buffer preview
  (setopt helm-ff-DEL-up-one-level-maybe t)
  (when (modulep! :completion helm)
    (define-key global-map [remap switch-to-buffer] #'helm-mini)))

(use-package highlight-quoted
  :defer
  :init
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))

(use-package hl-todo
  :defer
  :init
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'yaml-mode-hook #'hl-todo-mode)
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO" warning bold)
          ("FIXME" error bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("HACK" font-lock-constant-face bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold))))

(use-package hyperbole :disabled
  :commands hkey-either
  :init
  (setq hsys-org-enable-smart-keys t)
  ;; (run-with-idle-timer 15 nil #'hyperbole-mode)
  (setq hkey-init nil))

(use-package iedit
  :defer
  :init
  ;; pre-bind the default key to silence init message
  (keymap-set global-map "C-\;" #'iedit-mode))

(use-package iflipb
  :defer
  :init
  (setq iflipb-wrap-around t))

;; maybe this is the one that subtly messes with undo?
;; it may be messing with set-goal-column
(use-package iscroll :disabled
  :hook ((text-mode elfeed-show-mode eww-mode shr-mode) . iscroll-mode))

(use-package inline-anki
  :ensure (:repo "https://github.com/meedstrom/inline-anki")
  :config
  (add-to-list 'inline-anki-ignore-file-regexps "daily/")
  (setopt inline-anki-send-tags '(not "noexport"
                                      "ARCHIVE"
                                      "stub"
                                      "fren"
                                      "privy"
                                      "pub"))
  (after! org
    (add-to-list 'org-structure-template-alist '("f" . "flashcard")))
  (add-to-list 'inline-anki-fields '("Online mirror" . my-anki-field-for-webpage))
  ;; (add-to-list 'inline-anki-ignore-file-regexps "/daily/")
  )

(use-package llama
  :defer
  :config (global-llama-fontify-mode))

(elpaca transient)

(use-package magit
  ;; ;; My daily driver is 30, and 29 is just for testing packages in which case I
  ;; ;; also want to revert to builtin transient 0.4.3
  ;; :if (> emacs-major-version 29)
  ;; :requires git-commit
  :defer
  :init
  ;; https://magit.vc/manual/magit/Performance.html
  (setq magit-refresh-status-buffer nil))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package massmapper
  :ensure (:repo "https://github.com/meedstrom/massmapper")
  :demand
  :init
  (add-hook 'elpaca-after-init-hook #'massmapper-mode)
  (add-hook 'massmapper-keymap-found-hook #'massmapper-define-super-like-ctl)
  (add-hook 'massmapper-keymap-found-hook #'massmapper-homogenize -50)
  ;; (add-hook 'massmapper-keymap-found-hook #'massmapper-protect-ret-and-tab -75)
  (setopt massmapper-debug-level 0)
  (setopt massmapper-homogenizing-winners
          '(("C-x C-s" . global-map)
            ("C-x C-f" . global-map)
            ("C-x C-q" . global-map)
            ("C-x C-;" . global-map)
            ("C-x C-l" . global-map)
            ("C-c l" . org-journal-mode-map)
            ("C-c C-c")
            ("C-c C-," . org-mode-map))))

(use-package nameless
  :defer
  :init
  (setopt nameless-prefix "⁓")
  (setopt nameless-private-prefix t)
  (setopt nameless-affect-indentation-and-filling nil)
  :config
  (set-face-attribute 'nameless-face nil :inherit 'unspecified))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package objed
  :commands objed-ipipe)

(use-package org
  :ensure nil
  :defer
  :init
  (setq org-time-stamp-custom-formats (cons "%Y-%b-%d" "%Y-%m-%d %a %H:%M"))
  ;; (after! org
  ;; (setq org-time-stamp-formats (cons "%Y-%m-%d" "%Y-%m-%d %a %H:%M")))
  (setq org-pretty-entities t)
  (setq org-archive-location "~/org/noagenda/archive.org::datetree/")
  (setq org-clock-persist t)
  (setq org-clock-auto-clock-resolution t)
  ;; I'd prefer `nofold', but the prop drawer shows org-node refs and backlinks
  ;; (setq org-startup-folded 'nofold)
  (setq citar-bibliography '("~/org/refs/library_biblatex.bib"))
  (setq org-archive-save-context-info '(time file itags olpath))
  (setq org-export-backends '(html latex odt texinfo))
  (setq org-export-with-toc nil)
  (setq org-clock-out-remove-zero-time-clocks t)
  (setq org-clock-idle-time 5)
  (setq org-hide-leading-stars nil)
  (setq org-clock-mode-line-total 'today)
  (setq org-clock-in-resume t)
  (setq org-catch-invisible-edits 'smart)
  (setq org-ctrl-k-protect-subtree t)
  (setq org-M-RET-may-split-line '((headline . nil) (default . t)))
  (setq org-cycle-separator-lines 3)
  (setq org-datetree-add-timestamp nil)
  (setq org-edit-src-content-indentation 0)
  ;; (setq org-ellipsis "⤵")
  (setq org-ellipsis "…")
  (setq org-hide-emphasis-markers t) ; hide the *, =, and / markers
  (setq org-image-max-width 300)
  ;; (setq org-image-actual-width '(200)) ; use #ATTR if available, else 200 px
  ;; (setq org-latex-compiler "xelatex") ; allow unicode (åäö) in VERBATIM blocks
  (setq org-log-done 'time)
  (setq org-log-into-drawer t) ; hide spam
  (setq org-modules '(org-id org-habit org-gamify ol-info ol-eww)) ;; `org-eww-copy-for-org-mode'
  (setq org-use-speed-commands t)
  (setq org-clock-x11idle-program-name (or (executable-find "xprintidle") "x11idle"))
  (setq org-replace-disputed-keys t)
  (setq org-tags-column 0)
  (setq org-startup-indented t)
  (setq org-clock-kill-emacs-query nil) ;; fix bug


  ;; Workaround the tide of org-element parser bugs since 9.5 rewrite
  ;; (setq org-element-use-cache nil)
  ;; (setq org-element-cache-persistent nil)

  ;; Could it cause org-element bugs due to daily page titles?
  ;; (setq-default org-display-custom-times t)

  ;; For inline-anki: override Org's underlines to represent cloze deletions and
  ;; make them look appropriate
  (defface my-cloze '((t . (:box t))) "Cloze face")
  (setq org-emphasis-alist '(("*" bold)
                             ("/" italic)
                             ("_" my-cloze)
                             ("=" org-verbatim verbatim)
                             ("~" org-code verbatim)
                             ("+" (:strike-through t))))

  (add-hook 'org-mode-hook #'me/org-setup-prettify)
  ;; (add-hook 'org-mode-hook #'org-resolve-clocks 95)
  ;; (add-hook 'org-mode-hook #'org-clock-persistence-insinuate)
  ;; (add-hook 'org-clock-in-hook #'org-clock-save)
  ;; (add-hook 'org-clock-out-hook #'bh/clock-out-maybe 90)
  ;; (add-hook 'text-mode-hook #'turn-off-smartparens-mode)

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

  ;;   ;; Allow tab to jump to links
  ;;   (add-hook 'org-tab-after-check-for-table-hook #'me/org-next-link)
  ;;   (setq org-cycle-include-plain-lists t)
  ;;   (defun me/org-next-link (&optional arg)
  ;;     "Right after jumping to a next link, make S-TAB available as a way to
  ;; jump backwards, repeatable."
  ;;     (interactive)
  ;;     (unless (and org-cycle-include-plain-lists
  ;;                  (not (thing-at-point 'url))
  ;;                  (let ((item (org-element-lineage
  ;;                               (org-element-at-point) '(item plain-list) t)))
  ;;                    (and item (= (pos-bol) (org-element-post-affiliated item)))))
  ;;       (let ((map (make-sparse-keymap))
  ;;             (cmd (lambda () (interactive) (me/org-next-link t))))
  ;;         (define-key map (kbd "<backtab>") cmd)
  ;;         (define-key map (kbd "S-<iso-lefttab>") cmd)
  ;;         (define-key map (kbd "C-S-i") cmd)
  ;;         (org-next-link arg)
  ;;         (set-transient-map map))))

  :config
  (unless after-init-time
    (setq debug-on-error t)
    (message "Org loaded during init, I don't want this"))

  ;; If using Doom's Org
  (if (and (boundp 'doom-version)
           (modulep! :lang org))
      ;; fix interference with org-transclusion
      (advice-remove 'org-link-search '+org--recenter-after-follow-link-a)
    ;; if not using Doom's org
    ;; (require 'org-indent)
    ;; (add-hook 'org-mode-hook #'org-indent-mode)
    (my-change-latex-scale) ;; Bigger LaTeX preview
    ;; Adapt LaTeX preview scale to the font zoom
    (add-hook 'text-scale-mode-hook #'my-change-latex-scale))
  )

(use-package org-agenda
  :ensure nil
  :defer
  :init
  ;; (setq org-agenda-todo-list-sublevels nil)
  (setq org-agenda-include-diary nil) ;; perf... :(
  (setq org-agenda-dim-blocked-tasks nil) ;; perf
  (setq org-agenda-use-tag-inheritance '(todo search)) ;; perf
  (setq org-agenda-ignore-properties '(stats)) ;; perf
  (setq org-agenda-inhibit-startup t) ;; perf
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
  )

(use-package org-daily-reflection
  :ensure (:fetcher github :repo "emacsomancer/org-daily-reflection")
  :after org-node
  :config (setq org-daily-reflection-dailies-directory
                (org-node--guess-daily-dir))
  (defun my-reflect ()
    (interactive)
    (delete-other-windows)
    (my-last-daily-file)
    (org-daily-reflection 'week 3)))

(use-package org-download
  :commands org-download-yank
  :init
  (setq org-download-heading-lvl nil)
  (setq org-download-image-dir "img/"))

;; https://github.com/progfolio/elpaca/issues/368
(defun +elpaca/build-if-new (e)
  (setf (elpaca<-build-steps e)
        (if-let ((default-directory (elpaca<-build-dir e))
                 (main (ignore-errors (elpaca--main-file e)))
                 (compiled (expand-file-name (concat (file-name-base main) ".elc")))
                 ((file-newer-than-file-p main compiled)))
            (progn (elpaca--signal e "Rebuilding due to source changes")
                   (cl-set-difference elpaca-build-steps
                                      '(elpaca--clone elpaca--configure-remotes elpaca--checkout-ref)))
          (elpaca--build-steps nil (file-exists-p (elpaca<-build-dir e))
                               (file-exists-p (elpaca<-repo-dir e)))))
  (elpaca--continue-build e))

(setq debug-on-message "^File error:.*")

(use-package el-job
  :ensure ( :build (+elpaca/build-if-new)
            :repo "https://github.com/meedstrom/el-job"
            :inherit nil)
  :init (setq el-job--debug-level 2))

(use-package org-node
  :ensure ( :build (+elpaca/build-if-new))
  :after org
  :config
  (setq org-node-extra-id-dirs '("~/org/" "~/emacs"))
  (add-to-list 'org-node-extra-id-dirs-exclude "/elpaca/")

  (org-node-cache-mode)
  (org-node-backlink-mode)
  (org-node-complete-at-point-mode)
  (add-hook 'after-save-hook 'org-node-rename-file-by-title)

  (setq org-node-backlink-aggressive t)
  (setq org-node-renames-allowed-dirs '("~/org/"))
  (setq org-read-date-prefer-future nil)
  ;; (setq org-node--debug nil)
  ;; (setopt org-node-perf-eagerly-update-link-tables t)
  (setopt org-node-perf-assume-coding-system 'utf-8-unix)
  ;; (setopt org-node-ask-directory "/home/kept/roam")
  ;; (setopt org-node-prefer-with-heading t)
  ;; (setopt org-node-slug-fn #'org-node-slugify-like-roam-default)
  ;; (setopt org-node-datestamp-format "%Y%m%dT%H%M%S--")
  ;; (setopt org-node-datestamp-format "%Y%m%d%H%M%S-")
  ;; (setopt org-node-datestamp-format "")
  ;; (setopt org-node-slug-fn #'org-node-slugify-for-web)
  ;; (setq org-node-creation-fn #'org-capture)
  ;; (setopt org-node-creation-fn #'org-node-new-file)
  ;; (setopt org-node-alter-candidates t)

  (after! org-roam-id
    ;; Restore default
    (org-link-set-parameters
     "id" :follow #'org-id-open :store #'org-id-store-link-maybe))
  (setq org-node-filter-fn
        (lambda (node)
          (not
           (or (string-search "archive/" (org-node-get-file-path node))
               (string-search "noagenda/" (org-node-get-file-path node))))))

  (setq org-node-seq-defs
        (list
         ;; The series of all nodes that have tag :pub:, sorted by the
         ;; datestamp that I've put in their :CREATED: property.
         (org-node-seq-def-on-tags-sort-by-property
          "w" "My public notes (visible on the web)" "pub" "CREATED")
         ;; All notes in creation order!  Once again via :CREATED: property.
         (org-node-seq-def-on-any-sort-by-property
          "a" "All notes" "CREATED")
         ;; My dailies/journal/diary, what you want to call it.  Currently I
         ;; still have them like org-roam-dailies expects: confined to a
         ;; subdirectory, with filenames such as "2024-11-18.org".
         (org-node-seq-def-on-filepath-sort-by-basename
          "d" "Dailies" "~/org/daily/" nil t)))



  ;; (setq org-node-series-defs
  ;;       (list
  ;;        ;;(org-node-mk-series-on-tags-sorted-by-property
  ;;        ;; "w" "My public notes (visible on the web)" "pub" "CREATED")

  ;;        '("d" :name "Daily-files"
  ;;          :version 2
  ;;          :classifier (lambda (node)
  ;;                        (let ((path (org-node-get-file-path node)))
  ;;                          (when (string-prefix-p "/home/kept/roam/daily/" path)
  ;;                            (let ((ymd (org-node-helper-filename->ymd path)))
  ;;                              (when ymd
  ;;                                (cons ymd path))))))
  ;;          :whereami (lambda ()
  ;;                      (org-node-helper-filename->ymd buffer-file-name))
  ;;          :prompter (lambda (key)
  ;;                      ;; Tip: Consider `org-read-date-prefer-future' nil
  ;;                      (let ((org-node-series-that-marks-calendar key))
  ;;                        (org-read-date)))
  ;;          :try-goto (lambda (item)
  ;;                      (org-node-helper-try-visit-file (cdr item)))
  ;;          :creator (lambda (sortstr key)
  ;;                     (let ((org-node-datestamp-format "")
  ;;                           (org-node-slug-fn (lambda (&rest _) sortstr))
  ;;                           (org-node-ask-directory "/home/kept/roam/daily/"))
  ;;                       (org-node-create (format-time-string
  ;;                                         "%Y-%b-%d"
  ;;                                         (org-time-string-to-time sortstr))
  ;;                                        (org-id-new)
  ;;                                        key))))))

  ;; Make sure the extracted subtree inherits any CREATED property,
  ;; else creates one for today
  (advice-add 'org-node-extract-subtree :around
              (defun my-inherit-creation-date (orig-fn &rest args)
                (let ((parent-creation (org-entry-get nil "CREATED" t)))
                  (apply orig-fn args)
                  ;; Now in the new buffer
                  (org-entry-put nil "CREATED"
                                 (or parent-creation
                                     (format-time-string
                                      (org-time-stamp-format t t))))))))

(use-package org-node-fakeroam
  :ensure (:build (+elpaca/build-if-new))
  :after org-node
  :config
  (setq org-node-fakeroam-fast-render-persist t)
  (add-hook 'org-roam-mode-hook #'visual-line-mode)
  (org-node-fakeroam-jit-backlinks-mode)
  ;; (org-node-fakeroam-db-feed-mode)
  (org-node-fakeroam-redisplay-mode)
  (org-node-fakeroam-fast-render-mode)
  (add-hook 'org-open-at-point-functions
            #'org-node-try-visit-ref-node)
  ;; Good in eliezers-sequences.org
  (add-to-list 'org-roam-buffer-postrender-functions
               #'magit-section-show-level-2)

  ;; TODO Do a hard-rewrap and cache that output? This is hella slow.
  ;; (add-hook 'org-roam-mode-hook #'org-indent-mode)

  (add-hook 'me/load-theme-hook
            (defun me/theme-mod-roam ()
              (set-face-attribute 'org-roam-title nil :height 1.5)))
  (when after-init-time
    (me/theme-mod-roam)))

(use-package org-noter :disabled
  :init
  (add-hook 'org-noter-notes-mode-hook #'abbrev-mode)
  (add-hook 'org-noter-notes-mode-hook (lambda () rainbow-delimiters-mode 0)))

(use-package org-roam
  :defer
  :init
  (add-hook 'org-roam-capture-new-node-hook #'org-node-put-created)
  (setq org-roam-file-exclude-regexp '("logseq/bak/" "logseq/version-files/"))
  (setq org-roam-link-auto-replace nil)
  (setq org-roam-db-update-on-save nil)
  (setq org-roam-directory "~/org/")
  (setq org-roam-ui-browser-function #'my-browse-url-chromium-kiosk)
  (setq org-roam-dailies-capture-templates
        `(("d" "default" entry "* %<%H:%M>\n%?" :if-new
           (file+head "%<%Y-%m-%d>.org"
                      ,(string-join '("#+title: %<%Y-%b-%d>"
                                      "#+filetags: :daily:")
                                    "\n"))
           :immediate-finish t
           :jump-to-captured t)
          ("t" "test alternative" entry "* %<%H:%M> Alt\n%?" :if-new
           (file+head "%<%Y-%m-%d>.org"
                      ,(string-join '("#+title: %<%Y-%b-%d>"
                                      "#+filetags: :daily:")
                                    "\n")))))
  :config
  )

(use-package org-transclusion
  :ensure (:repo "https://github.com/meedstrom/org-transclusion")
  :defer
  :config
  (set-face-background 'org-transclusion "#222")
  (setopt org-transclusion-exclude-elements '(property-drawer comment keyword)))

(use-package quickroam
  :ensure (:repo "https://github.com/meedstrom/quickroam")
  :defer)

(use-package prism
  :defer
  ;; Wishlist: an odd default for Lisp is that the parens enclosing a sexp
  ;; differ in color from the symbols inside -- I'd like it the same color
  :init
  (setq prism-comments nil)
  ;; The default (40 50 60) is a nice fix for fruit-salad themes but if the
  ;; theme already uses muted colors, the effect is... not good
  (setq prism-desaturations '(0 20 60))
  :config
  (add-hook 'me/load-theme-hook #'prism-set-colors)
  ;; (add-hook 'web-mode-hook #'prism-mode) ;; infinite loop in .svelte files
  (add-hook 'typescript-mode-hook #'prism-mode)
  (add-hook 'typescript-tsx-mode-hook #'prism-mode)
  (add-hook 'js-base-mode-hook #'prism-mode))

(use-package smartparens
  :config ;; y u no autoloads
  ;; Smartparens guide: https://gist.github.com/pvik/8eb5755cc34da0226e3fc23a320a3c95
  ;; Author's config: https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
  ;; Xah's simplification: https://old.reddit.com/r/emacs/comments/3sfmkz/could_this_be_a_pareditsmartparens_killer/cwxocld/
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  (add-hook #'emacs-lisp-mode-hook #'smartparens-mode)
  (keymap-set smartparens-strict-mode-map ";" #'sp-comment)
  (keymap-set global-map "C-<left>" #'sp-forward-barf-sexp)
  (keymap-set global-map "C-<right>" #'sp-forward-slurp-sexp)
  (keymap-set global-map "C-M-<left>" #'sp-backward-slurp-sexp)
  (keymap-set global-map "C-M-<right>" #'sp-backward-barf-sexp)
  (define-key global-map [remap kill-whole-line] #'sp-kill-whole-line)

  ;; In agreement with the Doom module
  (keymap-set global-map "M-<backspace>" #'sp-backward-unwrap-sexp)
  (keymap-set global-map "M-<delete>" #'sp-unwrap-sexp)
  (keymap-set global-map "C-M-b" #'sp-backward-sexp)
  (keymap-set global-map "C-M-f" #'sp-forward-sexp)
  (keymap-set global-map "C-M-d" #'sp-down-sexp)
  (keymap-set global-map "C-M-n" #'sp-next-sexp)
  (keymap-set global-map "C-M-p" #'sp-previous-sexp)
  (keymap-set global-map "C-M-t" #'sp-transpose-sexp)

  ;; Don't bind sp-kill-sexp in global-map
  ;; https://github.com/Fuco1/smartparens/issues/1186
  (keymap-set smartparens-mode-map "C-M-k" #'sp-kill-sexp)

  (add-hook 'read-only-mode-hook #'turn-off-smartparens-mode)

  ;; Still haven't really used these
  (keymap-set global-map "C-M-a" #'sp-backward-down-sexp)
  (keymap-set smartparens-mode-map "C-k" #'me/sp-kill-hybrid-sexp)
  (keymap-set global-map "C-M-e" #'sp-up-sexp)
  (keymap-set global-map "C-M-u" #'sp-backward-up-sexp)
  (keymap-set global-map "C-'" #'sp-mark-sexp)
  ;; (keymap-set global-map "C-;" #'sp-comment)
  (keymap-set global-map "M-[" #'sp-wrap-round)

  ;; Unassimilated Smartparens commands to try out.
  ;; (keymap-set global-map "C-M-<delete>" #'sp-splice-sexp-killing-forward)
  ;; (keymap-set global-map "C-M-<backspace>" #'sp-splice-sexp-killing-backward)
  ;; (keymap-set global-map "C-2 a" #'sp-join-sexp)
  ;; (keymap-set global-map "C-2 b" #'sp-select-next-thing)
  ;; (keymap-set global-map "C-2 c" #'sp-beginning-of-sexp)
  ;; (keymap-set global-map "C-2 d" #'sp-beginning-of-next-sexp)
  ;; (keymap-set global-map "C-2 e" #'sp-end-of-sexp)
  ;; (keymap-set global-map "C-2 f" #'sp-add-to-next-sexp)
  ;; (keymap-set global-map "C-2 g" #'sp-add-to-previous-sexp)
  ;; (keymap-set global-map "C-2 h" #'sp-split-sexp)
  ;; (keymap-set global-map "C-2 i" #'sp-splice-sexp)
  ;; (keymap-set global-map "C-2 j" #'sp-emit-sexp)
  ;; (keymap-set global-map "C-2 k" #'sp-absorb-sexp)
  ;; (keymap-set global-map "C-2 l" #'sp-convolute-sexp)
  ;; (keymap-set global-map "C-2 m" #'sp-forward-symbol)
  ;; (keymap-set global-map "C-2 n" #'sp-backward-symbol)
  ;; (keymap-set global-map "C-2 o" #'sp-wrap)
  ;; (keymap-set global-map "C-2 p" #'sp-backward-up-sexp)
  ;; (keymap-set global-map "C-2 q" #'sp-up-sexp)
  ;; (keymap-set global-map "C-2 r" #'sp-select-next-thing-exchange)
  ;; (keymap-set global-map "C-2 s" #'sp-select-previous-thing)
  )

(use-package svelte-mode
  :defer)

;; (use-package transient)

(use-package treesit-auto
  :init
  (setq treesit-language-source-alist
        '((svelte "https://github.com/Himujjal/tree-sitter-svelte")))
  (setq treesit-auto-install 'prompt)
  :config
  ;; (cl-pushnew 'svelte treesit-auto-langs)
  ;; (push (make-treesit-auto-recipe
  ;;        :lang 'svelte
  ;;        :ts-mode 'svelte-ts-mode
  ;;        :remap 'svelte-mode
  ;;        :url "https://github.com/Himujjal/tree-sitter-svelte"
  ;;        :ext "\\.svelte\\'")
  ;;       treesit-auto-recipe-list)
  (setq treesit-auto-langs '( typescript tsx html json css javascript
                              toml yaml
                              python r
                              ))
  (treesit-auto-install-all)
  (treesit-auto-add-to-auto-mode-alist)
  (global-treesit-auto-mode))

(use-package orderless
  :config
  (setopt completion-styles '(orderless basic))
  (add-to-list 'orderless-style-dispatchers
               #'me/handle-initialism-first-pattern))

(use-package vertico
  :config
  ;; (keymap-set vertico-map "<tab>" #'embark-act-with-completing-read)
  (setopt vertico-cycle t)
  (setopt vertico-count 14)
  (setopt vertico-resize nil)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (define-key vertico-map [remap delete-backward-char]
              #'vertico-directory-delete-char)
  (vertico-mode))

(use-package ws-butler
  :config
  ;; Fix problem with guix.el
  (add-to-list 'ws-butler-global-exempt-modes #'minibuffer-inactive-mode)
  ;; Fix for org because the org-element parser throws hella warnings since 9.5
  ;; (add-to-list 'ws-butler-global-exempt-modes #'org-mode)
  (ws-butler-global-mode))


;;; Backups
(setopt
 ;; Put them in the unusual path /home/backups/ to avoid cluttering rg output.
 backup-directory-alist `(("." . "/home/backups"))
 delete-old-versions t ;; nil led to Emacs looking broken for newbie-me
 vc-make-backup-files t ;; I don't commit regularly in every project
 version-control t)

;; Graceful degradation
(unless (file-writable-p "/home/backups/")
  (error "Disabling backups because can't write to: /home/backups/")
  (setq backup-directory-alist nil)
  (setq make-backup-files nil))

;; Lesson learned
(add-hook 'after-save-hook #'my-fix-invalid-backup-settings)


;;; Stuff

;; Some load paths
(dolist (dir '("/home/kept/emacs/key-seqs-finder/"
               "/home/kept/emacs/lintorg/"
               "/home/kept/emacs/twee-mode/"
               "/home/kept/emacs/nbt/"))
  (add-to-list 'load-path dir))
(require 'nbt)

;; TODO: Periodically re-test internet connectivity and set this, would be
;; useful for `my-stim' among other commands.
(defvar internet-connected nil)

;; TODO: how to find parent process?
;; (defvar child-emacs nil)
;; (process-attributes (emacs-pid))



;;; Doom ideas

(defun doom-run-switch-window-or-frame-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (unless (equal (old-selected-frame) (selected-frame))
      (run-hooks 'doom-switch-frame-hook))
    (unless (or (minibufferp)
                (equal (old-selected-window) (minibuffer-window)))
      (run-hooks 'doom-switch-window-hook))))

(defun doom-run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks 'doom-switch-buffer-hook)))

(my-hook-once 'elpaca-after-init-hook
  (add-hook 'window-selection-change-functions #'doom-run-switch-window-or-frame-hooks-h)
  (add-hook 'window-buffer-change-functions #'doom-run-switch-buffer-hooks-h)
  ;; `window-buffer-change-functions' doesn't run for server-visited files
  (add-hook 'server-visit-hook #'doom-run-switch-buffer-hooks-h))


;;; Font

;; (set-face-font 'default (font-spec :family "Iosevka Nerd Font" :size 30))
(set-face-font 'default (font-spec :family "Iosevka Nerd Font" :size 33))
;; (progn
;;   (set-face-font 'default (font-spec :family "Iosevka Nerd Font" :size 66))
;;   (setq vertico-count 7))

;; for recording
;; (set-face-font 'default (font-spec :family "Iosevka Nerd Font" :size 16))
;; (set-frame-width nil 160)

;; For my Surface Pro screen (2736x1824).  Here are the respective fonts'
;; maximum size that still let me split the screen into two 80-column panes.
;;
;; FONTS THAT SUPPORT LIGATURES https://wiki.archlinux.org/title/Font#Monospaced
;; (setq doom-font (font-spec :family "Iosevka Nerd Font" :size 32))
;; (setq doom-font (font-spec :family "Hurmit Nerd Font" :size 26))
;; (setq doom-font (font-spec :family "Hasklug Nerd Font" :size 27))
;; (setq doom-font (font-spec :family "JetBrains Mono Nerd Font" :size 27))
;; (setq doom-font (font-spec :family "Lilex Nerd Font" :size 27))
;; (setq doom-font (font-spec :family "CaskaydiaCove Nerd Font" :size 28))
;; (setq doom-font (font-spec :family "FiraCode Nerd Font" :size 26))


;;; Debugging

(defun me/enable-extra-debug ()
  (interactive)
  ;; (add-to-list 'interrupt-process-functions #'me/log-process-name)
  ;; (setq-default debug-on-signal 'quit debug-on-quit t)
  ;; (setq debug-on-signal t)
  ;; (setq backtrace-on-redisplay-error nil)
  ;; (debug-watch 'org-mode)
  (delq 'user-error debug-ignored-errors) ;; Also enter debugger for `user-error'
  )


;;;; Builtin

(fset #'display-startup-echo-area-message #'ignore)
(setq inhibit-startup-screen t)
(blink-cursor-mode 0)
(recentf-mode)
(savehist-mode)
(save-place-mode)
(column-number-mode)
(display-battery-mode)
(delete-selection-mode)
(pixel-scroll-precision-mode)

(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
(add-hook 'help-mode-hook #'visual-line-mode)
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; For `consult-outline' in elisp buffers
(me/unihook emacs-lisp-mode-hook
  (setq-local outline-regexp
              (if (string-search user-emacs-directory default-directory)
                  ";;;;*[^#]\\|(use-package "
                ";;;;*")))

;; Configure `hippie-expand'
(add-hook 'special-mode-hook #'my-hippie-config)
(add-hook 'prog-mode-hook #'my-hippie-config)
(add-hook 'text-mode-hook #'my-hippie-config)

;; Perf (especially Org)
(global-auto-composition-mode 0)
(setq-default bidi-display-reordering nil)
(setq-default bidi-inhibit-bpa t)

;; Catch mistakes in my elisp on save
;; (add-hook 'after-save-hook #'my-compile-and-drop)

;; Prevent accidental edits (easy to miss on files like this)
(add-hook 'so-long-mode-hook #'read-only-mode)

;; (auto-save-visited-mode) ;; NOTE see undoom.el
;; (context-menu-mode)
;; (repeat-mode)

;; Limit scrollback because gcc and R can spit out enough to slow my system (at
;; least when fontified).
;; Good values:
;; 2^12 on Latitude E7250.
;; 2^10 on Thinkpad X200.
(setq comint-buffer-maximum-size (expt 2 10))
(add-hook 'comint-output-filter-functions
          #'my-truncate-buffer-and-move-excess)

(setq savehist-additional-variables '(kill-ring
                                      register-alist
                                      mark-ring global-mark-ring
                                      search-ring regexp-search-ring))

(setq emacs-lisp-docstring-fill-column 72) ;; Emacs 30 bumped default from 65
(setq mode-line-percent-position nil)
(setq mode-line-modes nil)
(setq undo-limit (* 4 1000 1000)) ;; 4 MB (default 160 kB)
(setq undo-strong-limit (* 6 1000 1000))
(setq ring-bell-function #'ignore)
(setq sentence-end-double-space nil) ;; But see `my-fill-unfill-respect-double-space'
(setq gnus-select-method '(nntp "news.eternal-september.org"))
(setq byte-compile-warnings '(not free-vars))
(setq kill-whole-line t) ;; TIL after 9 years of emacs
(setq vc-follow-symlinks t)
(setq auth-sources '("~/.authinfo")) ;; https://magit.vc/manual/ghub/Storing-a-Token.html
(setq shr-max-image-proportion 0.5)
(setq initial-scratch-message nil)
(setq initial-buffer-choice #'recentf-open-files)
(setq mouse-yank-at-point t)
(setq recentf-max-saved-items 1000)
(setq save-interprogram-paste-before-kill t)
(setq select-enable-primary t)
(setq enable-local-variables :all)
(setq warning-display-at-bottom nil)
(setq custom-safe-themes t)
(setq message-log-max 8000)
;; (setq warning-minimum-log-level :debug)
(setq disabled-command-function nil) ;; bug: clutters init.el, not custom.el
(setq kill-read-only-ok t)
(setq kill-ring-max 600)
(setq-default enable-recursive-minibuffers t)
(setq-default fill-column 79)
(setq-default indent-tabs-mode nil)
(setq view-read-only t)
(setq debug-on-error t)
(setq vc-msg-copy-id-to-kill-ring nil)
(setq mouse-drag-and-drop-region-cross-program t) ;; no effect on wayland?
(setq show-paren-context-when-offscreen t)
(setq help-enable-variable-value-editing t)
(setq proced-enable-color-flag t)
(setq abbrev-suggest t)
(setq use-short-answers t)
(setq eval-expression-print-length 32)
(setq eval-expression-print-level 8)

;; Don't clear my echo area
(setq garbage-collection-messages nil)
(setq auto-save-no-message t)
(setq suggest-key-bindings nil) ;; prefer to show command's return value
(setq save-silently t)

;; Browse with Firefox or EWW depending on the link
(setq browse-url-generic-program "firefox")
(setq browse-url-handlers
      '(;;("github.com" . browse-url-generic)
        ("melpa.org" . browse-url-generic)
        ("fanfiction.net" . browse-url-generic)
        ;; Default
        ("." . eww-browse-url)))


;;;; Keys cleanup

;; Commands on 'too good' locations (risk that I get used to them).
(keymap-unset global-map "<f2>" t) ;; 2C-command
(keymap-unset global-map "<f3>" t) ;; kmacro-start-macro-or-insert-counter
(keymap-unset global-map "<f4>" t) ;; kmacro-end-or-call-macro
(keymap-unset global-map "<f5>" t) ;; NOTE: which-key-paging-key is here by default
(keymap-unset global-map "<f6>" t)
(keymap-unset global-map "<f7>" t)
(keymap-unset global-map "<f8>" t)
(keymap-unset global-map "<f9>" t)
(keymap-unset global-map "<f10>" t) ;; menu-bar-open
(keymap-unset global-map "<insert>" t) ;; overwrite-mode
(keymap-unset global-map "C-SPC" t)
(keymap-unset global-map "C-\\" t) ;; toggle-input-method
(keymap-unset global-map "C-q" t) ;; quoted-insert
(keymap-unset global-map "C-x (" t)
(keymap-unset global-map "C-x )" t)
(keymap-unset global-map "C-x *" t)
(keymap-unset global-map "C-x C-SPC" t)
(keymap-unset global-map "C-x C-z" t)
(keymap-unset global-map "C-x DEL" t) ;; bro just use M-- M-k
(keymap-unset global-map "C-x SPC" t)
(keymap-unset global-map "C-x k" t) ;; Discourage unproductive behavior
(keymap-unset global-map "C-x z" t)
(keymap-unset global-map "C-z" t) ;; suspend-frame
(keymap-unset global-map "M-." t) ;; xref-find-definitions
(keymap-unset global-map "M-`" t) ;; tmm-menubar
(keymap-unset global-map "M-i" t) ;; tab-to-tab-stop
(keymap-unset global-map "M-j" t) ;; default-indent-new-line
(keymap-unset global-map "M-m" t) ;; back-to-indentation
(keymap-unset global-map "M-o" t) ;; facemenu-keymap
(keymap-unset global-map "M-q" t) ;; fill-paragraph
(keymap-unset global-map "M-r" t) ;; move-to-window-line-top-bottom
(keymap-unset global-map "M-z" t) ;; zap-to-char
(keymap-unset global-map "M-~" t) ;; not-modified
(keymap-unset global-map "<XF86Back>" t) ;; previous-buffer
(keymap-unset global-map "<XF86Forward>" t) ;; next-buffer

;; Temporary unsets as training
(keymap-unset global-map "C-s" t)
(keymap-unset global-map "C-r" t)
(keymap-unset global-map "M-%" t)
(keymap-unset global-map "C-M-%" t)

(when (boundp 'doom-version)
  (keymap-unset global-map "C-'" t) ;; imenu
  (keymap-unset global-map "M--" t)
  (keymap-unset global-map "M-=" t))

(when (boundp 'spacemacs-version)
  (keymap-unset elisp-slime-nav-mode-map "M-," t)
  (keymap-unset elisp-slime-nav-mode-map "M-." t)
  (keymap-unset evil-emacs-state-map "C-z" t))

(after! geiser-mode
  (keymap-unset geiser-mode-map "M-," t)
  (keymap-unset geiser-mode-map "M-." t)
  (keymap-unset geiser-mode-map "M-`" t))

(after! geiser-repl
  (keymap-unset geiser-repl-mode-map "M-," t)
  (keymap-unset geiser-repl-mode-map "M-." t)
  (keymap-unset geiser-repl-mode-map "M-`" t))

(after! em-hist
  ;; be docile like M-x shell (don't "hijack" point)
  (keymap-unset eshell-hist-mode-map "<up>" t)
  (keymap-unset eshell-hist-mode-map "<down>" t))


;;;; Keys: fix prefix args

;; Don't waste good keys (C-123456890) on digit-argument.  In exchange, make it
;; more convenient to access them in other ways.
;;
;; - instead of C-u, let C-=, M-=, s-= be universal argument
;; - Let C--, M--, s-- be negative argument
;; - Let bare - and = be neg. and univ. argument when any hydra is open
;; - Let bare - and = be neg. and univ. argument when any prefix argument has
;;   been called and awaiting next input
;; - Allow typing M-= M-9 M-d, much better than M-= 9 M-d

(keymap-unset global-map "C-u" t)
(keymap-unset universal-argument-map "C-u" t)

(after! hydra
  (define-key hydra-base-map (kbd "C-u") nil)
  (define-key hydra-base-map (kbd "=") #'hydra--universal-argument)
  (define-key hydra-base-map (kbd "-") #'hydra--negative-argument))

(let ((modifiers '("C-" "M-" "s-" "H-" "A-"))
      (digits (split-string "1234567890" "" t)))
  (dolist (mod modifiers)
    ;; Some modes bind e.g. M-- (org-mode with org-replace-disputed-keys t), so
    ;; override everywhere.  Actually even if we haven't discovered any
    ;; conflicts it makes sense to encode that this must work everywhere.
    ;; However we may run into a problem where it also overrides hydra-base-map...
    ;;
    ;; TODO: Don't rely on general (perhaps provide a way in
    ;; deianira-mass-remap.el, although just Emacs internals would be great).
    ;; You'll note dei--known-keymaps does NOT include hydra-base-map as it's
    ;; not a mode map.  In other words transient maps like that will work as
    ;; intended.  Elegant.  Does general override have the same elegance?
    (after! general
      (define-key general-override-mode-map (kbd (concat mod "=")) #'universal-argument)
      (define-key general-override-mode-map (kbd (concat mod "-")) #'negative-argument))

    (define-key global-map (kbd (concat mod "=")) #'universal-argument)
    (define-key global-map (kbd (concat mod "-")) #'negative-argument)
    (define-key universal-argument-map (kbd (concat mod "=")) #'universal-argument-more)
    ;; necessary?
    (after! hydra
      (define-key hydra-base-map (kbd (concat mod "=")) #'hydra--universal-argument)
      (define-key hydra-base-map (kbd (concat mod "-")) #'hydra--negative-argument))
    (dolist (d digits)
      (define-key global-map (kbd (concat mod d)) nil)
      (define-key universal-argument-map (kbd (concat mod d)) #'digit-argument)
      ;; REVIEW: does it mess with nonum hydras?
      (after! hydra
        (define-key hydra-base-map (kbd (concat mod d)) #'hydra--digit-argument)))))


;;;; Keys

;; Create minor mode maps for modes that lack them
(defvar-keymap my-abbrev-minor-mode-map)
(add-to-list 'minor-mode-map-alist (cons 'abbrev-mode my-abbrev-minor-mode-map))


;;; Grand list
;; One day, I might relocate these settings to be more context-relevant
;; (i.e. near use-package forms), then consult the total list with
;; `general-describe-keybindings', although that one is cluttered by Doom's
;; settings, or bind-key's `describe-personal-keybindings'.  But I enjoy
;; handling them this way, and the upside with setting the bindings this early
;; is that I still have them when half my init has broken.

(require 'defrepeater) ;; not needed if I would just remember to call `repeat'!

(keymap-set global-map "<f10> a" #'my-save-buffer-and-amend)
(keymap-set global-map "<f10> d" #'org-download-yank)
(keymap-set global-map "<f10> e" #'eww)
(keymap-set global-map "<f10> g" #'guix-popup)
(keymap-set global-map "<f10> k" #'gif-screencast-start-or-stop)
(keymap-set global-map "<f3> f r" #'crux-rename-buffer-and-file)
(keymap-set global-map "<f10> l" #'mw-thesaurus-lookup)
(keymap-set global-map "<f10> n" #'my-normie-toggle)
(keymap-set global-map "<f10> p" #'my-pipe)
(keymap-set global-map "<f10> r c" #'my-copy-region-to-variable)
(keymap-set global-map "<f10> r e" #'my-eval-and-replace-print)
(keymap-set global-map "<f10> r p" (defrepeater #'my-cycle-path-at-point))
(keymap-set global-map "<f10> r v" #'my-replace-var-at-point-with-value)
(keymap-set global-map "<f10> r w" #'my-copy-region-or-rest-of-line-to-other-window)
(keymap-set global-map "<f10> s" #'my-save-buffer-and-commit)
(keymap-set global-map "<f2> e d" #'eval-defun)
(keymap-set global-map "<f2> e e" #'eval-last-sexp)
(keymap-set global-map "<f2> e l" #'load-library)
(keymap-set global-map "<f2> e p" #'eval-print-last-sexp)
(keymap-set global-map "<f2> e r" #'eval-region)
(keymap-set global-map "<f2> e s" #'ess-eval-region-or-function-or-paragraph-and-step) ;; ess everywhere
(keymap-set global-map "<f2> e x" #'eval-expression)
(keymap-set global-map "<f2> m" #'my-last-daily-file)
(keymap-set global-map "<f2> z" #'my-sleep)
(keymap-set global-map "<f3> f d" #'me/delete-this-file)
(keymap-set global-map "<f3> q q" #'save-buffers-kill-emacs)
(keymap-set global-map "<f5>" #'repeat)
(keymap-set global-map "C-0" #'hippie-expand)
(keymap-set global-map "C-1" #'switch-to-buffer)
(keymap-set global-map "C-2" #'other-window)
(keymap-set global-map "C-3" #'unexpand-abbrev)
(keymap-set global-map "C-4" #'my-stim)
(keymap-set global-map "M-o m" #'org-node-fakeroam-show-buffer)
(keymap-set global-map "C-5" #'my-prev-file-in-dir)
(keymap-set global-map "C-6" #'my-next-file-in-dir)
(keymap-set global-map "C-8" #'kill-whole-line)
(keymap-set global-map "C-9" #'duplicate-dwim)
(keymap-set global-map "C-<next>" #'iflipb-next-buffer)
(keymap-set global-map "C-<prior>" #'iflipb-previous-buffer)
(keymap-set global-map "C-M-/" #'dabbrev-expand)
(keymap-set global-map "C-h h" #'my-describe-last-key)
(keymap-set global-map "C-h M" #'describe-mode)
(keymap-set global-map "C-h P" #'finder-by-keyword) ;; original C-h p
(keymap-set global-map "C-h m" #'consult-minor-mode-menu)
(keymap-set global-map "C-h p" #'find-library)
(keymap-set global-map "C-h q" #'quoted-insert)
(keymap-set global-map "C-h r" #'consult-info)
(keymap-set global-map "M-o o" #'hkey-operate)
(keymap-set global-map "C-h s" #'find-function)
(keymap-set global-map "C-h t" #'me/toggle-profiler)
(keymap-set global-map "C-q" #'my-dired-shell-cycle)
(keymap-set global-map "C-x C-\;" (defrepeater #'comment-line))
(keymap-set global-map "C-x C-c" #'restart-emacs)
(keymap-set global-map "C-x g g" #'magit-status)
(keymap-set global-map "C-x g t" #'git-timemachine)
(keymap-set global-map "C-x s" #'save-some-buffers)
(keymap-set global-map "M-/" #'dabbrev-completion)
(keymap-set global-map "M-1" #'switch-to-buffer)
(keymap-set global-map "M-2" #'my-other-window-any-frame-hyprland)
(keymap-set global-map "C-r" #'embark-act)
(keymap-set global-map "M-<backspace>" #'sp-backward-unwrap-sexp)
(keymap-set global-map "M-<delete>" #'sp-unwrap-sexp)
(keymap-set global-map "M-<down>" #'drag-stuff-down)
(keymap-set global-map "M-<f3>" #'kill-current-buffer)
(keymap-set global-map "M-<f4>" #'kill-current-buffer) ;; EXWM
(keymap-set global-map "M-<insert>" #'sp-rewrap-sexp)
(keymap-set global-map "M-<up>" #'drag-stuff-up)
(keymap-set global-map "M-g a a" (defrepeater #'avy-pop-mark))
(keymap-set global-map "M-g a c" #'avy-goto-char-2)
(keymap-set global-map "M-g a g c" #'avy-goto-char-2)
(keymap-set global-map "M-g a g e" #'avy-goto-end-of-line)
(keymap-set global-map "M-g a g l" #'avy-goto-line)
(keymap-set global-map "M-g a g o" #'avy-goto-word-or-subword-1)
(keymap-set global-map "M-g a g q" #'avy-goto-subword-1)
(keymap-set global-map "M-g a g s" #'avy-goto-symbol-1)
(keymap-set global-map "M-g a g w" #'avy-goto-word-1)
(keymap-set global-map "M-g a k" #'avy-kill-region)
(keymap-set global-map "M-g a m l" #'avy-move-line)
(keymap-set global-map "M-g a m r" #'avy-move-region)
(keymap-set global-map "M-g a n" #'avy-next)
(keymap-set global-map "M-g a o" #'avy-goto-symbol)
(keymap-set global-map "M-g a p" #'avy-prev)
(keymap-set global-map "M-g a r" #'avy-resume)
(keymap-set global-map "M-g a s" #'avy-isearch)
(keymap-set global-map "M-g a w" #'avy-kill-ring-save-region)
(keymap-set global-map "M-g c" #'goto-char)
(keymap-set global-map "M-g e" #'consult-error)
(keymap-set global-map "M-g h" #'hkey-either)
(keymap-set global-map "M-g i" #'consult-imenu)
(keymap-set global-map "M-g k" #'consult-global-mark)
(keymap-set global-map "M-g l" #'consult-line)
(keymap-set global-map "M-g m" #'consult-mark)
(keymap-set global-map "M-g o" #'consult-outline)
(keymap-set global-map "M-g t" #'avy-goto-char-timer)
(keymap-set global-map "M-g z" #'avy-goto-word-or-subword-1)
(keymap-set global-map "M-m g" (defrepeater #'pop-global-mark)) ;; was C-x C-SPC
(keymap-set global-map "M-m c" #'mc/mark-pop)
(keymap-set global-map "M-m m" #'set-mark-command) ;; was C-SPC
(keymap-set global-map "M-m p" (defrepeater #'pop-to-mark-command))
(keymap-set global-map "M-m r" #'rectangle-mark-mode) ;; was C-x SPC
(keymap-set global-map "M-m x" #'exchange-point-and-mark) ;; also on C-x C-x
(keymap-set global-map "M-o (" #'app-launcher-run-app)
(keymap-set global-map "M-o -" #'doom/decrease-font-size)
(keymap-set global-map "M-o 1" (defrepeater #'my-insert-other-buffer-file-name-and-cycle))
(keymap-set global-map "M-o 2" (defrepeater #'my-toggle-selective-display))
(keymap-set global-map "M-o 3" #'elfeed)
(keymap-set global-map "M-o 5" #'my-lookup-word)
(keymap-set global-map "M-o <f3>" #'git-messenger:popup-message)
(keymap-set global-map "M-o <next>" (defrepeater #'my-next-buffer-of-same-mode))
(keymap-set global-map "M-o <prior>" (defrepeater #'my-previous-buffer-of-same-mode))
(keymap-set global-map "M-o =" #'doom/increase-font-size)
(keymap-set global-map "M-o M--" #'doom/decrease-font-size)
(keymap-set global-map "C-x g l" #'vc-msg-show)
(keymap-set global-map "M-o a" #'org-agenda)
(keymap-set global-map "M-o b" #'backup-walker-start)
(keymap-set global-map "M-o c" #'org-capture)
(keymap-set global-map "M-o d" #'my-insert-today)
(keymap-set global-map "M-o t" #'me/goto-today)
(keymap-set global-map "M-o f" #'org-node-find)
(keymap-set global-map "M-o g" #'org-node-grep)
(keymap-set global-map "M-o h" #'consult-find)
(keymap-set global-map "M-o i" #'org-node-insert-link)
(keymap-set global-map "M-o n" #'org-node-nodeify-entry)
(keymap-set global-map "M-o l" #'helm-locate)
(keymap-set global-map "M-o p" #'my-spawn-process)
(keymap-set global-map "M-o r" #'vertico-repeat)
(keymap-set global-map "M-o s" #'org-node-series-dispatch)
(keymap-set global-map "M-o w" #'sp-rewrap-sexp)
(keymap-set global-map "M-o x" #'execute-extended-command)
(keymap-set global-map "M-o y" (defrepeater #'my-fill-unfill-respect-double-space))
(keymap-set global-map "M-o z" #'org-node-visit-random)
(keymap-set global-map "M-q b" (defrepeater #'bury-buffer))
(keymap-set global-map "M-q k" (defrepeater #'kill-current-buffer))
(keymap-set global-map "M-q r" #'my-revisit-buffer)
(keymap-set global-map "M-q u" (defrepeater #'unbury-buffer))
;; (keymap-set global-map "M-q" #'completion-at-point)
(keymap-set global-map "M-s 5" #'query-replace)
(keymap-set global-map "M-s 6" #'query-replace-regexp)
(keymap-set global-map "M-s d" #'me/rg-cwd)
(keymap-set global-map "M-s l" #'+default/find-file-under-here)
(keymap-set global-map "M-s m" #'consult-line-multi)
(keymap-set global-map "M-s p" #'consult-ripgrep)
(keymap-set global-map "M-s r" #'isearch-backward)
(keymap-set global-map "M-s s" #'isearch-forward)
(keymap-set global-map "M-|" #'my-shell-command-replace-region)
;; (keymap-set global-map "C-q" #'+shell/here)
;; (keymap-set global-map "C-x k c" #'consult-kmacro)
;; (keymap-set global-map "M-o j" #'+default/find-file-under-here)
;; (keymap-set global-map "M-o k" #'+default/search-project)
;; (keymap-set global-map "M-r" #'hkey-either)
;; (keymap-set global-map "TAB" #'my-tab-command)

(keymap-set isearch-mode-map "<down>" #'isearch-repeat-forward)
(keymap-set isearch-mode-map "<up>" #'isearch-repeat-backward)
(keymap-set isearch-mode-map "M-s" #'isearch-repeat-forward)
(keymap-set isearch-mode-map "M-r" #'isearch-repeat-backward)
(keymap-set my-abbrev-minor-mode-map "`" #'expand-abbrev)

;; (keymap-set "" #'consult-focus-lines)  ;; Man.  Disturbing command.
;; (keymap-set minibuffer-mode-map "M-g i" #'consult-imenu-multi)
;; (keymap-set "" #'browse-url)

;; Some Greek letters
;; (This sequence doesn't align with the official Greek alphabet)
(define-key key-translation-map (kbd "<f7> a") (kbd "α")) ;;alpha
(define-key key-translation-map (kbd "<f7> b") (kbd "β")) ;;beta
(define-key key-translation-map (kbd "<f7> c") (kbd "χ")) ;;chi
(define-key key-translation-map (kbd "<f7> d") (kbd "δ")) ;;delta
(define-key key-translation-map (kbd "<f7> e") (kbd "ε")) ;;epsilon
(define-key key-translation-map (kbd "<f7> f") (kbd "φ")) ;;phi
(define-key key-translation-map (kbd "<f7> g") (kbd "γ")) ;;gamma
(define-key key-translation-map (kbd "<f7> h") (kbd "θ")) ;;theta
(define-key key-translation-map (kbd "<f7> i") (kbd "ι")) ;;iota
(define-key key-translation-map (kbd "<f7> k") (kbd "κ")) ;;kappa
(define-key key-translation-map (kbd "<f7> l") (kbd "λ")) ;;lambda
(define-key key-translation-map (kbd "<f7> m") (kbd "μ")) ;;mu
(define-key key-translation-map (kbd "<f7> n") (kbd "η")) ;;eta
(define-key key-translation-map (kbd "<f7> o") (kbd "ω")) ;;omega
(define-key key-translation-map (kbd "<f7> p") (kbd "π")) ;;pi
(define-key key-translation-map (kbd "<f7> r") (kbd "ρ")) ;;rho
(define-key key-translation-map (kbd "<f7> s") (kbd "σ")) ;;sigma
(define-key key-translation-map (kbd "<f7> t") (kbd "τ")) ;;tau
(define-key key-translation-map (kbd "<f7> u") (kbd "υ")) ;;upsilon
(define-key key-translation-map (kbd "<f7> v") (kbd "ν")) ;;nu
(define-key key-translation-map (kbd "<f7> x") (kbd "ξ")) ;;xi
(define-key key-translation-map (kbd "<f7> z") (kbd "ζ")) ;;zeta
;; (keymap-set key-translation-map "<f7> w" "")
;; (keymap-set key-translation-map "<f7> y" "")
;; (keymap-set key-translation-map "<f7> j" "")
;; (keymap-set key-translation-map "<f7> q" "")

(setopt doom-leader-alt-key "<f3>")
(setopt doom-localleader-alt-key "<f4>")

;; Vimmers' Stockholm syndrome with typing capital letters...  There
;; needs to exist an alternative module like doom-keybinds but that binds no
;; capital letters {nor any modifier, just lowercase things}
(after! doom-keybinds
  (keymap-set doom-leader-map "f d" (keymap-lookup doom-leader-map "f D"))
  (keymap-set doom-leader-map "f c" (keymap-lookup doom-leader-map "f C"))

  ;; Overrides for Org localleader
  (my-hook-once 'org-load-hook
    (map! :map org-mode-map :localleader "i" #'org-node-nodeify-entry)
    (map! :map org-mode-map :localleader "h" #'org-node-insert-heading)))

(after! mu4e
  ;; Uppercase key bindings for common actions, really?  Die.
  ;; .. thinking that I should automate this, prior art in massmapper.el
  (keymap-unset mu4e-main-mode-map "C" t)
  (keymap-unset mu4e-main-mode-map "U" t)
  (keymap-unset mu4e-main-mode-map "R" t)
  (keymap-set mu4e-main-mode-map "c" #'mu4e-compose-new)
  (keymap-set mu4e-main-mode-map "u" #'mu4e-update-mail-and-index)
  (keymap-set mu4e-main-mode-map "r" #'mu4e-compose-reply)
  (keymap-unset mu4e-compose-mode-map "C-U" t)
  (keymap-set mu4e-compose-mode-map "C-u" #'mu4e-update-mail-and-index))

(after! dired-hist
  (keymap-set dired-mode-map "l" #'dired-hist-go-back)
  ;; the hell
  (keymap-set dired-mode-map "L" #'dired-hist-go-forward))

(after! cus-edit
  (keymap-set custom-mode-map "q" #'kill-current-buffer))

(after! ess-mode
  (keymap-set ess-mode-map "<f1> <f2>" #'ess-abort)
  (keymap-set ess-mode-map "<f1> <f3>" #'ess-interrupt)
  (keymap-set ess-mode-map "C-<return>" #'ess-eval-line))

(after! eww
  (keymap-set eww-mode-map "q" #'kill-current-buffer)
  (keymap-set eww-bookmark-mode-map "w" #'my-eww-bookmark-copy-url))

(after! vertico
  (keymap-set vertico-map "M-<backspace>" #'vertico-directory-up)
  (keymap-set vertico-map "<next>" #'scroll-up-command)
  (keymap-set vertico-map "<prior>" #'scroll-down-command))

(after! ledger-mode
  (keymap-set ledger-mode-map "M-<return>" #'crux-duplicate-current-line-or-region))

(after! shell
  (keymap-set shell-mode-map "C-S-n" #'my-new-shell))

(after! esh-mode
  (keymap-set eshell-mode-map "C-S-n" #'my-new-eshell)
  (keymap-set eshell-mode-map "<f4> n" (defrepeater #'my-esh-narrow-dwim)))

(after! ctrlf
  (keymap-set ctrlf-minibuffer-mode-map "<down>"   #'ctrlf-forward-default)
  (keymap-set ctrlf-minibuffer-mode-map "M-<down>" #'ctrlf-forward-alternate)
  (keymap-set ctrlf-minibuffer-mode-map "<up>"     #'ctrlf-backward-default)
  (keymap-set ctrlf-minibuffer-mode-map "M-<up>"   #'ctrlf-backward-alternate))

(after! dired
  (keymap-set dired-mode-map "b" #'dired-up-directory)
  (keymap-set dired-mode-map ")" #'dired-git-info-mode)
  (keymap-set dired-mode-map "M-<up>" #'dired-up-directory)
  ;; (keymap-set dired-mode-map "s-<return>" #'my-dired-open-file-with-default-tool)
  (keymap-set dired-mode-map "s-<return>" #'embark-open-externally))

(after! embark
  (keymap-set embark-general-map "M-h" #'hkey-either))

(after! grep
  (keymap-set grep-mode-map "C-x C-q" #'wgrep-change-to-wgrep-mode))

(after! view
  (keymap-set view-mode-map "e" #'my-view-exit-and-reopen-as-root))

(after! org-keys
  (keymap-set org-mode-map "C-c u" #'org-node-insert-heading))

(after! timer-list
  (keymap-set timer-list-mode-map "a" #'my-timer-list-autorefresh))

(after! dired
  ;; Dired's default unbound keys: `, b, E, J, K, r, z, <backspace>
  ;; Dired's useless keys: h, 1234567890
  )

(after! which-key
  (keymap-set which-key-mode-map "DEL" #'which-key-undo-key))


;;; Civilize C-g
;; Doesn't always work, see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=58808

;; TODO: move this to kmonad or some such external program.  Possibilities:
;; - Translate keys only when emacs is in focus, if possible
;; - Translate keys globally, and use EXWM simulation keys to translate back
;;   into a real escape for other apps

;; (define-key function-key-map    (kbd "<escape>") nil)
;; (define-key key-translation-map (kbd "<escape>") nil)
;; (define-key input-decode-map    (kbd "<escape>") nil)

(define-key function-key-map    (kbd "<escape>") (kbd "C-g"))
(define-key key-translation-map (kbd "<escape>") (kbd "C-g"))
(define-key input-decode-map    (kbd "<escape>") (kbd "C-g"))

;; broken
;; https://lists.gnu.org/archive/html/emacs-devel/2008-09/msg00638.html
;; (set-quit-char ?\[)



;;; More repeaters!

;; (define-repeat-map my-buffer-thumbing
;;   ("<right>"   next-buffer
;;    "C-<right>" next-buffer
;;    "<left>"   previous-buffer
;;    "C-<left>" previous-buffer))

;; (define-repeat-map my-nav
;;   ("f" forward-char
;;    "b" backward-char
;;    "n" next-line
;;    "p" previous-line))

;; ;; from author of define-repeat-map
;; (define-repeat-map my-case
;;   ("c" capitalize-word
;;    "u" upcase-word
;;    "l" downcase-word)
;;   (:continue "f" forward-word
;;              "b" backward-word)
;;   (:enter downcase-dwim
;;           upcase-dwim
;;           capitalize-dwim))

;; TODO: mc/ commands have some magic to avoid asking about re-running
;; themselves once for all cursors ... We need to apply the magic to the
;; repeating version of the command as well.  I considered using
;; define-repeat-map, but it does not make sense to me to bind every mc/
;; variant inside the same repeat-map.  I want just the single one to become
;; repeatable, although the correct response in this sort of situation is to
;; just remember the `repeat' command.  But correct for who? If I want to type
;; <f3> m n n n n n n n n instead of using repeat, there should bloody well be
;; some convenient elisp to allow it.  And this is the convenient elisp, it
;; just does not work for self-aware commands.

;; (define-key global-map [remap mc/mark-next-like-this] (defrepeater #'mc/mark-next-like-this))
;; (define-key global-map [remap mc/mark-previous-like-this] (defrepeater #'mc/mark-previous-like-this))

;; While we're at it, enhance the classic `repeat'.  Note that it's totally
;; separate from the Emacs 28 repeat-map system.

;; Let me type a digit such as 5 after a `repeat' to repeat another 5 times.
(advice-add #'repeat :after #'my-enable-post-repeat-transient-map)

(my-normie:abnormalize)


;;;; Completion

;; Wishlist

;; - Some commands should not sort by length but alphabetic. e.g. M-x recentf
;;   (not that I use that one).  how to config such thing?

;; - Marginalia annotations go off-screen when one of the files have long name
;;   (especially when you use vertico-buffer-mode so you only get half screen
;;   width).  How to fix?



;;;; Life-organization essentials 2024-06-06

(setopt org-reverse-note-order t)
(setopt org-agenda-todo-ignore-scheduled 'all)
(setopt org-habit-show-all-today t)
(setopt org-habit-show-done-always-green t)
(setopt org-habit-graph-column 60)
(setopt org-habit-following-days 0)
(setopt org-habit-preceding-days 14)
(after! org (setopt org-todo-keywords '((sequence "IDEA" "DONE"))))
(setq org-agenda-files
      (seq-filter #'file-exists-p
                  '("~/org/daily-review.org"
                    "~/org/outcomes.org"
                    "~/org/ideas.org")))

(setq org-capture-templates
      '(("o" "Outcome"
         entry (file "~/org/outcomes.org")
         "* %?"
         :prepend t
         :hook ( org-node-put-created
                 org-id-get-create ))

        ("c" "Component"
         entry (function (lambda ()
                           (org-id-open "3ec7f712-2437-4222-8905-72d39ba6188a" nil)
                           (widen)
                           (push-mark (point-min) t)
                           (goto-char (point-max))
                           (activate-mark t)
                           (consult-org-heading nil 'region-start-level)
                           (deactivate-mark t)))
         "* COMPONENT %?"
         :prepend t
         :hook org-node-put-created
         :prepare-finalize my-put-currency)

        ("t" "Thread"
         entry (file+headline "~/org/daily-review.org" "Threads")
         "* THREAD %?"
         :prepend t
         :hook ( my-turn-into-org-habit
                 org-node-put-created ))

        ("r" "Reviewable"
         entry (file+headline "~/org/daily-review.org" "Reviewables")
         "* IDEA %?"
         :prepend t
         :hook ( my-turn-into-org-habit
                 org-node-put-created ))

        ("i" "Idea"
         entry (file+headline "~/org/ideas.org" "Unsorted")
         "* IDEA %?"
         :prepend t
         )

        ("s" "Capture into ID-node series"
         plain (function org-node-series-capture-target) nil
         :empty-lines-after 1
         :no-save t)

        ("n" "Capture into ID node"
         plain (function org-node-capture-target) nil
         :empty-lines-after 1
         :no-save t)

        ("j" "Jump to ID node"
         plain (function org-node-capture-target) nil
         :jump-to-captured t
         :immediate-finish t)

        ("q" "Make quick stub ID node"
         plain (function org-node-capture-target) nil
         :immediate-finish t)))

(defvar my-currency-hist nil)
(defun my-put-currency ()
  (interactive)
  (let ((value (read-string "CURRENCY_DELTAS: "
                            (or (org-entry-get nil "CURRENCY_DELTAS")
                                "(( 10))")
                            'my-currency-hist)))
    (if value
        (org-entry-put nil "CURRENCY_DELTAS" value)
      (org-entry-delete nil "CURRENCY_DELTAS"))))

(defun my-turn-into-org-habit ()
  "Turn heading near point into an org-habit."
  (interactive)
  (unless (org-get-todo-state)
    (org-todo))
  (org-entry-put nil "STYLE" "habit")
  (org-entry-put nil "SCHEDULED" (format-time-string "<%F .+1d>")))

(add-hook 'me/load-theme-hook
          (defun my-theme-mod-life ()
            (after! org
              (let ((green (face-foreground 'success))
                    (grey (face-foreground 'org-drawer)))
                ;; instead of red todo, i use green IDEA
                (set-face-foreground 'org-todo green)
                (set-face-foreground 'org-done grey)))
            (after! org-habit
              ;; the default red color doesn't end up helping my psyche
              (set-face-attribute
               'org-habit-overdue-face ()
               :background (or (face-foreground 'font-lock-comment-face) 'unspecified))
              (set-face-attribute
               'org-habit-overdue-future-face ()
               :background (or (face-foreground 'font-lock-comment-face) 'unspecified)))))


;;; Auto-save-visited replica

;; I used `auto-save-visited-mode' for years.  But many Emacs features are noisy
;; on save, and I finally tired of the noise.  We can configure the classic
;; `auto-save-mode' to grant us largely the same convenience:
(setq auto-save-timeout 5)
(setq auto-save-no-message t)
(setq my-save-all-timer (run-with-idle-timer 40 t #'my-save-all))
;; (add-function :after after-focus-change-function #'my-save-all)
;; (add-hook 'window-buffer-change-functions #'my-save-all)
;; (add-hook 'magit-pre-display-buffer-hook #'my-save-all)
;; (add-hook 'magit-pre-refresh-hook #'my-save-all)

(advice-add 'after-find-file :before #'my-auto-recover-this-file)
(let (mutually-recursed-once)
  (defun my-auto-recover-this-file (&optional _ _ _ after-revert _)
    (unless (or mutually-recursed-once after-revert)
      (when (file-newer-than-file-p (or buffer-auto-save-file-name
                                        (make-auto-save-file-name))
                                    buffer-file-name)
        ;; 1. Gotta patch this function so it can skip the prompt
        ;; 2. Also, it calls `after-find-file' itself, thus we need the
        ;;    `mutually-recursed-once' check.
        (setq mutually-recursed-once t)
        (unwind-protect
            (recover-file buffer-file-name)
          (setq mutually-recursed-once nil))))))


;; wait, you mean to tell me that nromally when a loop visits many files to
;; edit them, find-file-noselect just emits polite warnings that the user won't
;; notice (on account of the loop), so the loop will go ahead unhindered???

;; ive been protected by the above advice, i didnt know the default will be so
;; gung ho


;;; Gradually preload packages in the background

(add-hook 'elpaca-after-init-hook
          (defun me/progressive-preload ()
            (while-no-input
              (while-let ((next-lib (pop me/progressive-preload-queue)))
                (require next-lib nil t)))
            (if me/progressive-preload-queue
                (run-with-idle-timer 2 nil #'me/progressive-preload)))
          99)

(defvar me/progressive-preload-queue
  '(dired
    org
    org-agenda
    elisp-mode
    comint
    eshell
    esh-mode
    em-alias
    em-banner
    em-basic
    em-cmpl
    em-elecslash
    em-extpipe
    em-hist
    em-ls
    em-pred
    em-prompt
    em-rebind
    em-script
    em-smart
    em-term
    em-tramp
    em-unix
    em-xtra))


;;;; Experiment zone

(add-hook 'before-save-hook
          (defun me/make-parent-directory ()
            (mkdir (file-name-directory buffer-file-name) t)))
;; ;; Doom does the above at find-file time:
;; (add-hook! 'find-file-not-found-functions
;;   (defun doom-create-missing-directories-h ()
;;     "Automatically create missing directories when creating new files."
;;     (unless (file-remote-p buffer-file-name)
;;       (let ((parent-directory (file-name-directory buffer-file-name)))
;;         (and (not (file-directory-p parent-directory))
;;              (y-or-n-p (format "Directory `%s' does not exist! Create it?"
;;                                parent-directory))
;;              (progn (make-directory parent-directory 'parents)
;;                     t))))))


(setopt helpful-max-buffers nil) ;; what's the point of killing buffers
(setopt ranger-map-style 'emacs)
(setopt which-key-idle-delay 0.2)
(setopt rainbow-x-colors nil) ;; only colorize hex strings

;; (use-package scroll-on-drag
;;   :config (keymap-set ))

(me/genhook elpaca-after-init-hook
  (setq my-stim-collection (my-stim-collection-generate)))

;; Wishlist: That consult-outline, and consult-line with the following setting,
;; would start near where point is.  That is, scroll!
;; (setq consult-line-start-from-top t)

;; (defvar me/consult-curr-line nil)

;; (advice-add 'consult--line-candidates :after
;;             (defun blargh (_top curr-line)
;;               (setq me/consult-curr-line curr-line)))

;; (add-hook 'minibuffer-setup-hook
;;           (defun me/setup-consult-line-and-outline ()
;;             (when (memq this-command '(consult-line consult-outline))
;;               (vertico-next me/consult-curr-line)

;;               (message "%s" me/consult-curr-line
;;                        ;; (orderless-annotation 'always )
;;                        )
;;               ))
;;           100)


(defun my-publish ()
  (interactive)
  (let* ((el (concat user-emacs-directory "lib-publish-blog.el"))
         (eln (comp-el-to-eln-filename el)))
    (unless (file-newer-than-file-p eln el)
      (native-compile el))
    (load eln))
  (call-interactively #'my-publish-begin))

;; When closing and reopening Emacs often (esp with 2+ simultaneous instances),
;; ALL THE TIME things missing from recentf.  Becaus the design implements no
;; crash-only principles.
;; (advice-add #'recentf-track-opened-file :after #'recentf-save-list)
;; ... now it saves backupfiles~ because of not running the kill-hook
;; `recentf-track-closed-file' prior to save
;; Final fix, I hope:
(advice-add #'recentf-track-opened-file :override
            (defun my-track-file ()
              (when (and buffer-file-name
                         (recentf-keep-p buffer-file-name))
                (recentf-add-file buffer-file-name)
                (let ((save-silently t)
                      (inhibit-message t))
                  (recentf-save-list)))))


;; Python
;; (use-package pyvenv)
(use-package pyvenv-auto
  :hook ((python-mode . pyvenv-auto-run)))
(use-package anaconda-mode)
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; (elpaca (nano :fetcher github :repo "rougier/nano-emacs")

;;   ;; Theme
;;   (require 'nano-faces)
;;   (require 'nano-theme)
;;   (require 'nano-theme-dark)
;;   (require 'nano-theme-light)

;;   (cond
;;    ((member "-default" command-line-args) t)
;;    ((member "-dark" command-line-args) (nano-theme-set-dark))
;;    (t (nano-theme-set-light)))
;;   (call-interactively 'nano-refresh-theme)

;;   ;; Nano default settings (optional)
;;   (require 'nano-defaults)

;;   ;; Nano session saving (optional)
;;   ;; (require 'nano-session)

;;   ;; Nano header & mode lines (optional)
;;   (require 'nano-modeline))

;; ;; Nano key bindings modification (optional)
;; ;; (require 'nano-bindings)

;; ;; Compact layout (need to be loaded after nano-modeline)
;; ;; (require 'nano-compact)

;;; from https://github.com/jamescherti/minimal-emacs.d

;; Disable the warning "X and Y are the same file". Ignoring this warning is
;; acceptable since it will redirect you to the existing buffer regardless.
(setq find-file-suppress-same-file-warnings t)

;; Resizing the Emacs frame can be costly when changing the font. Disable this
;; to improve startup times with fonts larger than the system default.
(setq frame-resize-pixelwise t)

;; However, do not resize windows pixelwise, as this can cause crashes in some
;; cases when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; Move point to top/bottom of buffer before signaling a scrolling error.
(setq scroll-error-top-bottom t)

;; Keeps screen position if the scroll command moved it vertically out of the
;; window.
(setq scroll-preserve-screen-position t)

;; Continue wrapped lines at whitespace rather than breaking in the
;; middle of a word.
(setq-default word-wrap t)

;; Disable wrapping by default due to its performance cost.
(setq-default truncate-lines t)

;; ...
;; it loaded my "wip.el" file that has no autoloads nor provides
(setq help-enable-completion-autoload nil)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
