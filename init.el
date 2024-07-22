;; -*- lexical-binding: t; -*-

;; Init Elpaca the package manager (snippet provided at their README)
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
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
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
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

;; Config use-package
(elpaca elpaca-use-package
  (setopt use-package-always-ensure t)
  (setopt use-package-compute-statistics t)
  (if init-file-debug
      (setopt use-package-verbose t)
    (setopt use-package-expand-minimally t))
  ;; Make the :ensure keyword call Elpaca
  (elpaca-use-package-mode))

;; Needed during init
(use-package no-littering)
(use-package compat) ;; to compile lib.el
(use-package crux) ;; to compile lib.el
(use-package dash) ;; to compile lib.el
(use-package named-timer)
(use-package defrepeater)
(elpaca-wait)

(setq custom-file (concat user-emacs-directory "custom.el"))
(setq custom-safe-themes t)
(set-frame-parameter nil 'alpha-background 82)
(load-theme 'wombat)


;;;; Prep a progressive preload to run afterwards

(add-hook 'elpaca-after-init-hook
          (lambda () (run-with-idle-timer 2 nil #'me/progressive-preload)))

(defun me/progressive-preload ()
  (while-no-input
    (while-let ((next-lib (pop me/progressive-preload-queue)))
      (message "Preloading... (%S)" next-lib)
      (let ((inhibit-message t))
        (ignore-errors
          (require next-lib nil t)))))
  (if me/progressive-preload-queue
      (run-with-idle-timer 2 nil #'me/progressive-preload)
    (message "Preloading... done")))

;; (advice-add 'use-package :after
;;             (lambda (package-name &rest _)
;;               (push package-name me/progressive-preload-queue)))

;; (advice-add 'elpaca :after
;;             (lambda (&rest args)
;;               (when (symbolp (car args))
;;                 (push (car args) me/progressive-preload-queue))))

;; Pre-fill with some builtins
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


;;;; Load

(defmacro me/load-newest-ensure-compiled (path)
  `(let* ((el (concat (string-remove-suffix ".el" ,path) ".el"))
          (elc (concat el "c")))
     (when (file-newer-than-file-p el elc)
       (byte-compile-file el))
     (load elc)))

(defvar me/elapsed-gc-after-init nil)
(defvar me/time-at-init-done nil)

(unwind-protect
    (let ((.emacs.d user-emacs-directory))
      ;; Compile a personal library of defuns.  The rest of init is just a
      ;; program that sets hooks to (compiled) functions and turns (compiled)
      ;; modes on.  Compiling that program would be all footgun and no speedup.
      (me/load-newest-ensure-compiled (concat .emacs.d "lib"))
      ;; Compile these too as QC
      (dolist (file (directory-files (concat .emacs.d "semi-packaged/")
                                     t "^\\w.*el$"))
        (me/load-newest-ensure-compiled file))
      (load (concat .emacs.d "private"))
      (load (concat .emacs.d "late-init")))
  ;; Undo overrides from early-init.el
  (setq file-name-handler-alist me/original-fnha)
  (setq load-prefer-newer t)
  (setq-default coding-system-for-read nil)
  (setq-default coding-system-for-write nil)
  (setq me/elapsed-gc-after-init (me/measure-time (garbage-collect)))
  (setq me/time-at-init-done (current-time))
  (add-hook 'elpaca-after-init-hook
            (lambda ()
              (let ((post-init-gc (me/measure-time (garbage-collect))))
                (message "Init took %.2fs (GC %.2fs), post-init took %.2fs (GC %.2fs)"
                         (float-time (time-subtract after-init-time
                                                    before-init-time))
                         me/elapsed-gc-after-init
                         (float-time (time-since me/time-at-init-done))
                         post-init-gc))
              (setq gc-cons-threshold 800000))
            99))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
(put 'list-timers 'disabled nil)
