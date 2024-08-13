;; -*- lexical-binding: t; -*-

;; Init Elpaca the package manager (snippet from their README)
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

;; Require packages needed during init
(use-package no-littering)
(use-package compat)
(elpaca-wait)
(use-package defrepeater)
(use-package crux) 
(use-package dash) 
(elpaca-wait)


;;; Load my config

(let ((.emacs.d user-emacs-directory))
  (load (concat .emacs.d "lib.el"))
  (dolist (file (directory-files (concat .emacs.d "semi-packaged/")
                                 t "^\\w.*el$"))
    (load file))
  (load (concat .emacs.d "private.el"))
  (load (concat .emacs.d "late-init.el"))
  (load (setq custom-file (concat .emacs.d "custom.el"))))


;;; After init

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (run-with-idle-timer 2 nil #'me/progressively-preload)))

(defun me/progressively-preload ()
  "Preload lazy packages in the background."
  (while-no-input
    (while-let ((next-lib (pop me/progressive-preload-queue)))
      (let ((inhibit-message t))
        (message "Preloading... (%S)" next-lib)
        (ignore-errors
          (require next-lib nil t)))))
  (if me/progressive-preload-queue
      (run-with-idle-timer 2 nil #'me/progressively-preload)))

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

;; Undo overrides from early-init.el
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (message "Init took %.2fs, post-init took %.2fs "
                     (float-time (time-subtract after-init-time
                                                before-init-time))
                     (float-time (time-since after-init-time)))
            (setq file-name-handler-alist me/original-fnha)
            (setq gc-cons-threshold 800000))
          99)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
