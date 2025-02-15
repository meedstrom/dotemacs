;; -*- lexical-binding: t; -*-

;; Disarm package.el in preparation for Elpaca
(setq package-enable-at-startup nil)

;; Temp override for faster init (cut 2.5s to 1.5s)
(add-hook 'elpaca-after-init-hook
          `(lambda ()
             "Undo temporary overrides, and report init time."
             (setq file-name-handler-alist ',file-name-handler-alist)
             (setq gc-cons-threshold ,gc-cons-threshold)
             (garbage-collect)
             (message "Init took %.2fs, post-init took %.2fs "
                      (float-time (time-subtract after-init-time
                                                 before-init-time))
                      (float-time (time-since after-init-time))))
          95)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)

;; Temporarily nil modeline for a visually quiet init
(add-hook 'elpaca-after-init-hook
          `(lambda ()
             (setq-default mode-line-format
                           ;; No "git-master"
                           ',(remove '(vc-mode vc-mode) mode-line-format)))
          80)
(setq-default mode-line-format nil)

;; Set GUI parameters early for a visually quiet init
(setq initial-frame-alist '((fullscreen . maximized)
                            (background-color . "#444")
                            (foreground-color . "#fff")))
(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (tool-bar-lines . nil)
                            (menu-bar-lines . nil)))

;; https://github.com/emacscollective/no-littering#native-compilation-cache
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (file-name-as-directory
    (file-name-concat user-emacs-directory "var" "eln-cache"))))

;;; Stuff to try from https://github.com/jamescherti/minimal-emacs.d/

;; ;; Reduce rendering/line scan work by not rendering cursors or regions in
;; ;; non-focused windows.
;; (setq-default cursor-in-non-selected-windows nil)
;; (setq highlight-nonselected-windows nil)

;; ;; Don't ping things that look like domain names.
;; (setq ffap-machine-p-known 'reject)

;; ;; Disable warnings from the legacy advice API. They aren't useful.
;; (setq ad-redefinition-action 'accept)

;; ;; By default, Emacs "updates" its ui more often than it needs to
;; (setq idle-update-delay 1.0)

;; ;; Font compacting can be very resource-intensive, especially when rendering
;; ;; icon fonts on Windows. This will increase memory usage.
;; (setq inhibit-compacting-font-caches t)

;; (setq initial-major-mode 'fundamental-mode)
;; (setq initial-scratch-message nil)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
