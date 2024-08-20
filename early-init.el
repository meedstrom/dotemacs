;; -*- lexical-binding: t; -*-

;; Disarm package.el
(setq package-enable-at-startup nil)

;; Temp override for faster init (cut 2.5s to 1.5s)
(defvar me/original-fnha file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'elpaca-after-init-hook
          (lambda ()
            "Undo temporary overrides, and report init time."
            (message "Init took %.2fs, post-init took %.2fs "
                     (float-time (time-subtract after-init-time
                                                before-init-time))
                     (float-time (time-since after-init-time)))
            (setq file-name-handler-alist me/original-fnha)
            (setq gc-cons-threshold 800000))
          95)

;; Set GUI parameters early for a visually quiet init
(setq initial-frame-alist '((fullscreen . maximized)
                            (background-color . "#444")
                            (foreground-color . "#fff")))
(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (tool-bar-lines . nil)
                            (menu-bar-lines . nil)))

;; Temporarily nil modeline for a visually nice init
(add-hook 'elpaca-after-init-hook
          `(lambda ()
             (setq-default mode-line-format
                           ;; No "git-master"
                           ',(remove '(vc-mode vc-mode) mode-line-format)))
          80)
(setq-default mode-line-format nil)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
