;; -*- lexical-binding: t; -*-

;; Disarm package.el
(setq package-enable-at-startup nil)

;; Temp override for faster init (2.5s init became 1.5s)
(defvar me/original-fnha file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)

;; Set GUI parameters in early-init to reduce the flicker of reconfiguration
(setq initial-frame-alist '((fullscreen . maximized)
                            (background-color . "#444")
                            (foreground-color . "#fff")))
(setq default-frame-alist '((vertical-scroll-bars . nil)
                            (tool-bar-lines . nil)
                            (menu-bar-lines . nil)
                            (alpha-background . 85)))

;; Temp nil modeline saves ~20ms (@1GHz), but more importantly the init
;; process looks nicer
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
