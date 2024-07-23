;; -*- lexical-binding: t; -*-

;; Disarm package.el
(setq package-enable-at-startup nil)

;; Temp override for faster init (2.5s init became 1.5s)
(defvar me/original-fnha file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)

;; Disable GUI stuff in early-init to prevent visual flicker associated with
;; loading-then-disabling-anyway
(tool-bar-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)
(scroll-bar-mode 0)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
