;; -*- lexical-binding: t; -*-

;; Prevent package.el because Elpaca will manage things
(setq package-enable-at-startup nil)

;; Temp override for much faster init (2.5s init became 1.5s)
(defvar me/original-fnha file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold most-positive-fixnum)

;; Make dangerous assumptions to save ~50ms on post-init (@1GHz)
;; (setq load-prefer-newer nil)
;; (setq-default coding-system-for-read 'utf-8-unix)
;; (setq-default coding-system-for-write 'utf-8-unix)

;; Temp nil modeline to save ~20ms (@1GHz)
(add-hook 'elpaca-after-init-hook
          `(lambda ()
             (setq-default mode-line-format
                           ;; No "git-master"
                           ',(remove '(vc-mode vc-mode) mode-line-format)))
          80)
(setq-default mode-line-format nil)

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
