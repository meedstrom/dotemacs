;; -*- lexical-binding: t; -*-

(run-with-idle-timer 2 nil #'require 'dired)
(run-with-idle-timer 3 nil #'require 'org)
(run-with-idle-timer 4 nil #'require 'org-roam)
(run-with-idle-timer 5 nil #'org-agenda nil "n")
(run-with-idle-timer 6 nil #'require 'magit)
(run-with-idle-timer 7 nil #'require 'eshell)
(run-with-idle-timer 8 nil #'require 'esh-mode)
(run-with-idle-timer 9 nil (lambda ()
                             ;; Load all eshell modules by just creating an
                             ;; actual eshell in the background
                             (let ((inhibit-message t))
                               (save-window-excursion
                                 (eshell)
                                 (bury-buffer)))))

(run-with-idle-timer 2 nil #'beginend-global-mode)
(run-with-idle-timer 2 nil #'global-form-feed-mode)
(add-hook 'emacs-lisp-compilation-mode-hook #'form-feed-mode)

(require 'apheleia)
(apheleia-global-mode)
(setopt apheleia-log-debug-info t)

(require 'eager-state)
(eager-state-preempt-kill-emacs-hook-mode)
(advice-add #'kill-emacs :before (lambda (&rest _) (setq kill-emacs-hook nil)))

;; (use-package circadian
;;   :config
;;   ;; (el-patch-defun circadian-a-earlier-b-p (time-a time-b)
;;   ;;   "Compare to time strings TIME-A and TIME-B by hour and minutes."
;;   ;;   (or (and (= (cl-first time-a) (cl-first time-b))
;;   ;;            ((el-patch-swap <= <) (cl-second time-a) (cl-second time-b)))
;;   ;;       (< (cl-first time-a) (cl-first time-b))))

;;   ;; (add-hook 'circadian-after-load-theme-hook #'prism-set-colors)
;;   ;; Close enough
;;   (setq calendar-latitude 60)
;;   (setq calendar-longitude 10)
;;   (setopt circadian-themes '(("5:00" . doom-gruvbox-light)
;;                              ("11:00" . doom-flatwhite)
;;                              ("11:40" . doom-gruvbox)
;;                              ("15:00" . doom-sourcerer)
;;                              ("18:00" . ef-rosa)))
;;   (circadian-setup))

(use-package prism
  :defer
  :init
  (setopt prism-comments nil)
  ;; The default (40 50 60) is a nice fix for fruit-salad themes but if the
  ;; theme already uses muted colors, the effect is... not good
  (setopt prism-desaturations '(0 20 60))
  ;; (setopt prism-desaturations '(0))
  ;; Btw, another odd default for Lisp is that the parens enclosing a sexp
  ;; differ in color from the symbols inside -- people arent used to this
  ;; (unless they are already used to rainbow-delimiters)
  :config
  ;; Replace rainbow-delimiters (it's on a dozen hooks in Doom, so this method
  ;; is easiest).
  (fset 'rainbow-delimiters-mode #'prism-mode)
  (add-hook 'doom-load-theme-hook #'prism-set-colors)
  ;; (add-hook 'web-mode-hook #'prism-mode) ;; infinite loop in .svelte files
  (add-hook 'typescript-mode-hook #'prism-mode)
  (add-hook 'typescript-tsx-mode-hook #'prism-mode)
  (add-hook 'js-base-mode-hook #'prism-mode))
