;;  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Used to try implementing a multiline prompt -- it's pretty unreliable.  The
;; way shell-mode does it, it has a `shell-prompt-pattern', but won't use it by
;; default for anything other than C-c C-p motion.  Instead, it relies on
;; comint using the "field" text property to mark a prompt.  Docstring of
;; `shell-prompt-pattern' says: "The pattern should probably not match more
;; than one line.  If it does, Shell mode may become confused trying to
;; distinguish prompt from input on lines which don't start with a prompt."
;; Eshell works in a similar way AFAIK.
;;
;; Anyhoo... A single-line prompt can be plenty informative.

;;; Config:

;; First, set up the hook `my-real-eshell-post-command-hook' as a reliable
;; substitute for eshell-post-command-hook.
(add-hook 'eshell-pre-command-hook #'my-esh-time-cmd-1)
(add-hook 'eshell-post-command-hook #'my-esh-time-cmd-2)

;; Always time slow commands. No more re-running just to prepend "time ..."
(add-hook 'my-real-eshell-post-command-hook #'my-esh-print-elapsed-maybe)

;; Save all outputs in variables $a1, $a2, $a3 etc (global Emacs variables)
(add-hook 'my-real-eshell-post-command-hook #'my-esh-save-output-into-backref)

;; Timestamp for when the command was sent (not when the prompt opened)
(add-hook 'eshell-pre-command-hook #'my-esh-timestamp-update)

;;; Lib:

(defvar my-esh-buffer-counter 0)
(defvar-local my-esh-last-cmd-start nil)
(defvar-local my-esh-backref-counter 1)
(defvar-local my-esh-id nil)
;; (make-variable-buffer-local 'my-esh-last-cmd-start)
;; (make-variable-buffer-local 'my-esh-backref-counter)
;; (make-variable-buffer-local 'my-esh-id)

(defvar my-real-eshell-post-command-hook nil
  "Hook run after a non-blank command.
Functions here have access to the variable
`my-esh-last-cmd-start', a time represented as seconds since
1970, which can be compared to the current output of
`time-to-seconds', usually a somewhat higher number.")

(defun my-esh-re-propertize-prompt-at-point ()
  (interactive)
  (when (< (- (point) (line-beginning-position)) 2)
    (forward-char 2))
  (let ((beg (search-backward "〈 "))
        (end (search-forward " 〉 "))
        (inhibit-read-only t))
    ;; Use exactly the same properties as `eshell-emit-prompt'
    (remove-text-properties beg end '(read-only
                                      font-lock-face
                                      front-sticky
                                      rear-nonsticky))
    (add-text-properties beg end '(read-only t
                                             font-lock-face eshell-prompt
                                             front-sticky (font-lock-face read-only)
                                             rear-nonsticky (font-lock-face read-only)))))

(defun my-esh-timestamp-update ()
  (save-excursion
    (search-backward " 〉")
    (search-backward "／")
    (search-backward "／")
    (let ((inhibit-read-only t))
      (insert (format-time-string "ran at %H:%M"))
      (my-esh-re-propertize-prompt-at-point))))

;; TODO Save lisp return values as actual lisp, not a string.  See
;; unintended result at $c18:
;;
;; 〈 ran at 14:49／／result $c17 〉 echo {pwd} (+ 2 3) {cat /tmp/.X1-lock}
;; ("/home/me/.doom.d" 5 1594)
;; 〈 ran at 14:49／／result $c18 〉 -map 'eshell-stringify $c17
;; ("40" "34" "47" "104" "111" "109" "101" "47" "109" "101" "47" "46" "100" "111" "111" "109" "46" "100" "34" "32" "53" "32" "49" "53" "57" "52" "41")
(defun my-esh-save-output-into-backref ()
  "Save last output into a variable and echo its name in the prompt."
  (require 'eshell)
  (require 'em-prompt)
  (when my-esh-last-cmd-start
    (let ((output (buffer-substring (eshell-beginning-of-output)
                                    (1- (eshell-end-of-output))))
          (i my-esh-backref-counter))
      (unless (string-blank-p output)
        (cl-incf my-esh-backref-counter)
        (unless my-esh-id
          (setq-local my-esh-id (my-int-to-consonants my-esh-buffer-counter))
          (cl-incf my-esh-buffer-counter))
        ;; Save the backref as a global Elisp variable
        (set (intern (format "%s%d" my-esh-id i)) output)
        (eshell-previous-prompt 1)
        (if (search-backward "／" nil t)
            (progn
              (forward-char 1)
              (let ((inhibit-read-only t)
                    (query-replace-skip-read-only nil)
                    (inhibit-message t))
                (insert "result " (format "$%s%d" my-esh-id i))
                (my-esh-re-propertize-prompt-at-point))
              (goto-char (point-max)))
          (warn "Eshell: Failed to find backref placeholder"))))))

(defun my-esh-time-cmd-1 ()
  (setq my-esh-last-cmd-start (time-to-seconds)))

(defun my-esh-time-cmd-2 ()
  "Run `my-real-eshell-post-command-hook'.
Designed for `post-command-hook'."
  (run-hooks 'my-real-eshell-post-command-hook)
  ;; Possible bug(?) runs post-command-hook after no-ops, but pre-command-hook
  ;; didn't beforehand.  By setting this variable at pre-command-hook, and
  ;; nulling it after post-command-hook, we can inspect the variable to know
  ;; if a real command did run.
  (setq my-esh-last-cmd-start nil))

(defun my-esh-print-elapsed-maybe ()
  (when my-esh-last-cmd-start
    (let ((n (- (time-to-seconds) my-esh-last-cmd-start))
          (inhibit-read-only t))
      (when (> n 1)
        (save-mark-and-excursion
          (save-match-data
            (eshell-previous-prompt 1)
            ;; (eshell-next-prompt 1) ;; b/c other post-command hooks may have run
            (search-backward " 〉")
            (search-backward "／")
            (insert "took " (if (> n 50)
                                (format "%.0fs" n)
                              (if (> n 5)
                                  (format "%.1fs" n)
                                (format "%.2fs" n))))
            (my-esh-re-propertize-prompt-at-point)))))))


