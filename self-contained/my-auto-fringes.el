;; -*- lexical-binding: t; -*-

;; ;; TODO: actually let's set window fringes, so that the whitespace in the centre blends together
;; ;; i.e. instead of fringes F-2F-F, let's have just F-F-F.
;; (defun fringe-from-fill-column ()
;;   "Return a cons cell where the `car' is the amount of windows
;; you would be able to fit side-by-side in a frame as wide as
;; `frame-width' while being sized to `fill-column', and the `cdr'
;; is the max possible pixel value you can set for the fringe-width
;; in this configuration.

;; Assumes that you want the windows to fit 1 more character than
;; the fill column i.e. for a `fill-column' of 80, that you want a
;; window at least 81 chars wide.

;; Assumes there are no window dividers."
;;   (let* ((pixels-per-char (/ (frame-pixel-width) (frame-width)))
;;          (needed-window-char-width (1+ (default-value 'fill-column)))
;;          (needed-window-px-width (* needed-window-char-width pixels-per-char))
;;          (n-windows-possible (/ (frame-pixel-width) needed-window-px-width))
;;          (leftover-px (mod (frame-pixel-width) needed-window-px-width))
;;          (leftover-px-per-window (/ leftover-px n-windows-possible)))
;;     (cons n-windows-possible (/ leftover-px-per-window 2))))
;; ;; (set-fringe-mode (cdr (fringe-from-fill-column)))

(defun my-window-leftover-px-after-satisfying-fill-column ()
  (let* ((pixels-per-char (/ (window-pixel-width) (window-total-width)))
         (window-minimum-px (* (1+ fill-column) pixels-per-char)))
    (- (window-pixel-width) window-minimum-px)))

;; OK, I know what to do.  This function gonna run mutually-recursed-once for
;; every live window, right?  Then first it checks if the window is already
;; insufficient for fill-column, and minimizes the fringes if so.  Otherwise,
;; it looks up the parent to check if it is a horizontal group.  If the width
;; of that group matches the frame width, then it will try to do something that
;; results in F-F-F arrangement. otherwise it falls back to the dumb method
;; that would result in F-2F-F.  Don't really need code to cover that since it
;; seems extremely hard to end up in that situation and those ppl can live with
;; it.


(defvar fif--modes-to-reverse '(window-divider-mode))

(defun fif--pad-fringes-to-fill-column (&rest _)
  (interactive)
  (cond
   ;; In horizontal group
   ((or (window-combined-p (selected-window) t) ;; horiz
        (window-combined-p (selected-window))) ;; vert
    (let ((siblings (cddar (window--subtree (window-parent)))))
      (if (>= fill-column (window-total-width))
          ;; Fallback if the window is already insufficient for fill-column.
          (dolist (win siblings)
            (with-selected-window win
              (set-window-fringes win nil nil)
              (dolist (mode fif--modes-to-reverse)
                (funcall mode))))
        (dolist (win siblings)
          (with-selected-window win
            (let ((max-px (my-window-leftover-px-after-satisfying-fill-column)))
              (set-window-fringes (selected-window) (/ max-px 2) (/ max-px 2) t)
              (dolist (mode fif--modes-to-reverse)
                (funcall mode 0))))))))
   ;; In vertical group
   ((window-combined-p (selected-window))
    )
   ;; No group; must be root window
   ((frame-root-window-p (selected-window))
    (let ((max-px (my-window-leftover-px-after-satisfying-fill-column)))
      (set-window-fringes (selected-window) (/ max-px 2) (/ max-px 2))))
   ((window-minibuffer-p))
   (t
    (message "pad-fringes-to-fill-column: Not expected to be here"))))

(defun undo-fringes-a (fn &rest args)
  (walk-windows (lambda (win) (set-window-fringes win nil nil)))
  (apply fn args)
  (walk-windows (lambda (win) (fif--pad-fringes-to-fill-column))))

;; (advice-add 'split-window :around 'undo-fringes-a)
;; (advice-add 'delete-window :around 'undo-fringes-a)
;; (remove-hook 'window-size-change-functions #'fif--pad-fringes-to-fill-column)
;; (remove-hook 'window-buffer-change-functions #'fif--pad-fringes-to-fill-column)
