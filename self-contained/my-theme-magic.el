;; -*- lexical-binding: t; -*-

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

;; Features:

;; 1. On init, select a random theme from a user-configured list.  Actually,
;;    different lists for day and night.

;; 2. Better load-theme command, that first unloads previous themes, and runs
;;    a hook so you can trigger e.g. powerline-reset, prism-set-colors

;; 3. For Prism users, provide a way to desaturate a "fruit-salad" theme but
;;    not themes that already use toned-down colors

;; 4. Command to cycle themes randomly (use with `repeat' for maximum pleasure)

;; 5. Command to toggle a timer for regularly switching theme


;; Note to self, some rules of thumb to distinguish good themes from bad:

;; - if you have a high-DPI monitor, remember that italics are ok now
;; - for prism-mode, none of the faces should be grey
;; - for prism-mode, none of the faces should be much darker than the rest

;;; User config:

(defvar me/load-theme-hook '(me/prism-desaturate-maybe
                             ;; prism-set-colors
                             ;; me/italicize-comments
                             me/fix-pdftools-midnight-colors))

(defvar me/okay-themes-day
  '(doom-flatwhite
    ef-cyprus
    ef-arbutus
    ef-eagle
    ef-day))

(defvar me/okay-themes-night
  '(doom-one
    doom-tomorrow-night
    doom-dark+
    doom-manegarm
    doom-bluloco-dark
    doom-Iosvkem
    doom-nord
    ef-bio
    ef-rosa
    ef-dream
    ef-cherie
    ef-melissa-dark
    ef-elea-dark
    doom-sourcerer
    doom-zenburn
    doom-outrun-electric
    doom-badger
    doom-rouge
    doom-solarized-dark-high-contrast
    doom-dracula))

(defvar me/themes-to-desaturate
  '(monokai-pro
    doom-storage-tube
    doom-dracula))

;; Note to self, these don't need desaturate:
;;
;; doom-Iosvkem
;; doom-solarized-dark-high-contrast (surprisingly)
;; doom-rouge
;;

;;; Core

(defun me/load-theme (&optional theme)
  (interactive)
  (unless theme
    (setq theme (completing-read "Select theme: " (custom-available-themes))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme (intern-soft theme))
  (run-hooks 'me/load-theme-hook))

;; Bonus.  Switch themes WITHOUT closing minibuffer (like Helm alternate
;; actions)!  Here's how.  Say C-; is bound to `embark-act', then type
;; M-x me/load-theme.
;; Then when selecting a theme, don't type RET!  Type C-; C-m instead.
(setq embark-quit-after-action
      '((me/load-theme . nil)
        (t . t)))

;;; Optional hooks

(defun me/prism-desaturate-maybe ()
  "Modify `prism-desaturations' to suit current theme."
  (when (and (length= custom-enabled-themes 1)
             (boundp 'prism-desaturations))
    (let ((theme (car custom-enabled-themes)))
      (if (member theme me/themes-to-desaturate)
          ;; Tone-down the themes I've identified as fruit-salads
          (setq prism-desaturations '(40 50 60))
        (setq prism-desaturations '(0 20 60)))))
  ;; If Prism is loaded, apply the new settings
  (when (fboundp #'prism-set-colors)
    (prism-set-colors)))

(defun me/italicize-comments ()
  (set-face-italic 'font-lock-comment-face t))

;;; Scheduled theming

(defun me/toggle-scheduled-theming (&optional called-interactively)
  "Turn scheduled theming on and off.
This means load random theme every once in a while, and
effectively makes your setup respond to day and night because the
selections are taken from `me/okay-themes-day' in the day and
`me/okay-themes-night' otherwise."
  (interactive "p")
  (if (and called-interactively
           (member me/theme-timer timer-list))
      (progn
        (message "Will no longer randomly change theme")
        (cancel-timer me/theme-timer))
    (message "Starting random theme switching")
    (me/load-random-theme-and-reschedule)))

(defvar me/theme-timer (timer-create))

(defun me/load-random-theme-and-reschedule ()
  (me/load-random-theme)    
  (cancel-timer me/theme-timer)    
  (setq me/theme-timer
        (run-with-timer 3600 nil #'me/load-random-theme-and-reschedule)))

(defun me/load-random-theme ()
  (interactive)
  (let ((hour (decoded-time-hour (decode-time)))
        (shift (me/winter-shift (decoded-time-month (decode-time)))))
    (if (< (+ 7 shift) hour (- 17 shift))
        (me/load-theme (seq-random-elt me/okay-themes-day))
      (me/load-theme (seq-random-elt me/okay-themes-night)))))

;; Screw dealing with location providers
(defun me/winter-shift (x)
  "Rate how close month X is to midwinter.
X is the numeric month 1-12, and the result is a number between 0 and 3.
This can be used to adjust hours on account of daylight."
  (/ (if (> x 6)
         (- x 6)
       (- 6 x))
     2))

;;; Finally

;; Enable at init
(add-hook 'elpaca-after-init-hook #'me/load-random-theme-and-reschedule)
