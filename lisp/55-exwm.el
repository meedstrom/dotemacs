;; -*- lexical-binding: t; -*-

(general-unbind exwm-mode-map "C-c")
(general-def exwm-mode-map "M-s q" #'exwm-input-send-next-key)

(setq exwm-replace t)
(setq exwm-input-simulation-keys '(([?\s-a] . [home])
                                   ([?\s-b] . [left])
                                   ([?\s-d] . [delete])
                                   ([?\s-e] . [end])
                                   ([?\s-f] . [right])
                                   ([?\s-g] . [escape])
                                   ([?\s-k] . [S-end delete])
                                   ([?\s-m] . [return])
                                   ([?\s-n] . [down])
                                   ([?\s-p] . [up])
                                   ([?\s-s] . [C-f])
                                   ([?\s-v] . [next])
                                   ([?\s-w] . [C-x])
                                   ([?\s-y] . [C-v])
                                   ([?\s-/] . [C-z])
                                   ([?\M-d] . [C-S-right delete])
                                   ;; ([f8]    . [menu])
                                   ))
(setq exwm-input-prefix-keys '(?\s-x ?\M-x ?\M-1 ?\M-2 menu f1 f2 f3 f4 f5 f7 f10 f11 f12))
(setq exwm-input-global-keys `((,(kbd "C-M-<delete>") . exwm-reset)
                               (,(kbd "s-<delete>") . exwm-reset)
                               (,(kbd "M-<f4>") . kill-current-buffer)
                               (,(kbd "<XF86MonBrightnessDown>") . my-backlight-dec)
                               (,(kbd "<XF86MonBrightnessUp>") . my-backlight-inc)))

(add-hook 'exwm-update-class-hook #'my-exwm-rename-buffer)
(add-hook 'exwm-update-title-hook #'my-exwm-rename-buffer)

;; exwm-enable just adds exwm-init on various hooks
;; (when (fboundp #'exwm-enable)
;;  (exwm-enable))

;; not good
;; (setc exwm-workspace-minibuffer-position 'bottom)
;; (add-hook 'exwm-init-hook #'exwm-workspace-attach-minibuffer)