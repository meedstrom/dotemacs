;; -*- lexical-binding: t; -*-

(remove-hook 'dired-mode-hook #'dired-omit-mode) ;; undoom

(setc wdired-allow-to-change-permissions t)
(setc global-auto-revert-non-file-buffers t)

(after! dired-git-info
  (setq dgi-commit-message-format "%s") ;; undoom
  (add-hook 'dired-after-readin-hook #'dired-git-info-auto-enable))

(after! dired-x
  (add-to-list 'dired-omit-extensions ".eshell-command-history")
  (add-to-list 'dired-omit-extensions ".eshell-scrollback"))