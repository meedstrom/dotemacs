;; Configure small lazy packages -*- lexical-binding: t; -*-

(use-package objed
  :commands objed-ipipe)
(use-package hyperbole
  :commands hkey-either)
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode))
(use-package iscroll
  :hook ((text-mode elfeed-show-mode eww-mode shr-mode) . iscroll-mode))
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode))
(use-package org-node
  :hook (org-mode . org-node-enable))

(setq debug-on-error t)

(after! org-node
  ;; Make sure the extracted subtree inherits any CREATED property,
  ;; else creates one for today
  (advice-add 'org-node-extract-subtree :around
              (lambda (fn &rest args)
                (let ((parent-creation
                       (save-excursion
                         (while (not (or (bobp) (org-entry-get nil "CREATED")))
                           (org-up-heading-or-point-min))
                         (org-entry-get nil "CREATED"))))
                  (apply fn args)
                  (org-entry-put nil "CREATED"
                                 (or parent-creation (format-time-string "[%F]"))))))

  (require 'org-node-experimental)
  (advice-add 'org-node-cache--scan :override #'org-node-experimental--scan)
  ;; (advice-remove 'org-node-cache--scan #'org-node-experimental--scan)
  (advice-add 'org-node-cache-reset :before #'org-node-experimental--clear-extra-hash-tables)
  ;; (advice-remove 'org-node-cache-reset #'org-node-experimental--clear-extra-hash-tables)
  (org-node-cache-reset)
  (setq org-node-format-candidate-fn #'my-format-with-olp))

(defun my-format-with-olp (node title)
  "Prepend with the outline path."
  (declare (side-effect-free t))
  (concat
   (cl-loop
    for id in (org-node-experimental--olpath->ids
               (plist-get node :file-path)
               (plist-get node :pseudo-olpath))
    concat (concat (plist-get (gethash id org-nodes) :title) " -> "))
   title))

(after! org-roam-mode
  ;; (advice-remove 'org-roam-backlinks-get #'org-node--fabricate-roam-backlinks)
  (advice-add 'org-roam-backlinks-get :override #'org-node--fabricate-roam-backlinks))

(setopt helpful-max-buffers nil) ;; what's the point of killing buffers
(setopt iflipb-wrap-around t)
(setopt ranger-map-style 'emacs)
(setopt which-key-idle-delay 0.2)
(setopt rainbow-x-colors nil) ;; only colorize hex strings
(setopt calibredb-root-dir "~/Calibre Library/")
(setopt calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
(setopt calibredb-format-width 8)

(after! multiple-cursors
  ;; auto-save every 5 seconds destroys what youre doing!
  ;; should make a PR
  (add-to-list 'mc/unsupported-minor-modes 'auto-save-visited-mode)
  (add-to-list 'mc/unsupported-minor-modes 'nameless-mode))

(after! inline-anki
  (setopt inline-anki-send-tags '(not
                                  "noexport"
                                  "ARCHIVE"
                                  "stub"
                                  "fren"
                                  "privy"))
  (add-to-list 'inline-anki-fields '("Online mirror" . my-anki-field-for-webpage))
  (add-to-list 'inline-anki-ignore-file-regexps "/daily/")
  (after! org
    (add-to-list 'org-structure-template-alist '("f" . "flashcard"))))

(after! elfeed
  (setq elfeed-db-directory (concat doom-private-dir "elfeed/db/")
        elfeed-enclosure-default-dir (concat doom-private-dir "elfeed/enclosures/"))
  (make-directory elfeed-db-directory t)
  ;; filter into junk
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :entry-title (rx (or "MCMXXX"
                                                     "A&R"))
                                :add 'junk))
  (setopt elfeed-curl-max-connections 1)
  (setopt elfeed-search-filter "@2-months-ago -junk +unread")
  (use-package elfeed-org
    :config
    (setopt rmh-elfeed-org-files
            '("/home/kept/roam/contemporaries.org"))
    (elfeed-org)))

(after! ws-butler
  ;; Fix problem with guix.el
  (add-to-list 'ws-butler-global-exempt-modes #'minibuffer-inactive-mode)
  ;; Fix for org because the org-element parser throws hella warnings since 9.5
  (add-to-list 'ws-butler-global-exempt-modes #'org-mode))

(after! deianira
  (which-key-mode 0)
  (fset 'which-key-mode #'ignore)
  (after! hydra
    (define-key hydra-base-map (kbd "<f5>") #'hydra-repeat))
  (setq dei-ignore "C-")
  (setq dei-invisible-leafs
        (seq-difference dei-invisible-leafs '("<menu>" "SPC"))))

(use-package copilot
  :disabled
  :defer
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package nameless
  :hook (emacs-lisp-mode . nameless-mode)
  :init
  (setopt nameless-prefix "⁓")
  (setopt nameless-private-prefix t)
  (setopt nameless-affect-indentation-and-filling nil)
  :config
  (set-face-attribute 'nameless-face nil :inherit 'unspecified))
