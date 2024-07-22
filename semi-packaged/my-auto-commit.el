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

;; Auto-commit on save in specific directories.
;;
;; Multiple auto-commits in a row, i.e. commits with the message "Auto-commit",
;; are amended if it's still the same day.  In typical conditions, that means
;; your git log ends up showing one commit per day.

;;; Code:

(defvar my-auto-pull-and-push-dirs
  '("/home/kept/roam/organice/")
  "In which directories do you want `vc-pull-and-push' on save?")

(defvar my-auto-commit-dirs
  '("/home/kept/roam/"
    "/home/me/.doom.d/")
  "In which directories do you want to auto-commit all changes?")

(defun my-auto-commit-maybe ()
  "Do a new commit if the last was on a different day.
Otherwise just amend today's commit.

Only operate if the project root directory is a member of
`my-auto-commit-dirs'.

If there are untracked files, do nothing and print a message,
because in this situation it's more possible that the user will
want to do the commits manually.

Suitable on `after-save-hook'."
  (require 'magit)
  (require 'magit-git)
  (require 'project)
  (require 'vc)
  (let ((project (project-current)))
    (when project
      (when (member (project-root project) my-auto-pull-and-push-dirs)
        ;; (shell-command "git pull")
        (vc-pull-and-push)
        ;; (setq proc (vc-pull))
        ;; Borrowed from `vc-pull-and-push'
        ;; (when (and (processp proc) (process-buffer proc))
        ;;   (with-current-buffer (process-buffer proc)
        ;;     (if (and (eq (process-status proc) 'exit)
        ;;              (zerop (process-exit-status proc)))
        ;;         (let ((vc--inhibit-async-window t))
        ;;           (vc-push arg))
        ;;       (vc-exec-after
        ;;        (lambda ()
        ;;          (let ((vc--inhibit-async-window t))
        ;;            (vc-push arg)))
        ;;        proc))))
        )
      (when (member (project-root project) my-auto-commit-dirs)
        (let ((last-commit-date (shell-command-to-string
                                 "git log -n 1 --pretty=format:%cs"))
              (last-commit-msg (shell-command-to-string
                                "git log -n 1 --pretty=format:%s")))
          (if (string-search "Fatal" last-commit-date)
              (message "Git failed, probably not a Git repo: %s" default-directory)
            ;; Special case for Org-Roam: auto-stage new notes, bc it happens often
            (and (equal "org" (file-name-extension (buffer-file-name)))
                 (string-search org-roam-directory default-directory)
                 ;; (magit-run-git "add" (buffer-file-name))
                 (my-exec "git" "add" (buffer-file-name)))

            (if (magit-untracked-files)
                (message "Won't auto-commit.  Stage untracked files or edit .gitignore")
              ;; TODO: check if you pushed this auto-commit to origin. then we need
              ;;       a new commit.
              ;;       somethign (magit-get-current-remote)
              (when (magit-unstaged-files)
                (if (and (equal last-commit-date (format-time-string "%F"))
                         (equal last-commit-msg "Auto-commit"))
                    ;; Same day, so amend today's autocommit
                    ;; (magit-commit-amend '("--all" "--reuse-message=HEAD"))
                    (my-exec "git" "commit" "--all" "--amend" "--reuse-message=HEAD")
                  ;; New day, new commit
                  ;; (magit-commit-create '("--all" "--message=Auto-commit"))
                  (my-exec "git" "commit" "--all" "--message=Auto-commit"))
                ;; Maybe also push the newly committed changes
                (when (member (project-root project) my-auto-pull-and-push-dirs)
                  (vc-pull-and-push))))))))))

(define-minor-mode my-auto-commit-mode
  "Automatically git-commit on save, in select directories."
  :global t
  (if my-auto-commit-mode
      (add-hook 'after-save-hook #'my-auto-commit-maybe)
    (remove-hook 'after-save-hook #'my-auto-commit-maybe)))

(my-auto-commit-mode)
