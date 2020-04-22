;;; magit-delta.el --- Use delta when viewing diffs in Magit -*- lexical-binding: t; -*-

;; Author: Dan Davison <dandavison7@gmail.com>
;; URL: https://github.com/dandavison/magit-delta
;; Version: 0.1

;;; Commentary:

;; Use M-x magit-delta-mode to toggle between using delta, and normal Magit behavior.

;;; Code:

(require 'magit)
(require 'xterm-color)

(defvar magit-delta-delta-executable "delta"
  "The delta executable on your system to be used by Magit.")

(defvar magit-delta--git-wrapper-template "#!/bin/bash
set -o pipefail

GIT=\"%s\"
DELTA=\"%s\"
DELTA_ARGS=(
    %s
)

delta_should_handle () {
  for arg in \"$@\"; do
      [[ ($arg = diff || $arg = show) ]] && break
  done
}

if delta_should_handle \"$@\"; then
    \"$GIT\" \"$@\" | \"$DELTA\" \"${DELTA_ARGS[@]}\"
else
    exec \"$GIT\" \"$@\"
fi")

(defvar magit-delta-light-theme "GitHub"
  "The color theme to use when Emacs has a light background.")

(defvar magit-delta-dark-theme "Monokai Extended"
  "The color theme to use when Emacs has a dark background.")

(setq magit-delta-git-executable nil
      magit-delta-magit-git-executable--orig-value magit-git-executable
      magit-delta--magit-diff-refine-hunk--orig-value magit-diff-refine-hunk
      magit-diff-convert-ansi-escape-sequences-function--orig-value magit-diff-convert-ansi-escape-sequences-function
      magit-diff-convert-ansi-escape-sequences-always--orig-value magit-diff-convert-ansi-escape-sequences-always)

(define-minor-mode magit-delta-mode
  "Use delta to view diffs in magit.

https://github.com/dandavison/delta"
  :lighter " magit-delta"
  :global t
  (let ((magit-faces-to-override
         '(magit-diff-context-highlight
           magit-diff-added
           magit-diff-added-highlight
           magit-diff-removed
           magit-diff-removed-highlight)))
    (cond
     (magit-delta-mode
      (setq magit-delta-git-executable (magit-delta--install-git-wrapper)
            magit-git-executable magit-delta-git-executable
            magit-delta--magit-diff-refine-hunk--orig-value magit-diff-refine-hunk
            magit-diff-convert-ansi-escape-sequences-always t
            magit-diff-convert-ansi-escape-sequences-function #'magit-delta-convert-ansi-escape-sequences-using-xterm-color
            face-remapping-alist (nconc
                                  (--remove (member (car it) magit-faces-to-override)
                                            face-remapping-alist)
                                  (--map (cons it 'default) magit-faces-to-override))
            magit-diff-refine-hunk nil))
     ('deactivate
      (when magit-delta-git-executable
        (delete-file magit-delta-git-executable)
        (setq magit-delta-git-executable nil))
      (setq magit-git-executable magit-delta-magit-git-executable--orig-value
            magit-diff-convert-ansi-escape-sequences-always magit-diff-convert-ansi-escape-sequences-always--orig-value
            magit-diff-convert-ansi-escape-sequences-function magit-diff-convert-ansi-escape-sequences-function--orig-value
            face-remapping-alist (--remove (member (car it) magit-faces-to-override)
                                           face-remapping-alist)
            magit-diff-refine-hunk magit-delta--magit-diff-refine-hunk--orig-value)))))

(defun magit-delta-make-delta-options ()
  (list
   "--theme" (if (eq (frame-parameter nil 'background-mode) 'light)
                 magit-delta-light-theme magit-delta-dark-theme)
   "--max-line-distance" "0.6"
   "--24-bit-color" (if xterm-color--support-truecolor "always" "never")
   "--color-only"))

(defun magit-delta--install-git-wrapper ()
  (let ((git-wrapper-file
         (expand-file-name "magit-delta-git" temporary-file-directory)))
    (when (file-exists-p git-wrapper-file)
      (delete-file git-wrapper-file))
    (with-temp-file git-wrapper-file
      (insert (format magit-delta--git-wrapper-template
                      magit-git-executable magit-delta-delta-executable
                      (string-join
                       (mapcar #'shell-quote-argument (magit-delta-make-delta-options))
                       " "))))
    (set-file-modes git-wrapper-file 500)
    git-wrapper-file))

(defun magit-delta-convert-ansi-escape-sequences-using-xterm-color ()
  (let ((buffer-read-only nil))
    (xterm-color-colorize-buffer 'use-overlays)))

(provide 'magit-delta)
;;; magit-delta.el ends here
