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

(defvar magit-delta-light-theme "GitHub"
  "The color theme to use when Emacs has a light background.")

(defvar magit-delta-dark-theme "Monokai Extended"
  "The color theme to use when Emacs has a dark background.")

(setq magit-delta--magit-diff-refine-hunk--orig-value magit-diff-refine-hunk
      magit-diff-preprocess-git-output-function--orig-value magit-diff-preprocess-git-output-function
      magit-diff-preprocess-git-output-always--orig-value magit-diff-preprocess-git-output-always)

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
      (setq magit-delta--magit-diff-refine-hunk--orig-value magit-diff-refine-hunk
            magit-diff-preprocess-git-output-always t
            magit-diff-preprocess-git-output-function #'magit-delta-call-delta-and-convert-ansi-escape-sequences
            face-remapping-alist (nconc
                                  (--remove (member (car it) magit-faces-to-override)
                                            face-remapping-alist)
                                  (--map (cons it 'default) magit-faces-to-override))
            magit-diff-refine-hunk nil))
     ('deactivate
      (setq magit-diff-preprocess-git-output-always magit-diff-preprocess-git-output-always--orig-value
            magit-diff-preprocess-git-output-function magit-diff-preprocess-git-output-function--orig-value
            face-remapping-alist (--remove (member (car it) magit-faces-to-override)
                                           face-remapping-alist)
            magit-diff-refine-hunk magit-delta--magit-diff-refine-hunk--orig-value)))))

(defun magit-delta-delta-args ()
  (list
   "--theme" (if (eq (frame-parameter nil 'background-mode) 'light)
                 magit-delta-light-theme magit-delta-dark-theme)
   "--max-line-distance" "0.6"
   "--24-bit-color" (if xterm-color--support-truecolor "always" "never")
   "--color-only"))

(defun magit-delta-call-delta-and-convert-ansi-escape-sequences ()
  (apply #'call-process-region
         (point-min) (point-max) magit-delta-delta-executable t t nil (magit-delta-delta-args))
  (let ((buffer-read-only nil))
    (xterm-color-colorize-buffer 'use-overlays)))

(provide 'magit-delta)

;;; magit-delta.el ends here
