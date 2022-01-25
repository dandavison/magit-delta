;;; magit-delta.el --- Use Delta when displaying diffs in Magit -*- lexical-binding: t; -*-

;; Author: Dan Davison <dandavison7@gmail.com>
;; URL: https://github.com/dandavison/magit-delta
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (magit "20200426") (xterm-color "2.0"))

;; SPDX-License-Identifier: MIT

;;; Commentary:

;; This package integrates Delta (https://github.com/dandavison/delta) with
;; Magit (https://github.com/magit), so that diffs in Magit are displayed with
;; color highlighting provided by Delta.
;;
;; Use M-x magit-delta-mode to toggle between using Delta, and normal Magit
;; behavior.

;;; Code:
(require 'magit)
(require 'xterm-color)
(require 'dash)

(defgroup magit-delta nil
  "Magit delta customizations."
  :group 'magit-diff
  :group 'magit-modes)

(defcustom magit-delta-delta-executable "delta"
  "The delta executable on your system to be used by Magit."
  :type 'string
  :group 'magit-delta)

(defcustom magit-delta-default-light-theme "GitHub"
  "The default color theme when Emacs has a light background."
  :type 'string
  :group 'magit-delta)

(defcustom magit-delta-default-dark-theme "Monokai Extended"
  "The default color theme when Emacs has a dark background."
  :type 'string
  :group 'magit-delta)

(defcustom magit-delta-delta-args
  `("--max-line-distance" "0.6"
    "--true-color" ,(if xterm-color--support-truecolor "always" "never")
    "--color-only")
  "Delta command line arguments as a list of strings.

If the color theme is not specified using --theme, then it will
be chosen automatically according to whether the current Emacs
frame has a light or dark background. See `magit-delta-default-light-theme' and
`magit-delta-default-dark-theme'.

--color-only is required in order to use delta with magit; it
will be added if not present."
  :type '(repeat string)
  :group 'magit-delta)

(defcustom magit-delta-hide-plus-minus-markers t
  "Whether to hide the +/- markers at the beginning of diff lines."
  :type '(choice (const :tag "Hide" t)
                 (const :tag "Show" nil))
  :group 'magit-delta)

(defun magit-delta--make-delta-args ()
  "Make final list of delta command-line arguments."
  (let ((args magit-delta-delta-args))
    (unless (-intersection '("--syntax-theme" "--light" "--dark") args)
      (setq args (nconc
                  (list "--syntax-theme"
                        (if (eq (frame-parameter nil 'background-mode) 'dark)
                            magit-delta-default-dark-theme
                          magit-delta-default-light-theme))
                       args)))
    (unless (member "--color-only" args)
      (setq args (cons "--color-only" args)))
    args))

(defvar magit-delta--magit-diff-refine-hunk--orig-value nil)


;;;###autoload
(define-minor-mode magit-delta-mode
  "Use Delta when displaying diffs in Magit.

https://github.com/dandavison/delta"
  :lighter " Δ"
  (let ((magit-faces-to-override
         '(magit-diff-context-highlight
           magit-diff-added
           magit-diff-added-highlight
           magit-diff-removed
           magit-diff-removed-highlight)))
    (cond
     (magit-delta-mode
      (add-hook 'magit-diff-wash-diffs-hook #'magit-delta-call-delta-and-convert-ansi-escape-sequences)
      (setq magit-delta--magit-diff-refine-hunk--orig-value
            magit-diff-refine-hunk

            magit-diff-refine-hunk
            nil

            face-remapping-alist
            (nconc
             (--remove (member (car it) magit-faces-to-override)
                       face-remapping-alist)
             (--map (cons it 'default) magit-faces-to-override))))
     ('deactivate
      (remove-hook 'magit-diff-wash-diffs-hook #'magit-delta-call-delta-and-convert-ansi-escape-sequences)
      (setq magit-diff-refine-hunk
            magit-delta--magit-diff-refine-hunk--orig-value

            face-remapping-alist
            (--remove (member (car it) magit-faces-to-override)
                      face-remapping-alist))))))

(defun magit-delta-call-delta-and-convert-ansi-escape-sequences ()
  "Call delta on buffer contents and convert ANSI escape sequences to overlays.

The input buffer contents are expected to be raw git output."
  (apply #'call-process-region
         (point-min) (point-max)
         magit-delta-delta-executable t t nil (magit-delta--make-delta-args))
  (let ((buffer-read-only nil))
    (xterm-color-colorize-buffer 'use-overlays)
    (if magit-delta-hide-plus-minus-markers
        (magit-delta-hide-plus-minus-markers))))

(defun magit-delta-hide-plus-minus-markers ()
  "Apply text properties to hide the +/- markers at the beginning of lines."
  (save-excursion
    (goto-char (point-min))
    ;; Within hunks, hide - or + at the start of a line.
    (let ((in-hunk nil))
      (while (re-search-forward "^\\(diff\\|@@\\|+\\|-\\)" nil t)
        (cond
         ((string-equal (match-string 0) "diff")
          (setq in-hunk nil))
         ((string-equal (match-string 0) "@@")
          (setq in-hunk t))
         (in-hunk
          (add-text-properties (match-beginning 0) (match-end 0)
                               '(display " "))))))))

(provide 'magit-delta)

;;; magit-delta.el ends here
