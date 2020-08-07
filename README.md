This Emacs package provides a minor mode which configures [Magit](https://github.com/magit/magit) to use [delta](https://github.com/dandavison/delta) when displaying diffs.

1. Install [delta](https://github.com/dandavison/delta) (≥ 0.0.18).
2. Install magit-delta from [MELPA](https://melpa.org/#/getting-started).
3. Use `M-x magit-delta-mode` to toggle between using delta, and normal magit behavior.

<br>
<br>
<table><tr><td>
  <img width=500px src="https://user-images.githubusercontent.com/52205/80056404-23745500-84f2-11ea-9ecd-832376faf2f1.png" alt="image" />
</td></tr></table>

In the event that `magit-delta-mode` is your preferred mode, one can enable this in `.emacs` like so:

```emacs
(magit-delta-mode)
```

However, this has a startup cost when executing a new `emacs` session. If `emacs` has a keybinding to start `magit-diff-buffer-file` on a buffer, this function may help if added to that keybinding:

```emacs
(setq magit-delta-loaded nil)
(defun magit-delta-diff-buffer-file ()
  (interactive)
  (unless magit-delta-loaded
      (progn
        (magit-delta-mode)
        (setq magit-delta-loaded t)))
  (magit-diff-buffer-file))
```

### Credit
- [magit](https://github.com/magit/magit)
- [xterm-color](https://github.com/atomontage/xterm-color)
