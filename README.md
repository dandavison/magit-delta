*-- PR  [magit#4091](https://github.com/magit/magit/pull/4091) pending: currently, this must be used with branch [customizable-ansi-color-conversion](https://github.com/dandavison/magit/tree/customizable-ansi-color-conversion) of Magit --*

This Emacs package provides a minor mode which configures [Magit](https://github.com/magit/magit) to use [delta](https://github.com/dandavison/delta) when displaying diffs.

First, install [delta](https://github.com/dandavison/delta), and use `M-x package-install` to install the Emacs package [xterm-color](https://github.com/atomontage/xterm-color). Then load `magit-delta.el` fom this repo and use:

```
M-x magit-delta-mode
```

<br>
<br>
<table><tr><td>
  <img width=600px src="https://user-images.githubusercontent.com/52205/79643434-ec5c1780-8170-11ea-8352-afe896ba51b0.png" alt="image" />
</td></tr></table>


### Credit
- [magit](https://github.com/magit/magit)
- [xterm-color](https://github.com/atomontage/xterm-color)
