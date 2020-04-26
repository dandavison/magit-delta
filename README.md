This Emacs package provides a minor mode which configures [Magit](https://github.com/magit/magit) to use [delta](https://github.com/dandavison/delta) when displaying diffs.

First, install [delta](https://github.com/dandavison/delta) (≥ 0.0.18).

Next, you need recent versions of both Magit and [xterm-color](https://github.com/atomontage/xterm-color):

```
M-x package-delete magit
M-x-package-install magit

M-x package-delete xterm-color
M-x-package-install xterm-color
```

Then load `magit-delta.el` from this repo and use:

```
M-x magit-delta-mode
```

<br>
<br>
<table><tr><td>
  <img width=500px src="https://user-images.githubusercontent.com/52205/80056404-23745500-84f2-11ea-9ecd-832376faf2f1.png" alt="image" />
</td></tr></table>


### Credit
- [magit](https://github.com/magit/magit)
- [xterm-color](https://github.com/atomontage/xterm-color)
