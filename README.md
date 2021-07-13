This Emacs package provides a minor mode which configures [Magit](https://github.com/magit/magit) to use [delta](https://github.com/dandavison/delta) when displaying diffs.

1. Install [delta](https://github.com/dandavison/delta).
2. Install magit-delta from [MELPA](https://melpa.org/#/getting-started).
3. Use `M-x magit-delta-mode` to toggle between using delta, and normal magit behavior.
   To activate it automatically:
    ```emacs-lisp
    (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))
    ```
    
    Or with `use-package`
    ```emacs-lisp
    (use-package magit-delta
      :hook (magit-mode . magit-delta-mode))
    ```

<br>
<br>
<table><tr><td>
  <img width=500px src="https://user-images.githubusercontent.com/52205/80056404-23745500-84f2-11ea-9ecd-832376faf2f1.png" alt="image" />
</td></tr></table>

If you run emacs as a terminal application (`emacs -nw`) and colors are not being rendered correctly, then follow the instructions here: https://www.gnu.org/software/emacs/manual/html_node/efaq/Colors-on-a-TTY.html.


### Credit
- [magit](https://github.com/magit/magit)
- [xterm-color](https://github.com/atomontage/xterm-color)
