# Emacs configuration

## Installing

1. Close Emacs.
2. Delete `~/.emacs` or `~/.emacs.d` if they exist. This is
   where Emacs looks for configuration files, and deleting these files
   and directories will ensure that you start with a clean slate.
3. Clone this repository and link it to ~/.emacs.d.
4. Create the file `~/.lein/profiles.clj` and add this line to it:

```clojure
{:user {:plugins [[cider/cider-nrepl "0.8.1"]]}} 
```

Then open Emacs.

## Upgrading

Before upgrading, ensure that your `.emacs.d` directory is under
version control so that you can always revert to a known good state.

To upgrade:

1. Edit `.emacs.d/init.el`, adding these lines after line 12:

   ```elisp
   (add-to-list 'package-archives
                '("melpa-stable" . "http://stable.melpa.org/packages/") t)
   
   (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
   ```

2. Close Emacs.
3. Run `rm -Rf .emacs.d/elpa/cider-*`
4. Open Emacs. You'll probably see some errors and your theme won't
   load. That's ok.
5. In Emacs, run `M-x package-refresh contents`.
6. In Emacs, run `M-x package-install cider`.
7. Close and re-open Emacs.
8. Open `.lein/profiles.clj` and remove `[cider/cider-nrepl "0.8.1"]` from it.

