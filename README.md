My dotemacs
===========

Preparation
-----------

### Windows & WSL

* VcXsrv
* Google Japanese Input
* [mozc_emacs_helper for Windows](https://github.com/smzht/mozc_emacs_helper) - mozc_emacs_helper.exe
* [iRi-E/mozc-el-extensions](https://github.com/iRi-E/mozc-el-extensions/) - mozc-cursor-color.el


Setup routine
-------------

* Clone this repository to `$HOME/.emacs.d`
* Start emacs
* `M-x load-file ~/.emacs.d/custom.el`
* `package-install-selected-packages`


### Windows & WSL

Setup mozc

* Download `mozc_emacs_helper.exe` and place it in `site-bin`.
  * Create `site-bin/mozc_emacs_helper.sh`
    ```
    #!/bin/sh

    cd
    mozc_emacs_helper.exe "$@" 2> /dev/null
    ```
  * Add execute permission `site-bin/mozc_emacs_helper.exe` and `site-bin/mozc_emacs_helper.sh`
* Download `mozc-cursor-color.el` and place it in `site-lisp`.

Create symlink for open-junk-mode

```
$ ln -s <path/to/junk> ~/.emacs.d/var/junk
```


### macOS

Create symlink for open-junk-mode

```
$ ln -s <path/to/junk> ~/.emacs.d/var/junk
```
