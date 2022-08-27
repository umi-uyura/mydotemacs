Keybinding cheatsheet
=====================

* [GNU Emacs Reference Cards](https://www.gnu.org/software/emacs/refcards/index.html) - [PDF](https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf)
* [Furoku_CheatSheet_Emacs_責](https://gihyo.jp/assets/files/magazine/SD/2015/201510/download/Furoku_CheatSheet_Emacs.pdf)


### Rectangles（矩形領域）

| 概要 | 関数名 | キーバインド |
|------|--------|--------------|
| 矩形リージョン先頭に文字列を挿入 | string-rectangle | C-x r t |
| 矩形リージョンを削除 | delete-rectangle | C-x r d |
| 矩形リージョンをkill | kill-rectangle | C-x r k |
| 最後にkillされた矩形リージョンをyank | yank-rectangle | C-x r y |
| 矩形リージョンをkillringに保存 | copy-rectangle-as-kilil | C-x r M-w |
| 矩形リージョンにスペースを挿入 | open-rectangle | C-x r o |
| 矩形リージョンをスペースに置換 | clear-rectangle | C-x r c |
| 矩形リージョンの先頭のスペースを削除 | delete-whitespace-rectangle | - |
| 矩形リージョンの先頭に行番号を挿入 | rectangle-number-lines | C-x r N |
