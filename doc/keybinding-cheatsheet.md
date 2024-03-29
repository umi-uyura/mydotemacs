Keybinding cheatsheet
=====================

* [GNU Emacs Reference Cards](https://www.gnu.org/software/emacs/refcards/index.html) - [PDF](https://www.gnu.org/software/emacs/refcards/pdf/refcard.pdf)
* [Furoku_CheatSheet_Emacs_責](https://gihyo.jp/assets/files/magazine/SD/2015/201510/download/Furoku_CheatSheet_Emacs.pdf)

* キーバインドのイタリック表記はinit.el等で設定しているもの

### Editing

| 概要 | 関数名 | キーバインド |
|------|--------|--------------|
| バッファを読み込み直す | revert-buffer | - |
| ファイルをroot権限で開く | `/sudo::<path>` | 例) `/sudo::/etc/hosts` |

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

### multiple-cursors

See: https://github.com/magnars/multiple-cursors.el

| 概要 | 関数名 | キーバインド |
|------|--------|--------------|
| （文字列を選択して）同じ文字列を一括して選択状態にする | mc/mark-all-like-this-dwin | *C-c C-r* |
| （リージョンを有効にして）入力した文字列を一括して選択状態にする | mc/mark-all-in-region | *C-c C-r* |
| （リージョンを有効にして）各行の先頭にカーソルを追加 | mc/edit-lines | *C-u C-c C-r* |

### web-mode

| 概要 | 関数名 | キーバインド |
|------|--------|--------------|
| 次のタグへ移動する |  | *C-c C-t n* |
| 前のタグへ移動する |  | *C-c C-t p* |
| マッチする開始／終了タグへ移動する |  | *C-c C-t m* |
| タグを選択する | | *C-c C-t s* |
| タグの内容を選択する | | *C-c C-e a* |
| タグを閉じる | | *C-c C-e /* |
| タグを挿入する | | *C-c C-e i* |
| タグを囲むタグを挿入する | | *C-c C-e w* |
| タグを削除する | | *C-c C-e v*
| タグを（内容ごと）削除する | | *C-c C-e k* |
| タグをリネームする | | *C-c C-e r* |
