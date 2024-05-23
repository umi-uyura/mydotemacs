;;; init.el --- Emacs init file

;;; Commentary:
;;;

;;; Code:

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ constant                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; Linux
(defconst run-linux (equal system-type 'gnu/linux))

;; Windows
(defconst run-windows (or (equal system-type 'windows-nt)
                          (equal system-type 'ms-dos)
                          (equal system-type 'cygwin)))

;; WSL
(defconst run-wsl (and (eq system-type 'gnu/linux)
                       (/= (length (getenv "WSL_DISTRO_NAME")) 0)))

;; Mac OS X/GNU-Darwin
(defconst run-darwin (equal system-type 'darwin))

;; CLI
(defconst run-cli (not window-system))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ load-path                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(defvar lisp-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path lisp-path)
(let ((default-directory lisp-path))
  (normal-top-level-add-subdirs-to-load-path))

(defvar site-lisp-path (expand-file-name "site-lisp" user-emacs-directory))
(when (file-directory-p site-lisp-path)
  (add-to-list 'load-path site-lisp-path)
  (let ((default-directory site-lisp-path))
    (normal-top-level-add-subdirs-to-load-path)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ site-init                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(defvar site-init-file (expand-file-name "site-init.el" site-lisp-path))
(when (file-exists-p site-init-file)
  (load site-init-file))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ global settings                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; Increase the amount of data which Emacs reads from the process
;; - for lsp-mode
;;   https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Enable find file at point
(ffap-bindings)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ key binding                                                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(global-set-key (kbd "C-h") 'delete-backward-char)           ; Backspace
(global-set-key (kbd "C-x ?") 'help-command)                 ; Help
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)       ; ibuffer
(global-set-key (kbd "C-x B") 'list-buffers)
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)        ; 行折り返しのON/OFF
(global-set-key (kbd "C-x i") 'company-complete)             ; 補完呼び出し (company-mode)
(global-set-key (kbd "C-x F") 'recentf-open-files)           ; 最近使用したファイルから開く (recentf-mode)
(global-set-key (kbd "C-x f") 'find-file-in-project)         ; 現在のプロジェクト内から開く (find-file-in-project)
(global-set-key (kbd "C-c J") 'open-junk-file)               ; メモファイル作成 (open-junk-file)
(global-set-key (kbd "<henkan>") 'toggle-input-method)       ; 変換キー
(global-set-key (kbd "<muhenkan>") 'toggle-input-method)     ; 無変換キー
;; (global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method) ; 半角/全角キー
(global-set-key (kbd "C-;") #'(lambda() (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C--") #'(lambda() (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "M-0") #'(lambda() (interactive) (text-scale-set 0)))

; "yes or no"を"y or n"にする
(fset 'yes-or-no-p 'y-or-n-p)

; for macOS
(when run-darwin
  (setq mac-option-modifier 'meta)              ; option => meta(command)
  (setq mac-command-modifier 'super)            ; command => option
  (define-key global-map (kbd "M-¥") [?\\])     ; backslash
  )


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ window system configuration                                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(when window-system
  ; Windows (WSL)
  (when run-wsl
    (setq default-frame-alist
          (append '((top                  . 31)
                    ) default-frame-alist))
    (set-face-attribute 'default nil
                        :family "UDEV Gothic 35"
                        :foundry "twr "
                        :slant 'normal
                        :weight 'normal
                        :height 104
                        :width 'normal)
    )
  ; macOS (MacBook Air 2013 Mid)
  (when run-darwin
    (setq default-frame-alist
          (append '((width                . 86)
                    (height               . 49)
                    (top                  . 0)
                    (left                 . 720)
                    ) default-frame-alist))
    (set-face-attribute 'default nil
                        :family "UDEV Gothic 35"
                        :foundry "nil"
                        :slant 'normal
                        :weight 'normal
                        :height 140
                        :width 'normal)
    )
  (setq initial-frame-alist default-frame-alist))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ mozc                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(when run-linux
  (require 'mozc-im)
  (require 'mozc-popup)
  (require 'mozc-cursor-color)
  (require 'wdired)

  (setq default-input-method "japanese-mozc-im")
  (setq mozc-candidate-style 'popup)
  (setq mozc-cursor-color-alist '((direct        . "#AA5542")
                                  (read-only     . "#CC5542")
                                  (hiragana      . "#6aaf50")
                                  (full-katakana . "#cc8512")
                                  (half-ascii    . "#9b55c3")
                                  (full-ascii    . "#DC8CC3")
                                  (half-katakana . "#fb8512")))
  (setq mozc-helper-program-name "mozc_emacs_helper.sh")

  (defun enable-input-method (&optional arg interactive)
    (interactive "P\np")
    (if (not current-input-method)
        (toggle-input-method arg interactive)))

  (defun disable-input-method (&optional arg interactive)
    (interactive "P\np")
    (if current-input-method
        (toggle-input-method arg interactive)))

  (defun isearch-enable-input-method ()
    (interactive)
    (if (not current-input-method)
        (isearch-toggle-input-method)
      (cl-letf (((symbol-function 'toggle-input-method)
                 (symbol-function 'ignore)))
        (isearch-toggle-input-method))))

  (defun isearch-disable-input-method ()
    (interactive)
    (if current-input-method
        (isearch-toggle-input-method)
      (cl-letf (((symbol-function 'toggle-input-method)
                 (symbol-function 'ignore)))
        (isearch-toggle-input-method))))

  ;; IME をトグルするキー設定
  (global-set-key (kbd "C-o") 'toggle-input-method)
  (define-key isearch-mode-map (kbd "C-o") 'isearch-toggle-input-method)
  (define-key wdired-mode-map (kbd "C-o") 'toggle-input-method)

  ;; IME を無効にするキー設定
  (global-set-key (kbd "C-<f1>") 'disable-input-method)
  (define-key isearch-mode-map (kbd "C-<f1>") 'isearch-disable-input-method)
  (define-key wdired-mode-map (kbd "C-<f1>") 'disable-input-method)

  ;; (global-set-key (kbd "C-j") 'disable-input-method)
  ;; (define-key isearch-mode-map (kbd "C-j") 'isearch-disable-input-method)
  ;; (define-key wdired-mode-map (kbd "C-j") 'disable-input-method)

  ;; IME を有効にするキー設定
  (global-set-key (kbd "C-<f2>") 'enable-input-method)
  (define-key isearch-mode-map (kbd "C-<f2>") 'isearch-enable-input-method)
  (define-key wdired-mode-map (kbd "C-<f2>") 'enable-input-method)

  ;; (global-set-key (kbd "C-o") 'enable-input-method)
  ;; (define-key isearch-mode-map (kbd "C-o") 'isearch-enable-input-method)
  ;; (define-key wdired-mode-map (kbd "C-o") 'enable-input-method)

  ;; mozc-cursor-color を利用するための対策
  (defvar-local mozc-im-mode nil)
  (add-hook 'mozc-im-activate-hook (lambda () (setq mozc-im-mode t)))
  (add-hook 'mozc-im-deactivate-hook (lambda () (setq mozc-im-mode nil)))
  (advice-add 'mozc-cursor-color-update
              :around (lambda (orig-fun &rest args)
                        (let ((mozc-mode mozc-im-mode))
                          (apply orig-fun args))))

  ;; isearch を利用する前後で IME の状態を維持するための対策
  (add-hook 'isearch-mode-hook (lambda () (setq im-state mozc-im-mode)))
  (add-hook 'isearch-mode-end-hook
            (lambda ()
              (unless (eq im-state mozc-im-mode)
                (if im-state
                    (activate-input-method default-input-method)
                  (deactivate-input-method)))))

  ;; wdired 終了時に IME を OFF にする
  (advice-add 'wdired-finish-edit
              :after (lambda (&rest args)
                       (deactivate-input-method)))

  ;; Windows の mozc では、セッション接続直後 directモード になるので
  ;; hiraganaモード にする
  (advice-add 'mozc-session-execute-command
              :after (lambda (&rest args)
                       (when (eq (nth 0 args) 'CreateSession)
                         ;; (mozc-session-sendkey '(hiragana)))))
                         (mozc-session-sendkey '(Hankaku/Zenkaku)))))
)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ migemo                                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(unless (boundp 'migemo-dictionary)
      (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict"))
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ isearch                                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(define-key isearch-mode-map (kbd "C-h") 'isearch-del-char)     ; isearch中に検索文字を削除


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ visual-regexp / visual-regexp-steroids                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(global-set-key (kbd "M-%") 'vr/replace)
(global-set-key (kbd "C-M-%") 'vr/query-replace)
(global-set-key (kbd "C-M-s") 'vr/isearch-forward)
(global-set-key (kbd "C-M-r") 'vr/isearch-backward)
(global-set-key (kbd "C-c M-RET") 'vr/mc-mark)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ yasnippet                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(defun my-yas-minor-mode-hook ()
  (define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)
  (define-key yas-minor-mode-map (kbd "C-c C-s i") 'yas-insert-snippet)
  )
(add-hook 'yas-minor-mode-hook 'my-yas-minor-mode-hook)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ magit                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(global-set-key (kbd "C-c m s") 'magit-status)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ tempbuf                                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'tempbuf)
(add-hook 'help-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'completion-list-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'dired-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'bookmark-bmenu-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'magit-mode-hook 'turn-on-tempbuf-mode)
(add-hook 'calendar-mode-hook 'turn-on-tempbuf-mode)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ multi cursor                                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 文字列を選択しているとき
;; →同じ文字列を選択状態にする(mc/mark-all-like-this-dwin)
;; リージョンを有効にしているとき
;; →文字列を入力し、入力した文字列を選択状態にする(mc/mark-all-in-region)
;; リージョンを有効にしている + C-u
;; →各行にカーソルを追加(mc/edit-lines)
(global-set-key (kbd "C-c C-r") 'mc/mark-all-dwim)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ japanese-holidays                                             ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(with-eval-after-load "calendar"
  (require 'japanese-holidays)
  (setq calendar-holidays ; 他の国の祝日も表示させたい場合は適当に調整
        (append japanese-holidays holiday-local-holidays holiday-other-holidays))
  (setq calendar-mark-holidays-flag t)	; 祝日をカレンダーに表示
  ;; 土曜日・日曜日を祝日として表示する場合、以下の設定を追加します。
  ;; デフォルトで設定済み
  (setq japanese-holiday-weekend '(0 6)	   ; 土日を祝日として表示
        japanese-holiday-weekend-marker	   ; 土曜日を水色で表示
        '(holiday nil nil nil nil nil japanese-holiday-saturday))
  (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)
  (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ go-translate                                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'go-translate)

(setq gt-langs '(en ja))
(setq my-gt-engines
      (list (gt-deepl-engine :key (getenv "DEEPL_AUTH_KEY") :pro nil)
            (gt-google-engine)
            (gt-bing-engine)
            (gt-chatgpt-engine)))
(setq gt-default-translator
      (gt-translator :engines my-gt-engines
                     :render (gt-buffer-render)))

(global-set-key (kbd "C-c T") 'gt-do-translate)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ lsp-mode                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(setq lsp-use-plists t)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ web-mode                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ go-mode                                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(add-hook 'go-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ rainbow-csv                                                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(load "rainbow-csv.el")


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ customize                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ auto-package-update                                           ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(auto-package-update-maybe)


(provide 'init)
;;; init.el ends here
