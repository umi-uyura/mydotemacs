;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ load-path                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(let ((default-directory (expand-file-name "lisp" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ global settings                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(blink-cursor-mode 0)           ; カーソルは点滅させない


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ key binding                                                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(global-set-key (kbd "C-h") 'delete-backward-char)           ; Backspace
(global-set-key (kbd "C-x ?") 'help-command)                 ; Help起動
(global-set-key (kbd "C-c t") 'toggle-truncate-lines)        ; 行折り返しのON/OFF
(global-set-key (kbd "C-x F") 'recentf-open-files)           ; 最近使用したファイルから開く (recentf-mode)
(global-set-key (kbd "C-c J") 'open-junk-file)
(global-set-key (kbd "<henkan>") 'toggle-input-method)       ; 変換キー
(global-set-key (kbd "<muhenkan>") 'toggle-input-method)     ; 無変換キー
;; (global-set-key (kbd "<zenkaku-hankaku>") 'toggle-input-method) ; 半角/全角キー

; "yes or no"を"y or n"にする
(fset 'yes-or-no-p 'y-or-n-p)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ mozc                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

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
;;; @ customize                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
