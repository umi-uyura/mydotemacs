(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-file-name "~/.emacs.d/var/abbrev_defs")
 '(ad-redefinition-action 'accept)
 '(auto-package-update-delete-old-versions t)
 '(auto-package-update-last-update-day-filename "~/.emacs.d/var/.last-package-update-day")
 '(auto-package-update-prompt-before-update t)
 '(auto-package-update-show-preview t)
 '(auto-save-list-file-prefix "~/.emacs.d/var/auto-save-list/.saves-")
 '(backup-directory-alist '((".*" . "~/.emacs.d/var/backup")))
 '(blink-cursor-mode nil)
 '(bookmark-default-file "~/.emacs.d/var/bookmarks")
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(company-selection-wrap-around t)
 '(create-lockfiles nil)
 '(current-language-environment "Japanese")
 '(custom-enabled-themes '(ample-zen))
 '(custom-safe-themes
   '("ace9f12e0c00f983068910d9025eefeb5ea7a711e774ee8bb2af5f7376018ad2" "c7eb06356fd16a1f552cfc40d900fe7326ae17ae7578f0ef5ba1edd4fdd09e58" default))
 '(delete-selection-mode t)
 '(dired-dwim-target 'dired-dwim-target-next)
 '(dired-listing-switches "-alh")
 '(editorconfig-mode t)
 '(emojify-emojis-dir "~/.emacs.d/site-etc/emojis")
 '(ffip-use-rust-fd t)
 '(gc-cons-threshold 100000000)
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-emojify-mode t)
 '(global-flycheck-mode t)
 '(gptel-prompt-prefix-alist
   '((markdown-mode . "## ")
     (org-mode . "*** ")
     (text-mode . "## ")))
 '(grep-command "rg -niH --no-heading --sort=path --color=always -e ")
 '(grep-find-command
   '("rg -niH --no-heading --color=always --sort=path -e '' \"$(pwd)\"" . 53))
 '(gt-buffer-render-window-config
   '((display-buffer-reuse-window display-buffer-in-direction)
     (direction . below)))
 '(gts-buffer-window-config
   '((display-buffer-reuse-window display-buffer-in-side-window)
     (side . bottom)))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js2-strict-missing-semi-warning nil)
 '(lsp-keymap-prefix "C-c C-l")
 '(lsp-session-file "~/.emacs.d/var/.lsp-session-v1")
 '(magit-status-headers-hook
   '(magit-insert-error-header magit-insert-diff-filter-header magit-insert-head-branch-header magit-insert-upstream-branch-header magit-insert-push-branch-header magit-insert-tags-header magit-insert-user-header))
 '(make-backup-files t)
 '(markdown-asymmetric-header t)
 '(markdown-fontify-code-blocks-natively t)
 '(mc/list-file "~/.emacs.d/var/.mc-lists.el")
 '(mozc-leim-title "あ")
 '(open-junk-file-format "~/.emacs.d/var/junk/%Y%m%d-%H%M%S." t)
 '(package-archive-priorities '(("melpa" . 5) ("jcs-elpa" . 0)))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")
     ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")))
 '(package-selected-packages
   '(alert websocket claude-code 0xc 0x0 0blayout dirvish editorconfig jq-mode dotenv-mode find-file-in-project gptel powershell go-translate wgrep projectile web-mode web-beautify apache-mode nginx-mode typescript-mode vue-mode yaml-mode dockerfile-mode php-mode migemo visual-regexp visual-regexp-steroids direnv company lsp-ui lsp-mode go-mode js2-mode ahk-mode csv-mode auto-package-update multiple-cursors japanese-holidays rainbow-mode rainbow-delimiters emojify restclient yasnippet-snippets ample-zen-theme textile-mode flycheck magit open-junk-file yasnippet markdown-mode))
 '(projectile-known-projects-file "~/.emacs.d/var/projectile-bookmarks.eld")
 '(python-indent-offset 2)
 '(recentf-max-saved-items 100)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/var/recentf")
 '(request-storage-directory "~/.emacs.d/var/request")
 '(scroll-conservatively 1)
 '(scroll-preserve-screen-position t)
 '(select-active-regions nil)
 '(standard-indent 2)
 '(tool-bar-mode nil)
 '(tramp-persistency-file-name "~/.emacs.d/var/tramp")
 '(transient-history-file "~/.emacs.d/var/transient/history.el")
 '(transient-levels-file "~/.emacs.d/var/transient/levels.el")
 '(transient-values-file "~/.emacs.d/var/transient/values.el")
 '(url-configuration-directory "~/.emacs.d/var/url/")
 '(visible-bell t)
 '(yas-global-mode t)
 '(yas-snippet-dirs
   '("~/.emacs.d/etc/snippets" "~/.emacs.d/site-etc/yasnippet/snippets" yasnippet-snippets-dir)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:inherit nil)))))
