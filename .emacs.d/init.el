;; ==============================
;;; load-path設定
;; ==============================
;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
             (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp")

;; ==============================
;;; パッケージ管理
;; ==============================
;; パッケージ管理のインストール候補を増やす
(require 'package)
(add-to-list 'package-archives'("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; ==============================
;;; anything設定
;; ==============================
(require 'anything)

;; ==============================
;;; powerline設定
;; ==============================
(require 'powerline)
(powerline-default-theme)

;; ==============================
;;; emmet-mode設定
;; ==============================
(require 'emmet-mode)
;; マークアップ言語全部で使う
(add-hook 'sgml-mode 'emmet-mode)
;; CSSにも使う
(add-hook 'css-mode-hook 'emmet-mode)
;; indentはスペース2つ
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

;; ==============================
;;; ag設定
;; ==============================
(require 'ag)
;;  検索キーワードをハイライト
(setq ag-highlight-search t)
;; 検索用バッファを使い回す(検索ごとに新バッファを作らない)
(setq ag-reuse-buffers t)

;; ==============================
;;; wgrep設定
;; ==============================
(add-hook 'ag-mode-hook '(lambda ()
                           (require 'wgrep-ag)
                           (setq wgrep-auto-save-buffer t);; 編集完了と同時に保存
                           (setq wgrep-enable-key "e");; "e"キーで編集モードにする
                           (wgrep-ag-setup)))

;; ==============================
;;; auto-complete設定
;; ==============================
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (require 'auto-complete)
                                   (auto-complete-mode t)
                                   ))
(require 'auto-complete-config)
(ac-config-default)

;; ==============================
;;; color-theme設定
;; ==============================
;; color-themeをHoberに変更
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-hober))

;; ==============================
;;; js2-mode
;; ==============================
(autoload 'js2-mode "js2-mode" nil t)

;; ==============================
;;; stylus-mode
;; ==============================
(autoload 'stylus-mode "stylus-mode" nil t)

;; ==============================
;;; php-mode
;; ==============================
(require 'php-mode)

;; ==============================
;;; オートセーブ設定
;; ==============================
;; バックアップファイルを作らない
(setq backup-inhibited t)

;; オートセーブファイルを作らない
(setq auto-save-dafault nil)

;; ==============================
;;; 起動時メッセージ設定
;; ==============================
(setq inhibit-startup-message t)

;; ==============================
;;; 同名ファイルのバッファ名の識別文字列設定
;; ==============================
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; ==============================
;;; コードフォールディング設定
;; ==============================
(add-hook 'js-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            ;; Activate the folding mode
                        (hs-minor-mode t)))

;; ==============================
;;; ユーザ情報
;; ==============================
;; ユーザー情報
(setq user-full-name "Go Ohtani")
(setq user-mail-address "otani_go@cyberagent.co.jp")

;; ==============================
;;; 言語設定
;; ==============================
;; 日本語IM用の設定
(setq default-input-method "MacOSX")

;; 日本語の設定（UTF-8）
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)

;; ==============================
;;; タイトルバー
;; ==============================
;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

;; ==============================
;;; 行番号
;; ==============================
;; 行番号を常に表示
(require 'linum)
(global-linum-mode t)
(setq linum-format "%4d ")

;; ==============================
;;; タブ設定
;; ==============================
;; tabの表示幅を設定
(setq-default tab-width 4)

;; インデントにtab文字を使用しない
(setq-default indent-tabs-mode nil)

;; c-modeのインデント調整
(add-hook 'c-mode-common-hook
          '(lambda ()
                          (setq c-basic-offset 4)))

;; ==============================
;;; スクロール設定
;; ==============================
(setq scroll-step 1)

;; ==============================
;;; underline設定
;; ==============================
(global-hl-line-mode)
(setq hl-line-face 'underline)

;; ==============================
;;; 空白表示
;; ==============================
(require 'whitespace)
(setq whitespace-style '(face tabs tab-mark spaces space-mark))
(set-face-foreground 'whitespace-space "blue")
(set-face-background 'whitespace-space nil)
(set-face-foreground 'whitespace-newline "blue")
(set-face-background 'whitespace-newline nil)
(global-whitespace-mode 1)

;; ==============================
;;; 対応する括弧
;; ==============================
;; 対応する括弧を強調して表示
(setq show-paren-delay 0); 表示までの秒数
(show-paren-mode t); 有効化
(set-face-background 'show-paren-match-face "blue")
(set-face-underline-p 'show-paren-match-face "yellow")

;; ==============================
;;; 補完設定
;; ==============================
; 検索の際、大文字小文字を区別しない
(setq completion-ignore-case t)

;; ==============================
;;; 終了時
;; ==============================
;; 行末の空白を保存前に削除。
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ==============================
;;; global key map設定
;; ==============================
;; コピペ
(define-key global-map (kbd "C-c C-c") 'kill-ring-save); コピー
(define-key global-map (kbd "C-c C-x") 'kill-region); カット
(define-key global-map (kbd "C-c C-v") 'anything-show-kill-ring); kill-ring表示

;; 行全消
(define-key global-map (kbd "C-c C-d") 'kill-whole-line)

;; "C-m"にnewline-and-indentを割り当てる
(define-key global-map (kbd "C-m") 'newline-and-indent)

;; "C-t"でウィンドウを切り替える
(define-key global-map (kbd "C-t") 'other-window)

;; ウィンドウのサイズを変更
(define-key global-map (kbd "C-<up>") 'enlarge-window)
(define-key global-map (kbd "C-<down>") 'shrink-window)
(define-key global-map (kbd "C-<left>") 'shrink-window-horizontally)
(define-key global-map (kbd "C-<right>") 'enlarge-window-horizontally)

;; "C-z"でundo
(define-key global-map (kbd "C-z") 'undo)

;; ag検索
(define-key global-map (kbd "C-c C-g") 'ag)

;; コードフォールディング
(global-set-key (kbd "C-o") 'hs-toggle-hiding)

;; 先頭/末尾に移動
(define-key global-map (kbd "C-c <") 'beginning-of-buffer); バッファの先頭に移動
(define-key global-map (kbd "C-c >") 'end-of-buffer); バッファの末尾に移動

;; emmet-expand-line
(define-key emmet-mode-keymap (kbd "C-c C-e") 'emmet-expand-line); 展開
