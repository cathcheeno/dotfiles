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
(add-to-list 'package-archives'("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; ==============================
;;; エンコーディング設定
;; ==============================
(prefer-coding-system 'utf-8-unix)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; ==============================
;;; exec-path-from-shell設定
;; ==============================
(exec-path-from-shell-initialize)

;; ==============================
;;; anything設定
;; ==============================
(require 'anything)

;; ==============================
;;; dash-at-point設定
;; ==============================
;; go-modeのときのDocsetはgoとする
;;(add-to-list 'dash-at-point-mode-alist '(go-mode . "go"))

;; ==============================
;;; magit
;; ==============================
(require 'magit)
;; 文字色変更
(set-face-foreground 'magit-diff-added "yellow"); 追加行
(set-face-foreground 'magit-diff-added-highlight "yellow"); 追加行
(set-face-foreground 'magit-diff-removed "magenta"); 削除行
(set-face-foreground 'magit-diff-removed-highlight "magenta"); 削除行
(set-face-foreground 'magit-diff-file-heading "gold"); Diffのファイル名部分
(set-face-foreground 'magit-diff-file-heading-highlight "gold"); Diffのファイル名部分
(set-face-foreground 'magit-diff-hunk-heading "white"); Diffのhunk部分
(set-face-foreground 'magit-diff-hunk-heading-highlight "white"); Diffのhunk部分
(set-face-foreground 'magit-diff-context "#666666"); 変化無し行
(set-face-foreground 'magit-diff-context-highlight "#666666"); 変化無し行
(set-face-foreground 'magit-diff-conflict-heading nil); conflict時の自身サイド
(set-face-foreground 'magit-diff-our nil); conflict時の自身サイド
(set-face-foreground 'magit-diff-our-highlight nil); conflict時の自身サイド
(set-face-foreground 'magit-diff-base nil); conflict時のベース
(set-face-foreground 'magit-diff-base-highlight nil); conflict時のベース
(set-face-foreground 'magit-diff-their nil); conflict時の他人サイド
(set-face-foreground 'magit-diff-their-highlight nil); conflict時の他人サイド

;; 背景色変更
(set-face-background 'magit-diff-file-heading "#003d69"); Diffのファイル名部分
(set-face-background 'magit-diff-file-heading-highlight "#003d69"); Diffのファイル名部分
(set-face-background 'magit-diff-hunk-heading "#333333"); Diffのhunk部分
(set-face-background 'magit-diff-hunk-heading-highlight "#333333"); Diffのhunk部分
(set-face-background 'magit-diff-context "#111111"); 変化無し行
(set-face-background 'magit-diff-context-highlight "#111111"); 変化無し行
(set-face-background 'magit-diff-added "#111111"); 追加行
(set-face-background 'magit-diff-added-highlight "#111111"); 追加行
(set-face-background 'magit-diff-removed "#111111"); 削除行
(set-face-background 'magit-diff-removed-highlight "#111111"); 削除行
(set-face-background 'magit-section-highlight nil); 選択部分
(set-face-background 'magit-diff-conflict-heading nil); conflict時の自身サイド
(set-face-background 'magit-diff-our nil); conflict時の自身サイド
(set-face-background 'magit-diff-our-highlight nil); conflict時の自身サイド
(set-face-background 'magit-diff-base nil); conflict時のベース
(set-face-background 'magit-diff-base-highlight nil); conflict時のベース
(set-face-background 'magit-diff-their nil); conflict時の他人サイド
(set-face-background 'magit-diff-their-highlight nil); conflict時の他人サイド

;; ==============================
;;; emmet-mode設定
;; ==============================
;(require 'emmet-mode)
;; マークアップ言語全部で使う
;(add-hook 'sgml-mode-hook 'emmet-mode)
;; CSSにも使う
;(add-hook 'css-mode-hook 'emmet-mode)
;; indentはスペース2つ
;(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

;; ==============================
;;; ag設定
;; ==============================
(require 'ag)
;;  検索キーワードをハイライト
(setq ag-highlight-search t)
;; 検索用バッファを使い回す(検索ごとに新バッファを作らない)
(setq ag-reuse-buffers t)

;; ==============================
;;; editorconfig
;; ==============================
(setq edconf-exec-path "/usr/local/bin/editorconfig"); Homebrew でインストールしたコマンドのパス
(load "editorconfig")
(editorconfig-mode 1)

;; ==============================
;;; wgrep設定
;; ==============================
(add-hook 'ag-mode-hook '(lambda ()
                           (require 'wgrep-ag)
                           (setq wgrep-auto-save-buffer t);; 編集完了と同時に保存
                           (setq wgrep-enable-key "e");; "e"キーで編集モードにする
                           (wgrep-ag-setup)))

;; ==============================
;;; color-theme設定
;; ==============================
;; color-themeをHoberに変更
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-hober))

;; ==============================
;;; Patch 24.5's sgml-mode with the new attribute offset feature.
;; ==============================
(require 'load-relative)
(load-relative "./sgml-mode-patch.el")
(require 'sgml-mode)

;; ==============================
;;; js2-mode
;; ==============================
;;(autoload 'js2-mode "js2-mode" nil t)

;; ==============================
;;; js2-jsx-mode
;; ==============================
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.flow\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
(add-to-list 'auto-mode-alist '("\\.tag\\'" . js2-jsx-mode))

;; ==============================
;;; pug-mode
;; ==============================
(require 'pug-mode)
(add-to-list 'auto-mode-alist '("\\.tag\\'" . pug-mode))

;; ==============================
;;; dockerfile-mode
;; ==============================
(autoload 'dockerfile-mode "dockerfile-mode" nil t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; ==============================
;;; terraform-mode
;; ==============================
(require 'terraform-mode)
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
(add-to-list 'auto-mode-alist '("\\.tfvars\\'" . terraform-mode))

;; ==============================
;;; css-mode
;; ==============================
(require 'css-mode)
(add-to-list 'auto-mode-alist '("\\.pcss\\'" . css-mode))

;; ==============================
;;; yaml-mode
;; ==============================
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; ==============================
;;; ansible minor mode
;; ==============================
(require 'ansible)
(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))

;; ==============================
;;; web-mode
;; ==============================
(require 'web-mode)
;(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;(setq web-mode-content-types-alist
;      '(("jsx" . "\\.js[x]?\\'")))
;(set-face-foreground 'web-mode-html-tag-bracket-face "#cccccc")
;(add-to-list 'web-mode-comment-formats '("js" . "// "))
;(setq web-mode-indent-style 2)
;(setq web-mode-markup-indent-offset 2)
;(setq web-mode-css-indent-offset 2)
;(setq web-mode-code-indent-offset 2)



;; ==============================
;;; stylus-mode
;; ==============================
(autoload 'stylus-mode "stylus-mode" nil t)

;; ==============================
;;; php-mode
;; ==============================
(require 'php-mode)

;; ==============================
;;; swift-mode
;; ==============================
(require 'swift-mode)

;; ==============================
;;; powerline
;; ==============================
(require 'powerline)

;; ==============================
;;; smart-mode-line
;; ==============================
(setq sml/no-confirm-load-theme t)
(setq sml/theme 'powerline)
(sml/setup)

;; ==============================
;;; auto-complete設定
;; ==============================
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (require 'auto-complete)
                                   (auto-complete-mode t)
                                   ))
(require 'auto-complete-config)
(global-auto-complete-mode t)
(ac-config-default)
(add-to-list 'ac-modes 'swift-mode)
(add-to-list 'ac-modes 'js2-mode)
(add-to-list 'ac-modes 'js2-jsx-mode)

;; ==============================
;;; windowのactive/inactive状態に応じて、モードラインをスタイルを変更する
;; ==============================
(set-face-attribute 'mode-line
                    nil
                    :background "blue")

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
;(add-hook 'web-mode-hook
;          (lambda ()
;            ;; Scan the file for nested code blocks
;            (imenu-add-menubar-index)
;            ;; Activate the folding mode
;                        (hs-minor-mode t)))

;; ==============================
;;; ユーザ情報
;; ==============================
;; ユーザー情報
(setq user-full-name "Go Ohtani")
(setq user-mail-address "cathcheeno@gmail.com")

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
;;; 列番号
;; ==============================
(line-number-mode t)
(column-number-mode t)

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
(setq scroll-conservatively 35
  scroll-margin 0
  scroll-step 1)

;; ==============================
;;; underline設定
;; ==============================
;; (global-hl-line-mode)
;; (setq hl-line-face 'underline)

;; ==============================
;;; underline設定 高速化
;; ==============================
(require 'hl-line)
(defun global-hl-line-timer-function ()
  (global-hl-line-unhighlight-all)
  (let ((global-hl-line-mode t))
    (global-hl-line-highlight)))
(setq global-hl-line-timer
      (run-with-idle-timer 0.03 t 'global-hl-line-timer-function))
(setq hl-line-face 'underline)
;; (cancel-timer global-hl-line-timer)

;; ==============================
;;; 空白表示
;; ==============================
(require 'whitespace)
(setq whitespace-style '(face tabs tab-mark spaces space-mark))
(set-face-foreground 'whitespace-space "blue")
(set-face-background 'whitespace-space nil)
(set-face-foreground 'whitespace-newline "blue")
(set-face-background 'whitespace-newline nil)
(set-face-foreground 'whitespace-tab "blue")
(set-face-background 'whitespace-tab nil)
(global-whitespace-mode 1)

;; ==============================
;;; ミニバッファ
;; ==============================
(set-face-foreground 'minibuffer-prompt "yellow")

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
;;; コピペ
;; ==============================
;; クリップボードの内容をヤンク可能にする
(setq x-select-enable-primary t)

;; ==============================
;;; global key map設定
;; ==============================
;; コピペ
(require 'bind-key)
(bind-key* "C-c C-c" 'kill-ring-save); コピー
(bind-key* "C-c C-x" 'kill-region); カット
(bind-key* "C-c C-v" 'anything-show-kill-ring); kill-ring表示
(bind-key* "C-c C-r" 'query-replace); 置換

;; "C-m"にnewline-and-indentを割り当てる
(bind-key "C-m" 'newline-and-indent)

;; "C-t"でウィンドウを切り替える
(bind-key "C-t" 'other-window)

;; ウィンドウのサイズを変更
(bind-key* "C-<up>" 'enlarge-window)
(bind-key* "C-<down>" 'shrink-window)
(bind-key* "C-<left>" 'shrink-window-horizontally)
(bind-key* "C-<right>" 'enlarge-window-horizontally)

;; "C-z"でundo
(bind-key* "C-z" 'undo)

;; ag検索
(bind-key* "C-c C-g" 'ag)

;; コードフォールディング
(global-set-key (kbd "C-o") 'hs-toggle-hiding)

;; 先頭/末尾に移動
(bind-key* "C-c <" 'beginning-of-buffer); バッファの先頭に移動
(bind-key* "C-c >" 'end-of-buffer); バッファの末尾に移動

;; grep
(bind-key* "C-c C-g" 'rgrep)

;; eshell
(bind-key* "C-c C-e" 'eshell)
;(define-key web-mode-map (kbd "C-c C-e") 'eshell)

;; only with web-mode
;(define-key web-mode-map (kbd "C-;") nil)
;(define-key web-mode-map (kbd "C-c ;") 'web-mode-comment-or-uncomment)

;; dash-at-point
(bind-key* "C-c C-o" 'dash-at-point)
;;(define-key global-map (kbd "C-c C-d") 'dash-at-point-with-docset); docsetを指定して検索する
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-mode dockerfile-mode terraform-mode pug-mode yaml-mode wgrep-ag web-mode swift-mode stylus-mode smart-mode-line-powerline-theme pkg-info php-mode malabar-mode magit load-relative let-alist js2-mode go-mode exec-path-from-shell emmet-mode editorconfig-core editorconfig dash-at-point color-theme bind-key auto-complete anything ansible ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
