; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;;--------------------------------------------------------------------------------
;; Emacs Server
;;--------------------------------------------------------------------------------
;; For Windows, refer to http://sky-y.hatenablog.jp/entry/20111224/1324714853
;(if window-system (progn
;                      (server-start)))
(require 'server)
(unless (server-running-p)
  (server-start))

(setenv "LANG" "C")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Send key type data to fluentd
;; (defun my-insert-hook ()                                                                                                                                                ;;
;;   (start-process "post-fluent" nil "curl" "--noproxy" "localhost" "-X" "POST" "-d" (concat "json={\"mode\":\"" (symbol-name major-mode) "\"}") "localhost:8764/emacs")) ;;
;; (add-hook 'post-self-insert-hook 'my-insert-hook)                                                                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;--------------------------------------------------------------------------------
;; Org-mode
;;--------------------------------------------------------------------------------
;; Following settings are from http://doc.norang.ca/org-mode.html#TasksAndStates

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)
;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; List of org files
(setq org-agenda-files (quote ("~/org/")))


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))



;; To change the state by C-c C-t KEY
(setq org-use-fast-todo-selection t)

(setq org-directory "~/org")
(setq org-default-notes-file "~/org/refile.org")

;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)


;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (
              ("D" "Daily Action List"
               (
                (agenda "" ((org-agenda-ndays 1)
                            (org-agenda-sorting-strategy
                             (quote ((agenda time-up priority-down tag-up) )))
                            (org-deadline-warning-days 0)
                            ))))
              ("h" "(Home)Agenda and Lists with context"
               ((agenda)
                (tags-todo "COMPUTER")
                (tags-todo "MANA")
                (tags-todo "MA")
                (tags-todo "NA")
                (tags-todo "MONEY")
                (tags-todo "READING")))
              (" " "(Office)Agenda and Lists with context"
               ((agenda)
                (tags-todo "MM")
                (tags-todo "BS")
                (tags-todo "ETC"))))))


(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/refile.org")
               "* TODO %?\n\n")
              ("s" "mm todo" entry (file "~/org/refile.org")
               "* TODO %? :MM:\n\n")
             ("r" "respond" entry (file "~/org/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n\n")
              ("n" "note" entry (file "~/org/refile.org")
               "* %? :NOTE:\n\n")
              ("j" "Journal" entry (file+datetree "~/org/journal.org")
               "* %?\n\n")
              ("w" "org-protocol" entry (file "~/org/refile.org")
               "* TODO Review %c\n\n")
              ("m" "Meeting" entry (file "~/org/refile.org")
               "* MEETING with %? :MEETING:\n")
;;              ("p" "Phone call" entry (file "~/org/refile.org")
;;               "* PHONE %? :PHONE:\n")
              ("i" "Idea " entry (file+datetree "~/org/idea.org")
               "* IDEA %? :IDEA:\n")
              ("h" "Habit" entry (file "~/org/refile.org")
               "* NEXT %?\n\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

(setq org-refile-targets (quote (("gtd.org" :maxlevel . 1) 
                             ("someday.org" :level . 2))))

;;--------------------------------------------------------------------------------
;; scratchの初期メッセージ
;;--------------------------------------------------------------------------------
(setq initial-scratch-message "Done is better than perfect, datte.")

;;--------------------------------------------------------------------------------
;; Package manager
;;--------------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; ロードパスの追加
(setq load-path
      (append
       (list
       (expand-file-name "~/.emacs.d/elisp/")
       )
       load-path))


(setq my-packages
      '(
    zenburn-theme
    yasnippet
    wgrep
    tabbar
    popwin
    popup
    markdown-mode
    elscreen
    dropdown-list
    color-moccur
    cmake-mode
    c-eldoc
    auto-install
    auto-complete
    anything
;    gtags
    ))

(require 'cl)
(mapcar (lambda (x)
          (when (not (package-installed-p x))
            (package-install x)))
        my-packages)


(require 'auto-install)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install/"))
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)


;;--------------------------------------------------------------------------------
;; 外観
;;--------------------------------------------------------------------------------
(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpa/zenburn-theme-20130523.1753/'"))
;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-zenburn)
(load-theme 'zenburn t)
;; 画面透過
(set-frame-parameter nil 'alpha 95)
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;; IME OFF時の初期カーソルカラー
(set-cursor-color "dark cyan")

;; IME ON/OFF時のカーソルカラー
(add-hook 'input-method-activate-hook
          (lambda() (set-cursor-color "dark red")))
(add-hook 'input-method-inactivate-hook
          (lambda() (set-cursor-color "dark cyan")))


;;--------------------------------------------------------------------------------
;; 環境依存のファイル読み込み
;;--------------------------------------------------------------------------------
(if (eq system-type 'windows-nt) (load "~/.emacs.d/init_windows.el")
  (if (eq system-type 'darwin) (load "~/.emacs.d/init_mac.el")
      (load "~/.emacs.d/init_linux.el")))


;;--------------------------------------------------------------------------------
;; ミニバッファ設定
;;--------------------------------------------------------------------------------
;; ミニバッファの履歴を保存する
(savehist-mode 1)

;; ミニバッファの履歴の保存数を増やす
(setq history-length 3000)


;;--------------------------------------------------------------------------------
;; 同名バッファを識別しやすくする
;;--------------------------------------------------------------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; auto `chmod +x` to `#!`
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


;;--------------------------------------------------------------------------------
;; 基本設定
;;--------------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(auto-save-list-file-name nil t)
 '(auto-save-list-file-prefix nil)
 '(column-number-mode t)
 '(delete-auto-save-files t)
 '(display-time-mode t)
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/org/gtd.org_archive" "/home/tatezono/org/gtd.org" "/home/tatezono/org/idea.org" "/home/tatezono/org/journal.org" "/home/tatezono/org/refile.org")))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(w32-symlinks-handle-shortcuts t)
 '(yas-trigger-key "TAB"))


;;--------------------------------------------------------------------------------
;; モードライン
;;--------------------------------------------------------------------------------
(display-time)                  ; 現在時刻
(column-number-mode t)          ; 行数表示
(which-function-mode t)         ; 現在の関数名
;(add-to-list 'load-path
;             (expand-file-name "~/.emacs.d/elpa/powerline-20130511.1749/'"))
;(require 'powerline)
;(powerline-center-theme)


;; ;;--------------------------------------------------------------------------------
;; ;; popwin 設定
;; ;;--------------------------------------------------------------------------------

;; 重いので使わないことにした 2013.11.01
;; (setq pop-up-windows nil)
;; (require 'popwin nil t)
;; (when (require 'popwin nil t)
;;   (setq popwin:popup-window-position 'bottom)    ; 表示位置
;;   (setq anything-samewindow nil)
;;   (setq display-buffer-function 'popwin:display-buffer)
;;   (push '("anything" :regexp t :height 0.5) popwin:special-display-config)
;;   (push '("*Completions*" :height 0.4) popwin:special-display-config)
;;   (push '("*compilation*" :height 0.4 :noselect t :stick t) popwin:special-display-config)
;;   )


;;--------------------------------------------------------------------------------
;; ediff
;;--------------------------------------------------------------------------------
;; diff のウインドウを左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)


;;--------------------------------------------------------------------------------
;; モード共通設定
;;--------------------------------------------------------------------------------
(global-font-lock-mode t)          ; 色付け
(setq font-lock-maximum-decoration t) ; 色付けを最適化
(setq fast-lock-cache-directories '("~/.emacs-flc" "."))
(show-paren-mode t)                ; 対応する括弧をハイライト
(setq show-paren-delay 0)          ; 表示までの秒数: 0秒
(global-linum-mode t)              ; 行数表示

;; キーワードのハイライト
(defadvice font-lock-mode (before my-font-lock-mode2 ())
  (font-lock-add-keywords
   major-mode
   '(
     ("!" . font-lock-warning-face)          ; !
;     ("[0-9]+" . font-lock-constant-face)    ; 数字
     ("TODO" 0 my-face-todo append)          ; TODO
     ("FIXME" 0 my-face-todo append)         ; FIXME
     ("　" 0 my-face-zenkaku-s append)       ; 全角スペース
     ("\t" 0 my-face-tab append)             ; タブ
;    ("^    " 0 my-face-u-1 append)         ; スペース４インデント
     ("[ ]+$" 0 my-face-endspace append)     ; 行末スペース
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode2)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks '(lambda ()
                              (if font-lock-mode nil (font-lock-mode t))))

;; 貼り付けた後インデントを行う
(global-set-key [?\C-\S-y] (lambda ()
                             (interactive)
                             (yank)
                             (indent-region (region-beginning) (region-end))))

;;--------------------------------------------------------------------------------
;; auto-complete
;;--------------------------------------------------------------------------------
(require 'auto-complete)
(require 'auto-complete-config)    ; 必須ではないですが一応
(global-auto-complete-mode t)
(setq ac-use-menu-map t)           ;補完候補をC-n/C-p で選択


;;--------------------------------------------------------------------------------
;; hs-mode (コード折りたたみ)
;;--------------------------------------------------------------------------------
(add-hook 'c++-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'c-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'scheme-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'lisp-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'python-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
(add-hook 'ruby-mode-hook
          '(lambda ()
             (hs-minor-mode 1)))
;; 現在位置の折りたたみをON/OFF
(define-key global-map (kbd "C-M-p") 'hs-toggle-hiding)

;; 現在のレベルを折りたたみ
(define-key global-map (kbd "C-M-u") 'hs-hide-level)

;; すべての折りたたみを解除
(define-key global-map (kbd "C-M-i") 'hs-show-all)


;;--------------------------------------------------------------------------------
;; color-moccur
;; (http://d.hatena.ne.jp/IMAKADO/20080724/1216882563)
;;--------------------------------------------------------------------------------
(require 'color-moccur)
;; 複数の検索語や、特定のフェイスのみマッチ等の機能を有効にする
;; 詳細は http://www.bookshelf.jp/soft/meadow_50.html#SEC751
(setq moccur-split-word t)
;; migemoがrequireできる環境ならmigemoを使う
;;(when (require 'migemo nil t) ;第三引数がnon-nilだとloadできなかった場合にエラーではなくnilを返す
;;  (setq moccur-use-migemo t))

;;; anything-c-moccurの設定
(require 'anything-c-moccur)
;; カスタマイズ可能変数の設定(M-x customize-group anything-c-moccur でも設定可能)
(setq anything-c-moccur-anything-idle-delay 0.2 ;`anything-idle-delay'
      anything-c-moccur-higligt-info-line-flag t ; `anything-c-moccur-dmoccur'などのコマンドでバッファの情報をハイライトする
      anything-c-moccur-enable-auto-look-flag t ; 現在選択中の候補の位置を他のwindowに表示する
      anything-c-moccur-enable-initial-pattern t) ; `anything-c-moccur-occur-by-moccur'の起動時にポイントの位置の単語を初期パターンにする

;;; キーバインドの割当(好みに合わせて設定してください)
(global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur) ;バッファ内検索
(global-set-key (kbd "C-M-o") 'anything-c-moccur-dmoccur) ;ディレクトリ
(add-hook 'dired-mode-hook ;dired
          '(lambda ()
             (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur)))



;;--------------------------------------------------------------------------------
;; tabbar
;;--------------------------------------------------------------------------------
   (require 'tabbar)

   ;; tabbar有効化
   (tabbar-mode)

   ;; タブ切替にマウスホイールを使用（0：有効，-1：無効）
   (tabbar-mwheel-mode -1)

   ;; タブグループを使用（t：有効，nil：無効）
   (setq tabbar-buffer-groups-function nil)

   ;; ボタン非表示
   (dolist (btn '(tabbar-buffer-home-button
                  tabbar-scroll-left-button
                  tabbar-scroll-right-button))
     (set btn (cons (cons "" nil) (cons "" nil))))

   ;; タブ表示 一時バッファ一覧
   (defvar tabbar-displayed-buffers
     '("*Backtrace*" "*Colors*"
       "*Faces*" "*Apropos*" "*Customize*" "*shell*" "*Help*")
     "*Regexps matches buffer names always included tabs.")

   ;; 作業バッファの一部を非表示
   (setq tabbar-buffer-list-function
         (lambda ()
           (let* ((hides (list ?\  ?\*))
                  (re (regexp-opt tabbar-displayed-buffers))
                  (cur-buf (current-buffer))
                  (tabs (delq
                         nil
                         (mapcar
                          (lambda (buf)
                            (let ((name (buffer-name buf)))
                              (when (or (string-match re name)
                                        (not (memq (aref name 0) hides)))
                                buf)))
                          (buffer-list)))))
             (if (memq cur-buf tabs)
                 tabs
               (cons cur-buf tabs)))))

   ;; キーバインド設定
   (global-set-key (kbd "<C-tab>")   'tabbar-forward-tab)
   (global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)

   ;; タブ表示欄の見た目（フェイス）
   (set-face-attribute 'tabbar-default nil
                       :background "SystemMenuBar")

   ;; 選択タブの見た目（フェイス）
   (set-face-attribute 'tabbar-selected nil
                       :foreground "turquoise"
                       :background "SystemMenuBar"
;                       :box (list
;                             :line-width 1
;                             :color "gray80"
;                             :style 'released-button)
;                       :overline "#F3F2EF"
;                       :weight 'bold
 ;                      :family "ＭＳ Ｐゴシック"
                       )

   ;; 非選択タブの見た目（フェイス）
   (set-face-attribute 'tabbar-unselected nil
                       :foreground "dark khaki"
                       :background "SystemMenuBar"
;                       :box (list
;                             :line-width 1
;                             :color "gray80"
;                             :style 'released-button)
;                       :overline "#F3F2EF"
;                       :family "ＭＳ Ｐゴシック"
                       )

   ;; タブ間隔の調整
   (set-face-attribute 'tabbar-separator t
                       :height 0.5)

;; https://github.com/dougalcorn/emacs.d/blob/master/elscreen-config.el
;;--------------------------------------------------------------------------------
;; 操作（マウス）
;;--------------------------------------------------------------------------------
(if window-system (progn
            (mouse-wheel-mode t)               ; マウスホイールサポート
            (setq mouse-wheel-follow-mouse t)
))


;;--------------------------------------------------------------------------------
;; 操作（キー）
;;--------------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)                ; y/nで選択
(define-key query-replace-map [return] 'act) ; RETでyes選択
(global-set-key "\M-g" 'goto-line)           ; 行ジャンプ
(global-set-key "\C-h" 'delete-backward-char); 削除をC-hで



;;--------------------------------------------------------------------------------
;; 検索
;;--------------------------------------------------------------------------------
(global-hi-lock-mode 1)
(require 'wgrep nil t)

;;--------------------------------------------------------------------------------
;; anything
;;--------------------------------------------------------------------------------
(require 'anything-startup)
(add-to-list 'anything-sources 'anything-c-source-emacs-commands)
(global-set-key (kbd "C-x ;") 'anything)
(global-set-key (kbd "C-'") 'anything)
(define-key global-map (kbd "M-x")
  (lambda ()
    "Execute emacs commands in anything"
    (interactive)
    (anything '(anything-c-source-emacs-commands))))
(setq anything-idle-delay 0.1)


;;--------------------------------------------------------------------------------
;; C 言語設定
;;--------------------------------------------------------------------------------

;; GDB
(load "~/.emacs.d/elisp/gdb.el")

(add-hook 'c-mode-common-hook
          '(lambda ()
             (local-set-key [f12] 'compile)
             (gtags-mode 1)))



;; インデントのデフォルト設定
(setq-default indent-tabs-mode nil)
(setq c-default-style "k&r")
(setq-default c-basic-offset 4)
(setq-default tab-width 4)


(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   'c-mode
   '(
     ("!" . font-lock-warning-face)          ; !
     ("[0-9]+" . font-lock-constant-face)    ; 数字
     ("TODO" 0 my-face-todo append)          ; TODO
     ("FIXME" 0 my-face-todo append)         ; FIXME
     ("　" 0 my-face-zenkaku-s append)       ; 全角スペース
     ("\t" 0 my-face-tab append)             ; タブ
;    ("^    " 0 my-face-u-1 append)         ; スペース４インデント
     ("[ ]+$" 0 my-face-endspace append)     ; 行末スペース
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks '(lambda ()
                              (if font-lock-mode nil (font-lock-mode t))))


;; 1行の文字数が100を超えると指摘
(add-hook 'c-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("^[^\n]\\{100\\}\\(.*\\)$" 1 font-lock-warning-face t)))
            (setq show-trailing-whitespace t)))


;; [F9] でインデント設定をスペースに切り替え
;(define-key global-map [f9] 'use-space-indent)
;(defun use-space-indent ()
;  (interactive)
;  'funda-mode
;  (interactive)
;  (setq-default indent-tabs-mode nil)
;  (setq c-default-style "k&r")
;  (setq c-basic-offset 4)
;  (setq tab-width 4)
;  (c-mode)
;  (show-tab-indent)
;  (message "[NOTICE] set indent : SPACE"))

;; ;; [F11] でインデント設定をタブに切り替え
;; (define-key global-map [f11] 'use-tab-indent)
;; (defun use-tab-indent ()
;;   (interactive)
;;   'funda-mode
;;   (interactive)
;;   (setq-default indent-tabs-mode t)
;;   (setq c-default-style "k&r")
;;   (setq c-basic-offset 4)
;;   (setq tab-width 4)
;;   (c-mode)
;; ;  (show-space4-indent)
;;   (message "[NOTICE] set indent style: TAB"))

;; [F11] でバッファ全体のインデントをスペース化
;(define-key global-map [f11] 'untabify-and-indent-whole-buffer)
;(defun untabify-and-indent-whole-buffer ()
;  (interactive)
;  (indent-region (point-min) (point-max))
;  (untabify (point-min) (point-max))
;  (message "[NOTICE] all indent was converted to SPACE"))

;; [F12] コード整形モード
;; 以下の箇所でwarningを出す
;; ・TABの使用
;; ・1行で80文字以上
;; ・行末の警告
;; (define-key global-map [f12] 'tabify-and-indent-whole-buffer)
;; (defun tabify-and-indent-whole-buffer ()
;;   (interactive)
;;   (add-hook 'c-mode-hook
;;             (lambda ()
;;               (flymake-mode t)
;;               (font-lock-add-keywords nil
;;                                       '(("^[^\n]\\{80\\}\\(.*\\)$" 1 font-lock-warning-face t)))
;;               (font-lock-add-keywords major-mode '(
;;                                                    ("\\<if\\>"
;;                                                     ("[^<>=]\\(=\\)[^=]" nil nil (1 font-lock-warning-face))
;;                                                     )))

;;   ))
;;   (c-mode)
;;   (show-tab-indent)
;;   (message "[NOTICE] code checking mode"))


;; 日付の挿入
(define-key global-map [f5]
  '(lambda ()
     (interactive)
     (insert (format-time-string "%Y-%m-%d"))))

(define-key global-map (kbd "<C-f5>")
  '(lambda ()
     (interactive)
     (insert (format-time-string "%m/%d(%a)"))))

;; 署名の挿入
(define-key global-map [f6]
  '(lambda ()
     (interactive)
     (insert "zonomasa")))

;;--------------------------------------------------------------------------------
;; C-eldoc
;;--------------------------------------------------------------------------------
(load "c-eldoc")
(add-hook 'c-mode-hook
          (lambda ()
            (set (make-local-variable 'eldoc-idle-delay) 0.20)
            (c-turn-on-eldoc-mode)
            ))


;;--------------------------------------------------------------------------------
;; Flymakeの設定
;;--------------------------------------------------------------------------------
;; http://d.hatena.ne.jp/mmitou/20120604/1338828611
;; http://d.hatena.ne.jp/pyopyopyo/20070715/
(require 'flymake)

;; Flymake のログレベルを上げる。*Messages* で確認
(setq flymake-log-level 3)

;; c-mode に登録
;(add-hook 'c-mode-hook
;          (lambda ()
;            (flymake-mode t)
;            ))

 (defun flymake-get-make-cmdline (source base-dir)
   (list "make"
         (list  "PCLINUX=yes" "-C"
                (concat base-dir "src")
               (concat "CHK_SOURCES=../" source)
;;               "SYNTAX_CHECK_MODE=1"
               "check-syntax")))


(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)

;;エラーメッセージをミニバッファで表示させる
(global-set-key "\M-n" 'flymake-goto-next-error)
(global-set-key "\M-p" 'flymake-goto-prev-error)
(setq flymake-run-in-place nil)
(setq flymake-run-in-place t)
;; gotoした際にエラーメッセージをminibufferに表示する
(defun display-error-message ()
  (message (get-char-property (point) 'help-echo)))
(defadvice flymake-goto-prev-error (after flymake-goto-prev-error-display-message)
  (display-error-message))
(defadvice flymake-goto-next-error (after flymake-goto-next-error-display-message)
  (display-error-message))
(ad-activate 'flymake-goto-prev-error 'flymake-goto-prev-error-display-message)
(ad-activate 'flymake-goto-next-error 'flymake-goto-next-error-display-message)


;;--------------------------------------------------------------------------------
;; yasnippet
;;--------------------------------------------------------------------------------

(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpa/yasnippet/'"))
(require 'yasnippet)
(add-to-list 'load-path
             (expand-file-name "~/.emacs.d/elpa/dropdown-list/'"))
(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
       "~/.emacs.d/elpa/yasnippet/snippets"))
(yas-global-mode 1)


;; 単語展開キーバインド (ver8.0から明記しないと機能しない)
;; (setqだとtermなどで干渉問題ありでした)
;; もちろんTAB以外でもOK 例えば "C-;"とか


;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)


;;--------------------------------------------------------------------------------
;; GNU global(gtags.el)
;;--------------------------------------------------------------------------------
(when (require 'gtags nil t)
  (require 'anything-gtags)
  (setq gtags-path-style 'relative)
  (global-set-key "\M-t" 'gtags-find-tag)     ;関数の定義元へ
  (global-set-key "\M-r" 'gtags-find-rtag)    ;関数の参照先へ
  (global-set-key "\M-s" 'gtags-find-symbol)  ;変数の定義元/参照先へ
                                        ;(global-set-key "\M-f" 'gtags-find-file)   ;ファイルにジャンプ
  (global-set-key "\C-t" 'gtags-pop-stack)   ;前のバッファに戻る
  (global-set-key "\C-cf" 'gtags-parse-file2)   ;前のバッファに戻る
  (add-hook 'c-mode-common-hook
            '(lambda ()
               (gtags-mode 1)))

  ;; update GTAGS
  (defun update-gtags (&optional prefix)
    (interactive "P")
    (let ((rootdir (gtags-get-rootpath))
          (args (if prefix "-v" "-iv")))
      (when rootdir
        (let* ((default-directory rootdir)
               (buffer (get-buffer-create "*update GTAGS*")))
          (save-excursion
            (set-buffer buffer)
            (erase-buffer)
            (let ((result (process-file "gtags" nil buffer nil args)))
              (if (= 0 result)
                  (message "GTAGS successfully updated.")
                (message "update GTAGS error with exit status %d" result))))))))
  (add-hook 'after-save-hook 'update-gtags)
  (defun gtags-parse-file2 ()
    (interactive)
    (if (gtags-get-rootpath)
        (let*
            ((root (gtags-get-rootpath))
             (path (buffer-file-name))
             (gtags-path-style 'root)
             (gtags-rootdir root))
          (if (string-match (regexp-quote root) path)
              (gtags-goto-tag
               (replace-match "" t nil path)
               "f" nil)
            ;; delegate to gtags-parse-file
            (gtags-parse-file)))
      ;; delegate to gtags-parse-file
      (gtags-parse-file)))
)
;;--------------------------------------------------------------------------------
;; Markdown-mode
;;--------------------------------------------------------------------------------
(setq auto-mode-alist (cons '("\\.md" . gfm-mode) auto-mode-alist))

;;--------------------------------------------------------------------------------
;; 独自変数の定義
;;--------------------------------------------------------------------------------
;; 色の定義（font-lock用）
(defface my-face-todo '((t (:foreground "green yellow" :underline t :bold t))) nil)
(defvar my-face-todo 'my-face-todo)
(defface my-face-zenkaku-s '((t (:background "medium aquamarine"))) nil)
(defvar my-face-zenkaku-s 'my-face-zenkaku-s)
(defface my-face-tab '((t (:foreground "gray40" :underline t))) nil)
(defvar my-face-tab 'my-face-tab)
(defface my-face-endspace '((t (:background "coral1"))) nil)
(defvar my-face-endspace 'my-face-endspace)


;;--------------------------------------------------------------------------------
;; 独自関数の定義
;;--------------------------------------------------------------------------------
;; インデントにスペース4を使っている場合に表示する
;; 使用しなくなった
(defun show-space4-indent ()
  (defadvice font-lock-mode (before my-font-lock-mode ())
    (font-lock-add-keywords
     major-mode
     '(
       ("　" 0 my-face-b-1 append)
;       ("\t" 0 my-face-b-2 append)
       ("^    " 0 my-face-u-1 append)
       ("[ ]+$" 0 my-face-u-1 append)
       )))
  (ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
  (ad-activate 'font-lock-mode)
  (add-hook 'find-file-hooks '(lambda ()
                                (if font-lock-mode
                                    nil
                                  (font-lock-mode t))))
)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;; https://github.com/narusemotoki/.emacs.d/tree/master/elisp/ac-mozc
(require 'ac-mozc)
(define-key ac-mode-map (kbd "C-c C-SPC") 'ac-mozc-complete)
(require 'org)
(add-to-list 'ac-modes 'org-mode)
(add-hook 'org-mode-hook 
      (lambda ()
            (delete 'ac-source-words-in-same-mode-buffers ac-sources)
            (add-to-list 'ac-sources 'ac-source-mozc)
            (set (make-local-variable 'ac-auto-show-menu) 0.01)))



(require 'multi-term)
(setq multi-term-program shell-file-name)
(add-to-list 'term-unbind-key-list '"M-x")
(add-hook 'term-mode-hook
         '(lambda ()
            ;; C-h を term 内文字削除にする
            (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
            ;; C-y を term 内ペーストにする
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            ))
(global-set-key (kbd "C-c t") '(lambda ()
                                (interactive)
                                (multi-term)))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
