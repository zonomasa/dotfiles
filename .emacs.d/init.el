; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

;;--------------------------------------------------------------------------------
;; Emacs Server
;;--------------------------------------------------------------------------------
;; For Windows, refer to http://sky-y.hatenablog.jp/entry/20111224/1324714853
;(if window-system (progn
;                      (server-start)))
(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t)
(setq server-socket-dir "~/.emacs.d")
(unless (server-running-p)
  (server-start))

(setenv "LANG" "C")


;;--------------------------------------------------------------------------------
;; Org-mode
;;--------------------------------------------------------------------------------
;; Following settings are from http://doc.norang.ca/org-mode.html#TasksAndStates

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
(require 'org)

;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)


;; List of org files
(setq org-agenda-files (quote ("~/org/")))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING" "MAIL"))))


(setq org-todo-keyword-faces
      (quote (
              ;;("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "cyan" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("MAIL" :foreground "magenta" :weight bold)
              ("PHONE" :foreground "magenta" :weight bold))))


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
                (tags-todo "MONEY")
                (tags-todo "READING")))
                ("O" "(Office)Agenda and Lists with context"
                ((agenda)
                (tags-todo "ETC"))))))


(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/refile.org")
               "* TODO %?\n\n")
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
              ("i" "Idea " entry (file+datetree "~/org/idea.org")
               "* IDEA %? :IDEA:\n")
              ("h" "Habit" entry (file "~/org/refile.org")
               "* NEXT %?\n\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

(setq org-refile-targets (quote (("gtd.org" :maxlevel . 1)
                             ("someday.org" :level . 2))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t))) ; this line activates dot


;;--------------------------------------------------------------------------------
;; scratchの初期メッセージ
;;--------------------------------------------------------------------------------
(setq initial-scratch-message "HOGE")


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
    wgrep
    tabbar
    popwin
    popup
    markdown-mode
    elscreen
    color-moccur
    cmake-mode
    c-eldoc
    auto-complete
    helm
    multi-term
    undo-tree
    helm-gtags
    helm-c-moccur
    ac-mozc
    recentf-ext
    ))

(require 'cl)
(mapcar (lambda (x)
          (when (not (package-installed-p x))
            (package-install x)))
        my-packages)

;(require 'auto-install)
;(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install/"))
;(auto-install-update-emacswiki-package-name t)
;(auto-install-compatibility-setup)


;;--------------------------------------------------------------------------------
;; 外観
;;--------------------------------------------------------------------------------

;(when window-system
  (load-theme 'zenburn t)
;)
;; 画面透過
(set-frame-parameter nil 'alpha 96)
(setq frame-title-format (format "%%f - @%s" (system-name)))

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
  (if (eq system-type 'cygwin) (load "~/.emacs.d/init_windows.el")
    (if (eq system-type 'darwin) (load "~/.emacs.d/init_mac.el")
      (load "~/.emacs.d/init_linux.el"))))


;; ホスト毎の設定ファイル
(load (locate-user-emacs-file (concat "init_" (replace-regexp-in-string "\\..*" "" (system-name)))) t)


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
 '(custom-safe-themes
   (quote
    ("f024aea709fb96583cf4ced924139ac60ddca48d25c23a9d1cd657a2cf1e4728" default)))
 '(delete-auto-save-files t)
 '(display-time-mode t)
 '(global-auto-revert-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(org-agenda-files
   (quote
    ("~/org/gtd.org_archive" "~/org/gtd.org" "~/org/idea.org" "~/org/journal.org" "~/org/refile.org")))
 '(package-selected-packages
   (quote
    (recentf-ext ac-mozc helm-c-moccur helm-gtags undo-tree multi-term helm auto-complete auto-install c-eldoc cmake-mode color-moccur elscreen markdown-mode popup popwin tabbar wgrep zenburn-theme)))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(w32-symlinks-handle-shortcuts t))

;;--------------------------------------------------------------------------------
;; モードライン
;;--------------------------------------------------------------------------------
(display-time)                  ; 現在時刻
(column-number-mode t)          ; 列数表示
(which-function-mode t)         ; 現在の関数名


(defvar mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (eldoc-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . "")
    (undo-tree-mode . "")
    (helm-gtags-mode . " HG")
    (flymake-mode . " Fm")
    (auto-complete-mode . "")
    (helm-mode . "")
    ;; Major modes
    (lisp-interaction-mode . "Li")
    (python-mode . "P/l")
    (ruby-mode   . "R/l")
    (emacs-lisp-mode . "E/l")
    (markdown-mode . "M/d")))

(defun clean-mode-line ()
  (interactive)
  (loop for (mode . mode-str) in mode-line-cleaner-alist
        do
        (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
          (when old-mode-str
            (setcar old-mode-str mode-str))
          ;; major mode
          (when (eq mode major-mode)
            (setq mode-name mode-str)))))

(add-hook 'after-change-major-mode-hook 'clean-mode-line)


;;--------------------------------------------------------------------------------
;; ediff
;;--------------------------------------------------------------------------------
;; diff のウインドウを左右に並べる
(setq ediff-split-window-function 'split-window-horizontally)




;;--------------------------------------------------------------------------------
;; auto-complete
;;--------------------------------------------------------------------------------
(require 'auto-complete)
(require 'auto-complete-config)
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
(global-set-key "\C-cr" 'rgrep)


;;--------------------------------------------------------------------------------
;; Auto-Complete
;;--------------------------------------------------------------------------------
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)


;;--------------------------------------------------------------------------------
;; C 言語設定
;;--------------------------------------------------------------------------------

;; GDB
(load "~/.emacs.d/elisp/gdb.el")

(global-set-key [f12] 'compile)

;; インデントのデフォルト設定
(setq-default indent-tabs-mode nil)
(setq c-default-style "k&r")
(setq-default c-basic-offset 2)
(setq-default tab-width 2)


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
                                    '(("^[^\n]\\{100\\}\\(.*\\)$" 1 "gray" t)))
;;                                    '(("^[^\n]\\{100\\}\\(.*\\)$" 1 font-lock-warning-face t)))
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


;; 日付の挿入
(define-key global-map [f5]
  '(lambda ()
     (interactive)
     (insert (format-time-string "%Y-%m-%d"))))

(define-key global-map (kbd "<C-f5>")
  '(lambda ()
     (interactive)
     (insert (format-time-string "%m/%d(%a)"))))


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
;; Markdown-mode
;;--------------------------------------------------------------------------------
(setq auto-mode-alist (cons '("\\.\\(md\\|txt\\)$" . gfm-mode) auto-mode-alist))

; インデント動作がおかしい対策
; http://blog.shibayu36.org/entry/2015/08/04/190956
(add-hook 'markdown-mode-hook
          '(lambda ()
             (electric-indent-local-mode -1)))


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

;;--------------------------------------------------------------------------------
;; ac-mozc
;; https://github.com/narusemotoki/.emacs.d/tree/master/elisp/ac-mozc
;;--------------------------------------------------------------------------------
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

;;--------------------------------------------------------------------------------
;; undo-tree
;;--------------------------------------------------------------------------------
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)


;;--------------------------------------------------------------------------------
;; デフォルト文字コード
;; 冒頭に書くと何者かによって設定が上書きされてしまう。
;;--------------------------------------------------------------------------------
(set-default-coding-systems 'utf-8)


;;--------------------------------------------------------------------------------
;; Helm
;; http://d.hatena.ne.jp/a_bicky/20140104/1388822688
;;--------------------------------------------------------------------------------
(when (require 'helm-config nil t)
  (helm-mode 1)

  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x ;")   'helm-mini)
  (define-key global-map (kbd "C-x b")   'helm-buffers-list)


  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
;;  (define-key helm-c-read-file-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

  ;; Disable helm in some functions
  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))

  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "^\\^" input-pattern)
                        ;; '^' is a pattern for basename
                        ;; and not required because the directory name is prepended
                        (substring input-pattern 1)
                      (concat ".*" input-pattern))))))


  (when (require 'helm-c-moccur nil t)
    (global-set-key (kbd "M-o") 'helm-c-moccur-occur-by-moccur)
    (global-set-key (kbd "C-M-o") 'helm-c-moccur-dmoccur)
    (add-hook 'dired-mode-hook
              '(lambda ()
                 (local-set-key (kbd "O") 'helm-c-moccur-dired-do-moccur-by-moccur)))
    (global-set-key (kbd "C-M-s") 'helm-c-moccur-isearch-forward)
    (global-set-key (kbd "C-M-r") 'helm-c-moccur-isearch-backward))
  (when (require 'helm-gtags nil t)
    (add-hook 'helm-gtags-mode-hook
              '(lambda ()
                 (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
                 (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
                 (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
                 (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)
                 (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
                 (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
                 ))
    (add-hook 'c-mode-hook 'helm-gtags-mode))
)

;;--------------------------------------------------------------------------------
;; ACE-JUMP
;;--------------------------------------------------------------------------------
;; ヒント文字に使う文字を指定する
(setq ace-jump-mode-move-keys
      (append "asdfghjkl;:]qwertyuiop@zxcvbnm,." nil))
;; ace-jump-word-modeのとき文字を尋ねないようにする
(setq ace-jump-word-mode-use-query-char nil)
(global-set-key (kbd "C-<") 'ace-jump-char-mode)
(global-set-key (kbd "C->") 'ace-jump-word-mode)
(global-set-key (kbd "C-M-<") 'ace-jump-line-mode)


;;--------------------------------------------------------------------------------
;; recentf-ext
;;--------------------------------------------------------------------------------
(when (require 'recentf nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  (setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1)
  (require 'recentf-ext))


;;--------------------------------------------------------------------------------
;; Markdown mode
;;--------------------------------------------------------------------------------
