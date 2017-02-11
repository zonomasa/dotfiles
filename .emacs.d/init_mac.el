;;--------------------------------------------------------------------------------
;; 必要に応じて日本語入力をOFF
;;--------------------------------------------------------------------------------
(mac-auto-ascii-mode 1)

;;--------------------------------------------------------------------------------
;; キー設定
;;--------------------------------------------------------------------------------
;; CmdとOptの入れ替え
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

(setq default-input-method "MacOSX")

;;--------------------------------------------------------------------------------
;; 外観
;;--------------------------------------------------------------------------------

;; ;;; デフォルトのフレームパラメータでフォントセットを指定
;; (add-to-list 'default-frame-alist '(font . "fontset-myfonts"))

;;; フォントサイズの比を設定
(dolist (elt '(("^-apple-hiragino.*" . 1.2)
               (".*osaka-bold.*" . 1.2)
               (".*osaka-medium.*" . 1.2)
               (".*courier-bold-.*-mac-roman" . 1.0)
               (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
               (".*monaco-bold-.*-mac-roman" . 0.9)))
  (add-to-list 'face-font-rescale-alist elt))

;;; デフォルトフェイスにフォントセットを設定
;;; (これは起動時に default-frame-alist に従ったフレームが作成されない現象への対処)
;(set-face-font 'default "fontset-myfonts")


(set-face-attribute 'default nil
                   :family "Ricty"
                   :height 160)
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty"))

