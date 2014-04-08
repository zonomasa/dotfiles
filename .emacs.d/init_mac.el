; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;;--------------------------------------------------------------------------------
;; キー設定
;;--------------------------------------------------------------------------------
;; CmdとOptの入れ替え
;;(setq ns-command-modifier (quote meta))
;;(setq ns-alternate-modifier (quote super))

(setq default-input-method "MacOSX")

;;--------------------------------------------------------------------------------
;; 外観
;;--------------------------------------------------------------------------------
;;; フォントセットを作る
;;; http://d.hatena.ne.jp/setoryohei/20110117/1295336454
;; (let* ((fontset-name "myfonts") ; フォントセットの名前
;;        (size 14) ; ASCIIフォントのサイズ [9/10/12/14/15/17/19/20/...]
;;        (asciifont "Inconsolata") ; ASCIIフォント
;; ;       (jpfont "Hiragino Maru Gothic ProN") ; 日本語フォント
;;        (font (format "%s-%d:weight=normal:slant=normal" asciifont size))
;;        (fontspec (font-spec :family asciifont))
;;        (jp-fontspec (font-spec :family jpfont))
;;        (fsn (create-fontset-from-ascii-font font nil fontset-name)))
;;   (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
;;   (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
;;   (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec) ; 半角カナ
;;   (set-fontset-font fsn '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
;;   (set-fontset-font fsn '(#x0370 . #x03FF) fontspec) ; ギリシャ文字
;;   )

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
                   :height 140)
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty"))

