; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-

(set-face-attribute 'default nil
                   :family "Ricty"
                   :height 120)
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Ricty"))

;;(add-to-list 'default-frame-alist '(font . "Ricty-13"))

;; Ubuntu can use it.
;; Need to exec following command.
;; $ sudo apt-get install emacs-mozc emacs-mozc-bin
(setq header-line-format nil)
(define-key global-map (kbd "C-;") 'toggle-input-method)
(when (require 'mozc nil t)
  (set-language-environment "Japanese")
  (setq default-input-method "japanese-mozc")
;;  (setq mozc-candidate-style 'overlay)
  (setq mozc-candidate-style 'echo-area)
  (set-face-attribute 'mozc-cand-overlay-even-face 'nil
                      :background "gray" :foreground "black")
  (set-face-attribute 'mozc-cand-overlay-odd-face 'nil
                      :background "gray" :foreground "black"))


