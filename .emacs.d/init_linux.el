(when window-system
(set-fontset-font
 nil 'japanese-jisx0208
   (font-spec :family "Ricty")))

(setq header-line-format nil)
(define-key global-map (kbd "C-;") 'toggle-input-method)
(define-key global-map (kbd "<hiragana-katakana>") 'toggle-input-method)
(when (require 'mozc nil t)
  (set-language-environment "Japanese")
  (setq default-input-method "japanese-mozc"))


