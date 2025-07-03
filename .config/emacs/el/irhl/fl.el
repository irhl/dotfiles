;;; fl.el -*- lexical-binding: t -*-
;; flowershop theme by irhl

;; a list of fonts that i use for this theme
;; FontAwesome         for the cloud icon spacing
;; Maple Mono NF       for the cloud icon symbol
;; Aporetic Sans Mono  for universal font

;; 06-07-2025 06:11:45 AM PST
;; defface isn't needed.
;; face-spec-set appears to assign the faces that have not
;; been created by defface, unlike custom-set-faces.

(defmacro define-face-specs (&rest specs)
  `(dolist (spec ',specs)
     (face-spec-set (car spec) `((t ,(cadr spec))))))

(define-face-specs
 (rainbow-delimiters-depth-1-face    (:foreground "#c79d83"))
 (rainbow-delimiters-depth-2-face    (:foreground "#f2ba98"))
 (rainbow-delimiters-depth-3-face    (:foreground "#91cee2"))
 (rainbow-delimiters-depth-4-face    (:foreground "#f291e2"))
 (rainbow-delimiters-depth-5-face    (:foreground "#81c698"))
 (rainbow-delimiters-depth-6-face    (:foreground "#f2ba98"))
 (rainbow-delimiters-depth-7-face    (:foreground "#b4aae2"))
 (rainbow-delimiters-depth-8-face    (:foreground "#83cbb6"))
 (rainbow-delimiters-depth-9-face    (:foreground "#e9a498"))
 (rainbow-delimiters-depth-10-face   (:foreground "#c79d83"))
 (rainbow-delimiters-depth-11-face   (:foreground "#c79d83"))
 (rainbow-delimiters-depth-12-face   (:foreground "#c79d83"))
 (rainbow-delimiters-base-error-face (:foreground "#e27294"))

 (font-lock-builtin-face             (:foreground "#918b8b"))
 (font-lock-comment-face             (:foreground "#d2cbc6"))
 (font-lock-comment-delimiter-face   (:foreground "#d2cbc6"))
 (font-lock-doc-string-face          (:foreground "#d2cbc6"))
 (font-lock-constant-face            (:foreground "#918b8b"))
 (font-lock-function-name-face       (:foreground "#918b8b"))
 (font-lock-keyword-face             (:foreground "#5e5958"))
 (font-lock-string-face              (:foreground "#918b8b"))
 (font-lock-type-face                (:foreground "#918b8b"))
 (font-lock-variable-name-face       (:foreground "#918b8b"))
 (font-lock-preprocessor             (:foreground "#918b8b"))
 (font-lock-warning-face             (:foreground "#918b8b"))

 (cursor                             (:background "#9b9292"))
 (mc/cursor-face                     (:foreground "#cfedf7"))
 (minibuffer-prompt                  (:foreground "#9b9292"))
 (hl-line                            (:foreground "#d2cbc6" :background "#fcf5ee"))
 (isearch                            (:background "#e5dddc" :foreground "#5e5958"))
 (highlight                          (:background "#e5dddc" :foreground "#5e5958"))
 (lazy-highlight                     (:background "#e5dddc" :foreground "#5e5958"))
 (show-paren-mismatch                (:background "#f5cac3" :foreground "#f5cac3"))
 (show-paren-match                   (:background "#9b9292" :foreground "#efe9e3"))
 (region                             (:background "#efe9e3" :foreground "#9b9292"))

 (link                               (:foreground "#9b9292"))
 (button                             (:foreground "#d3c2bb" :background "#ebebeb" :underline nil))
 (escape-glyph                       (:foreground "#9b9292"))
 (help-key-binding                   (:foreground "#d2cbc6" :box nil))
 (warning                            (:foreground "#f5cac3"))
 (window-divider                     (:foreground "#fcf5ee"))
 (window-divider-first-pixel         (:foreground "#fcf5ee"))
 (window-divider-last-pixel          (:foreground "#fcf5ee"))
 (vertical-border                    (:foreground "#f00000"))

 (dired-directory                    (:foreground "#7a7473"))
 (dired-header                       (:foreground "#f00000"))
 (dired-ignored                      (:foreground "#f00000"))
 (dired-marked                       (:foreground "#d2cbc6"))
 (dired-mark                         (:foreground "#f5cac3"))
 (dired-set-id                       (:foreground "#5e5958"))
 (dired-special                      (:foreground "#5e5958"))
 (dired-warning                      (:foreground "#5e5958"))
 (dired-symlink                      (:foreground "#9b9292"))
 (dired-broken-symlink               (:foreground "#b6d5ed" :background  unspecified))

(default
 (:foreground "#5e5958"
  :background "#fcf5ee"
  :family "Aporetic Serif Mono"
  :weight bold
  :height 100))

(icon
 (:family      "FontAwesome"
  :height       1.0
  :weight       bold
  :foreground  "#c0b4c1"
  :background  "#eddeef"
  :box (:color "#eddeef" :line-width 4 :style nil)))

(icon-title
 (:family      "Aporetic Sans Mono"
  :weight       bold
  :foreground  "#cabdcc"
  :background  "#f7e8f9"
  :box (:color "#f7e8f9" :line-width 3 :style nil)))

(line-state
 (:family      "Aporetic Sans Mono"
  :weight       bold
  :foreground  "#dccede"
  :background  "#f7e8f9"
  :box (:color "#f7e8f9" :line-width 3 :style nil)))

(line-text
 (:family      "Aporetic Sans Mono"
  :weight       bold
  :foreground  "#cabdcc"
  :background  "#f7e8f9"
  :overline    "#f7e8f9"
  :box (:color "#f7e8f9" :line-width 3 :style nil)))

(line-blank
 (:family      "Aporetic Sans Mono"
  :weight       bold
  :foreground  "#fcf5ee"
  :background  "#fcf5ee")))
