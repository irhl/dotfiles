;;; fl.el -*- lexical-binding: t -*-
;;
;; INSTALL:
;; load the theme as any other .el file.
;;
;;       ,-_,.
;;     ,( _  ))
;;     7 (_) 77
;;     ((   :))
;;      ~__>~'
;;       cY?'
;;       `l,__
;;        l7-'
;;       ;l
;;       _i_,
;;      l___l
;;      \___/ fl.el (flowershop by irhl)

(unless (>= emacs-major-version 29)
  (error
    "hola payaso fuck you no fudgee barr for you"))

(defmacro ext (modes &rest customizations)
  `(dolist (mode ,modes)
     (font-lock-add-keywords
      mode
      (append '(,@customizations)
              (mapcar (lambda (substitution)
                        `(,(car substitution)
                          (0 (progn
                               (put-text-property
                               (match-beginning 0) (match-end 0)
                               'display , (cadr substitution))))))
                      ',customizations)))))
(ext
 '(org-mode emacs-lisp-mode
   c-mode lua-mode sh-mode bash-mode ahk-mode powershell)

 ("\\<lambda\\>" "λ")
 ("\\<interactive\\>" "+")
 ("\\<defun\\>" "fn")
 ("\\<defmacro\\>" "macro")
 ("\\<defvar\\>" "var")

 ("\\_<\\([+-]?[0-9]+\\.?[0-9]*\\)\\_>"
  1 '(face (:foreground "#404040"))))

(ext
 '(org-mode)

 ("\\(\\#\\)" " ")

 ("^\\([ \t]*\\*+[ \t]\\)"
  1 '(face (:foreground "#f7f7f7"))))

(defface icon '((t nil)) "header active icon")
(defface icon-inactive '((t nil)) "header inactive icon.")

(custom-set-faces
 '(rainbow-delimiters-depth-1-face    ((t (:foreground "#c79d83"))))
 '(rainbow-delimiters-depth-2-face    ((t (:foreground "#ebc3a9"))))
 '(rainbow-delimiters-depth-3-face    ((t (:foreground "#84badd"))))
 '(rainbow-delimiters-depth-4-face    ((t (:foreground "#e889c6"))))
 '(rainbow-delimiters-depth-5-face    ((t (:foreground "#6ebb77"))))
 '(rainbow-delimiters-depth-6-face    ((t (:foreground "#ebc3a9"))))
 '(rainbow-delimiters-depth-7-face    ((t (:foreground "#a99de0"))))
 '(rainbow-delimiters-depth-8-face    ((t (:foreground "#83cbb6"))))
 '(rainbow-delimiters-depth-9-face    ((t (:foreground "#e9a498"))))
 '(rainbow-delimiters-depth-10-face   ((t (:foreground "#c79d83"))))
 '(rainbow-delimiters-depth-11-face   ((t (:foreground "#c79d83"))))
 '(rainbow-delimiters-depth-12-face   ((t (:foreground "#c79d83"))))
 '(rainbow-delimiters-base-error-face ((t (:foreground "#e27294"))))

 '(font-lock-builtin-face             ((t (:foreground "#918b8b"))))
 '(font-lock-comment-face             ((t (:foreground "#d2cbc6"))))
 '(font-lock-comment-delimiter-face   ((t (:foreground "#d2cbc6"))))
 '(font-lock-doc-face                 ((t (:foreground "#d2cbc6"))))
 '(font-lock-constant-face            ((t (:foreground "#9cd2bb"))))
 '(font-lock-function-name-face       ((t (:foreground "#bea292"))))
 '(font-lock-keyword-face             ((t (:foreground "#cfbbb0"))))
 '(font-lock-doc-string-face          ((t (:foreground "#d2cbc6"))))
 '(font-lock-string-face              ((t (:foreground "#5e5958"))))
 '(font-lock-type-face                ((t (:foreground "#5e5958"))))
 '(font-lock-variable-name-face       ((t (:foreground "#b3a7a9"))))
 '(font-lock-preprocessor             ((t (:foreground "#b3a7a9"))))
 '(font-lock-warning-face             ((t (:foreground "#5e5958"))))
 '(vertical-border                    ((t (:foreground "#f3ebeb"))))
 '(window-divider                     ((t (:foreground "#f5efe8"))))
 '(window-divider-first-pixel         ((t (:foreground "#f5efe8"))))
 '(window-divider-last-pixel          ((t (:foreground "#f5efe8"))))
 '(warning                            ((t (:foreground "#5e5958"
                                           :weight bold))))
 '(default                            ((t (:foreground "#5e5958"
                                           :background "#fcf5ee"))))
 ;; SECTION: menu
 '(minibuffer-prompt                  ((t (:foreground "#9a9392"))))
 '(help-key-binding                   ((t (:foreground "#d2cbc6"))))

 '(cursor                             ((t (:background "#9a9392"))))
 '(hl-line                            ((t (:background "#f5efe8"))))
 '(highlight                          ((t (:foreground "#f5cac3"))))
 '(show-paren-match                   ((t (:background "#faefea"))))
 '(show-paren-mismatch                ((t (:background "#f5cac3"))))
 '(region                             ((t (:background "#dcd6d3"))))
 '(link                               ((t (:foreground "#9a9392"))))
 '(button                             ((t (:foreground "#d3c2bb"))))
 '(dired-directory                    ((t (:foreground "#9a9392"))))

 ;; SECIION: org-mode
 '(org-level-1                     ((t (:height 1.5 :weight bold))))
 '(org-level-2                     ((t (:height 1.4 :weight bold))))
 '(org-level-3                     ((t (:height 1.3 :weight bold))))
 '(org-level-4                     ((t (:height 1.2 :weight bold))))
 '(org-level-5                     ((t (:height 1.1 :weight bold))))
 '(org-level-6                     ((t (:height 1.0 :weight bold))))
 '(org-document-title              ((t (:height 1.4 :weight bold))))

 '(org-todo
   ((t (:foreground "#e9c8c3"
        :weight bold
        :family "Comic Code Medium"))))

 '(org-headline-todo
   ((t (:height 0.9
        :foreground "#918b8b"
        :weight bold))))

 '(org-meta-line
   ((t (:foreground "#d2cbc6"
        :background "#f4ede6" ))))

 '(org-block ((t (:background "#f8f1ea" ))))
 '(org-table ((t (:foreground "#c9c6c3" ))))
 '(org-date  ((t (:foreground "#ccbfc2"
                  :underline nil))))

 ;; SECTION: fudgee barr
 '(icon
   ((t (:family "FontAwesome"
        :weight bold
        :background  "#f9dcce"
        :foreground  "#bca59b"
        :overline    "#f9dcce"
        :box (:color "#f9dcce"
              :line-width 3
              :style nil)))))
 '(icon-inactive
   ((t (:family "FontAwesome"
        :weight bold
        :background  "#e6ddd7"
        :foreground  "#acaeb1"
        :overline    "#e6ddd7"
        :box (:color "#e6ddd7"
              :line-width 3
              :style nil)))))
 '(mode-line
   ((t :weight bold
       :foreground  "#b6a7a0"
       :background  "#fae7de"
       :overline    "#fae7de"
       :box (:color "#fae7de"
             :line-width 2
             :style nil))))
 '(mode-line-inactive
   ((t :weight bold
       :foreground  "#acaeb1"
       :background  "#f0e8e2"
       :overline    "#f0e8e2"
       :box (:color "#f0e8e2"
             :line-width 2
             :style nil))))
 '(header-line
   ((t nil )))
 '(header-line-highlight
   ((t :box (:color nil))))
 '(mode-line-highlight
   ((t :box (:color nil)))))
