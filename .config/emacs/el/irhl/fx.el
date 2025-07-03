;;; fx.el -*- lexical-binding: t -*-
;; a place for testing art and stuff
;; this is where i keep stuff before
;; moving them to the regular /el files.

(defun apply-replacement (bindings)
  (dolist (binding bindings)
    (font-lock-add-keywords
     nil
     `((,(concat "\\b" (car binding) "\\b") 0
        (progn (put-text-property (match-beginning 0) (match-end 0)
                                 'display ,(cdr binding))))))))

(defun apply-replacement-hook ()
  "visually replace certain keywords with shorter alternatives."
  (apply-replacement
   '(("defun" . "fn")
     ("defmacro" . "macro")
     ("defvar" . "var"))))

(add-hook 'emacs-lisp-mode-hook 'apply-replacement-hook)




;;
(defun my/custom-font-lock-keywords ()
  "Custom font lock for text-mode and prog-mode."
  (font-lock-add-keywords
   nil
   '(("^\\([ \t]*•+\\)[ \t]"
      1 '(:inherit dired-marked)) )))

(add-hook 'text-mode-hook 'my/custom-font-lock-keywords)
(add-hook 'prog-mode-hook 'my/custom-font-lock-keywords)




;;
(setq my/svg-tag-base-options
      '(:font-weight bold
        :font-size 9.5
        :stroke 2.5
        :radius 2
        :margin 0
        :beg 2 :end -2))

(setq svg-tag-tags
      `(("//[0-9a-zA-Z- ]+?//" .
         ((lambda (tag)
            (my/svg-tag-with-face tag 'font-lock-comment-face))))

        ("//LOAD//" .
         ((lambda (tag)
            (my/svg-tag-with-face tag 'font-lock-builtin-face))))))




;;
;; outlining section faces
(defface header1
  '((t (:foreground "#606060"
        :weight bold
        :height 1.3
        :box (:color "#f5f5f5" :line-width 4 :style nil)
        ))) "")

(defface header2
  '((t (:foreground "#606060"
        :weight bold
        :height 1.2
        :box (:color "#f5f5f5" :line-width 4 :style nil)
        ))) "")

(defface header3
  '((t (:foreground "#bababa"
        :weight bold
        :height 1.2
        :box (:color "#f5f5f5" :line-width 4 :style nil)
        ))) "")

(defface textPink
  '((t (:foreground "#f7bbcb"
        :weight bold))) "")

(defface textCyan
  '((t (:foreground "#b0e1e5"
        :weight bold)))"")




;;
;; outlining section main
;; Define your custom delimiters and associated faces
(defvar ioutline-delimiters
  '(("##" . header1)
    ("#@" . header2)
    ("#!" . header3)
    ("**" . textPink)
    ("!!" . textCyan)))

(defvar-local ioutline-overlays nil
  "List of overlays currently applied by ioutline-mode.")

(defun ioutline-clear-overlays ()
  "Remove all overlays created by ioutline-mode."
  (mapc #'delete-overlay ioutline-overlays)
  (setq ioutline-overlays nil))

(defun ioutline-apply-overlays (&rest _)
  (ioutline-clear-overlays)
  (let* ((delims (mapcar #'car ioutline-delimiters))
         (re (concat
              "\\(" (mapconcat #'regexp-quote delims "\\|") "\\)" ; 1: open
              "[ \t]*\\(.*?\\)[ \t]*"
              "\\(" (mapconcat #'regexp-quote delims "\\|") "\\)"))) ; 3: close
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward re nil t)
        (let* ((open (match-string 1))
               (close (match-string 3))
               (face (cdr (assoc open ioutline-delimiters))))
          (when (and face (string= open close))
            (let ((o1 (make-overlay (match-beginning 1) (match-end 1)))
                  (o2 (make-overlay (match-beginning 3) (match-end 3)))
                  (o3 (make-overlay (match-beginning 2) (match-end 2))))
              (overlay-put o1 'display "")
              (overlay-put o1 'evaporate t)
              (overlay-put o2 'display "")
              (overlay-put o2 'evaporate t)
              (overlay-put o3 'face face)
              (push o1 ioutline-overlays)
              (push o2 ioutline-overlays)
              (push o3 ioutline-overlays))))))))

(define-minor-mode ioutline-mode
  "Minor mode to highlight and hide custom delimited inline text sections."
  :lighter " iOutline"
  (if ioutline-mode
      (progn
        (add-hook 'after-change-functions #'ioutline-apply-overlays nil t)
        (ioutline-apply-overlays))
    (remove-hook 'after-change-functions #'ioutline-apply-overlays t)
    (ioutline-clear-overlays)))

;; Enable in text and programming buffers
(add-hook 'text-mode-hook #'ioutline-mode)
(add-hook 'prog-mode-hook #'ioutline-mode)





;; this is all intended to replace my stuff that depend on org,
;; i no longer use org to create outlines.

;; this was created before i discovered the outline minor mode lol
;; but whatever, i already wasted time on this so let's keep expanding.

;; usage: #TESSasdasd#TEST
;; this is different from the outline minor mode,
;; because this requires you to surround the text
;; with delimiters to apply the outline.
