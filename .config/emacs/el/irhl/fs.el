;; fs.el -*- lexical-binding: t -*-
;; editor statusline

(defun get-mark-count ()
  "return a string like [+N] if N files are marked in Dired."
  (when (derived-mode-p 'dired-mode)
    (let ((count (dired-marked-files-count)))
      (when (> count 0)
        (format "[+%d] " count)))))

(defun my/tab-bar-padding-top ()
  (propertize " \n" 'display '(raise -0.4) 'face 'line-blank))

(defun my/tab-bar-divider ()
  (propertize " \n" 'display '(raise 0) 'face 'line-blank))

(defun my/tab-bar-top ()
  (let* ((mark (or (get-mark-count) ""))
         (edit-state (or edit-state "OTHERS"))
         (file (file-name-nondirectory (or buffer-file-name "Untitled")))

         (icon-str     "    ︎ ")
         (str-left-1   (format " %s " file))
         (str-left-2   (format "[%s] %s" edit-state mark))

         (line         (format-mode-line "%l"))
         (col          (format-mode-line "%c"))
         (str-right    (format " Ln %s, Col %s" line col))

         (right-width (string-width str-right))
         (padding `((space :align-to (- right ,(1+ right-width)))))

         (result (concat
                  (propertize icon-str     'face 'icon)
                  (propertize str-left-1   'face 'line-text)
                  (propertize str-left-2   'face 'line-state)
                  (propertize " "          'display padding 'face 'line-text)
                  (propertize str-right    'face 'line-text))))
    result))

(setq tab-bar-format
      '(my/tab-bar-padding-top
        my/tab-bar-top))

(tab-bar-mode 1)

;; for constant updates to support line/column indicator
(defun my/tab-bar-update ()
  (when (and (display-graphic-p)
             (bound-and-true-p tab-bar-mode))
    (setq tab-bar-format (copy-sequence tab-bar-format))
    (force-window-update (selected-window))))

(add-hook 'post-command-hook #'my/tab-bar-update)
