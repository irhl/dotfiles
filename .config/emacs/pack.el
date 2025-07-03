;;; package.el -*- lexical-binding: t -*-
(require 'package)

(setq package-archives
      '(("elpa"  . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(when (featurep 'use-package)

(use-package ahk-mode)
(use-package lua-mode)
(use-package powershell)
(use-package fontawesome)
(use-package multiple-cursors)

(use-package gcmh
  :ensure t
  :init
  (setq gcmh-idle-delay 5         ;; GC when idle for 5s
        gcmh-high-cons-threshold (* 100 1024 1024)) ;; 100MB during activity
  (gcmh-mode 1))

(use-package svg-lib)
(use-package svg-tag-mode
  :hook ((text-mode . svg-tag-mode)
         (prog-mode . svg-tag-mode))
  :config
  (defun svg-tag-make (tag &rest args)
    (let* ((face (or (plist-get args :face) 'svg-tag-default-face))
           (font-family (svg-tag--face-attribute face :family))
           (font-weight (or (plist-get args :font-weight)
                            (svg-tag--face-attribute face :weight)))
           (stroke (or (plist-get args :stroke) 0))
           (foreground (svg-tag--face-attribute face :foreground))
           (background (svg-tag--face-attribute face :background))
           (inverse (plist-get args :inverse))
           (tag (string-trim tag))
           (beg (or (plist-get args :beg) 0))
           (end (or (plist-get args :end)))
           (args (svg-tag--plist-delete args :stroke))
           (args (svg-tag--plist-delete args :foreground))
           (args (svg-tag--plist-delete args :background))
           (args (svg-tag--plist-delete args :face))
           (args (svg-tag--plist-delete args :inverse))
           (args (svg-tag--plist-delete args :beg))
           (args (svg-tag--plist-delete args :end)))
      (apply #'svg-lib-tag (substring tag beg end) nil
             :stroke stroke
             :font-family font-family
             :font-weight font-weight
             :foreground (if inverse background foreground)
             :background (if inverse foreground background)
             args)))
  (defun my/svg-tag-with-face (tag face)
    (apply #'svg-tag-make tag
           (plist-put (copy-sequence my/svg-tag-base-options) :face face))))

(use-package smartparens
  :hook ((prog-mode . smartparens-mode)
         (lisp-mode . smartparens-strict-mode)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook ((css-mode lua-mode emacs-lisp-mode) . rainbow-mode))

(use-package tree-sitter-langs)
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package undo-fu)
(use-package undo-fu-session
  :after undo-fu
  :config
  (setq undo-fu-session-directory (expand-file-name "hist-undo" user-emacs-directory)
        undo-fu-session-file-limit nil
        undo-fu-session-linear t)
  (undo-fu-session-global-mode)))

;; silence warning
(defvar my/warnings-to-kill nil)
(defun my/auto-kill-warnings-buffer (orig-fn &rest args)
  "Advice to kill *Warnings* buffer immediately after it's shown."
  (apply orig-fn args)
  (when-let ((buf (get-buffer "*Warnings*")))
    (when (get-buffer-window buf 'visible)
      (kill-buffer buf))))

(advice-add 'display-warning :around #'my/auto-kill-warnings-buffer)
