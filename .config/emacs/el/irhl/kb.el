;;; kb.el -*- lexical-binding: t; -*-

;; kb provides stuff to reconstruct modal editing,
;; this does not completely override the global-map
;; instead it acts as a layer and does take priority
;; than the default edit behavior.

;; kb will not completely override the global-map,
;; the editor itself is vast, we must explore more
;; of the defaults than seek ignorance.

;; check if emacs starts with normal-map
;; otherwise, edit state will show NULL.
(defvar edit-state 'NULL)

(defvar initial-global-map (current-global-map)
  "snapshot of the current-global-map before modal editing is applied.")

(defvar map-normal (make-sparse-keymap))
(set-keymap-parent map-normal initial-global-map)

(defvar map-insert (make-sparse-keymap))
(set-keymap-parent map-insert initial-global-map)

(defun switch-to-normal-mode () (interactive)
  (setq edit-state 'NORMAL)
  (use-global-map map-normal))

(defun switch-to-insert-mode () (interactive)
  (unless buffer-read-only
    (setq edit-state 'INSERT)
    (use-global-map map-insert)))





(defun switch-abort () (interactive)
  (switch-to-normal-mode)
  (while (> (recursion-depth) 0)
    (abort-recursive-edit))
  (when (bound-and-true-p multiple-cursors-mode)
    (mc/keyboard-quit))
  (keyboard-quit))

(add-hook 'minibuffer-setup-hook #'switch-to-insert-mode)
(add-hook 'minibuffer-inactive-mode-hook #'switch-to-normal-mode)

(add-hook 'isearch-mode-hook     #'switch-to-insert-mode)
(add-hook 'isearch-mode-end-hook #'switch-to-normal-mode)





(defun kb-define-key (mode key cmd)
  (pcase mode
    (:normal (define-key map-normal (kbd key) cmd))
    (:insert (define-key map-insert (kbd key) cmd))
    (:dired  (with-eval-after-load 'dired
               (define-key dired-mode-map (kbd key) cmd)))
    (:diredb (with-eval-after-load 'dired
               (define-key dired-mode-map (kbd key)
                 `(lambda () (interactive) (dired ,cmd))))) ))

(defmacro kb (mode &rest bindings)
  `(dolist (binding ',bindings)
     (kb-define-key ,mode (car binding) (cdr binding))))

;; unset unassigned keys in normal-map
(setq my/char-codes

        ;; symbols and punctuation
      '(33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
        58 59 60 61 62 63 64 91 92 93 94 95 96

        ;; lowercase letters a-z
        97 98 99 100 101 102 103 104 105 106 107 108
        109 110 111 112 113 114 115 116 117 118 119
        120 121 122

        ;; uppercase letters A-Z
        65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
        80 81 82 83 84 85 86 87 88 89 90

        ;; digits 0-9
        48 49 50 51 52 53 54 55 56 57))

(mapc (lambda (key)
        (define-key map-normal (kbd (char-to-string key)) nil))
      my/char-codes)

;; suspend-frame is assigned in <C-z>, <C-x C-z>
;; this command causes the cursor to be visually hidden.
;; it can be annoying if you have them assigned to undo,
;; it is best we ensure the command does nothing instead.
(fset 'suspend-frame (lambda () (interactive)))

;; apply normal-map
(switch-to-normal-mode)





(kb :insert
    ("<escape>"  . switch-abort)
    ("C-S-v"     . clipboard-yank)
    ("C-z"       . undo-only)
    ("C-r"       . undo-redo))

(kb :normal
    ("a"         . switch-to-insert-mode)
    ("<escape>"  . switch-abort)
    ("<home>"    . my/machine-export)
    ("<print>"   . my/machine-capture)
    ("<f5>"      . reload)
    ("M-i"       . my/machine-instructions)
    ("M-x"       . execute-extended-command)
    ("M-X"       . eval-expression)
    ("C-h c"     . describe-char)
    ("C-h k"     . describe-key)
    ("C-h m"     . describe-mode)
    ("C-h f"     . describe-function)
    ("C-h d"     . list-faces-display)
    ("C-<prior>" . backward-page)
    ("C-<next>"  . forward-page)
    ("<prior>"   . scroll-down)
    ("<next>"    . scroll-up)
    ("<left>"    . backward-char)
    ("<right>"   . forward-char)
    ("<down>"    . next-line)
    ("<up>"      . previous-line)
    ("E"         . move-beginning-of-line)
    ("e"         . move-end-of-line)
    ("g"         . beginning-of-buffer)
    ("G"         . end-of-buffer)
    ("j"         . jump-to-match-paren)
    ("W"         . my/pull-line)
    ("d"         . my/kill-line)
    ("D"         . open-line)
    ("v"         . set-mark-command)
    ("V"         . kill-ring-save)
    ("C-v"       . clipboard-yank)
    ("C-z"       . undo-only)
    ("C-r"       . undo-redo)
    ("C-s"       . isearch-forward)
    ("C-x f"     . find-file)
    ("C-x b"     . ibuffer)
    ("C-x s"     . save-buffer)
    ("C-x 4"     . buf-window-swap)
    ("q"         . my/delete-window)
    ("C-x 1"     . delete-other-windows)
    ("C-x 1"     . split-window-below)
    ("C-x 3"     . split-window-right)
    ("<M-up>"    . windmove-up)
    ("<M-down>"  . windmove-down)
    ("<M-left>"  . windmove-left)
    ("<M-right>" . windmove-right)
    ("<C-up>"    . shrink-window)
    ("<C-down>"  . enlarge-window)
    ("<C-left>"  . shrink-window-horizontally)
    ("<C-right>" . enlarge-window-horizontally)
    ("<S-prior>" . mc/mark-previous-like-this)
    ("<S-next>"  . mc/mark-next-like-this))

(kb :dired
    ("<left>"    . my/dired-previous-directory)
    ("<right>"   . my/dired-file-open)
    ("RET"       . my/dired-file-open)
    ("y"         . my/dired-file-copy-path)
    ("a"         . dired-mark-subdir-files)
    ("A"         . dired-toggle-marks)
    ("SPC"       . dired-nnn-toggle-mark)
    ("p"         . dired-nnn-paste)
    ("v"         . dired-nnn-move)
    ("q"         . my/delete-window))

(kb :diredb
    ("be"        . user-emacs-directory)
    ("ba"        . "~/")
    ("bs"        . "~/.local/share/")
    ("bc"        . "~/.config/")
    ("bd"        . "~/Downloads/")
    ("bp"        . "~/Projects/")
    ("bm"        . "~/Music/")
    ("bg"        . "~/git/"))
