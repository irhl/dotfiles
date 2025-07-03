;;;; init.el -*- lexical-binding: t -*-

(unless (>= emacs-major-version 29)
  (error "This config requires Emacs 29 or higher"))

(prefer-coding-system 'utf-8)

;; base directories
(setq default-directory user-emacs-directory)
(defconst my/elisp-dir (expand-file-name "el/" user-emacs-directory))

;; cache/misc files
(setq custom-file   (expand-file-name "var/cc.el" user-emacs-directory))
(setq savehist-file (expand-file-name "var/mh.el" user-emacs-directory))

(with-eval-after-load 'multiple-cursors
  (setq mc/list-file (expand-file-name "var/mc.el" user-emacs-directory)))





;; package loading 1
(defvar my/package-loaded nil)

(defun my/load-package ()
  (unless my/package-loaded
    (load-file (expand-file-name "pack.el" user-emacs-directory))
    (setq my/package-loaded t)
    (message "package loaded!")))

(defun my/load-package-if-graphic (frame)
  (when (display-graphic-p frame)
    (my/load-package)))

(if (display-graphic-p)
    (my/load-package)
  (add-hook 'after-make-frame-functions #'my/load-package-if-graphic))

;; package loading 2
(defvar my/reload-failed nil)

(defvar my/reload-list
  '("irhl/kb.el"
    "irhl/fo.el"
    "irhl/fm.el"
    "irhl/fl.el"
    "irhl/fx.el"
    "irhl/fs.el"
    "jcfk/dired-nnn.el"
    ))

(defun reload ()
  (interactive)
  (setq my/reload-failed nil)
  (dolist (file my/reload-list)
    (let ((path (expand-file-name file my/elisp-dir)))
      (condition-case err
          (progn
            (load path)
            (message "loaded: %s" path))
        (error
         (push file my/reload-failed)
         (message "Error loading %s: %s" file (error-message-string err)))))))

;; initial package load
(reload)





;; accessories
(setq visible-bell nil
      ring-bell-function 'ignore
      server-client-instructions nil
      inhibit-startup-screen t
      initial-buffer-choice nil
      confirm-kill-processes nil
      confirm-nonexistent-file-or-buffer nil)

(setq make-backup-files nil
      create-lockfiles nil
      auto-save-default nil
      auto-save-list-file-prefix nil)

(setq history-length 25)
(savehist-mode 1)

;; y/n thing
(setq use-short-answers t)

;; makes tabs show 4 spaces wide
(setq tab-width 4)

;; use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)

;; word wrapping off
(setq-default truncate-lines t)

;; hide dollar sign at the end of truncated lines
(set-display-table-slot standard-display-table 0 ?\ )

;; trim trailing whitespaces on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; no cursor blink
(blink-cursor-mode 0)

;; show matching pairs
(show-paren-mode 1)

;; show cursor line
(global-hl-line-mode 0)

;; ensures replacement in selected region
(delete-selection-mode 1)

;; disable that yellOw selection thing
(setq select-enable-secondary nil)

;; disable shift selection
(setq shift-select-mode nil)

;; no cursor on inactive windows
(setq-default cursor-in-non-selected-windows nil)

;; scroll line by line
(setq scroll-step 1
      scroll-margin 5
      scroll-conservatively 10000)

;; no unwanted gui elements
(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq use-dialog-box nil
      use-file-dialog nil)

;; frame base customization
(setq default-frame-alist
      '((left-fringe . 0)
        (right-fringe . 0)
        (internal-border-width . 24)
        (vertical-scroll-bars . nil)))

;; no frame resizing when font is enlarged
(setq frame-inhibit-implied-resize t)

;; startup frame resize: half width, height
(add-to-list 'default-frame-alist
             (cons 'width (/ (display-pixel-width)
                             (frame-char-width) 1)))

(add-to-list 'default-frame-alist
             (cons 'height (/ (display-pixel-height)
                              (frame-char-height) 1)))

;; no unwanted single line format
;; i use tab-bar-format for making a statusline
(setq-default mode-line-format nil)
(setq-default header-line-format nil)

;; window base customization
(setq window-divider-default-bottom-width 0)
(setq window-divider-default-right-width 25)
(setq window-divider-default-places t)
(window-divider-mode 1)

;; auto split vertically (side-by-side)
;; when emacs itself spawns a window
(setq split-width-threshold 0
      split-height-threshold nil)

(setq display-buffer-base-action
      '((display-buffer-reuse-window display-buffer-same-window)))

;;  +--------------+--------------+
;;  |              |              |
;;  |   window 0   |   window 1   |
;;  |              |              |
;;  +--------------+--------------+

(defun buf-window-limit ()
  (let ((all (window-list))
        (wins nil))

    ;; filter out minibuffer window
    (setq wins nil)
    (dolist (w all)
      (unless (window-minibuffer-p w)
        (setq wins (cons w wins))))
    (setq wins (nreverse wins))

    ;; delete extra windows beyond two
    (while (> (length wins) 2)
      (delete-window (car (last wins)))
      (setq wins (butlast wins)))

    ;; balance if exactly two
    (when (= (length wins) 2)
      (balance-windows))))

(add-hook 'window-configuration-change-hook #'buf-window-limit)

(defun buf-window-swap () (interactive)
  ;; check if there are exactly two windows
  (if (/= (count-windows) 2)
      (message "need exactly two windows to swap.")

    ;; otherwise, get the list of windows
    (let* ((wins (window-list))
           ;; get the buffers of first two windows
           (buf1 (window-buffer (car wins)))
           (buf2 (window-buffer (cadr wins))))

      ;; swap the buffers displayed in the first two windows
      (set-window-buffer (car wins) buf2)
      (set-window-buffer (cadr wins) buf1))))

;; buffer base stuff
(defun buf-dired-list ()
(let (bufs)
  (dolist (buf (buffer-list) bufs)
    (with-current-buffer buf
        (when (derived-mode-p 'dired-mode)
          (push buf bufs))))))

;; buffer base stuff
(defun buf-dired-list ()
  (let (bufs)
    (dolist (buf (buffer-list) bufs)
      (with-current-buffer buf
        (when (derived-mode-p 'dired-mode)
          (push buf bufs))))))

(defun buf-dired-limit ()
  (let* ((current (current-buffer))
         (dired-bufs (buf-dired-list))
         (kill-cands (remove current dired-bufs)))
    (when (> (length dired-bufs) 4)
      (setq kill-cands
            (sort kill-cands
                  (lambda (a b)
                    (time-less-p (buffer-local-value 'buffer-display-time a)
                                 (buffer-local-value 'buffer-display-time b)))))
      (while (> (+ 1 (length kill-cands)) 4)
        (when (buffer-live-p (car kill-cands))
          (kill-buffer (car kill-cands)))
        (setq kill-cands (cdr kill-cands))))))

(add-hook 'dired-mode-hook #'buf-dired-limit)

;; organize ibuffer list
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Special" (or (name . "^\\*.*\\*$")))
         ("Directory" (mode . dired-mode)))))

(add-hook 'ibuffer-mode-hook
          (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;; rename scratch buffer
(defun buf-create-startup (&optional base-buffer)
  (let ((name "untitled.el")
        buf)
    (setq buf (or (get-buffer name)
                  base-buffer
                  (get-buffer-create name)))
    (with-current-buffer buf
      (set-visited-file-name name nil t)
      (rename-buffer name t)
      (emacs-lisp-mode))
    buf))

(setq initial-buffer-choice
      (lambda ()
        (let ((scratch (get-buffer "*scratch*")))
          (buf-create-startup scratch))))
