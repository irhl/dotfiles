;;; fo.el -*- lexical-binding: t -*-

(defun my/delete-window (&optional window)
  "Delete WINDOW. If it's the only window, kill the buffer instead."
  (interactive)
  (let ((win (or window (selected-window))))
    (if (one-window-p t)
        (kill-buffer (window-buffer win))
      (delete-window win))))

(defun jump-to-match-paren ()
  "If on a paren, jump to its match."
  (interactive)
  (cond
   ((looking-at-p "\\s(") (forward-sexp 1))
   ((looking-back "\\s)" 1) (backward-sexp 1))
   (t (message "Not on a parenthesis."))))

(defun my/pull-line ()
  (interactive)
  (let* ((start (point))
         (end (line-end-position))
         (text (string-trim-left (buffer-substring start end))))
    (delete-region start end)
    (insert text)
    ;; restore cursor pos
    (goto-char start)))

(defun my/kill-line ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (looking-at "^$")
        (kill-whole-line)
      (kill-line))))

;; unrelated stuff
(defun my/machine-capture ()
  (interactive)
  (let ((choice (ido-completing-read "buffer: " (seq-remove (lambda (name) (string-prefix-p " " name))
                                                         (mapcar #'buffer-name (buffer-list))))))
    (with-current-buffer choice
      (clipboard-kill-ring-save (point-min) (point-max))
      (message "buffer content copied to clipboard!"))))

(defun my/machine-export ()
  (interactive)
  (if buffer-file-name
      (let* ((backup-dir  (expand-file-name "hist-back/" user-emacs-directory))
             (backup-name (read-string "file-name: " (file-name-nondirectory buffer-file-name)))
             (backup-path (expand-file-name backup-name backup-dir)))
        (make-directory backup-dir t)
        (copy-file buffer-file-name backup-path t)
        (message "file exported to %s" backup-path))
    (message "no file associated with this buffer")))

;; compiler stuff linux
(defvar my/term-process nil
  "the currently running foot terminal process.")

(defun my/machine-instructions ()
  (interactive)
  (let* ((file (file-name-nondirectory buffer-file-name))
         (base (file-name-sans-extension file))
         (dir (file-name-directory buffer-file-name))
         (cmd (format "make %s && ./%s; sleep 10" base base)))

    ;; kill previous terminal if it's still running
    (when (and my/term-process
               (process-live-p my/term-process))
      (kill-process my/term-process)
      (setq my/term-process nil))

    ;; launch new, store the process
    (setq my/term-process
          (start-process
           "foot-terminal"
           nil
           "foot" "-e" "bash" "-c"
           (format "cd %s && %s" dir cmd)))))
