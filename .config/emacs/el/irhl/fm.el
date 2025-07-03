;;; fm.el -*- lexical-binding: t -*-

(defun dired-marked-files-count ()
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (and (eq (char-after) dired-marker-char)
                   (dired-get-filename nil t))
          (setq count (1+ count)))
        (forward-line 1)))
    count))

(defun dired-get-marked-files-total ()
  (interactive)
  (message "%d" (dired-marked-files-count)))





(defun my/dired-previous-directory ()
  (interactive)
  (if (not (derived-mode-p 'dired-mode))
      (dired-jump)
    (dired "..")))

(defun my/dired-file-copy-path ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (when file
      (kill-new file)
      (message "Copied: %s" file))))

(defvar my/dired-file-open-list
  '(("docx\\|pdf"
     . "firefox")
    ("jpg\\|jpeg\\|png\\|webp\\|gif\\|mp4\\|mov\\|mkv\\|webm\\|wmv"
     . "mpv")
    ("mp3\\|flac"
     . "firefox")))

(defun my/dired-file-open ()
  (interactive)
  (when (eq major-mode 'dired-mode)
    (let* ((file (dired-get-file-for-visit))
           (lower (downcase file))
           (match (seq-find
                   (lambda (entry)
                     (string-match (format "\\.\\(%s\\)$" (car entry)) lower))
                   my/dired-file-open-list)))
      (if match
          (let* ((program (cdr match))
                 (command (format "nohup %s \"%s\" >/dev/null 2>&1 &" program file)))
            (call-process-shell-command command))
        (find-file file)))))





;; hide stuff like date of last modification
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; hide "." ".." in dired
(setq dired-omit-files "^\\.$\\|^\\.\\.$")
(add-hook 'dired-mode-hook #'dired-omit-mode)

(defun my/dired-ui-prepend-icons ()
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((filename (dired-get-filename nil t)))
          (when filename
            (dired-move-to-filename)
            (insert (if (file-directory-p filename) " " " "))))
        (forward-line 1)))))

(add-hook 'dired-after-readin-hook #'my/dired-ui-prepend-icons)

;; hide directory path
(defun my/dired-ui-clear-home-path ()
  "visually hide the initial absolute path line ('/...')"
  (when (derived-mode-p 'dired-mode)
    (save-excursion
      (goto-char (point-min))
      ;; match a line starting with optional whitespace followed by a slash
      (when (looking-at "^[ \t]*/.*\n?")
        (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov 'display "")
          (overlay-put ov 'evaporate t))))))

(add-hook 'dired-after-readin-hook #'my/dired-ui-clear-home-path)





;; bounce/loop scroll navigation in dired like in nnn
;;
;; we first need to put a newline, so we could
;; scroll from top to bottom, unlike the bottom
;; which already has eobp below.
;;
;; we use the eobp to know if the scrolling
;; has passed through the filename which is the
;; only solution i could think of.
(defvar-local my/dired-visual-newline-added nil)

(defun my/dired-visual-newline-insert ()
  (when (eq major-mode 'dired-mode)
    (unless my/dired-visual-newline-added
      (save-excursion
        (goto-char (point-min))
        (let ((ov (make-overlay (point) (point)))
              (dir (abbreviate-file-name default-directory)))
          (overlay-put ov 'before-string
                       (propertize (format "  PATH %s\n\n" dir)
                                   'face 'font-lock-comment-face))
          (overlay-put ov 'my/dired-visual-newline t)
          (setq my/dired-visual-newline-added t))))))

(defun my/dired-visual-newline-apply ()
  "apply visual newline to visible dired buffers."
  (dolist (win (window-list))
    (with-current-buffer (window-buffer win)
      (my/dired-visual-newline-insert))))

(add-hook 'dired-mode-hook #'my/dired-visual-newline-insert)
(add-hook 'window-configuration-change-hook #'my/dired-visual-newline-apply)

;; setup bounce/loop scrolling
(defun my/dired-first-file-pos ()
  "return the point position of the first file/dir"
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (not (dired-get-filename nil t)))
      (forward-line 1))
    (point)))

(defun my/dired-last-file-pos ()
  "return the point position of the last file/dir"
  (save-excursion
    (goto-char (point-max))
    (while (and (not (bobp))
                (not (dired-get-filename nil t)))
      (forward-line -1))
    (point)))

(defun my/dired-bounce-scroll ()
  "scroll from top to bottom, only triggers if point is not on a file/dir"
  (when (and (eq major-mode 'dired-mode)
             (not (dired-get-filename nil t))) ; <-- fix: don't jump yet if its a file
    (let ((first (my/dired-first-file-pos))
          (last  (my/dired-last-file-pos)))
      (cond
       ((< (point) first)
        (goto-char last)
        (dired-move-to-filename))
       ((> (point) last)
        (goto-char first)
        (dired-move-to-filename))))))

(defun my/dired-bounce-scroll-apply ()
  (add-hook 'post-command-hook #'my/dired-bounce-scroll nil t))

(add-hook 'dired-mode-hook #'my/dired-bounce-scroll-apply)





;; hide cursor in dired-mode
(defvar-local my/dired-visual--active nil)

(defun my/dired-visual-setup ()
  "enable stuff when entering dired"
  (when (derived-mode-p 'dired-mode)
    (hl-line-mode 1)
    (setq-local cursor-type nil)
    (setq my/dired-visual--active t)
    (add-hook 'post-command-hook #'my/dired-visual-cleanup nil t)))

(defun my/dired-visual-cleanup ()
  "cleanup stuff when leaving dired"
  (unless (derived-mode-p 'dired-mode)
    (when my/dired-visual--active
      (hl-line-mode -1)
      (kill-local-variable 'cursor-type)
      (setq my/dired-visual--active nil)
      (remove-hook 'post-command-hook #'my/dired-visual-cleanup t))))

(add-hook 'dired-mode-hook #'my/dired-visual-setup)
