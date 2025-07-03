;;; dired-nnn.el --- nnn-like file actions for Dired -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Jacob Fong
;; Author: Jacob Fong <jacobcfong@gmail.com>
;; Version: 0.1
;; Homepage: https://github.com/jcfk/dired-nnn

;; Package-Requires: ((emacs "28.2"))

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; nnn-like file actions for Dired, inspired by dired-hacks/dired-ranger.

;; See https://github.com/jcfk/dired-nnn for more information.

;;; Code:

(require 'dired)
(require 'dired-aux)

(defgroup dired-nnn nil
  "Implementation of nnn-like file actions for Dired."
  :group 'dired-nnn)

(defcustom dired-nnn-mark-new-files nil
  "If non-nil, recently pasted or moved files will be marked."
  :type 'boolean
  :group 'dired-nnn)

(defun dired-nnn--marked-files ()
  "Return list of the absolute paths of marked files in all dired buffers."
  (seq-remove
   (lambda (fpath)
     (member (file-name-nondirectory fpath) '("." "..")))
   (delete-dups
    (mapcan
     (lambda (dir-and-buf)
       (when (buffer-live-p (cdr dir-and-buf))
         (with-current-buffer (cdr dir-and-buf)
           (let ((ret (dired-get-marked-files nil nil nil t nil)))
             (cond ((length= ret 1) nil)
                   ((and (length= ret 2) (eq (car ret) t))
                    (cdr ret))
                   (t ret))))))
     dired-buffers))))

(defun dired-nnn--unmark-all-files ()
  "Unmark all marked files across dired buffers."
  (mapc
   (lambda (dir-and-buf)
     (with-current-buffer (cdr dir-and-buf)
       (dired-unmark-all-files ?*)))
   dired-buffers))

(defun dired-nnn--get-dir-name-constructor (dir)
  "Return a function that renames files to under DIR.

See `dired-create-files'."
  (lambda (fpath)
    (file-name-concat dir (file-name-nondirectory fpath))))

;; TODO does this actually refuse to mark the dots?
;;;###autoload
(defun dired-nnn-toggle-mark ()
  "Toggle selection of the file or directory under the point.

Cannot mark '.' or '..'."
  (interactive)
  (when (not (derived-mode-p 'dired-mode))
    (error "Not a dired buffer."))
  (condition-case nil
      (if (member (dired-get-filename)
                  (dired-nnn--marked-files))
          (dired-unmark nil)
        (dired-mark nil))
    (error "Cannot mark '.' or '..'")))

;;;###autoload
(defun dired-nnn-paste (arg)
  "Copy marked dired files into the current dired directory.

With `dired-nnn-mark-new-files', mark newly pasted files. With prefix ARG,
preserve marks on the original files, overriding `dired-nnn-mark-new-files'."
  (interactive "P")
  (when (not (derived-mode-p 'dired-mode))
    (error "Not a dired buffer."))
  (let ((how-to-mark (cond (arg 'src)
                           (dired-nnn-mark-new-files 'dest)
                           (t nil)))
        (cwd (dired-current-directory))
        (marked-files (dired-nnn--marked-files)))
    (when (not (eq how-to-mark 'src))
      (dired-nnn--unmark-all-files))
    (dired-create-files
     #'dired-copy-file
     "dired-nnn paste"
     marked-files
     (dired-nnn--get-dir-name-constructor cwd)
     (when (eq how-to-mark 'dest) ?*)))
  (revert-buffer))

;;;###autoload
(defun dired-nnn-move (arg)
  "Move the selected files to the cwd.

If ARG or `dired-nnn-mark-new-files' is non-nil, then recently created files
will be marked."
  (interactive "P")
  (when (not (derived-mode-p 'dired-mode))
    (error "Not a dired buffer."))
  (let ((cwd (dired-current-directory))
        (marked-files (dired-nnn--marked-files)))
    (dired-nnn--unmark-all-files)
    (dired-create-files
     #'dired-rename-file
     "dired-nnn move"
     marked-files
     (dired-nnn--get-dir-name-constructor cwd)
     (when (or arg dired-nnn-mark-new-files) ?*)))
  (revert-buffer))

(provide 'dired-nnn)

;;; dired-nnn.el ends here
