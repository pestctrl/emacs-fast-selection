;;; emacs-fast-selection.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Benson Chu

;; Author: Benson Chu <bensonchu457@gmail.com>
;; Created: [2022-03-06 16:59]

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'cl-lib)

(defvar efs/selected-face 'org-todo)
(defvar efs/unselected-face nil)

(defun efs/add-props (string &rest props)
  "Add text properties to entire string, from beginning to end.
PLIST may be a list of properties, PROPS are individual properties and values
that will be added to PLIST.  Returns the string that was modified."
  (add-text-properties 0 (length string)
                       props string)
  string)

(defun efs/construct-lookup-table (table)
  (let* ((tbl-iter (copy-sequence table))
         (char-iter ?a)
         (cnt 0)
         ntable)
    (while (setq e (pop tbl-iter))
      (let* ((tag-string (symbol-name e))
             (first-char
              (string-to-char (downcase tag-string)))
             key)
        ;; If nothing has used the first character
        (if (and (not (rassoc first-char ntable))
                 (not (rassoc first-char table)))
            ;; Use that as the key
            (setq key first-char)
          ;; Search for a free key
          (while (or (rassoc char-iter ntable)
                     (rassoc char-iter table))
            (incf char-iter))
          (setq key  char-iter))
        (setq tag-string (efs/add-props tag-string 'face nil))
        (push (cons tag-string key) ntable)))
    (nreverse ntable)))

(defun efs/construct-selection-buffer (table ntable)
  (let* ((maxlen (if (null table) 0
                   (apply #'max
                          (mapcar (lambda (x)
                                    (string-width (symbol-name x)))
                                  table))))
         (fwidth (+ maxlen 3 1 3))
         (ncol (/ (- (window-width) 4) fwidth))
         tbl char cnt current
         tag-string c tag-first-char)
    (save-excursion
      (save-window-excursion
        (set-buffer (get-buffer-create " *Emacs Fast Selection*"))

        (erase-buffer)
        (insert "\n\n")
        (setq tbl table
              char ?a
              cnt 0)
        (while (setq e (pop tbl))
          (let* ((tag-string (--> (symbol-name e)
                                 (efs/add-props it 'face nil)))
                 (key (cdr (assoc tag-string ntable))))
            (when (zerop cnt) (insert "  "))
            (insert "[" key "] " tag-string
                    (make-string (- fwidth 4 (length tag-string)) ?\ ))
            (when (= (cl-incf cnt) ncol)
              (setq cnt 0))))
        (insert "\n")
        (efs/update-efs-buffer nil)
        (current-buffer)))))

(defun efs/update-efs-buffer (current)
  (goto-char (point-min))
  (beginning-of-line 1)
  (delete-region (point) (point-at-eol))
  (let ((header (format "%-12s" "Selected:"))
        (selected-tags (--> current
                            (mapconcat 'identity it " ")
                            (efs/add-props it 'face efs/selected-face))))
    (insert header selected-tags))
  (let ((tag-re (concat "\\[.\\] \\(" org-tag-re "\\)")))
    (while (re-search-forward tag-re nil t)
      (let ((tag (match-string 1)))
        (add-text-properties
         (match-beginning 1) (match-end 1)
         (list 'face
               (cond
                ((member tag current) efs/selected-face)
                (t efs/unselected-face))))))))

(defun emacs-fast-selection (table)
  (let* ((exit-after-next nil)
         (lookup-table (efs/construct-lookup-table table))
         (efs-buffer (efs/construct-selection-buffer table lookup-table))
         current tag-string keypress)
    (save-excursion
      (save-window-excursion
        (delete-other-windows)
        (set-window-buffer (split-window-vertically) efs-buffer)
        (org-switch-to-buffer-other-window efs-buffer)
        (goto-char (point-min))
        (org-fit-window-to-buffer)
        (catch 'exit
          (while t
            (message "[a-z..]:toggle [SPC]:clear [RET]:accept")
            (setq keypress (let ((inhibit-quit t)) (read-char-exclusive)))
            (cond
             ((= keypress ?\r) (throw 'exit t))
             ((or (= keypress ?\C-g)
                  (and (= keypress ?q) (not (rassoc keypress lookup-table))))
              (setq quit-flag t))
             ((= keypress ?\ )
              (setq current nil)
              (when exit-after-next (setq exit-after-next 'now)))
             ((setq tag-string (car (rassoc keypress lookup-table)))
              (if (member tag-string current)
                  (setq current (delete tag-string current))
                (push tag-string current))
              (when exit-after-next (setq exit-after-next 'now))))

            ;; Create a sorted list
            (setq current
                  (sort current
                        (lambda (a b)
                          (assoc b (cdr (memq (assoc a lookup-table) lookup-table))))))
            (when (eq exit-after-next 'now) (throw 'exit t))
            (efs/update-efs-buffer current)
            (goto-char (point-min))))
        current))))

(let ((a (emacs-fast-selection '(Hello World Allow Woah Where Clock Create_Tab))))
  (message "%s" a))

(provide 'emacs-fast-selection)
;;; emacs-fast-selection.el ends here
