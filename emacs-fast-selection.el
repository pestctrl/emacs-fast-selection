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
  (declare (indent 2))
  (add-text-properties 0 (length string)
                       props string)
  string)

(defun efs/construct-lookup-table (table)
  (let* ((maxlen (if (null table) 0
                   (apply #'max
                          (mapcar (lambda (x)
                                    (if (stringp (car x)) (string-width (car x))
                                      0))
                                  table))))
         (fwidth (+ maxlen 3 1 3))
         (ncol (/ (- (window-width) 4) fwidth))
         (exit-after-next org-fast-tag-selection-single-key)
         (tbl-iter (copy-sequence table))
         (char-iter ?a)
         (cnt 0)
         ntable key)
    (setq cnt 0)
    (while (setq e (pop tbl-iter))
      (when (not (equal e '(:newline)))
        (let* ((tag-string (copy-sequence (car e)))
               (first-char
                (string-to-char (downcase tag-string))))
          (setq key (cond
                     ((cdr e) (cdr e))
                     ((and (not (rassoc first-char ntable))
                           (not (rassoc first-char table)))
                      first-char)
                     (t
                      (while (or (rassoc char-iter ntable)
                                 (rassoc char-iter table))
                        (incf char-iter))
                      char-iter)))
          (setq tag-string (efs/add-props tag-string 'face
                             nil))
          (push (cons tag-string key) ntable))))
    (nreverse ntable)))

(defun efs/construct-selection-buffer (table ntable)
  (let* ((maxlen (if (null table) 0
                   (apply #'max
                          (mapcar (lambda (x)
                                    (if (stringp (car x)) (string-width (car x))
                                      0))
                                  table))))
         (fwidth (+ maxlen 3 1 3))
         (ncol (/ (- (window-width) 4) fwidth))
         (done-keywords org-done-keywords)
         (exit-after-next org-fast-tag-selection-single-key)
         tbl char cnt current
         tag-string c tag-first-char)
    (save-excursion
      (save-window-excursion
        (set-buffer (get-buffer-create " *Emacs Fast Selection*"))

        (erase-buffer)
        (setq-local org-done-keywords done-keywords)
        (insert "\n\n")
        (org-fast-tag-show-exit exit-after-next)
        (setq tbl table
              char ?a
              cnt 0)
        (while (setq e (pop tbl))
          (cond
           ((equal e '(:newline))
            (unless (zerop cnt)
              (setq cnt 0)
              (insert "\n")
              (setq e (car tbl))
              (while (equal (car tbl) '(:newline))
                (insert "\n")
                (setq tbl (cdr tbl)))))
           (t
            (setq tag-string (copy-sequence (car e)))
            (setq tag-string (efs/add-props tag-string 'face
                               (when (member tag-string current)
                                 efs/selected-face)))
            (when (zerop cnt) (insert "  "))
            (insert "[" (cdr (assoc tag-string ntable)) "] " tag-string (make-string
                                                                      (- fwidth 4 (length tag-string)) ?\ ))
            (when (= (cl-incf cnt) ncol)
              (setq cnt 0)))))
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
  (let* ((expert nil) ;; TODO: Add expert mode?
         (exit-after-next org-fast-tag-selection-single-key)
         (lookup-table (efs/construct-lookup-table (remove-if
                                                    (lambda (x) (equal x '(:newline)))
                                                    table)))
         (efs-buffer (efs/construct-selection-buffer table lookup-table))
         current tag-string e c rtn)
    (save-excursion
      (save-window-excursion
        (delete-other-windows)
        (set-window-buffer (split-window-vertically) efs-buffer)
        (org-switch-to-buffer-other-window efs-buffer)
        (goto-char (point-min))
        (unless expert (org-fit-window-to-buffer))
        (setq rtn
              (catch 'exit
                (while t
                  (message "[a-z..]:toggle [SPC]:clear [RET]:accept [C-c]:%s"
                           (if expert "window" (if exit-after-next "single" "multi")))

                  (setq c (let ((inhibit-quit t)) (read-char-exclusive)))
                  (cond
                   ((= c ?\r) (throw 'exit t))
                   ((= c ?\C-c)
                    (if (not expert)
                        (org-fast-tag-show-exit
                         (setq exit-after-next (not exit-after-next)))
                      (setq expert nil)
                      (delete-other-windows)
                      (set-window-buffer (split-window-vertically) efs/buffer-name)
                      (org-switch-to-buffer-other-window efs/buffer-name)
                      (org-fit-window-to-buffer)))
                   ((or (= c ?\C-g)
                        (and (= c ?q) (not (rassoc c lookup-table))))
                    (setq quit-flag t))
                   ((= c ?\ )
                    (setq current nil)
                    (when exit-after-next (setq exit-after-next 'now)))
                   ((setq e (rassoc c lookup-table) tag-string (car e))
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
                  (goto-char (point-min)))))
        (if rtn
            (mapconcat 'identity current ":")
          nil)))))

(let ((a (emacs-fast-selection '(("Hello" . ?h) ("World") (:newline) ("Allow") ("Woah") ("Where") ("Clock") ("Create_Tab" . ?t)))))
  (message "%s" a))

(provide 'emacs-fast-selection)
;;; emacs-fast-selection.el ends here
