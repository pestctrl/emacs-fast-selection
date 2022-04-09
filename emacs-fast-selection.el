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

(defun efs/construct-selection-buffer (buffer table)
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
         tbl char cnt ntable ingroup intaggroup current)
    (save-excursion
      (save-window-excursion
        (set-buffer (get-buffer-create buffer))

        (erase-buffer)
        (setq-local org-done-keywords done-keywords)
        (let ((header (format "%-12s" "Selected:"))
              (selected-tags (--> current
                                  (mapconcat 'identity it " ")
                                  (efs/add-props it 'face efs/selected-face))))
          (insert header selected-tags "\n\n"))
        (org-fast-tag-show-exit exit-after-next)
        (setq tbl table char ?a cnt 0)
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
            (setq tg (copy-sequence (car e)) c2 nil)
            (if (cdr e)
                (setq c (cdr e))
              ;; automatically assign a character.
              (setq c1 (string-to-char
                        (downcase (substring
                                   tg (if (= (string-to-char tg) ?@) 1 0)))))
              (if (or (rassoc c1 ntable) (rassoc c1 table))
                  (while (or (rassoc char ntable) (rassoc char table))
                    (setq char (1+ char)))
                (setq c2 c1))
              (setq c (or c2 char)))
            (when ingroup (push tg (car groups)))
            (setq tg (efs/add-props tg 'face
                       (cond
                        ((not (assoc tg table))
                         (org-get-todo-face tg))
                        ((member tg current) efs/selected-face))))
            (when (equal (caar tbl) :grouptags)
              (efs/add-props tg 'face 'org-tag-group))
            (when (and (zerop cnt) (not ingroup) (not intaggroup)) (insert "  "))
            (insert "[" c "] " tg (make-string
                                   (- fwidth 4 (length tg)) ?\ ))
            (push (cons tg c) ntable)
            (when (= (cl-incf cnt) ncol)
              (unless (memq (caar tbl) '(:endgroup :endgrouptag))
                (insert "\n")
                (when (or ingroup intaggroup) (insert "  ")))
              (setq cnt 0)))))
        (insert "\n")
        (nreverse ntable)))))

(defun emacs-fast-selection (table)
  (let* ((expert nil) ;; TODO: Add expert mode?
         (current (flatten-list (mapcar (lambda (x) (list (car x) nil)) table)))
         (exit-after-next org-fast-tag-selection-single-key)
         (efs/buffer-name " *Emacs Fast Selection*")
         current tg e c ntable rtn)
    (save-excursion
      (save-window-excursion
        (setq ntable (efs/construct-selection-buffer efs/buffer-name table))
        (delete-other-windows)
        (set-window-buffer (split-window-vertically) (get-buffer-create efs/buffer-name))
        (org-switch-to-buffer-other-window efs/buffer-name)
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
                        (and (= c ?q) (not (rassoc c ntable))))
                    (setq quit-flag t))
                   ((= c ?\ )
                    (setq current nil)
                    (when exit-after-next (setq exit-after-next 'now)))
                   ((setq e (rassoc c ntable) tg (car e))
                    (if (member tg current)
                        (setq current (delete tg current))
                      (push tg current))
                    (when exit-after-next (setq exit-after-next 'now))))

                  ;; Create a sorted list
                  (setq current
                        (sort current
                              (lambda (a b)
                                (assoc b (cdr (memq (assoc a ntable) ntable))))))
                  (when (eq exit-after-next 'now) (throw 'exit t))
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
                                (t efs/unselected-face)))))))
                  (goto-char (point-min)))))
        (if rtn
            (mapconcat 'identity current ":")
          nil)))))

(emacs-fast-selection '(("Hello" . ?h) ("World") ("Clock") ("Create_Tab" . ?t)))

(provide 'emacs-fast-selection)
;;; emacs-fast-selection.el ends here
