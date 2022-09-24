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
(require 'subr-x)
(require 'eieio)

(defvar efs/selected-face 'org-todo)
(defvar efs/unselected-face nil)

(defclass efs-select-machine ()
  ((lookup-table :initfrom nil :initarg :lookup-table)
   (group-list :initform nil :initarg :group-list)
   (current :initform nil)))

(defun efs-sm/make (select-spec)
  (efs-select-machine :lookup-table (efs/construct-lookup-table (remove-if (lambda (x) (eq x :newline))
                                                                           (flatten-list select-spec)))
                      :group-list (remove-if (lambda (x) (not (consp x)))
                                             select-spec)))

(defmethod efs-sm/selected ((efsss efs-select-machine))
  (slot-value efsss 'current))

(defmethod efs-sm/key-to-sym ((efsss efs-select-machine) key)
  (with-slots (lookup-table) efsss
    (rassoc key lookup-table)))

(defmethod efs-sm/sym-to-key ((efsss efs-select-machine) sym)
  (with-slots (lookup-table) efsss
    (assoc sym lookup-table)))

(defmethod efs-sm/select ((efsss efs-select-machine) key)
  (with-slots (lookup-table group-list current) efsss
    (let* ((option (car (rassoc key lookup-table))))
      (if (member option current)
          (setf current (delq option current))
        (push option current))
      (when-let (group (car (remove-if-not (lambda (group) (member option group))
                                           group-list)))
        (let ((to-remove (remq option group)))
          (dolist (sym to-remove)
            (setf current (delq sym current))))))
    (setf current
          (sort current
                (lambda (a b)
                  (assoc b (cdr (memq (assoc a lookup-table) lookup-table))))))))

(defun efs/add-props (string &rest props)
  "Add text properties to entire string, from beginning to end.
PLIST may be a list of properties, PROPS are individual properties and values
that will be added to PLIST.  Returns the string that was modified."
  (add-text-properties 0 (length string)
                       props string)
  string)

(defun efs/construct-lookup-table (option-list)
  (let* ((char-iter ?a)
         (cnt 0)
         lookup-table)
    (dolist (sym option-list)
      (let* ((option-string (symbol-name sym))
             (first-char
              (string-to-char (downcase option-string)))
             key)
        ;; If nothing has used the first character
        (if (and (not (rassoc first-char lookup-table))
                 (not (rassoc first-char option-list)))
            ;; Use that as the key
            (setq key first-char)
          ;; Search for a free key
          (while (or (rassoc char-iter lookup-table)
                     (rassoc char-iter option-list))
            (incf char-iter))
          (setq key char-iter))
        (push (cons sym key) lookup-table)))
    (nreverse lookup-table)))

(defun efs/construct-selection-buffer (select-spec state-machine)
  (let* ((maxlen (if (null select-spec) 0
                   (apply #'max
                          (mapcar (lambda (x)
                                    (if (keywordp x) 0 (string-width (symbol-name x))))
                                  (flatten-list select-spec)))))
         (fwidth (+ maxlen 3 1 3))
         (ncol (/ (- (window-width) 4) fwidth))
         char cnt)
    (cl-labels ((insert-option
                 (sym)
                 (let* ((option-string (--> (symbol-name sym)
                                            (efs/add-props it 'face nil)))
                        (key (cdr (efs-sm/sym-to-key state-machine sym))))
                   (insert "[" key "] " option-string
                           (make-string (- fwidth 4 (length option-string)) ?\ ))
                   (when (= (cl-incf cnt) ncol)
                     (setq cnt 0))))
                (handle-keyword
                 (keyword)
                 (pcase keyword
                   (:newline (insert "\n") (setq cnt 0)))))
      (with-current-buffer (get-buffer-create " *Emacs Fast Selection*")
        (erase-buffer)
        (insert "\n\n")
        (setq char ?a
              cnt 0)
        (dolist (front select-spec)
          (when (zerop cnt) (insert "  "))
          (cond
           ((keywordp front) (handle-keyword front))
           ((symbolp front) (insert-option front))
           (t
            (insert "\n" "{ ")
            (dolist (sym front)
              (cl-assert (symbolp sym))
              (insert-option sym))
            (insert "}" "\n")
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
                            (mapconcat #'symbol-name it " ")
                            (efs/add-props it 'face efs/selected-face))))
    (insert header selected-tags))
  (let ((option-re "\\[.\\] \\([^ ]+\\)"))
    (while (re-search-forward option-re nil t)
      (let ((tag (match-string 1)))
        (add-text-properties
         (match-beginning 1) (match-end 1)
         (list 'face
               (cond
                ((member (intern tag) current) efs/selected-face)
                (t efs/unselected-face))))))))

(defun emacs-fast-select (&rest select-spec)
  (let* ((state-machine (efs-sm/make select-spec))
         (efs-buffer (efs/construct-selection-buffer select-spec state-machine))
         current option keypress exit-after-next)
    (save-excursion
      (save-window-excursion
        (delete-other-windows)
        (set-window-buffer (split-window-vertically) efs-buffer)
        (let (pop-up-frames pop-up-windows)
          (switch-to-buffer-other-window efs-buffer))
        (goto-char (point-min))
        ;; May need to revert to org-fit-window-to-buffer
        (fit-window-to-buffer)
        (catch 'exit
          (while t
            (save-excursion
              (message "[a-z..]:toggle [SPC]:clear [RET]:accept")
              (setq keypress (let ((inhibit-quit t)) (read-char-exclusive)))
              (cond
               ((= keypress ?\r) (throw 'exit t))
               ((or (= keypress ?\C-g)
                    (and (= keypress ?q) (not (efs-sm/key-to-sym state-machine keypress))))
                (setq quit-flag t))
               ((= keypress ?\ )
                (setf (slot-value state-machine 'current) nil)
                (when exit-after-next (setq exit-after-next 'now)))
               (t
                (efs-sm/select state-machine keypress)
                (when exit-after-next (setq exit-after-next 'now))))
              (when (eq exit-after-next 'now) (throw 'exit t))
              (efs/update-efs-buffer (slot-value state-machine 'current))
              (goto-char (point-min)))))
        (slot-value state-machine 'current)))))

(emacs-fast-select 'Hello
                   'World
                   '(Tabs Spaces))

(let ((a (emacs-fast-select 'Hello 'World :newline 'Allow
                            '(Woah Where Clock) 'Create_Tab)))
  (message "%s" a))

(provide 'emacs-fast-selection)
;;; emacs-fast-selection.el ends here
