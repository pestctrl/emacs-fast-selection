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
(defun efs-construct-selection-buffer ()
  (save-excursion
    (save-window-excursion
      )))

(defun emacs-fast-selection (table)
  "Fast tag selection with single keys.
CURRENT is the current list of tags in the headline, INHERITED is the
list of inherited tags, and TABLE is an alist of tags and corresponding keys,
possibly with grouping information.  TODO-TABLE is a similar table with
TODO keywords, should these have keys assigned to them.
If the keys are nil, a-z are automatically assigned.
Returns the new tags string, or nil to not change the current settings."
  (let* ((maxlen (if (null table) 0
		   (apply #'max
			  (mapcar (lambda (x)
				    (if (stringp (car x)) (string-width (car x))
				      0))
				  table))))
	 (buf (current-buffer))
	 (expert (eq org-fast-tag-selection-single-key 'expert))
	 (tab-tags nil)
	 (fwidth (+ maxlen 3 1 3))
	 (ncol (/ (- (window-width) 4) fwidth))
	 (i-face 'org-done)
	 (c-face 'org-todo)
     current
	 tg cnt e c char c1 c2 ntable tbl rtn
	 ov-start ov-end ov-prefix
	 (exit-after-next org-fast-tag-selection-single-key)
	 (done-keywords org-done-keywords)
	 groups ingroup intaggroup)
    (save-excursion
      (beginning-of-line)
      (if (looking-at org-tag-line-re)
	  (setq ov-start (match-beginning 1)
		ov-end (match-end 1)
		ov-prefix "")
	(setq ov-start (1- (point-at-eol))
	      ov-end (1+ ov-start))
	(skip-chars-forward "^\n\r")
	(setq ov-prefix
	      (concat
	       (buffer-substring (1- (point)) (point))
	       (if (> (current-column) org-tags-column)
		   " "
		 (make-string (- org-tags-column (current-column)) ?\ ))))))
    (move-overlay org-tags-overlay ov-start ov-end)
    (save-excursion
      (save-window-excursion
	(delete-other-windows)
	(set-window-buffer (split-window-vertically) (get-buffer-create " *Org tags*"))
	(org-switch-to-buffer-other-window " *Org tags*")
	(erase-buffer)
	(setq-local org-done-keywords done-keywords)
	(org-fast-tag-insert "Selected" current c-face "\n\n")
	(org-fast-tag-show-exit exit-after-next)
	(org-set-current-tags-overlay current ov-prefix)
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
	    (setq tg (org-add-props tg nil 'face
				    (cond
				     ((not (assoc tg table))
				      (org-get-todo-face tg))
				     ((member tg current) c-face))))
	    (when (equal (caar tbl) :grouptags)
	      (org-add-props tg nil 'face 'org-tag-group))
	    (when (and (zerop cnt) (not ingroup) (not intaggroup)) (insert "  "))
	    (insert "[" c "] " tg (make-string
				   (- fwidth 4 (length tg)) ?\ ))
	    (push (cons tg c) ntable)
	    (when (= (cl-incf cnt) ncol)
	      (unless (memq (caar tbl) '(:endgroup :endgrouptag))
		(insert "\n")
		(when (or ingroup intaggroup) (insert "  ")))
	      (setq cnt 0)))))
	(setq ntable (nreverse ntable))
	(insert "\n")
	(goto-char (point-min))
	(unless expert (org-fit-window-to-buffer))
	(setq rtn
	      (catch 'exit
		(while t
		  (message "[a-z..]:toggle [SPC]:clear [RET]:accept [TAB]:edit [!] %sgroups%s"
			   (if (not groups) "no " "")
			   (if expert " [C-c]:window" (if exit-after-next " [C-c]:single" " [C-c]:multi")))
		  (setq c (let ((inhibit-quit t)) (read-char-exclusive)))
		  (setq org-last-tag-selection-key c)
		  (cond
		   ((= c ?\r) (throw 'exit t))
		   ((= c ?!)
		    (setq groups (not groups))
		    (goto-char (point-min))
		    (while (re-search-forward "[{}]" nil t) (replace-match " ")))
		   ((= c ?\C-c)
		    (if (not expert)
			(org-fast-tag-show-exit
			 (setq exit-after-next (not exit-after-next)))
		      (setq expert nil)
		      (delete-other-windows)
		      (set-window-buffer (split-window-vertically) " *Org tags*")
		      (org-switch-to-buffer-other-window " *Org tags*")
		      (org-fit-window-to-buffer)))
		   ((or (= c ?\C-g)
			(and (= c ?q) (not (rassoc c ntable))))
		    (delete-overlay org-tags-overlay)
		    (setq quit-flag t))
		   ((= c ?\ )
		    (setq current nil)
		    (when exit-after-next (setq exit-after-next 'now)))
		   ((= c ?\t)
                    (condition-case nil
                        (unless tab-tags
                          (setq tab-tags
                                (delq nil
                                      (mapcar (lambda (x)
                                                (let ((item (car-safe x)))
                                                  (and (stringp item)
                                                       (list item))))
                                              (org--tag-add-to-alist
                                               (with-current-buffer buf
                                                 (org-get-buffer-tags))
                                               table))))))
                    (setq tg (completing-read "Tag: " tab-tags))
		    (when (string-match "\\S-" tg)
		      (cl-pushnew (list tg) tab-tags :test #'equal)
		      (if (member tg current)
			  (setq current (delete tg current))
			(push tg current)))
		    (when exit-after-next (setq exit-after-next 'now)))
		   ((setq e (rassoc c ntable) tg (car e))
		    (if (member tg current)
			(setq current (delete tg current))
		      (cl-loop for g in groups do
			       (when (member tg g)
				 (dolist (x g) (setq current (delete x current)))))
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
		  (org-fast-tag-insert "Selected" current c-face)
		  (org-set-current-tags-overlay current ov-prefix)
		  (let ((tag-re (concat "\\[.\\] \\(" org-tag-re "\\)")))
		    (while (re-search-forward tag-re nil t)
		      (let ((tag (match-string 1)))
			(add-text-properties
			 (match-beginning 1) (match-end 1)
			 (list 'face
			       (cond
				((member tag current) c-face)
				(t (get-text-property (match-beginning 1) '
						      face))))))))
		  (goto-char (point-min)))))
	(delete-overlay org-tags-overlay)
	(if rtn
	    (mapconcat 'identity current ":")
	  nil)))))

(emacs-fast-selection '(("Hello" . ?h) ("World") ("Clock") ("Create_Tab" . ?t)))

(provide 'emacs-fast-selection)
;;; emacs-fast-selection.el ends here
