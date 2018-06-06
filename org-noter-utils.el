;;; org-noter-utils.el --- Utility functions for org-noter       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  James Wang

;; Author: James Wang
;; Homepage: https://github.com/et2010/org-noter
;; Keywords: lisp pdf interleave annotate external sync notes documents org-mode
;; Package-Requires: ((emacs "24.4") (cl-lib "0.6"))
;; Version: 1.0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

(require 'pdf-info)

(defvar org-noter--site-directory)

(defun org-noter-utils-pdf-meta-advice (orig-func &optional arg)
  "Read META of BUFFER-OR-FILE. If the second argument is nil,
read current buffer."
  (let ((notes-file-path (org-noter-utils-read-pdf-meta "NotesFile" arg)))
    (-insert-at 5 (cons 'notes notes-file-path) (apply orig-func arg))))
(advice-add #'pdf-info-metadata :around #'org-noter-utils-pdf-meta-advice)

(defun org-noter-utils-read-pdf-meta (meta &optional buffer-or-file)
  "Read metadata from pdf file."
  (s-chomp
   (shell-command-to-string
    (format "%s %s %s"
            (expand-file-name "read_meta.py" org-noter--site-directory)
            (shell-quote-argument (pdf-info--normalize-file-or-buffer buffer-or-file))
            meta))))

(defun org-noter-utils-remove-pdf-meta (&optional buffer-or-file)
  "Remove notes file meta from BUFFER-OR-FILE."
  (interactive)
  (when (not (eq system-type 'windows-nt))
    (let ((ret-code
           (call-process-shell-command
            (format "%s %s NotesFile -r"
                    (expand-file-name "change_meta.py" org-noter--site-directory)
                    (shell-quote-argument
                     (pdf-info--normalize-file-or-buffer buffer-or-file))))))
      (if (= 0 ret-code)
          (message "NotesFile meta removed successfuly")
        (message "Error during removing metadata")))))

(defun org-noter-utils-add-pdf-meta (notes-file-path &optional buffer-or-file)
  "Remove notes file meta from BUFFER-OR-FILE."
  (interactive "fEnter a notes file: ")
  (when (not (eq system-type 'windows-nt))
    (let (abs-path)
      (message "Adding notes file path (%s) to\nPDF document (%s)" notes-file-path buffer-or-file)
      (cond ((or (not notes-file-path)
                 (directory-name-p notes-file-path)
                 (not (file-readable-p
                       (setq abs-path
                             (expand-file-name notes-file-path buffer-or-file)))))
             (error "\"%s\" is not a valid notes file name" abs-path))
            (t (let ((ret-code
                      (call-process-shell-command
                       (format "%s -a %s %s"
                               (expand-file-name "change_meta.py" org-noter--site-directory)
                               (shell-quote-argument
                                (pdf-info--normalize-file-or-buffer buffer-or-file))
                               (shell-quote-argument notes-file-path)))))
                 (if (= 0 ret-code)
                     (message "NotesFile meta added successfuly")
                   (message "Error during adding metadata"))))))))

(defun org-noter-utils-extract-doc-images (doc-path img-dir)
  "Extract images from a PDF document."
  (interactive)
  (let ((cmd (expand-file-name "get_pdf_images.py" org-noter--site-directory)))
    (call-process-shell-command
     (format "cd %s && %s %s"
             (shell-quote-argument img-dir)
             (shell-quote-argument cmd)
             (shell-quote-argument doc-path)))))

(defun org-noter-utils-remove-extracted-images ()
  "Remove extracted images"
  (interactive)
  (when (not (eq major-mode 'org-mode))
    (error "You are not in an org-mode buffer"))
  (when (org-before-first-heading-p)
    (error "This command must be issued inside an org heading"))
  (mapc (lambda (e) (delete-directory (concat (org-download--dir) "/" e) t))
        '("raw" "small"))
  (message "Image directories removed successfully!"))

(defun org-noter-utils-modify-dir-local-variable (mode variable value op)
  "Modify directory-local VARIABLE in .dir-locals.el depending on operation OP.

If OP is `add-or-replace' then delete all existing settings of
VARIABLE (except `mode' and `eval') and add a new directory-local VARIABLE
with VALUE to the MODE alist where MODE can be a mode name symbol or
a subdirectory name.

If .dir-locals.el was not found and OP is not `delete' then create
this file in the current directory.

If OP is `delete' then delete all existing settings of VARIABLE
from the MODE alist ignoring the input argument VALUE."
  (catch 'exit
    (unless enable-local-variables
      (throw 'exit (message "Directory-local variables are disabled")))
    (let* ((dir-or-cache (and (buffer-file-name)
                              (not (file-remote-p (buffer-file-name)))
                              (dir-locals-find-file (buffer-file-name))))
           (variables-file
            ;; If there are several .dir-locals, the user probably
            ;; wants to edit the last one (the highest priority).
            (cond ((stringp dir-or-cache)
                   (car (last (dir-locals--all-files dir-or-cache))))
                  ((consp dir-or-cache)	; result from cache
                   ;; If cache element has an mtime, assume it came
                   ;; from a file.  Otherwise, assume it was set
                   ;; directly.
                   (if (nth 2 dir-or-cache)
                       (car (last (dir-locals--all-files (car dir-or-cache))))
                     (cadr dir-or-cache)))
                  ;; Try to make a proper file-name.
                  (t (expand-file-name dir-locals-file))))
           variables)
      ;; I can't be bothered to handle this case right now.
      ;; Dir locals were set directly from a class.  You need to
      ;; directly modify the class in dir-locals-class-alist.
      (and variables-file (not (stringp variables-file))
	         (throw 'exit (message "Directory locals were not set from a file")))
      ;; Don't create ".dir-locals.el" for the deletion operation.
      (and (eq op 'delete)
	         (or (not variables-file)
	             (not (file-exists-p variables-file)))
	         (throw 'exit (message "No .dir-locals.el file was found")))
      (let ((auto-insert nil))
        (with-temp-file variables-file
          (widen)
          (goto-char (point-min))

          ;; Read alist of directory-local variables.
          (ignore-errors
	          (delete-region
	           (prog1 (point)
	             (setq variables (let ((read-circle nil))
			                           (read (current-buffer)))))
	           (point)))

          ;; Add or replace variable in alist of directory-local variables.
          (let ((mode-assoc (assoc mode variables)))
	          (if mode-assoc
	              (setq variables
		                  (cons (cons mode
			                            (if (eq op 'delete)
				                              (assq-delete-all variable (cdr mode-assoc))
				                            (cons
				                             (cons variable value)
				                             (if (memq variable '(mode eval))
				                                 (cdr mode-assoc)
				                               (assq-delete-all variable (cdr mode-assoc))))))
			                      (assq-delete-all mode variables)))
	            (setq variables
		                (cons `(,mode . ((,variable . ,value)))
		                      variables))))

          ;; Insert modified alist of directory-local variables.
          (insert ";;; Directory Local Variables\n")
          (insert ";;; For more information see (info \"(emacs) Directory Variables\")\n\n")
          (pp (sort variables
		                (lambda (a b)
		                  (cond
		                   ((null (car a)) t)
		                   ((null (car b)) nil)
		                   ((and (symbolp (car a)) (stringp (car b))) t)
		                   ((and (symbolp (car b)) (stringp (car a))) nil)
		                   (t (string< (car a) (car b))))))
	            (current-buffer)))))))

(provide 'org-noter-utils)
