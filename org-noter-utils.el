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
  (let ((ret-code
         (call-process-shell-command (format "%s %s NotesFile -r"
                                             (expand-file-name "change_meta.py" org-noter--site-directory)
                                             (shell-quote-argument
                                              (pdf-info--normalize-file-or-buffer buffer-or-file))))))
    (if (= 0 ret-code)
        (message "NotesFile meta removed successfuly")
      (message "Error during removing metadata"))))

(defun org-noter-utils-add-pdf-meta (notes-file-path &optional buffer-or-file)
  "Remove notes file meta from BUFFER-OR-FILE."
  (interactive "fEnter a notes file: ")
  (cond ((or (not notes-file-path)
             (directory-name-p notes-file-path)
             (not (file-readable-p notes-file-path)))
         (error "\"%s\" is not a valid notes file name" notes-file-path))
        (t (let ((ret-code
                  (call-process-shell-command
                   (format "%s -a %s %s"
                           (expand-file-name "change_meta.py" org-noter--site-directory)
                           (shell-quote-argument
                            (pdf-info--normalize-file-or-buffer buffer-or-file))
                           (shell-quote-argument notes-file-path)))))
             (if (= 0 ret-code)
                 (message "NotesFile meta added successfuly")
               (message "Error during adding metadata"))))))

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
  (mapcar (lambda (e) (delete-directory (concat (org-download--dir) "/" e) t))
          '("raw" "small"))
  (message "Image directories removed successfully!"))

(provide 'org-noter-utils)
