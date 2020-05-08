;;; ada-alire.el --- A lightweight Ada Alire's alr commander.

;; Copyright (C) 2020 Momozor <skelic3@gmail.com, momozor4@gmail.com>

;; Author: Momozor <skelic3@gmail.com, momozor4@gmail.com>
;; Version: 0.1
;; Keywords: ada, alire, alr, productivity
;; URL: https://github.com/momozor/ada-alire

;;; Commentary:

;; Ada Alire is a thin command wrapper run certain alr
;; commands without switching to terminal (more focus on project development).

;; Full documentation is available as an Info manual.

;;; Code:

(defvar *ada-project-root* nil)

(defun alr-in-root-directory? () nil)

(defun alr-version () nil)

(defun alr-print-current-project-root-path ()
  (interactive)
  (message *ada-project-root*))

(defun alr-cd-project-root ()
  (interactive)
  (let ((directory-path
         (read-directory-name "Project root absolute path: ")))
    (cd directory-path)
    (setf *ada-project-root* directory-path)
    (message "You are now in %s" directory-path)))

(defun alr-build () nil)

(defun alr-run () nil)

(defun alr-search-installable () nil)

(defun alr-with ()
  (interactive)
  (let ((crate-name
         (read-string "Crate name: ")))
    (cd "/home/momozor/quicklisp/local-projects/non_lisp/ada/nzping")
    
















    (shell-command (format "alr with %s" crate-name))))

  (defun alr-update () nil)

  (defun alr-info (number)
    "Multiply NUMBER by seven."
    (interactive "p")
    (message "The result is %d" (* 8 number)))

;;; ada-alire.el ends here
