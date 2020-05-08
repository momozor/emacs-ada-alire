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

(defvar *alire-project-root* nil)

(defun alr-version ()
  (interactive)
  (cd *alire-project-root*)
  (shell-command "alr version"))

(defun alr-print-current-project-root-path ()
  (interactive)
  (message *alire-project-root*))

(defun alr-cd-project-root ()
  (interactive)
  (let ((directory-path
         (read-directory-name "Project root absolute path: ")))
    (cd directory-path)
    (setf *alire-project-root* directory-path)
    (message "You are now in %s" directory-path)))

(defun alr-build ()
  (interactive)
  (cd *alire-project-root*)
  (message "Building project..")
  (shell-command "alr build"))

(defun alr-run ()
  (interactive)
  (cd *alire-project-root*)
  (message "Running project..")
  (shell-command "alr run"))

(defun alr-search-installable ()
  (interactive)
  (cd *alire-project-root*)
  (shell-command "alr search --list"))

(defun alr-with ()
  (interactive)
  (let ((crate-name
         (read-string "Crate name: ")))
    (cd *alire-project-root*)
    (shell-command (format "alr with %s" crate-name))))

(defun alr-update ()
  (interactive)
  (cd *alire-project-root*)
  (shell-command "alr update"))

;;; ada-alire.el ends here
