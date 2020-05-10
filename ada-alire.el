;;; ada-alire.el --- A lightweight Ada Alire's alr commander. -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Momozor <skelic3@gmail.com, momozor4@gmail.com>

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;; Author: Momozor <skelic3@gmail.com, momozor4@gmail.com>
;; Version: 0.1
;; Keywords: ada, alire, alr, productivity
;; URL: https://github.com/momozor/ada-alire

;;; Commentary:

;; Ada Alire is a thin command wrapper run certain alr
;; commands without switching to terminal (more focus on project development).

;; Full documentation is available as an Info manual.

;;; Code:

(defvar *current-directory* nil)
(defvar *alire-project-root* nil)

(defun alr-info ()
  (interactive)
  (setf *current-directory* (file-name-directory buffer-file-name))
  (cd *alire-project-root*)
  (shell-command "alr version")
  (cd *current-directory*))

(defun alr-set-project-path ()
  (interactive)
  (let ((directory-path
         (read-directory-name "Project absolute path: ")))
    (setf *alire-project-root* directory-path)
    (message "Set project path to %s" directory-path))

  (let ((change-directory-now?
         (read-string "Change directory into project now? ")))
    (when (or (string-equal "y" change-directory-now?)
              (string-equal "yes" change-directory-now?)
              (string-equal "Y" change-directory-now?)
              (string-equal "YES" change-directory-now?))
      (progn
        (alr-cd-to-project)))))

(defun alr-cd-to-project ()
  (interactive)
  (cd *alire-project-root*)
  (message "You are now in %s" *alire-project-root*))

(defun alr-print-project-path ()
  (interactive)
  (message *alire-project-root*))

(defun alr-build ()
  (interactive)
  (setf *current-directory* (file-name-directory buffer-file-name))
  
  (cd *alire-project-root*)
  (message "Building project..")
  (shell-command "alr build")

  (cd *current-directory*))

(defun alr-run ()
  (interactive)
  (setf *current-directory* (file-name-directory buffer-file-name))
  
  (cd *alire-project-root*)
  (message "Running project..")
  (shell-command "alr run")

  (cd *current-directory*))

(defun alr-list-installable-crates ()
  (interactive)
  (setf *current-directory* (file-name-directory buffer-file-name))
  (cd *alire-project-root*)
  (shell-command "alr search --list")

  (cd *current-directory*))

(defun alr-with ()
  (interactive)

  (setf *current-directory* (file-name-directory buffer-file-name))
  (cd *alire-project-root*)
  (let ((crate-name
         (read-string "Crate name: ")))
    (shell-command (format "alr with %s" crate-name)))

  (cd *current-directory*))

(defun alr-update ()
  (interactive)

  (setf *current-directory* (file-name-directory buffer-file-name))
  (cd *alire-project-root*)
  (shell-command "alr update")

  (cd *current-directory*))

;;; ada-alire.el ends here
