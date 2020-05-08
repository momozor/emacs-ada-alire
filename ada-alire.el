;;; ada-alire.el --- A lightweight Ada Alire's alr commander.

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

(defun alr-list-installable-crates ()
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
