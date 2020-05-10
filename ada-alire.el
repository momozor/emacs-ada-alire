;;; ada-alire.el --- A lightweight Ada Alire's alr commander

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

(provide 'ada-alire)

(defvar ada-alire-current-directory)
(defvar ada-alire-project-root)

(defun ada-alire-info ()
  (interactive)
  (setf ada-alire-current-directory (file-name-directory buffer-file-name))
  (cd ada-alire-project-root)
  (shell-command "alr version")
  (cd ada-alire-current-directory))

(defun ada-alire-set-project-path ()
  (interactive)
  (let ((directory-path
         (read-directory-name "Project absolute path: ")))
    (setf ada-alire-project-root directory-path)
    (message "Set project path to %s" directory-path))

  (let ((change-directory-now?
         (read-string "Change directory into project now? (y/n) ")))
    (when (or (string-equal "y" change-directory-now?)
              (string-equal "yes" change-directory-now?)
              (string-equal "Y" change-directory-now?)
              (string-equal "YES" change-directory-now?))
      
      (ada-alire-cd-to-project))))

(defun ada-alire-cd-to-project ()
  (interactive)
  (cd ada-alire-project-root)
  (message "You are now in %s" ada-alire-project-root))

(defun ada-alire-print-project-path ()
  (interactive)
  (message ada-alire-project-root))

(defun ada-alire-build ()
  (interactive)
  (setf ada-alire-current-directory (file-name-directory buffer-file-name))
  
  (cd ada-alire-project-root)
  (message "Building project..")
  (shell-command "alr build")

  (cd ada-alire-current-directory))

(defun ada-alire-run ()
  (interactive)
  (setf ada-alire-current-directory (file-name-directory buffer-file-name))
  
  (cd ada-alire-project-root)
  (message "Running project..")
  (shell-command "alr run")

  (cd ada-alire-current-directory))

(defun ada-alire-list-installable-crates ()
  (interactive)
  (setf ada-alire-current-directory (file-name-directory buffer-file-name))
  (cd ada-alire-project-root)
  (shell-command "alr search --list")

  (cd ada-alire-current-directory))

(defun ada-alire-install-crate ()
  (interactive)

  (setf ada-alire-current-directory (file-name-directory buffer-file-name))
  (cd ada-alire-project-root)
  (let ((crate-name
         (read-string "Crate name to install: ")))
    (shell-command (format "alr with %s" crate-name)))

  (cd ada-alire-current-directory))

(defun ada-alire-remove-crate ()
  (interactive)

  (setf ada-alire-current-directory (file-name-directory buffer-file-name))
  (cd ada-alire-project-root)

  (let ((crate-name
         (read-string "Crate name to remove: ")))
    (shell-command (format "alr with --del %s" crate-name)))

  (cd ada-alire-current-directory))

(defun ada-alire-update ()
  (interactive)

  (setf ada-alire-current-directory (file-name-directory buffer-file-name))
  (cd ada-alire-project-root)
  (shell-command "alr update")

  (cd ada-alire-current-directory))

;;; ada-alire.el ends here
