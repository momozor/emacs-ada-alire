;;; ada-alire.el --- A lightweight Ada Alire's alr commander  -*- lexical-binding: t -*-

;; Copyright (c) 2020 Momozor <skelic3@gmail.com, momozor4@gmail.com>
;; Copyright (C) 2015  Kevin W. van Rooijen

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


;; Author: Momozor <skelic3@gmail.com, momozor4@gmail.com>
;; Version: 0.2
;; Keywords: tools, ada
;; URL: https://github.com/momozor/ada-alire
;; Package-Requires: ((emacs "24") (ada-mode "7.1.1") (toml-mode "0.1.3"))

;;; Commentary:

;; Ada Alire is a thin command wrapper run certain alr
;; commands without switching to terminal (more focus on project development).

;; Full documentation is available as an Info manual.

;;; Code:

(require 'compile)
(require 'button)
(require 'ada-mode)

(provide 'ada-alire)

(defvar ada-alire-minor-mode-map (make-keymap) "ada-alire-mode keymap.")
(defvar ada-alire-minor-mode nil)

;;;###autoload
(define-minor-mode ada-alire-minor-mode
  "Ada Alire minor mode. Used to hold keybindings for ada-alire mode.

\\{ada-alire-minor-mode-map}"
  nil " ada-alire" ada-alire-minor-mode-map)

(define-key ada-alire-minor-mode-map (kbd "C-c C-c C-s") 'ada-alire-set-project-path)
(define-key ada-alire-minor-mode-map (kbd "C-c C-c C-c") 'ada-alire-cd-to-project)
(define-key ada-alire-minor-mode-map (kbd "C-c C-c C-r") 'ada-alire-run)
(define-key ada-alire-minor-mode-map (kbd "C-c C-c C-b") 'ada-alire-build)
(define-key ada-alire-minor-mode-map (kbd "C-c C-c C-i") 'ada-alire-install-crate)
(define-key ada-alire-minor-mode-map (kbd "C-c C-c C-u") 'ada-alire-uninstall-crate)

(defvar ada-alire-current-directory)
(defvar ada-alire-project-root)

(defun ada-alire--start (name command &optional last-command)
  (let* ((buffer (concat "*ada-alire (alr) " name "*")))

    (setf ada-alire-current-directory
          (file-name-directory buffer-file-name))

    (cd ada-alire-project-root)
    (shell-command (format "alr %s %s" command last-command)
                   buffer)

    (cd ada-alire-current-directory)))

(defun ada-alire-clean ()
  (interactive)

  (message "Cleaning project..")
  (ada-alire--start "clean" "clean"))

(defun ada-alire-info ()
  (interactive)

  (message "Showing project info..")
  (ada-alire--start "info" "version"))

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

(defun ada-alire-uninstall-crate ()
  (interactive)

  (setf ada-alire-current-directory (file-name-directory buffer-file-name))
  (cd ada-alire-project-root)

  (let ((crate-name
         (read-string "Crate name to uninstall: ")))
    (shell-command (format "alr with --del %s" crate-name)))

  (cd ada-alire-current-directory))

(defun ada-alire-update ()
  (interactive)

  (setf ada-alire-current-directory (file-name-directory buffer-file-name))
  (cd ada-alire-project-root)
  (shell-command "alr update")

  (cd ada-alire-current-directory))

;;; ada-alire.el ends here
