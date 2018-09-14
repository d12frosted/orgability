;;; orgability.el --- reading list manager with offline access support.

;; Copyright (c) 2018 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Created: 25 Apr 2018

;; Keywords: org-mode
;; Homepage: https://github.com/d12frosted/orgability

;; Package-Version: 0.2.0
;; Package-Requires: ((org "9.1.0") (org-cliplink "0.2") (org-board "1136") (org-brain "0.5"))

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;
;; Please checkout home page.

;;; Code:
;;

(require 'org-cliplink)
(require 'org-board)

(require 'orgability-utils)
(require 'orgability-drawer)
(require 'orgability-brain)

(defvar orgability-file (concat user-home-directory "orgability.org")
  "File for storing reading list.")

(defvar orgability-add-to-top t
  "If non-nil, entries are added to the top of the `orgability-file'.")

(defvar orgability-todo-keyword "TODO"
  "If non-nil, entries are added with a selected todo keyword.")

(defvar orgability-active-timestamp nil
  "If non-nil, ADDED timestamp is active.")

(defvar orgability-auto-archive t
  "If non-nil, entry is automatically archived.")

(defvar orgability-use-relative-archive-url t
  "If non-nil, use relative links to archive.

Useful when using several computers with different $HOME
directories.")

(defvar orgability-auto-agenda-redo t
  "If non-nil, redo agenda on clip.")

(defconst orgability-title "Reading list"
  "Title of `orgability-file'.")

(defconst orgability-category "reading-list"
  "Category of `orgability-file' entries.")

(defconst orgability-file-tags "reading"
  "Tags of `orgability-file' entries.")

(defconst orgability-topics-drawer "topics")

(defvar orgability-extract-http-title-f
  'org-cliplink-retrieve-title-synchronously
  "Function to extract title from http URL.")

(defun orgability-create-file (file)
  "Create an orgability FILE if it doesn't exist."
  (unless (file-exists-p file)
    (with-temp-file file
      (insert "#+TITLE: " orgability-title "\n"
              "#+CATEGORY: " orgability-category "\n"
              "#+FILETAGS: " orgability-file-tags "\n\n"))))

;;;###autoload
(defun orgability-open ()
  "Open archived file for entry at point."
  (interactive)
  (orgability--with-entry
   (call-interactively #'org-board-open)))

;;;###autoload
(defun orgability-clip ()
  "Store an URL from the clipboard."
  (interactive)
  (let ((url (substring-no-properties (current-kill 0))))
    (orgability-store-url url)))

;;;###autoload
(defun orgability-store-url (url)
  "Store an URL."
  (interactive "sUrl: ")
  (let ((title (funcall orgability-extract-http-title-f url)))
    (orgability-add-entry title url)))

(defun orgability-add-entry (title url &optional props)
  "Add read entry with TITLE and URL and optional PROPS."
  (unless title
    (setq title (read-string "Title: ")))
  (unless url
    (user-error "title is nil"))
  (unless orgability-file
    (user-error "`orgability-file' is not set"))
  (orgability-create-file orgability-file)
  (save-excursion
    (let ((buffer (find-file-noselect orgability-file)))
      (with-current-buffer buffer
        (if (and orgability-add-to-top
                 (progn (goto-char (point-min))
                        (search-forward "*" nil t)))
            (previous-line)
          (goto-char (point-max)))
        (org-insert-heading nil nil t)
        (when orgability-todo-keyword
          (insert orgability-todo-keyword " "))
        (insert title "\n")
        (org-set-property "URL" url)
        (org-set-property "ADDED" (concat
                                   (if orgability-active-timestamp "<" "[")
                                   (format-time-string "%Y-%02m-%02d")
                                   (if orgability-active-timestamp ">" "]")))
        (mapc (lambda (p) (org-set-property (car p) (cdr p))) props)
        (save-buffer)
        (when orgability-auto-archive
          (org-board-archive)
          (when orgability-use-relative-archive-url
            (let ((link (car (last
                              (org-entry-get-multivalued-property
                               (point) "ARCHIVED_AT")))))
              (org-set-property
               "ARCHIVED_AT"
               (replace-regexp-in-string
                (file-name-directory (buffer-file-name))
                ""
                link))))))
      (when org-agenda-buffer
        (with-current-buffer org-agenda-buffer
          (org-agenda-redo))))))

;;;###autoload
(defun orgability-add-topic ()
  "Add an `org-brain' topic to reading entry under point.

It works in a two-way fashion, meaning that the reading entry is
added as a resource to topic."
  (interactive)
  (orgability--with-entry
   (let* ((entry (orgability-brain-choose-entry))
          (link (orgability-brain-get-link entry))
          (id (org-id-get-create)))
     (unless (orgability--drawer-has-element orgability-topics-drawer link)
       ;; TODO: make sure that it's not being double added
       (orgability-brain-add-resource id entry)
       (orgability--drawer-add-element
        orgability-topics-drawer
        (org-make-link-string (orgability-brain-get-link entry)
                              (org-brain-title entry))))))
  (ignore-errors
    (org-agenda-redo)))

;;;###autoload
(defun orgability-delete-topic ()
  "Select and delete topic for the entry at point.

It works in a two-way fashion, meaning that the reading entry is
remove from resources of the topic."
  (interactive)
  (orgability--with-entry
   (let* ((id (org-id-get-create))
          (topics (orgability-list-topics))
          (target (completing-read
                   "Topic: "
                   (mapcar #'cdr topics)))
          (link (car (find-if (lambda (x)
                                (string-equal target
                                              (cdr x)))
                              topics))))
     ;; TODO: use orgability--drawer-del-element for this
     (orgability-brain-delete-resource id (orgability--unwrap-link link))
     (orgability--drawer-del-element orgability-topics-drawer link)))
  (ignore-errors
    (org-agenda-redo)))

(defun orgability-list-topics ()
  "Get the topics list of entry at point."
  (interactive)
  (orgability--drawer-list-elements
   orgability-topics-drawer
   #'orgability--drawer-link-parser))

(defvar orgability-agenda-relations-make-links nil
  "If non-nil, relations will be inserted as links in agenda.")

(defvar orgability-agenda-relations-separator " | "
  "Separator between relations in agenda.")

(defvar orgability-agenda-topics-column 24
  "Width of topics block in `org-agenda'.")

(defun orgability-agenda-list-topics ()
  "Returns string with topics to be inserted to `org-agenda'."
  (let* ((topics (orgability-list-topics))
         (cl orgability-agenda-topics-column)
         (l (length
             (string-join
              (seq-map #'cdr topics)
              orgability-agenda-relations-separator)))
         (extra-space (make-string (max 1 (- cl l)) ? )))
    (concat
     (string-join
      (seq-map
       (lambda (x)
         (if orgability-agenda-relations-make-links
             (org-make-link-string (car x) (cdr x))
           (cdr x)))
       topics)
      orgability-agenda-relations-separator)
     extra-space)))

(provide 'orgability)

;;; orgability.el ends here
