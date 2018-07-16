;;; orgability-utils.el --- reading list manager in `org-mode'

;; Copyright (c) 2018 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Created: 25 Apr 2018

;; Keywords: org-mode
;; Homepage: https://github.com/d12frosted/orgability

;; Package-Version: 0.1.1
;; Package-Requires: ((org-mode "9.1.0"))

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;

;;; Code:
;;

(require 'org)

(defmacro orgability--with-entry (&rest body)
  "Move to buffer and point of current entry for the duration of BODY."
  `(cond ((eq major-mode 'org-mode)
          (org-with-point-at (point) ,@body))
         ((eq major-mode 'org-agenda-mode)
          (org-agenda-with-point-at-orig-entry nil ,@body))))

(defun orgability--remove-till (itemr stopr)
  "Remove lines matching ITEMR until STOPR is found."
  (while (not (looking-at stopr))
    (if (looking-at itemr)
        (kill-whole-line)
      (forward-line 1)
      (beginning-of-line))))

(defun orgability--unwrap-link (link)
  "Get the link without the type."
  (funcall (orgability--compose
            (apply-partially #'string-remove-prefix "id:")
            (apply-partially #'string-remove-prefix "file:"))
           link))

(defun orgability--lookup-key (key-map function)
  "Returns key binding for a FUNCTION in a KEY-MAP."
  (string (car (rassoc function (cdr key-map)))))

(defun orgability--compose (&rest funs)
  "Return function composed of FUNS."
  (lexical-let ((lex-funs funs))
    (lambda (&rest args)
      (reduce 'funcall (butlast lex-funs)
              :from-end t
              :initial-value (apply (car (last lex-funs)) args)))))



(defvar orgability--drawer-list-prefix "- ")

(defun orgability--drawer-block (name &optional force inside)
  "Return the (beg . end) range of the body of the NAME drawer.
If the drawer does not exist, create it if FORCE is non-nil, or
return nil."
  (setq name (upcase name))
  (orgability--with-entry
   (unless (org-before-first-heading-p)
     (org-back-to-heading t)
     (end-of-line)
     (let* ((drawer-beg-regexp (concat "^[ \t]*:" name ":[ \t]*$"))
            (drawer-end-regexp "^[ \t]*:END:[ \t]*$")
            (bound (save-excursion
                     (if (search-forward-regexp org-heading-regexp)
                         (beginning-of-line)
                       (end-of-buffer))
                     (point)))
            (beg)
            (end))
       (when (search-forward-regexp drawer-beg-regexp bound t)
         (when inside
           (forward-line))
         (setq beg (line-beginning-position))
         (goto-char beg)
         (when (search-forward-regexp drawer-end-regexp bound t)
           (setq end (line-beginning-position))))
       (if (and (not (null beg))
                (not (null end)))
           (cons beg end)
         (when force
           (goto-char (cdr (org-get-property-block)))
           (forward-line 1)
           (open-line 1)
           (indent-for-tab-command)
           (insert ":" name ":\n")
           (indent-for-tab-command)
           (insert ":END:")
           (orgability--drawer-block name nil)))))))

(defun orgability--drawer-list-elements (name &optional parser prefix)
  "Get the content of NAME drawer as a list."
  (setq prefix (or prefix orgability--drawer-list-prefix))
  (setq parser (or parser #'identity))
  (let ((block (orgability--drawer-block name t t)))
    (orgability--with-entry
     (seq-map
      (lambda (line)
        (funcall parser (string-trim-left line prefix)))
      (split-string (buffer-substring-no-properties (car block) (cdr block)) "\n" t)))))

(defun orgability--drawer-link-url-parser (link)
  "Parse title from an org LINK."
  (replace-regexp-in-string "\\[\\[\\(.*\\)\\]\\[.*\\]\\]" "\\1" link))

(defun orgability--drawer-link-title-parser (link)
  "Parse title from an org LINK."
  (replace-regexp-in-string "\\[\\[.*\\]\\[\\(.*\\)\\]\\]" "\\1" link))

(defun orgability--drawer-link-parser (link)
  "Parse (url . title) from an org LINK."
  (cons
   (orgability--drawer-link-url-parser link)
   (orgability--drawer-link-title-parser link)))

(defun orgability--drawer-has-element (name el &optional testfn prefix)
  "Returns non-nil if NAME drawer contains element EL."
  (setq prefix (or prefix orgability--drawer-list-prefix))
  (setq testfn (or testfn #'orgability--link-p))
  (seq-contains (orgability--drawer-list-elements name nil prefix)
                el
                testfn))

(defun orgability--drawer-add-element (name el &optional testfn prefix)
  "Add element EL to NAME drawer."
  (setq prefix (or prefix orgability--drawer-list-prefix))
  (unless (orgability--drawer-has-element name el testfn prefix)
    (orgability--with-entry
     (goto-char (cdr (orgability--drawer-block name t)))
     (newline-and-indent)
     (forward-line -1)
     (insert (concat prefix el)))))

(defun orgability--drawer-del-element (name el &optional parser testfn prefix)
  (setq prefix (or prefix orgability--drawer-list-prefix))
  (setq parser (or parser #'identity))
  (setq testfn (or testfn #'orgability--link-p))
  (let ((block (orgability--drawer-block name t t)))
    (orgability--with-entry
     (goto-char (car block))
     (while (< (point) (cdr block))
       (if (funcall
            testfn
            el
            (funcall parser
                     (string-trim-left
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))
                      prefix)))
           (kill-whole-line)
         (forward-line 1)
         (beginning-of-line))))))

(defun orgability--link-p (url content)
  "Returns non-nil when CONTENT is a link to URL."
  (or (string-equal content url)
      (string-match-p (concat "\\[\\[" url "\\]\\[.*\\]\\]") content)))

(provide 'orgability-utils)

;;; orgability-utils.el ends here
