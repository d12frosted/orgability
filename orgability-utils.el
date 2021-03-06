;;; orgability-utils.el --- reading list manager in `org-mode'

;; Copyright (c) 2018 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Created: 25 Apr 2018

;; Keywords: org-mode
;; Homepage: https://github.com/d12frosted/orgability

;; Package-Version: 0.2.0
;; Package-Requires: ((org "9.1.0"))

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

(defun orgability--link-url-parser (link)
  "Parse title from an org LINK."
  (replace-regexp-in-string "\\[\\[\\(.*\\)\\]\\[.*\\]\\]" "\\1" link))

(defun orgability--link-title-parser (link)
  "Parse title from an org LINK."
  (replace-regexp-in-string "\\[\\[.*\\]\\[\\(.*\\)\\]\\]" "\\1" link))

(defun orgability--link-parser (link)
  "Parse (url . title) from an org LINK."
  (cons
   (orgability--link-url-parser link)
   (orgability--link-title-parser link)))

(defun orgability--link-p (url content)
  "Returns non-nil when CONTENT is a link to URL."
  (or (string-equal content url)
      (string-match-p (concat "\\[\\[" url "\\]\\[.*\\]\\]") content)))

(provide 'orgability-utils)

;;; orgability-utils.el ends here
