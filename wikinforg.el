;;; wikinforg.el --- Org-mode wikinfo integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Nicholas Vollmer

;; Author: Nicholas Vollmer <progfolio@protonmail.com>
;; URL: https://github.com/progfolio/wikinforg
;; Created: September 14, 2020
;; Keywords: org, convenience
;; Package-Requires: ((emacs "27.1") (wikinfo "0.0.0"))
;; Version: 0.0.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; TODO: finish commentary

(require 'wikinfo)
(require 'org)

;;; Code:
;;;; Customizations
(defgroup wikinforg nil
  "Org wikinfo integration"
  :group 'wikinforg
  :prefix "wikinforg-")

(defcustom wikinforg-include-extract t
  "Whether or not to include a summary in the resultant entry's body."
  :type 'boolean)

(defcustom wikinforg-query-format "%s"
  "Format string for queries."
  :type 'string)

;;;; Functions
(defun wikinforg--format-query (query)
  "Return formatted QUERY using `wikinforg-query-format' string."
  (format wikinforg-query-format query))

;;;; Commands
;;;###autoload
(defun wikinforg (&optional arg query predicate)
  "Return Org entry from `wikinfo'.
QUERY and PREDICATE are passed to `wikinfo'.
Don't know what I want to do with ARG yet.
for now a single universal arg causes the entry to be messaged instead of inserted."
  (interactive "P")
  (let* ((query (string-trim
                 (wikinforg--format-query (or query (read-string "Wikinforg: ")))))
         (info (wikinfo query predicate))
         (result (with-temp-buffer
                   (org-insert-heading)
                   (insert (or (wikinfo--plist-path info :wikinfo :title) query))
                   (dolist (keyword
                            (seq-filter
                             (lambda (el) (and (keywordp el)
                                               (not (member el '(:wikinfo)))))
                             info))
                     (org-set-property (substring (symbol-name keyword) 1)
                                       (format "%s" (plist-get info keyword))))
                   (let ((id (wikinfo--plist-path info :wikinfo :id)))
                     (org-set-property "URL" (format "%s?curid=%d" wikinfo-base-url id))
                     (org-set-property "wikinfo-id" (format "%s" id)))
                   (when wikinforg-include-extract
                     (goto-char (point-max))
                     (insert (wikinfo--plist-path info :wikinfo :extract)))
                   (buffer-string))))
    (if (or arg (not (called-interactively-p 'interactive)))
        (pcase arg
          ('(4) (message result))
          (_ result))
      ;;save-excursion doesn't work here?
      (let ((p (point)))
        (if (derived-mode-p 'org-mode)
            (org-paste-subtree nil result)
          (insert (with-temp-buffer
                    (org-mode)
                    (org-paste-subtree nil result)
                    (buffer-string))))
        (goto-char p)))))

;;;###autoload
;;@TODO: suffix should be optional
(defun wikinforg-capture (suffix)
  "Wikinforg wrapper for use in capture templates.
Call `wikinforg' command with search SUFFIX.
If the wikinforg call fails, the user's query is returned.
If the command is aborted, an empty string is returned so the capture will not error."
  (condition-case nil
      (let ((query (or (read-string (format "Wikinforg (%s): " suffix))
                       "")))
        (condition-case nil
            (wikinforg nil (concat query " " suffix))
          (quit (concat "* " query))))
    ;;@TODO: Assumes org-capture entry type of "entry"
    ;; should be more flexible or caller's responsibility.
    ;; Possibly just query org-current-plist for :type
    (quit "*")))

(provide 'wikinforg)

;;; wikinforg.el ends here
