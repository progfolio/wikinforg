;;; wikinforg.el --- Org-mode wikinfo integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Nicholas Vollmer

;; Author: Nicholas Vollmer <progfolio@protonmail.com>
;; URL: https://github.com/progfolio/wikinforg
;; Created: September 14, 2020
;; Keywords: org, convenience
;; Package-Requires: ((emacs "25.1") (wikinfo "0.0.0"))
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

(defun wikinforg--entry (info)
  "Return entry string from INFO plist.")

(defun wikinforg--get (infolist prop &optional separator)
  "Return formatted PROP string from INFOLIST."
  (string-join (plist-get infolist prop) (or separator ", ")))

(defun wikinforg (&optional arg search suffix)
  "Return Org entry from `wikinfo-infobox'.
If SUFFIX is non-nil, it is appended to SEARCH.
@TODO FINISH DOCSTRING.
ARG = thing at point updated?"
  (interactive "P")
  (let* ((search (or search
                     (read-string (concat "Wikinforg"
                                          (when suffix (format "(%s)" suffix))
                                          ": "))))
         (suffix (or suffix ""))
         (info (wikinfo (format "%s %s" search suffix)))
         (result (with-temp-buffer
                   (org-insert-heading)
                   (insert (wikinforg--get info :wikinfo-entity))
                   (dolist (keyword (remq :wikinfo-entity
                                          (seq-filter #'keywordp info)))
                     (org-set-property (substring (symbol-name keyword) 1)
                                       (wikinforg--get info keyword)))
                   (buffer-string))))
    (if (or arg (not (called-interactively-p 'interactive)))
        (pcase arg
          ('(4) (message result))
          (_ result))
      (save-excursion
        ;;@TODO: can we use some org-aware function
        ;;so respect content variable is respected?
        (insert (concat
                 (make-string (max (1- (org-outline-level)) 0) ?*)
                 result))))))

(defun wikinforg-capture (suffix)
  "Wikinforg wrapper for use in capture templates.
Call `wikinforg' command with search SUFFIX.
If the wikinforg call fails, the user's query is returned.
If the command is aborted, an empty string is returned so the capture will not error."
  (condition-case nil
      (let ((query (or (read-string (format "Wikinforg (%s): " suffix))
                       "")))
        (condition-case nil
            (wikinforg nil query suffix)
          (error (concat "* " query))))
    (quit "*")))

(provide 'wikinforg)

;;; wikinforg.el ends here
