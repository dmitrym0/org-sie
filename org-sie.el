;;; org-sie.el --- Simple Incremental Everything for Org  -*- lexical-binding: t; -*-
(require 'org-review)
(require 'org-drill)

;; Copyright © 2024 Dmitry Markushevich

;; Author: Dmitry Markushevich <dmitrym@gmail.com>
;; Keywords: org-mode, tasks, prioritization
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (org-review "20230119.1706") (org-drill "2.7.0"))
;; URL: https://github.com/dmitrym0/org-sie


;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Prioritize tasks based on "interest" or "motivation".


;;; Code:



(defvar org-sie-last-review-property "SIE_LAST_REVIEW")
(defvar org-sie-next-review-property "SIE_NEXT_REVIEW")

(setq org-review-last-timestamp-format 'inactive)


;; (setq org-review-last-property-name org-sie-last-review-property)
;; (setq org-review-next-property-name org-sie-next-review-property)

;; "How important is this to you?"
;; The higher the value the highter the importance.
;; 1  - not important. Not very interested.
;; 5  - very important. Most is interested.
;; (inverse is passed to org-drill)
(defun setup-org-sie-keybindings-for-agenda ()
  "Set up a keymap for specifying interest during the review process."
  (local-set-key (kbd "C-c d 1")
                 (lambda ()
                   (interactive)
                   (org-sie-get-next-review-from-agenda 5)
                   (org-agenda-next-item 1)
                   ))
  (local-set-key (kbd "C-c d 2")
                 (lambda ()
                   (interactive)
                   (org-sie-get-next-review-from-agenda 4)
                   (org-agenda-next-item 1)
                   ))
  (local-set-key (kbd "C-c d 3")
                 (lambda ()
                   (interactive)
                   (org-sie-get-next-review-from-agenda 3)
                   (org-agenda-next-item 1)
                   ))
  (local-set-key (kbd "C-c d 4")
                 (lambda ()
                   (interactive)
                   (org-sie-get-next-review-from-agenda 2)
                   (org-agenda-next-item 1)
                   ))

  (local-set-key (kbd "C-c d 5")
                 (lambda ()
                   (interactive)
                   (org-sie-get-next-review-from-agenda 1)
                   (org-agenda-next-item 1)
                   ))
  )



(global-set-key (kbd "C-c d s")
                (lambda ()
                  (interactive)
                  (org-sie-start-sie-on-heading)))

(defun org-sie-start-sie-on-heading ()
  "Add current heading to org-sie review list."
  (interactive)
  (org-set-property org-sie-next-review-property "[2000-01-01 Mon]"))

(defun org-sie-get-next-review-from-agenda (quality)
  "Return the next review date.
=QUALITY= is an int (0 through 5) that indicates our level of interest in
performing this task.  See =org-sie-get-next-review= for more info."
  (save-window-excursion
    (org-agenda-goto)
    (org-sie-get-next-review quality)))


;; borrowed from: https://github.com/chrisbarrett/nursery/blob/main/lisp/org-roam-review.el
(defun org-sie-get-next-review (quality)
  "Adapted from =org-drill=.  1-Soon 5-Later.

QUALITY is a number 0-5 inclusive.

- only use sm5 algorithm for simplicity
- use properties instead of SCHEDULED.
- remove support for 'weighting' a node."
  (-let* ((ofmatrix org-drill-sm5-optimal-factor-matrix)
          ((last-interval repetitions failures total-repeats meanq ease) (org-drill-get-item-data))
          ((next-interval repetitions ease failures meanq total-repeats new-ofmatrix)
           (org-drill-determine-next-interval-sm5 last-interval repetitions
                                                  ease quality failures
                                                  meanq total-repeats ofmatrix))
          (next-interval (round (if (cl-minusp next-interval)
                                    next-interval
                                  (max 1.0 (+ last-interval (- next-interval last-interval))))))
          (new-time (ts-adjust 'day next-interval (ts-now))))
    (setq org-drill-sm5-optimal-factor-matrix new-ofmatrix)
    (org-drill-store-item-data next-interval repetitions failures total-repeats meanq ease)

    (let ((next-review (ts-format "[%Y-%m-%d %a]" new-time)))
      (org-set-property org-sie-next-review-property next-review)
      next-review)))


(add-hook 'org-agenda-mode-hook 'setup-org-sie-keybindings-for-agenda)

(provide 'org-sie)
;;; org-sie.el ends here
