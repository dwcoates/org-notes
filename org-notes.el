;;; org-notes.el --- A simply way to link, connect, and browse notes taken in org-mode.
;;
;;; Author: Dodge W. Coates
;;
;;; Commentary:
;;
;; This aims to solve a couple of problems simply:
;;
;; 1. Provide a means for easily searching through notes and note tags simply
;; via helm.
;;
;; 2. Provide a way to add links to other notes inside of a give note, and
;; have those instantiate links automatically reciprocated in the linkee.
;;
;; 3. Create a natural, simple means for inter-connecting distant and related
;; concepts in a way that is as searchable and appreciable as possible.
;;
;;; Usage:
;;
;; The `helm-org-notes-find' function can be used to return the id for a given
;; note.  This is only useful as an API function.
;;
;;; Code:

(require 'org)
(require 'helm)

(defvar org-note-accepted-tasks '("NOTE" "LEARN" "REVIEW" "BUG" "ISSUE" "FEATURE" "DONE"))
(defvar org-note-locations nil)

(defun org-note-org-id-locations-load-advice (funct)
  "`org-id-locations-load' advice updating `org-note-locations' w/ FUNCT and ARGS."
  (funcall funct)
  (setq org-note-locations
        (remove-if 'not
         (mapcar
          'org-note-get-heading
          (mapcar 'cadr (org-id-hash-to-alist org-id-locations)))))
  (message "Updated org-id-locations. Contains %d notes."
           (length org-note-locations)))

(advice-add 'org-id-locations-load :around 'org-note-org-id-locations-load-advice)

(defun org-note-org-id-add-location-advice (funct &rest args)
  "`org-id-add-location' advice updating `org-note-locations' w/ FUNCT and and id from ARGS."
  (funcall funct args)
  (let ((head-id (org-note-get-heading (car args))))
    (when head-id
      (add-to-list 'org-note-locations head-id))))

(advice-add 'org-id-add-location :around 'org-note-org-id-add-location-advice)

(defun org-note-get-heading (id)
  "Return the cons cell consisting of heading and the ID to which heading corresponds."
  (let ((addr (org-id-find id)))
    (when addr
      (with-current-buffer
          (org-get-agenda-file-buffer (car addr))
        (goto-char (cdr addr))
        (when (member (elt (org-heading-components) 2)
                      org-note-accepted-tasks)
          (cons (org-get-heading) id))
        ))))

(defun helm-org-notes-lookup-note ()
  "Wrapper for `org-note-locations'."
  org-note-locations)

(defun helm-org-notes-find ()
  "Return the org-id for a given note in the org-note-locations alist."
  (helm :sources (helm-build-sync-source "Org Notes"
                   :candidates 'helm-org-notes-lookup-note
                   :candidate-number-limit 2500)
        :buffer "*Org Note Headerings*"))

(defun helm-org-notes-goto ()
  "Navigate to the location specified by an `helm-org-notes-find' call."
  (interactive)
  (org-id-goto (helm-org-notes-find))
  (outline-show-subtree))

(provide 'org-notes)

;;; org-notes.el ends here
