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

(defvar org-notes-accepted-tasks '("NOTE" "LEARN" "REVIEW" "BUG" "ISSUE" "FEATURE" "DONE"))
(defvar org-notes-locations nil)
(defvar org-notes-drawer-name "LINKS")

(defun org-notes-org-id-locations-load-advice (funct)
  "`org-id-locations-load' advice updating `org-notes-locations' w/ FUNCT and ARGS."
  (funcall funct)
  (setq org-notes-locations
        (remove-if 'not
         (mapcar
          'org-notes-get-heading
          (mapcar 'cadr (org-id-hash-to-alist org-id-locations)))))
  (message "Updated org-id-locations. Contains %d notes."
           (length org-notes-locations)))

(advice-add 'org-id-locations-load :around 'org-notes-org-id-locations-load-advice)

(defun org-notes-org-id-add-location-advice (funct &rest args)
  "`org-id-add-location' advice updating `org-notes-locations' w/ FUNCT and and id from ARGS."
  (funcall funct args)
  (let ((head-id (org-notes-get-heading (car args))))
    (when head-id
      (add-to-list 'org-notes-locations head-id))))

(advice-add 'org-id-add-location :around 'org-notes-org-id-add-location-advice)

(defun org-notes-get-heading (id)
  "Return the cons cell consisting of heading and the ID to which heading corresponds."
  (let ((addr (org-id-find id)))
    (when addr
      (with-current-buffer
          (org-get-agenda-file-buffer (car addr))
        (goto-char (cdr addr))
        (when (member (elt (org-heading-components) 2)
                      org-notes-accepted-tasks)
          (cons (org-get-heading) id))
        ))))

(defun org-notes--helm-lookup-note ()
  "Wrapper for `org-notes-locations'."
  org-notes-locations)

(defun org-notes--helm-find ()
  "Return the org-id for a given note in the `org-notes-locations' alist."
  (helm :sources (helm-build-sync-source "Org Notes"
                   :candidates 'org-notes--helm-lookup-note
                   :candidate-number-limit 2500)
        :buffer "*Org Notes Headings*"))

(defun org-notes-helm-goto ()
  "Navigate to the location specified by an `helm-org-notes-find' call."
  (interactive)
  (org-id-goto (org-notes--helm-find))
  (outline-show-subtree))

(defun org-notes-add-link-to-drawer (link entry-delimiter)
  "Add LINK to the links drawer, preceded by ENTRY-DELIMITER."
  (save-excursion
    (let ((org-log-into-drawer org-notes-drawer-name))
      (goto-char (org-log-beginning t))
      (insert (concat ":" entry-delimiter ": " link "\n"))
      (goto-char (org-log-beginning))
      (forward-line -1)
      (org-indent-drawer))))

(defun org-notes-helm-link-notes ()
  "Links selected note in a log drawer for current heading.
Also links the id of current heading in a link drawer under
heading corresponding to selected note.  Results in a two-way
link between two org headings."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Cannot link notes when not in an org context"))
  (let* ((loc-id (org-id-get-create))
         (loc-heading (or (org-get-heading t t) (error "Not an an org-mode heading")))
         (dest-id (org-notes--helm-find))
         (dest-heading (let ((case-fold-search)
                             (heading (concat
                                       "* "
                                       (car (rassoc dest-id org-notes-locations)))))
                         (string-match
                          org-complex-heading-regexp
                          heading)
                         (or (match-string 4 heading)
                             "UNKNOWN")))
         (forward-link (org-make-link-string
                        (concat "id:" dest-id)
                        dest-heading))
         (back-link (org-make-link-string
                     (concat "id:" loc-id)
                     loc-heading)))
    ;; Insert forward link in source note
    (when dest-id                       ; do nothing if org-notes--helm-find is quit unexpectedly
      (org-notes-add-link-to-drawer forward-link ">")
      ;; Insert backward link in linked note
      (save-excursion
        (with-temp-buffer
          (org-id-goto dest-id)
          (org-notes-add-link-to-drawer back-link "<")))
      (message "Linked '%s' and '%s'" loc-heading dest-heading))))

(provide 'org-notes)

;;; org-notes.el ends here
