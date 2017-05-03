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
;; Update `org-note-accepted-tasks' list to set the task types to which `org-notes'
;; applies.
;;
;; The `org-notes--helm-find' function can be used to return the id for a given
;; note.  This is only useful as an API function.
;;
;; The `org-notes-helm-goto' function is used to navigate to a particular note
;; using `helm-org-notes-find' interface.
;;
;; The `org-notes-helm-link-notes' function is used to link two notes
;; together. Should be called while point is in the context of an org heading, at
;; which point it will prompt you to select a note from `org-notes-locations'. The
;; selected note and heading context of point will each have links added to each
;; other's respective LINKS drawer.
;;
;; `helm-execute-persistent-action', bound to `C-z' by default, can be used to
;; display the currently selected org-note when in org-notes--helm-find, which
;; is used by `org-notes-goto' and `org-notes-helm-link-notes'
;;
;; These functions can be very useful for browsing notes; use `org-notes-goto'
;; to browse with `helm-execute-persistent-action', jump to an interesting
;; note, and use `org-notes-jump-to-note' to jump back to location from which
;; `org-notes-goto' was last called.
;;
;;; Code:

(require 'org)
(require 'helm)

(defvar org-notes-accepted-tasks '("NOTE" "LEARN" "REVIEW" "BUG" "ISSUE" "FEATURE" "DONE"))
(defvar org-notes-locations nil)
(defvar org-notes-drawer-name "LINKS")
(defvar org-notes-always-add-note nil
  "If non-nil, always insert a note with `org-notes-helm-link-notes' link.")
(defvar org-notes-show-latex-on-jump t)
(defvar org-notes-prompt-for-note t
  "If non-nil, ask to add a note inserted with `org-notes-helm-link-notes' link.
Has no effect if `org-notes-always-add-note' is non-nil.")
(defvar org-notes--jump-to-note-register (cons nil nil)
  "Possible locations for `org-notes-jump-to-note'.
These are stored as a cons cell in, the car of which is the last
location from which `org-notes-helm-goto' was called, and the cdr
the location of last note *linked to* by
`org-notes-helm-link-notes'." )

(define-key org-mode-map (kbd "C-c l") 'org-notes-add-link-to-drawer)
(define-key org-mode-map (kbd "C-c j") 'org-notes-helm-goto)
(global-set-key (kbd "C-c j") 'org-notes-helm-goto)
(define-key org-mode-map (kbd "C-c C-j") 'org-notes-jump-to-note)
(global-set-key (kbd "C-c C-j") 'org-notes-jump-to-note)

(defun org-notes--heading-regexp ()
  "Regular expression for parsing headings in `org-notes-locations'.
Group 1: (accepted) task
Group 2: priority cookie
Group 3: heading title
Group 4: tags"
    (concat
     "^"                                ; beginning of line
     (concat "\\(" (mapconcat 'identity org-notes-accepted-tasks "\\|") "\\)?") ; match accepted tasks
     "\\(?: +\\(\\[#.\\]\\)\\)?"         ; match priority cookies, which may or may not exist
     "\\(?: +\\(.*?\\)\\)??"             ; match base heading, which must exist
     "\\(?: +\\(:[[:alnum:]_@#%:]+:\\)\\)?" ; match tags, which may or may not exist
     "[ 	]*\\'"                               ; match rest of heading
     ))

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
          (cons (org-get-heading) id))))))

(defun org-notes--helm-lookup-note (source-tags)
  "Wrapper for sorting `org-notes-locations' using SOURCE-TAGS."
  (org-notes--sort-locations source-tags))

(defun org-notes--helm-display-note (candidate)
  "Display the note corresponding to CANDIDATE.
This will display the note corresponding to candidate in a
separate window, split from `helm-buffer'.  Used by
`helm-execute-persistent-action' in `org-notes--helm-find'."
  (save-excursion
    (let* ((buf-name (substring-no-properties
                      (let ((location (org-id-find candidate 'marker)))
                        (unless location
                          (error "Cannot find the candidate's location"))
                        (with-current-buffer (marker-buffer location)
                          (org-with-wide-buffer
                           (goto-char location)
                           (org-get-heading t t))))))
          (buf      (get-buffer-create buf-name))
          (win      (get-buffer-window buf))
          (helm--reading-passwd-or-string t))
      (cond ((and buf win (eq buf (get-buffer helm-current-buffer)))
             (user-error
              "Can't kill `helm-current-buffer' without quitting session"))
            ((and buf win)
             ;; remove window split, and remove the preview buffer
             (delete-window (get-buffer-window buf))
             (unless (kill-buffer buf)
               (error "Buffer not killed?")))
            (t
             ;; create preview buffer in the window split
             (switch-to-buffer buf)
             (delete-region (point-min) (point-max))
             (let* ((inhibit-message t) (org-inhibit-startup t)) (org-mode))
             (insert
              (let ((location (org-id-find candidate 'marker)))
                (with-current-buffer (marker-buffer location)
                  (org-with-wide-buffer
                   (org-with-limited-levels
                    (goto-char location)
                    (buffer-substring
                     (save-excursion (org-back-to-heading) (point))
                     (save-excursion (org-end-of-subtree) (point))))))))
             (beginning-of-buffer)
             ;; display latex fragments as images
             (org-notes-turn-on-display-latex-fragments)
             ;; turn on pretty entities
             (setq-local org-pretty-entities t)
             (org-restart-font-lock)
             ;; resize helm buffer
             (run-hooks helm-autoresize-mode-hook)
             ))))
  )

(defun org-notes-turn-on-display-latex-fragments ()
  "Display latex fragments in the current buffer.
Intended for use in the preview buffer, because
`org-preview-latex-fragment' is a dumb toggle function that doesn't
play well with `org-notes'."
  (when (and (display-graphic-p) (equal major-mode 'org-mode))
    (catch 'exit
      (save-excursion
        (let ((beg (save-excursion (org-back-to-heading) (point)))
              (end (save-excursion (org-forward-heading-same-level 1) (point))))
          (let ((file (buffer-file-name (buffer-base-buffer))))
            (org-format-latex
             (concat org-preview-latex-image-directory "org-ltximg")
             beg
             end
             (if (or (not file) (file-remote-p file))
                 temporary-file-directory
               default-directory)
             'overlays
             nil
             'forbuffer
             org-preview-latex-default-process)))))))

(defun org-notes--helm-split-window-for-display ()
  "`helm-execute-presistent-action' with split helm window."
  (interactive)
  (helm-execute-persistent-action 'persistent-action t))

(defvar org-notes-keymap
  (make-composed-keymap
   (let ((map (make-keymap)))
     (mapc (lambda (key)
             (define-key map key 'org-notes--helm-split-window-for-display))
           (where-is-internal 'helm-execute-persistent-action helm-map))
     map)
   helm-map)
  "Reassigns all bindings in `helm-map' for `helm-execute-persistent-action' to `org-notes--helm-split-window-for-display'.")

(defun org-notes--helm-find ()
  "Return the org-id for a given note in the `org-notes-locations' alist."
  (let ((note-locations (org-notes--helm-lookup-note
                         (when (eq major-mode 'org-mode)
                           (org-get-local-tags))))
        (resize helm-autoresize-mode)
        (helm-autoresize-min-height (floor (/ (frame-height) 2.0)))
        (helm-autoresize-max-height (floor (/ (frame-height) 2.0)))
        (helm-resize-on-pa-text-height (floor (/ (frame-height) 2.0)))
        (helm-truncate-lines t))
    (helm :sources (helm-build-sync-source "Org Notes"
                     :candidates note-locations
                     :candidate-number-limit 2500
                     :persistent-action 'org-notes--helm-display-note
                     :multiline t
                     :volatile t
                     :keymap org-notes-keymap)
          :buffer "*Org Notes Headings*")))

(defun org-notes-helm-goto ()
  "Navigate to the location specified by an `helm-org-notes-find' call."
  (interactive)
  (let ((entry-point (set-marker (make-marker) (point)))
        (location (org-notes--helm-find)))
    (when location
      (if entry-point
          (setcar org-notes--jump-to-note-register entry-point)
        (warn "Warning: Can't determine current point for org-notes jump register.
Register unchanged, and `org-notes-jump-to-note' will not be updated."))
      (org-id-goto location)
      (when entry-point
           (setcdr org-notes--jump-to-note-register
                   (set-marker (make-marker) (point))))
      (outline-show-subtree)
      (recenter)
      (when org-notes-show-latex-on-jump
        (org-notes-turn-on-display-latex-fragments)))))

(defun org-notes--pop-register (reg)
  "Not much of a pop for REG."
  (let ((temp (car reg)))
    (setcar reg (cdr reg))
    (setcdr reg temp))
  reg)

(defun org-notes-jump-to-note (arg)
  "Navigate to the location specified by ARG.

By default, jump to location from which `org-notes-helm-goto' was
called.  With prefix arg `\\[universal-argument]
\\[universal-argument]', this function will jump to last note
linked to by `org-notes-helm-link-notes'."
  (interactive "P")
  (if org-notes--jump-to-note-register
      (let* ((location (car org-notes--jump-to-note-register))
             (buf (marker-buffer location))
             (entry-point (set-marker (make-marker) (point))))
        (switch-to-buffer buf)
        (goto-char (marker-position location))
        (recenter)
        (org-notes--pop-register org-notes--jump-to-note-register)
        ;; Make entry location the point to jump to in next invocation
        ;; (setcar org-notes--jump-to-note-register entry-point)
        (when org-notes-show-latex-on-jump
          (org-notes-turn-on-display-latex-fragments)))
    (message
     (concat "org-notes does not have any interesting locations stored.  "
             "See docs for org-notes-jump-to-note"))))

(defun org-notes--sort-locations (&optional source-tags)
  "Sort `org-notes-locations' by the list of tags SOURCE-TAGS.
Sort priority for a given heading in `org-notes-locations' is
given by the number of tags shared between it and
SOURCE-TAGS (i.e., generally the tags for the heading of the
subtree in which point is positioned).

Secondary priority is given by the string magnitudes of the
heading titles (headings stripped of tasks, tags, priorities,
etc).

Therefore, if SOURCE-TAGS is nil, this sort will ignore tags
during the sort, and only sort by the magnitude of the heading
titles in `org-notes-locations'."
  (cl-sort
   (copy-seq org-notes-locations)
   (lambda (th1 th2)
     "Predicate for sort."
     (let ((tag-count-1 (car th1))
           (tag-count-2 (car th2))
           (title-1     (cdr th1))
           (title-2     (cdr th2)))
       (cond ((> tag-count-1 tag-count-2))
             ((= tag-count-1 tag-count-2)
              (string-lessp title-1 title-2)))))
   :key (lambda (hl-id)
          "Keys on which the predicate is applied."
          (let* ((heading (car hl-id))
                 (heading-title
                  (progn (string-match
                          (org-notes--heading-regexp) heading)
                         (or (match-string 3 heading) "")))
                 (tag-count
                  (length (remove nil
                                  (mapcar
                                   (lambda (tag) (member tag source-tags))
                                   (split-string
                                    (or (match-string 4 heading) "")
                                    ":" t))))))
            (cons tag-count heading-title)))))

(defun org-notes--insert-link (link entry-delimiter)
  "Insert LINK at point using `org-log-into-drawer', delimited by ENTRY-DELIMITER."
  (goto-char (org-log-beginning t))
  (insert (concat ":" entry-delimiter ": " link "\n"))
  (goto-char (org-log-beginning))
  (forward-line -1)
  (org-indent-drawer))

(defvar org-notes--insert-link-callback nil)

(defun org-notes--add-link-to-drawer (link entry-delimiter &optional note)
  "Add LINK to links drawer, preceded by ENTRY-DELIMITER, add org note iff NOTE.
This function will add an accompanying org note to the link if
NOTE is non-nil."
  (save-excursion
    (if note
        (progn (setq-default org-log-into-drawer org-notes-drawer-name)
               (let ((org-log-note-headings
                      (append
                       (list (cons 'note "Linked on %T"))
                       (assq-delete-all
                        'note (copy-seq org-log-note-headings)))))
                 (call-interactively 'org-add-note)
                 (setq org-notes--insert-link-callback
                       ;; will this closure work? I don't htink so
                       (apply-partially 'org-notes--insert-link
                                        link entry-delimiter))
                 (advice-add 'org-store-log-note
                             :around 'org-notes--store-note-advice)))
      (let ((org-log-into-drawer org-notes-drawer-name))
        (org-notes--insert-link link entry-delimiter)))))

(defun org-notes--store-note-advice (funct)
  "Advice necessary to grab onto `org-store-log-note' w/ FUNCT."
  (let ((org-log-into-drawer-temp org-log-into-drawer))
    (setq-default org-log-into-drawer org-notes-drawer-name)
    (funcall funct)
    (funcall org-notes--insert-link-callback)
    (setq org-notes--insert-link-callback nil)
    (advice-remove 'org-store-log-note
                   'org-notes--store-note-advice)
    (setq-default org-log-into-drawer org-log-into-drawer-temp)))

(defun org-notes-helm-link-notes (arg)
  "Links selected note in a log drawer for current heading with prefix arg ARG.
Also links the id of current heading in a link drawer under
heading corresponding to selected note.  Results in a two-way
link between two org headings.

Non-nil ARG will result in a prompt for a note to be added with
the linking."
  (interactive "P")
  (unless (eq major-mode 'org-mode)
    (error "Cannot link notes when not in an org context"))
  (catch
      (let* ((loc-heading (or (org-get-heading t t) (error "Not at an org-mode heading")))
          (dest-id (let ((helm-onewindow-p t))
                     (or (org-notes--helm-find) (throw 'exit nil))))
          (dest-heading (let ((case-fold-search)
                              (heading (car (rassoc dest-id org-notes-locations))))
                          (string-match
                           (org-notes--heading-regexp)
                           heading)
                          (or (match-string 3 heading)
                              "UNKNOWN")))
          (forward-link (org-make-link-string
                         (concat "id:" dest-id)
                         dest-heading))
          (back-link (org-make-link-string
                      (concat "id:" (org-id-get-create))
                      loc-heading))
          (note (or arg
                    org-notes-always-add-note
                    (when org-notes-prompt-for-note
                      (y-or-n-p "Add note for link? ")))))
        ;; Insert forward link in source note
        (org-notes--add-link-to-drawer forward-link ">" note)
        ;; Insert backward link in linked note
        (let ((dest-loc (org-id-find dest-id 'marker)))
          (unless dest-loc
            (error "Cannot find the candidate's location"))
          (with-current-buffer (marker-buffer dest-loc)
            (org-with-wide-buffer
             (goto-char dest-loc)
             (org-notes--add-link-to-drawer back-link "<"))))
        (message "Linked '%s' and '%s'" loc-heading dest-heading))))

(provide 'org-notes)

;;; org-notes.el ends here
