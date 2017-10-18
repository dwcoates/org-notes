;;; org-notes.el --- A simple way to link, connect, and browse notes taken in org-mode
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
;; These functions can be very useful for browsing notes; use `org-notes-helm-goto'
;; to browse via `helm-execute-persistent-action', jump to an interesting
;; note, and use `org-notes-jump-to-note' to jump back to the location from which
;; `org-notes-helm-goto' was called.
;;
;; org-notes will automatically display latex fragments and enable pretty
;; symbols when previewing. It also provides several variables for controlling
;; behavior:
;;
;; 1. `org-notes-show-latex-on-jump' controls whether the latex fragments for
;;    a subtree are rendered after jumping to it via `org-notes-helm-goto'.
;; 2. `org-notes-accepted-tasks' controls which tasks will be tracked by org-notes.
;; 3. `org-notes-hide-other-headings-after-jump' controls whether other headings
;;    will be automatically folded after jumping using `org-notes-helm-goto'.
;; 4. `org-notes-prompt-for-note' and `org-notes-always-add-note' control the
;;    behavior of the note feature for linking notes with
;;    `org-notes-helm-link-notes'.
;;
;;; Code:

(require 'org)
(require 'helm)
(require 'smartparens)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables and Bindings

(defvar org-notes-accepted-tasks '("NOTE" "LEARN" "REVIEW" "BUG"
                                   "ISSUE" "FEATURE" "NEXT")
  "Tasks that are considered for `org-notes'.
Must also have an `org-id' to be added to database.")

(defvar org-notes-locations nil
  "Alist containing known org-note titles and their IDs.")

(defvar org-notes-drawer-name "LINKS"
  "Name of drawer under which links for a particular org-note will be collected.")

(defvar org-notes-always-add-note nil
  "If non-nil, always insert a note with `org-notes-helm-link-notes' link.")

(defvar org-notes-show-latex-on-jump t
  "Display latex when jumping to another note.
When non-nil, inline Latex will be autormatically displayed after
jumping to another `org-note' with `org-notes-jump-to-note'.")

(defvar org-notes-display-latex-in-agenda t
  "Non-nil prompts `org-agenda' to render latex fragments in `org-agenda-buffer'.")

(defvar org-notes-prompt-for-note t
  "If non-nil, ask to add a note inserted with `org-notes-helm-link-notes' link.
Has no effect if `org-notes-always-add-note' is non-nil.")

(defvar org-notes-hide-other-headings-after-jump t
  "Non-nil means fold headings other than target heading after using `org-notes-helm-goto'.")

(defvar org-notes--jump-to-note-register (cons nil nil)
  "Possible locations for `org-notes-jump-to-note'.
These are stored as a cons cell in, the car of which is the last
location from which `org-notes-helm-goto' was called, and the cdr
the location of last note *linked to* by
`org-notes-helm-link-notes'." )

(defvar org-notes-show-drawers-on-cycle t
  "Show drawer contents on org visibility cycling.
Non-nil means show drawer default log drawer and links drawer on
cycling, unless the value is 'no-log-default, in which only the
links drawer will be show.")

(defvar org-notes-show-subtree-latex-on-cycle t
  "Non-nil means render latex for current subtree after cycles to visible.")

(defvar org-notes-notify-on-capture t "ID a note on capture when non-nil.")

(defvar org-notes-footnote-superscript t
  "Non-nil means wrap footnotes in `org-mode' superscript embellishment.")

(defvar org-notes-footnote-superscript t
  "Non-nil means wrap footnotes in `org-mode' superscript embellishment.")

(defcustom org-notes-locations-file (convert-standard-filename
				  (concat user-emacs-directory ".org-notes-locations"))
  "The file for remembering in which file an ID was defined.
This variable is only relevant when `org-id-track-globally' is set."
  :group 'org-notes
  :type 'file)

(defvar org-notes-autorender-delay 0.2
  "Delay in seconds for autorendering.")

(define-key org-mode-map (kbd "C-c l")   'org-notes-helm-link-notes)
(define-key org-mode-map (kbd "C-c j")   'org-notes-helm-goto)
(global-set-key          (kbd "C-c j")   'org-notes-helm-goto)
(define-key org-mode-map (kbd "C-c C-j") 'org-notes-jump-to-note)
(global-set-key          (kbd "C-c C-j") 'org-notes-jump-to-note)
(define-key org-mode-map (kbd "C-c w")   'org-notes-toggle-drawer-visibility)
(define-key org-mode-map (kbd "C-c q")   'org-notes-show-org-entry-latex)

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
     "[ 	]*\\'"))

(defun org-notes-drawer-cycle-regexp ()
  "Return the regexp for drawers to be shown after org visibility cycling.
Only has an effect if org-notes-drawer-cycle-on-visibility-change is non-nil."
    (format "\\(%s\\|^.*:%s:.*$\\)+?"
            (if (eq org-notes-show-drawers-on-cycle 'no-log-default)
                "" (concat "^.*:" (org-log-into-drawer) ":.*$"))
            org-notes-drawer-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org Note Location Tracking

(defun org-notes-locations-save ()
  "Save `org-notes-locations' in `org-notes-locations-file'."
  (let ((out (if (hash-table-p org-notes-locations)
                 (org-id-hash-to-alist org-notes-locations)
               org-notes-locations)))
    (with-temp-file org-notes-locations-file
      (let ((print-level nil)
            (print-length nil))
        (print out (current-buffer))))))

(add-hook 'kill-emacs-hook 'org-notes-locations-save)

(defun org-notes-locations-load ()
  "Read the data from `org-notes-locations-file'."
  (setq org-notes-locations nil)
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents-literally org-notes-locations-file)
          (goto-char (point-min))
          (setq org-notes-locations (read (current-buffer))))
      (error
       (message "Could not read org-notes from %s.  Setting it to nil."
                org-notes-locations-file))))
  ;; (setq org-notes-files (mapcar 'car org-id-locations)) this requires

  ;; changing the cons cells in `org-notes-locations' to lists, and therefore
  ;; all the cdr calls to cadr (setq org-notes-locations
  ;; (org-id-alist-to-hash org-notes-locations))
  )

(defun org-notes-update-note-locations ()
  "Set `org-notes-locations' to notes referred to by `org-id-locations'.
Relevent notes are those note types denoted by `org-notes-accepted-tasks'."
  (let ((inhibit-message t))
    (setq org-notes-locations
         (remove-if 'not
                    (mapcar
                     'org-notes-get-heading
                     (mapcar 'cadr (org-id-hash-to-alist org-id-locations))))))
  (org-notes-locations-save)
  (message "Updated org-id-locations.  Contains %d notes."
           (length org-notes-locations)))

(create-file-buffer "/home/dwcoates/personal/computers/machine-learning.org")

;; fix
(defun org-notes-create-note ()
  "Function for IDing a note on capture when `org-notes-notify-on-capture' is non-nil."
  (org-notes-create-id)
  (org-notes-locations-save))
(add-hook 'org-capture-before-finalize-hook 'org-notes-create-note)

(defun org-notes-create-id ()
  "Create an ID for captured note and add note to `org-notes-locations'."
  (org-id-get (point) 'create)
  (let ((head-id (org-notes-get-heading (car args))))
    (when head-id
      (push head-id org-notes-locations))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Core Helm Interface

(defun org-notes--helm-lookup-note (source-tags)
  "Wrapper for sorting `org-notes-locations' using SOURCE-TAGS."
  (org-notes--sort-locations source-tags))

(defvar org-notes-keymap
  (make-composed-keymap
   (let ((map (make-keymap)))
     (mapc (lambda (key)
             (define-key map key 'org-notes--helm-split-window-for-display))
           (where-is-internal 'helm-execute-persistent-action helm-map))
     map)
   helm-map)
  "Reassigns all bindings in `helm-map' for `helm-execute-persistent-action' to `org-notes--helm-split-window-for-display'.")

(defun org-notes--prettify-helm-buffer ()
  "Wrapper for displaying latex images in helm-buffer."
  (interactive)
  (with-current-buffer helm-buffer
    (org-notes--turn-on-display-latex-fragments t)))

(defun org-notes--helm-find ()
  "Return the org-id for a given note in the `org-notes-locations' alist."
  (unless org-notes-locations (org-notes-locations-load))
  (org-notes-delete-display-buffers)
  (let ((note-locations (org-notes--helm-lookup-note
                         (when (eq major-mode 'org-mode)
                           (org-get-local-tags))))
        (resize helm-autoresize-mode)
        (helm-truncate-lines t))
    (add-hook 'helm-update-hook 'org-notes--prettify-helm-buffer)
    (let ((ret (helm :sources (helm-build-sync-source "Org Notes"
                                :candidates note-locations
                                :candidate-number-limit 2500
                                :persistent-action 'org-notes--helm-display-note
                                :multiline t
                                :volatile t
                                :keymap org-notes-keymap)
                     :buffer "*Org Notes Headings*")))
      (remove-hook 'helm-update-hook 'org-notes--prettify-helm-buffer)
      ret)))

;; Complete IDs for `org-insert-link' with `org-notes--helm-find'.
(org-link-set-parameters "id" :complete 'org-notes--helm-find)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helm Persistent Action

(defvar org-notes--display-buffers nil
  "Buffers for displaying previews of org-note files.")

(defun org-notes--get-display-buffer (id)
  "Return display buffer corresponding to ID."
  (let* ((file (org-id-find-id-file id))
         (entry (assoc file org-notes--display-buffers)))
    (unless file
      (error "Cannot find the candidate's location"))
    (let ((buf (if entry (get-buffer (cdr entry))
                 (create-file-buffer file))))
      (with-current-buffer buf
        (when (not entry)
          ;; Create preview buffer.
          ;; Needed for the Latex image caching, as inline latex images are saved
          ;; and cached according to the file in which they live. This approach
          ;; prevents the Latex being re-compiled for the preview buffer.
          (insert-file-contents file)
          (let* ((inhibit-message t) (org-inhibit-startup t)) (org-mode))
          (push (cons file buf)
                org-notes--display-buffers))
        buf))))

(defun org-notes-delete-display-buffers ()
  "Delete all preview display buffers."
  (mapc (lambda (entry) (kill-buffer (cdr entry)))
        org-notes--display-buffers)
  (setq org-notes--display-buffers nil))

(defun org-notes--helm-display-note (candidate)
  "Display the note corresponding to CANDIDATE.
This will display the note corresponding to candidate in a
separate window, split from `helm-buffer'.  Used by
`helm-execute-persistent-action' in `org-notes--helm-find'."
  (save-excursion
    (let* ((buf      (org-notes--get-display-buffer candidate))
           (buf-name (with-current-buffer buf
                       (substring-no-properties
                        (org-with-wide-buffer
                         (goto-char (org-find-entry-with-id candidate))
                         (org-get-heading t t)))))
           (win      (get-buffer-window buf))
           (helm--reading-passwd-or-string t))
      (cond ((and buf win (eq buf (get-buffer helm-current-buffer)))
             (user-error
              "Can't kill `helm-current-buffer' without quitting session"))
            ((and buf win (equal (buffer-name buf) buf-name))
             ;; remove preview window if present
             (delete-window (get-buffer-window buf)))
            (t
             ;; create preview window if not present
             (switch-to-buffer buf)
             (rename-buffer buf-name)
             (widen)
             (goto-char (org-find-entry-with-id candidate))
             (narrow-to-region
              (save-excursion (org-back-to-heading) (point))
              (save-excursion (org-end-of-subtree) (point)))
             (goto-char (point-min))
             ;; turn on pretty entities
             (setq-local org-pretty-entities t)
             (org-restart-font-lock)
             ;; display latex fragments as images
             (org-notes--turn-on-display-latex-fragments t)
             ;; resize helm buffer
             (set-window-text-height win (floor (* (frame-height) 0.7)))
             (recenter (point-min)))))))

(defun org-notes--helm-split-window-for-display ()
  "`helm-execute-presistent-action' with split helm window."
  (interactive)
  (helm-execute-persistent-action 'persistent-action t))

;;;;;;;;;;;;;;;;;;
;;; User Functions

;; `org-notes-helm-goto'
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
      (when org-notes-hide-other-headings-after-jump
        (outline-hide-other))
      (outline-show-subtree)
      (recenter)
      (when org-notes-show-latex-on-jump
        (org-notes--turn-on-display-latex-fragments))
      )))

(defun org-notes-helm-insert-id (arg)
  "Insert at point the ID selected w/ ARG."
  (interactive "P")
  (insert (org-notes--helm-find)))

(defun org-notes-helm-insert-link (arg &optional description)
  "Insert a link using org-notes selection with ARG, DESCRIPTION.

If DESCRIPTION is nil, propmpt for a description.

If prefix arg ARG is non-nil, use header title as link discription

This is a temporary function.  Overwriting `org-insert-link' to
use this automatically is the goal."
  (interactive "P")
  (let*  ((id (org-notes--helm-find))
          (location (org-id-find id)))
    (org-insert-link
     (directory-file-name (car location))
     (concat "id:" id)
     (or description
         (and arg
              (let ((headline (car (rassoc id org-notes-locations))))
                (string-match (org-notes--heading-regexp) headline)
                (match-string 3 headline)))))))

;; `org-notes-helm-link-notes'
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
      (let* ((loc-heading (or (org-get-heading t t)
                              (error "Not at an org-mode heading")))
             (dest-id (let ((helm-onewindow-p t))
                        (or (org-notes--helm-find) (throw 'exit nil))))
             (dest-heading (let ((case-fold-search)
                                 (heading (car (rassoc dest-id
                                                       org-notes-locations))))
                             (string-match
                              (org-notes--heading-regexp)
                              heading)
                             (or (match-string 3 heading)
                                 "UNKNOWN")))
             (note (or arg
                       org-notes-always-add-note
                       (when org-notes-prompt-for-note
                         (y-or-n-p "Add note for link? ")))))
        ;; When region is active, also embed the link there.  This should
        ;; maybe be replaced with a general function, that will also be used
        ;; during the inserting of links in the drawers.  the advantage of
        ;; this is it gets to use built-in org-mode stuff.
        (when (use-region-p)
          (org-insert-link
           (directory-file-name (car (org-id-find dest-id)))
           (concat "id:" dest-id)
           (buffer-substring-no-properties
            (region-beginning)
            (region-end))))
        ;; Insert forward link in source note
        (org-notes--add-link-to-drawer dest-id dest-heading ">" note)
        ;; Insert backward link in linked note
        (let ((dest-loc (org-id-find dest-id 'marker)))
          (unless dest-loc
            (error "Cannot find the candidate's location"))
          (with-current-buffer (marker-buffer dest-loc)
            (org-with-wide-buffer
             (goto-char dest-loc)
             (org-notes--add-link-to-drawer (org-id-get-create) loc-heading "<"))))
        ;; close drawers after opening
        (save-excursion
          (org-back-to-heading)
          (org-cycle-hide-drawers 'children))
        (message "Linked '%s' and '%s'" loc-heading dest-heading))))

;; `org-notes-jump-to-note'
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
        (when (and org-notes-show-latex-on-jump
                   (equal major-mode 'org-mode))
          (org-notes--turn-on-display-latex-fragments)))
    (message
     (concat "org-notes does not have any interesting locations stored.  "
             "See docs for org-notes-jump-to-note"))))

;;;;;;;;;;;;
;; Footnotes

(defun org-notes-insert-footnote-superscript (label)
  "Insert a footnote with LABEL and return its starting point."
  (if org-notes-footnote-superscript
      (progn
        (insert (format "^{[fn:%s]}" label))
        (when (not (looking-at "[\.,!;]"))
          (just-one-space)))
    (insert "[fn:%s]"))
  (save-excursion (search-backward "]") (point)))

(defun org-footnote-new ()
  "Insert a new footnote.
This command prompts for a label.  If this is a label referencing an
existing label, only insert the label.  If the footnote label is empty
or new, let the user edit the definition of the footnote.

This is a revision of `org-footnote-new' in org-footnote.el."
  (interactive)
  (unless (org-footnote--allow-reference-p)
    (user-error "Cannot insert a footnote here"))
  (let* ((all (org-footnote-all-labels))
	 (label
	  (if (eq org-footnote-auto-label 'random)
	      (format "%x" (random most-positive-fixnum))
	    (org-footnote-normalize-label
	     (let ((propose (org-footnote-unique-label all)))
	       (if (eq org-footnote-auto-label t) propose
		 (completing-read
		  "Label (leave empty for anonymous): "
		  (mapcar #'list all) nil nil
		  (and (eq org-footnote-auto-label 'confirm) propose))))))))
    (cond ((not label)
           (goto-char
            (org-notes-insert-footnote-superscript ":")))
	  ((member label all)
	   (org-notes-insert-footnote-superscript label)
	   (message "New reference to existing note"))
	  (org-footnote-define-inline
       (goto-char
        (org-notes-insert-footnote-superscript (concat label ":")))
	   (org-footnote-auto-adjust-maybe))
	  (t
	   (org-notes-insert-footnote-superscript label)
	   (let ((p (org-footnote-create-definition label)))
	     ;; `org-footnote-goto-definition' needs to be called
	     ;; after `org-footnote-auto-adjust-maybe'.  Otherwise
	     ;; both label and location of the definition are lost.
	     ;; On the contrary, it needs to be called before
	     ;; `org-edit-footnote-reference' so that the remote
	     ;; editing buffer can display the correct label.
	     (if (ignore-errors (org-footnote-goto-definition label p))
		 (org-footnote-auto-adjust-maybe)
	       ;; Definition was created outside current scope: edit
	       ;; it remotely.
	       (org-footnote-auto-adjust-maybe)
	       (org-edit-footnote-reference)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low-level helper functions

(defun org-notes--turn-on-display-latex-fragments (&optional whole-buffer)
  "Display latex fragments in the current buffer, possibly over WHOLE-BUFFER.

Intended for use in the preview buffer, because
`org-preview-latex-fragment' is a dumb toggle function that doesn't
play well with `org-notes'.

Non-nil WHOLE-BUFFER results in the latex fragments being
displayed over the entire buffer, instead of only the current
subtree."
  (when (display-graphic-p)
    (save-excursion
      (let* ((beg (if whole-buffer
                      (point-min)
                    (org-with-wide-buffer
                     (save-excursion (org-back-to-heading) (point)))))
             (end (if whole-buffer
                      (point-max)
                    (org-with-wide-buffer
                     (save-excursion (outline-next-heading) (point)))))
             (file (buffer-file-name
                    (buffer-base-buffer)))
             (gen-latex (lambda (a b)
                          (org-format-latex
                           (concat
                            org-preview-latex-image-directory
                            "org-ltximg")
                           a b
                           (if (or (not file)
                                   (file-remote-p file))
                               temporary-file-directory
                             default-directory)
                           'overlays
                           nil
                           'forbuffer
                           org-preview-latex-default-process))))
        (org-with-wide-buffer
         (save-excursion
           (org-show-entry)
           (funcall gen-latex beg end))
         (unless whole-buffer
           (let ((first-sibling
                  (save-excursion (org-forward-heading-same-level 1) (point))))
             (while (<= (save-excursion
                          (outline-next-heading) (point)) first-sibling)
               (outline-next-heading)
               (funcall
                gen-latex
                (point)
                (save-excursion (end-of-line) (point))))))))
      )))

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

(defun org-notes--pop-register (reg)
  "Not much of a pop for REG."
  (let ((temp (car reg)))
    (setcar reg (cdr reg))
    (setcdr reg temp))
  reg)

(defun org-notes--insert-link-or-update (id title entry-delimiter)
  "Insert ID and TITLE at point using `org-log-into-drawer', delimited by ENTRY-DELIMITER."
  (let* ((org-log-into-drawer org-notes-drawer-name)
         (link-regexp "[ ]+:[<>]:[ ]+\\[\\[id:\\(.+\\)\\]\\[\\(.+\\)\\]\\]$")
         (beg (goto-char (org-log-beginning t)))
         (end (save-excursion (re-search-forward ":END:")
                              (point)))
         replacep)
    (while  (re-search-forward link-regexp end t)
      (let ((link-id (substring-no-properties (match-string 1)))
            (link-title (substring-no-properties (match-string 2))))
        (print id)
        (print link-id)
        (when (equal id link-id)
          (forward-line 0)
          (re-search-forward link-title end)
          (replace-match title)
          (setq replacep t))))
    (unless replacep
      (goto-char (org-log-beginning t))
      (insert (concat ":"
                      entry-delimiter
                      ": "
                      (org-make-link-string (concat "id:" id) title)
                      "\n"))
      (goto-char (org-log-beginning))
      (forward-line -1)
      (org-indent-drawer))))

(defvar org-notes--insert-link-callback nil)

(defun org-notes--add-link-to-drawer (id title entry-delimiter &optional note)
  "Add LINK to links drawer, preceded by ENTRY-DELIMITER, add org note iff NOTE.
This function will add an accompanying org note to the link if
NOTE is non-nil."
  (save-excursion
    (if note
        (progn
          (call-interactively 'org-add-note)
          (setq org-notes--insert-link-callback
                ;; will this closure work? I don't htink so
                (apply-partially 'org-notes--insert-link-or-update
                                 id title entry-delimiter))
          (advice-add 'org-store-log-note
                      :around 'org-notes--store-note-advice))
      (let ((org-log-into-drawer org-notes-drawer-name))
        (org-notes--insert-link-or-update id title entry-delimiter)))))

(defun org-notes--store-note-advice (funct)
  "Advice necessary to grab onto `org-store-log-note' w/ FUNCT."
  (let ((org-log-into-drawer-temp org-log-into-drawer)
        (org-log-note-headings
               (append
                (list (cons 'note "_Linked on_ %T"))
                (assq-delete-all
                 'note (copy-seq org-log-note-headings)))))
    (setq-default org-log-into-drawer org-notes-drawer-name)
    (funcall funct)
    (funcall org-notes--insert-link-callback)
    (setq org-notes--insert-link-callback nil)
    (advice-remove 'org-store-log-note
                   'org-notes--store-note-advice)
    (setq-default org-log-into-drawer org-log-into-drawer-temp)))

(defun org-notes--display-latex-in-org-agenda ()
  "Display latex fragment overlays in `org-agenda'.
This function is only effective if `org-notes-display-latex-in-agenda'
is non-nil.  It is added to the org-agenda-finalize-hook, which
is run whenever the agenda is updated or initialized."
  (when org-notes-display-latex-in-agenda
    (with-current-buffer org-agenda-buffer
    (org-notes--turn-on-display-latex-fragments t))))

(add-hook 'org-agenda-finalize-hook 'org-notes--display-latex-in-org-agenda)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Visiblity Cycling

(defun org-notes-cycle-drawers (state)
  "Show relevant drawers after a visibility state change, STATE.

See `org-notes--flag-drawers' for a description."
  (when (and org-notes-show-drawers-on-cycle
             (derived-mode-p 'org-mode)
             (not (memq state '(overview folded contents children))))
    (save-excursion
      (let* ((globalp (memq state '(contents all)))
             (beg (if globalp (point-min) (point)))
             (end (if globalp (point-max)
                    (save-excursion (outline-next-heading) (point)))))
        (org-notes--flag-drawers beg end)))))

(defun org-notes--flag-drawers (beg end &optional flag)
  "Show the drawers between BEG and END, with option FLAG.

If FLAG is equal to 'cycle, then cycle the visibility of relevant
drawers, otherwise drawer state is set to open iff FLAG and
`org-notes-show-drawers-on-cycle' are non-nil.

See `org-notes-show-drawers-on-cycle' for a complete
description of this variable's affect."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward (org-notes-drawer-cycle-regexp) end t)
      (let ((b (match-end 0)))
        (if (org-at-drawer-p)
            (org-flag-drawer (if (eq flag 'cycle)
                                 (org-drawer-visible-p)
                               flag))
          (user-error ":END: line missing at position %s" b))))))

(defun org-notes-toggle-drawer-visibility ()
  "Show or hide current subtree's interesting drawers.

This is a wrapper for `org-notes--flag-drawers'."
  (interactive)
  (org-notes--flag-drawers
   (save-excursion (org-back-to-heading) (point))
   (save-excursion (outline-next-heading) (point))
   'cycle))

(defun org-drawer-visible-p ()
  "Return the visiblity status of drawer at point.

Returns non-nil if at a drawer that is currently open, and nil
otherwise.

Uses `org-at-drawer-p', which is not very robust and won't
recognize being in the middle of a drawer.  This should not
matter."
  (save-excursion
    (when (org-at-drawer-p)
      (let ((beg (save-excursion (end-of-line) (point)))
            (end (save-excursion (end-of-visual-line) (point))))
        (not (overlays-in beg end))))))

(add-hook 'org-cycle-hook 'org-notes-cycle-drawers t)

(defun org-notes-show-subtree-latex (state)
  "Render all latex on subtree after a visibility state change, STATE."
  (when (and org-notes-show-subtree-latex-on-cycle
             (derived-mode-p 'org-mode)
             (not (memq state '(overview folded contents))))
    (org-notes--turn-on-display-latex-fragments)))

(defun org-notes-show-org-entry-latex ()
  "Show latex for current org entry at point."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (org-notes--turn-on-display-latex-fragments)
    (error "Not in org-mode")))

(add-hook 'org-cycle-hook 'org-notes-show-subtree-latex)

;;;;;;;;;;;;;;;;
;; Latex Editing

;; Define functions for wrapping org text with markup when region is active, and
;; simply entering the corresponding key, otherwise. For example, pressing =$= with
;; region active will insert a dollar sign at the beginning and end of the
;; region, and will simply insert a dollar sign at point, i.e., the expected
;; behavior, if no region is active.

(org-notes-create-latex-wrappers "\\mathcal{" "}" "m")
(org-notes-create-latex-wrappers "\\begin{align*}\n" "\n\\end{align*}" "a")
(org-notes-create-latex-wrappers "^{" "}" "i")
(org-notes-create-latex-wrappers "_{" "}" "u")
(org-notes-create-latex-wrappers "\\hat{" "}" "h")
(org-notes-create-latex-wrappers "\\bar{" "}" "b")
(org-notes-create-latex-wrappers "\\text{" "}" "t")
(org-notes-create-latex-wrappers "\\tilde{" "}" "e")
(org-notes-create-latex-wrappers "\\vec{" "}" "v")
(org-notes-create-latex-wrappers "\\mathbf{" "}" "o")
(org-notes-create-latex-wrappers "\\mathbb{" "}" "l")
(org-notes-create-latex-wrappers "\\mathbb{" "}" "l")

(defvar org-notes--autorender-last-call (float-time)
  "Last time `org-notes-auto-render-latex' was called.")

(defun org-notes-wrap-with-pair (pair beg end)
  "Insert delimiters corresponding to PAIR at BEG and END.

PAIR is a defined `smartparens' open delimiter."
    (when (and beg end (> end beg))
      (let* ((active-pair (progn (--first (equal (car it) pair)
                                          sp-pair-list))))
        (goto-char end)
        (save-excursion
          (insert (cdr active-pair))
          (goto-char beg)
          (insert (car active-pair))
          (sp--indent-region beg end))
        (forward-char (length (cdr active-pair)))
        (sp-get-thing t))))

(defun org-notes-sp-wrap-with-pair (pair beg end)
  "Might be superfluous..."
    (or (org-notes-wrap-with-pair pair beg end)
        (self-insert-command 1) ; returns nil
        ))

(defun org-notes-wrap-with-meta-latex-and-execute (beg end &optional long-latex)
  "Wrap region between BEG and END with LONG-LATEX or inline Latex markup."
    (let* ((open-delim (if long-latex "\\[" "$"))
           (latex (org-notes-sp-wrap-with-pair open-delim beg end)))
      (when latex
       (org-toggle-latex-fragment)
       latex)))

(defun org-notes-wrap-latex-with-delims-and-render (open key beg end)
     (let*  ((latex (org-notes-sp-wrap-with-pair open beg end)))
             ;; this is a bad hack.
             ;; Robust solution will be quite a bit more code
             (when latex
               (let* ((lb (plist-get latex :beg))
                      (le (plist-get latex :end))
                      (sp-max-pair-length (- le lb)))
                 (org-notes-wrap-with-meta-latex-and-execute
                  lb le (> sp-max-pair-length 70))))))

(defun org-notes--create-latex-wrapper (open close key)
  "Create bindings for OPEN and CLOSE delimiters to KEY.

Wrap previous Latex with OPEN when KEY is pressed following `C-c e' prefix.
Also binds (downcase key) to `org-notes-sp-wrap-with-pair' and (upcase
key) to `org-noteswrap-latex-with-delims-and-render'."
    `(progn
       (sp-pair ,open ,close :actions '(navigate))
       (define-key org-mode-map (kbd ,key)
         (lambda (beg end)
           (interactive (if (region-active-p)
                            (list (region-beginning)
                                  (region-end))
                          (list nil nil)))
           (org-notes-sp-wrap-with-pair ,open beg end)))
       (define-key org-mode-map (kbd ,(upcase key))
         (lambda (beg end)
           (interactive (if (region-active-p)
                            (list (region-beginning)
                                  (region-end))
                          (list nil nil)))
           (org-notes-wrap-latex-with-delims-and-render ,open ,close beg end)))
       (define-key org-mode-map (kbd ,(concat "C-c e " key))
         (lambda () (interactive)
           (org-notes-wrap-previous-latex
            t
            (lambda (beg end) (org-notes-wrap-with-pair ,open beg end)))))))

(defmacro org-notes-create-latex-wrappers (open close key)
  "Macro for `org-notes--create-latex-wrapper'."
    (org-notes--create-latex-wrapper open close key))

(defun org-notes-wrap-previous-latex (&optional keep-whitespace funct)
  "Wrap latex statement just before point with '$' and render.

Temporary and very sloppy; doing much better would require a cfg
of some sort, but this works pretty well.  See
`org-notes-parse-simple-latex'."
      (interactive)
      (unless keep-whitespace
        (while (and (not (looking-at "^"))
           (not (save-excursion
              (backward-char)
              (looking-at "[^\\\s-] "))))
          (backward-char)))
      (let* ((limit (line-beginning-position))
             (beg (org-notes-parse-simple-latex))
             (latex (and (not (equal (point) beg)) (> (point) limit))))
        (and latex
             (funcall (or funct
                 'org-notes-wrap-with-meta-latex-and-execute) beg (point))
             (or (just-one-space) t))))

(defun org-notes-parse-simple-latex ()
  "Return starting point of latex statement just before point.

Temporary and very sloppy; doing much better would require a cfg
of some sort, but this works pretty well."
  (save-excursion
    (when (save-excursion (backward-char) (looking-at "[)}]"))
      (sp-backward-sexp)
      ;; handle two-component latex structures like \frac{}{}
      (when (save-excursion (backward-char) (looking-at "[)}]"))
        (sp-backward-sexp)))
    (re-search-backward "[ ]" (line-beginning-position) t)
    (forward-char)
    (point)))

(defun org-notes-auto-render-latex ()
    "Replace two consecutive spaces with one space and wrap/render previous latex."
    (interactive)
    (unless (and
             (< (- (float-time) org-notes--autorender-last-call)
                org-notes-autorender-delay)
             ;; superfluous?
             (org-notes-wrap-previous-latex))
      (org-self-insert-command 1))
    (setq org-notes--autorender-last-call (float-time)))

;;
;; Bindings
;;

(define-key org-mode-map (kbd "SPC") 'org-notes-auto-render-latex)
(define-key org-mode-map (kbd "C-c k") 'org-cycle)

;; special latex meta wrappers
(sp-pair "$" "$")
(define-key org-mode-map (kbd "$")
  (lambda (beg end)
    (interactive (if (region-active-p)
                     (list (region-beginning)
                           (region-end))
                   (list nil nil)))
    (org-notes-wrap-with-meta-latex-and-execute beg end)))

(sp-pair "\\[" "\\]" :actions '(navigate))
(define-key org-mode-map (kbd "\\")
  (lambda (beg end)
    (interactive (if (region-active-p)
                     (list (region-beginning)
                           (region-end))
                   (list nil nil)))
    (org-notes-wrap-with-meta-latex-and-execute beg end t)))

;;;;;;;
;;; END

(provide 'org-notes)

;;; org-notes.el ends here
