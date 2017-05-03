# org-notes.el

## This aims to solve a couple of problems simply:

1. Provide a means for easily searching through notes and note tags simply
via helm.

2. Provide a way to add links to other notes inside of a give note, and
have those instantiate links automatically reciprocated in the linkee.

3. Create a natural, simple means for inter-connecting distant and related
concepts in a way that is as searchable and appreciable as possible.

## Requirements:

Requires only [helm](https://github.com/emacs-helm/helm). 

## Motivation

I wrote this to make organizing, browsing, and studying my math notes far
easier and much more pleasant. 

## Usage:

The `org-notes-helm-goto` function is used to navigate to a particular note
using `helm-org-notes-find` interface.

The `org-notes-helm-link-notes` function is used to link two notes
together. It should be called while point is in the context of an org heading,
and will then prompt you to select a note with which the current org heading
should be linked. These results are displayed in a helm buffer, and using
`helm-execute-persistent-action`, bound to `C-z` by default, will display
currently selected note in a preview buffer. 

The after selecting a target note with `org-notes-helm-link-notes`, each of
the two notes will have the other's link added its LINKS drawer.

At the end of this process, by default, `org-notes-helm-link-notes` will
prompt for a note to be added for the linking. This note will be inserted just
under the new link inside of the link drawer. Such a note might be used to
clarify the linking or explain why the two notes are related.

These functions can be very useful for browsing notes; use
`org-notes-helm-goto` along with its preview feature to peruse your
notes. After selecting a note an interesting note, you can use
`org-notes-jump-to-note` to jump back to the location from which
`org-notes-goto` was last called. This makes for very fast navigation of notes.

The preview feature also makes linking notes very natural and unemcumbering. I
daresay that the process of creating links is otherwise prohibitively tedious.

# Details

The `org-note-accepted-tasks` list defines the task types to which org-notes
applies. Only org headings with one of these tasks will be tracked by
org-notes.

The `org-notes--helm-find` function can be used to return the id for a given
note.  This is only useful as an API function.

org-notes will automatically display latex fragments and enable pretty
symbols when previewing. It also provides several variables for controlling
behavior:
1. `org-notes-show-latex-on-jump` controls whether the latex fragments for
   a subtree are rendered after jumping to it via `org-notes-helm-goto`.
2. `org-notes-accepted-tasks` controls which tasks will be tracked by org-notes.
3. `org-notes-hide-other-headings-after-jump` controls whether other headings
   will be automatically folded after jumping using `org-notes-helm-goto`.
4. `org-notes-prompt-for-note` and `org-notes-always-add-note` control the
   behavior of the note feature for linking notes with
   `org-notes-helm-link-notes`.
