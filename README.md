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

## Usage:

Update `org-note-accepted-tasks` list to set the task types to which org-notes
applies.

The `org-notes--helm-find` function can be used to return the id for a given
note.  This is only useful as an API function.

The `org-notes-helm-goto` function is used to navigate to a particular note
using `helm-org-notes-find` interface.

The `org-notes-helm-link-notes` function is used to link two notes
together. Should be called while point is in the context of an org heading, at
which point it will prompt you to select a note from `org-notes-locations`. The
selected note and heading context of point will each have links added to each
other's respective LINKS drawer.
