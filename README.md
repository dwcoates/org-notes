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

The `helm-org-notes-find` function can be used to return the id for a given
note.  This is only useful as an API function.

The `helm-org-notes-goto` function is used to navigate to a particular note
using `helm-org-notes-find` interface.
