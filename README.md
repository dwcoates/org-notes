# org-notes.el

## Goal:

Remove tedium and improve visual attractiveness of math and latex note-taking
in Emacs org-mode using systems for linking notes, browsing notes, and
handling latex markup rendering naturally.

_Rule of thumb for package development_: minimize time to comprehend a given
note and to appreciate its related ideas, but also make the process of
building requisite note network as encouraging and simple as possible.

## Problems addressed:

1. Provide a means for easily searching through notes and note tags simply via
   helm, including a way to quickly preview notes in an attractive way, even
   if they contain latex markup.

2. Provide a way to quickly and easily link two notes in an informative
   way, via the aforementioned browsing tool.
   
3. Make the manual browsing of notes easier by automatically rendering latex
   when unfolding headlines, providing methods for toggling drawer
   visibility, rendering latex in agenda buffers, etc.

## Motivation

I wrote this to make organizing, browsing, studying, and, to a lesser extent,
taking my math notes easier and more pleasant. I've written several packages
and many functions to this effect, including functions for wrapping latex
symbols with other latex symbols, latex yasnippets snippets, latex
integrations with yasnippet+org-pretty-symbols, etc. With a bit of practice
and utilities, real-time latex note-taking is more or less achievable,
depending on how fast the speaker is speaking.

## Requirements:

Requires only [helm](https://github.com/emacs-helm/helm). 

## Usage:

The `org-notes-helm-goto` function is used to navigate to a particular note
using the `org-notes--helm-find` interface, which includes a
persistent-action, bound to `C-z` by default, that will display currently
selected note in a separate preview buffer, with embedded latex and drawers
automatically visible. `C-z` again will close the current preview.

`org-notes--helm-find` will sort the available notes primarily by their tag
similarity with the note at point, and secondarily by alphabetical ordering of
the headline titles. This makes finding relevant notes to browse with the
persistent-action (or otherwise select) much easier, and encourages good
note-tagging practice.

Use `org-notes-jump-to-note` to jump back to the location from which
`org-notes-goto` was last called.

The `org-notes-helm-link-notes` function is used to link two notes together,
also via the `org-notes--helm-find` interface. It should be called while point is
in the context of an org heading, and will then prompt you to select a note
note to and from which to link.

After selecting a target note with `org-notes-helm-link-notes`, each of
the two notes will have added its link the other's "LINKS" drawer.

At the end of this process, by default, `org-notes-helm-link-notes` will
prompt for a note to be added as an explaination of this linking to be
inserted just under the new link in the link drawer.

The pretty preview feature and smart sorting of notes in
`org-notes--helm-find` make linking notes with `org-notes-helm-link-notes`
very natural and fast. I daresay that the process of creating links would
otherwise be prohibitively tedious.

## Customizable settings

1. `org-notes-accepted-tasks` is a list that controls which task types will be
    tracked by org-notes. For example: '("NOTE" "LEARN" "REVIEW" "NEXT")
2. `org-notes-prompt-for-note` and `org-notes-always-add-note` control the
   behavior of the link note feature for `org-notes-helm-link-notes`.
3. `org-notes-show-latex-on-jump` controls whether the latex fragments for
   a given subtree is rendered after jumping to it via `org-notes-helm-goto`.
4. `org-notes-hide-other-headings-after-jump` controls whether other headings
   will be automatically folded after jumping using `org-notes-helm-goto`.
5. `org-notes-hide-other-headings-after-jump`
6. `org-notes-show-drawers-on-cycle` sets the org-cycle behavior always open
   relevant drawers on cycle. A value of 'no-log-default means only show LINKS
   drawer. Never opens properties drawer.
7. `org-notes-show-subtree-latex-on-cycle` is analogous to
   `org-notes-show-drawers-on-cycle`, but for latex markup rendering.
8. `org-notes-display-latex-in-agenda`
