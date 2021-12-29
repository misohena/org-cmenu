;;; org-cmenu-setup.el --- Context Menu for Org-Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file defines the menu contents for each element type.

;;; Code:

(require 'org)
(require 'org-cmenu)
(require 'org-cmenu-tools)
(require 'org-cmenu-typedoc)

(defvar org-cmenu-setup-reset t)
(when org-cmenu-setup-reset
  (org-cmenu-reset))

;;;; All

(org-cmenu-add-group 'all '(:hidden) :hide '(lambda () t))
(org-cmenu-add-commands
 '(:hidden)
 '(("C-l" "Recenter" recenter :transient t)
   ("C-^" "Parent" org-cmenu-select-parent)
   ("C-\\" "Child" org-cmenu-select-child))
 'all
 'no-wrap)

(org-cmenu-add-commands
 '(:basic "Whole")
 '(("m" "Mark" org-cmenu-mark-datum)
   ("C-w" "Kill" org-cmenu-kill-datum)
   ("M-w" "Copy" org-cmenu-copy-datum)
   ("?" "Manual" org-cmenu-browse-type-document)
   (";" "Comment" org-cmenu-comment-element)))

(org-cmenu-add-commands
 '(:basic "Contents")
 '(("[ m" "Mark" org-cmenu-mark-contents)
   ("[ C-w" "Kill" org-cmenu-kill-contents)
   ("[ M-w" "Copy" org-cmenu-copy-contents)
   ("[ e" "Expose" org-cmenu-expose-contents)))

(org-cmenu-add-commands
 '(:basic "Affiliated Keyword")
 '(("ao" "ATTR_ORG" org-cmenu-add-attr-org)
   ("ah" "ATTR_HTML" org-cmenu-add-attr-html)
   ("al" "ATTR_LATEX" org-cmenu-add-attr-latex)
   ("an" "NAME" org-cmenu-add-name)
   ("ac" "CAPTION" org-cmenu-add-caption)))

;;;; Element
;;;;; section
;;;;; headline

;; ref:
;; - org-org-menu (in org.el)
;; - org-keys.el
;; - (org-speed-command-help)

(org-cmenu-add-commands
 '(:headline "Navi")
 '(("u" "Up" outline-up-heading :transient t)
   ("p" "Prev" outline-previous-visible-heading :transient t)
   ("n" "Next" outline-next-visible-heading :transient t)
   ("b" "Prev(SameLv)" outline-backward-same-level :transient t)
   ("f" "Next(SameLv)" outline-forward-same-level :transient t)
   ("j" "GoTo" org-goto))
 '(headline)
 'no-wrap)

(org-cmenu-add-commands
 '(:headline2 "Visibility")
 '(("TAB" "Cycle" org-cycle :transient t)
   ("S-TAB" "Cycle" org-shifttab :transient t)
   ("N" "Narrow/Wide" org-toggle-narrow-to-subtree :transient t))
 '(headline)
 'no-wrap)

(org-cmenu-add-commands
 '(:headline "Structure")
 '(("M-p" "Subtree Up" org-metaup :transient t)
   ("M-n" "Subtree Down" org-metadown :transient t)
   ("M-b" "<=Heading" org-metaleft :transient t)
   ("M-f" "  Heading=>" org-metaright :transient t)
   ("M-B" "<=Subtree" org-shiftmetaleft :transient t)
   ("M-F" "  Subtree=>" org-shiftmetaright :transient t)
   )
 '(headline)
 'no-wrap)

(org-cmenu-add-string
 '(headline)
 '(:headline :structure2)
 " ")
(org-cmenu-add-commands
 '(:headline :structure2)
 '(("l" "Clone" org-clone-subtree-with-time-shift :transient t)
   ("s" "Sort" org-sort)
   ("w" "Refile" org-refile)
   ("$" "Archive" org-archive-subtree-default-with-confirmation))
 '(headline)
 'no-wrap)

(org-cmenu-add-commands
 '(:headline "Data")
 '(("t" "TODO" org-todo :transient t)
   ("," "Priority" org-priority :transient t)
   (":" "Tag" org-set-tags-command :transient t)
   ("P" "Property" org-set-property :transient t)
   ("a" "Archive Tag" org-toggle-archive-tag :transient t))
 '(headline)
 'no-wrap)

(org-cmenu-add-commands
 '(:headline "Clock")
 '(("ci" "In" org-clock-in :transient t)
   ("co" "Out" org-clock-out :transient t)
   ("cc" "Cancel" org-clock-cancel :transient t)
   ("cs" "Switch" (lambda () (interactive) (org-clock-in '(4))) :transient t))
 '(headline)
 'no-wrap)

(org-cmenu-add-group '(headline) '(:headline-hidden) :hide '(lambda () t))
(org-cmenu-add-commands
 '(:headline-hidden)
 '(("C-u" "Up" outline-up-heading :transient t)
   ("C-p" "Prev" outline-previous-visible-heading :transient t)
   ("C-n" "Next" outline-next-visible-heading :transient t)
   ("C-b" "Prev(SameLv)" outline-backward-same-level :transient t)
   ("C-f" "Next(SameLv)" outline-forward-same-level :transient t)
   ("<backtab>" "Cycle" org-shifttab :transient t))
 '(headline)
 'no-wrap)

;;;;; babel-call
(org-cmenu-add-commands
 '(:babel-call "Babel Call")
 '(("e" "Execute" org-babel-execute-maybe)
   ("k" "Remove Result" org-babel-remove-result-one-or-many))
 '(babel-call)
 'no-wrap)

;;;;; center-block, quote-block, verse-block
;;;;; clock
;;;;; comment
;; uncomment => comment
;;;;; comment-block
;; uncomment => expose
;;;;; dynamic-block
;;;;; example-block
;;;;; export-block
;;;;; special-block
;;;;; fixed-width
;;;;; footnote-definition
;;;;; horizontal-rule
;;;;; inlinetask
;;;;; plain-list
(org-cmenu-add-commands
 '(:list "List")
 `(("S" "Copy as S-Exp" org-cmenu-plain-list-copy-as-sexp) ;;@todo Do only the current target list or enable only at top level
   ("t" "Make Subtree" org-list-make-subtree) ;;@todo enable only at top level
   ("s" "Sort" ,(org-cmenu-wrap-command-at-post-affiliated #'org-sort-list))
   ("r" "Repair" ,(org-cmenu-wrap-command-at-post-affiliated #'org-list-repair)) ;;@todo Do only the current target list or enable only at top level
   ("b" "Checkbox On/Off"
    (lambda () (interactive)
      (org-cmenu-mark-datum (org-cmenu-target-datum))
      (org-toggle-checkbox)))
   ("B" "Checkbox Add/Remove"
    (lambda () (interactive)
      (org-cmenu-mark-datum (org-cmenu-target-datum))
      (org-toggle-checkbox '(4)))))
 ;;@todo convert ordered-unordered
 'plain-list
 'no-wrap)

;;;;; item
(org-cmenu-add-commands
 '(:item "Item")
 `(("b" "Checkbox On/Off" org-toggle-checkbox)
   ("B" "Checkbox Add/Remove" (lambda () (interactive) (org-toggle-checkbox '(4)))))
 'item
 'no-wrap)

;;;;; keyword
;;;;; diary-sexp
;;;;; latex-environment
;;;;; paragraph
;;;;; planning
;; org-todo
;; org-schedule
;; org-deadline
;; org-cancel-repeater

;;;;; drawer
;;;;; property-drawer
(org-cmenu-add-commands
 '(:property-drawer "Property Drawer")
 '(("p" "Set Property" org-set-property))
 'property-drawer
 'no-wrap)

;;;;; node-property
(org-cmenu-add-commands
 '(:node-property "Node Property")
 '(("p" "Set Property" org-set-property))
 'node-property
 'no-wrap)

;;;;; src-block
(transient-define-prefix org-cmenu-transient-prefix-for-babel ()
  "The key prefix for Babel interactive key-bindings."
  [["Edit"
    ("j" "Insert Header Arg" org-babel-insert-header-arg)
    ("v" "Expand"  org-babel-expand-src-block)
    ("d" "Demarcate" org-babel-demarcate-block)
    ("x" "Do Key Sequence" org-babel-do-key-sequence-in-edit-buffer)]
   ["Move"
    ("p" "Previous" org-babel-previous-src-block :transient t)
    ("n" "Next" org-babel-next-src-block :transient t)
    ("u" "Goto Head" org-babel-goto-src-block-head)
    ;;("g" "Goto Named Block" org-babel-goto-named-src-block)
    ;;("r" "Goto Named Result" org-babel-goto-named-result)
    ("C-M-h" "Mark Block" org-babel-mark-block)]
   ["Execute & Result"
    ("e" "Execute" org-babel-execute-maybe)
    ;;("b" "Execute Buffer" org-babel-execute-buffer)
    ;;("s" "Execute Subtree" org-babel-execute-subtree)
    ("o" "Open Result" org-babel-open-src-block-result)
    ("k" "Remove Result" org-babel-remove-result-one-or-many)]]
  [["Info"
    ("i" "View Info" org-babel-view-src-block-info)
    ("c" "Check" org-babel-check-src-block)
    ("a" "SHA1 Hash" org-babel-sha1-hash)]
   ["Session"
    ("l" "Load in Session" org-babel-load-in-session)
    ("Z" "Switch to Session" org-babel-switch-to-session)
    ("z" "Switch to Session With Code" org-babel-switch-to-session-with-code)]
  ["Tangle"
   ("t" "Tangle" org-babel-tangle)
   ;;("f" "Tangle File" org-babel-tangle-file)
   ;;("i" "LOB Ingest" org-babel-lob-ingest)
   ;;("h" "Describe Bindings" org-babel-describe-bindings)
   ]])
(org-cmenu-add-commands
 '(:src-block "Src Block")
 '(("v" "Babel Keys" org-cmenu-transient-prefix-for-babel))
 'src-block
 'no-wrap)

;;;;; table, table-row, table-cell(object)

;; @todo Add Gnu Plot support?

(org-cmenu-add-commands
 '(:table "Table")
 '(("}" "Toggle Coordinate" org-table-toggle-coordinate-overlays :transient t)
   ("{" "Toggle Formula Dbg" org-table-toggle-formula-debugger)
   ("'" "Edit Formula" org-table-edit-formulas)
   ("tM" "Mark Fields" org-cmenu-table-mark-all :transient t)
   ("t+" "Sum" org-cmenu-table-sum-all :transient t)
   ("ts" "Sort" org-table-sort-lines)
   ("tt" "Transpose" org-table-transpose-table-at-point)
   ("S-TAB" "Cycle Width" org-cmenu-table-cycle-column-width :transient t))
 '(table table-row table-cell)
 'no-wrap)
(org-cmenu-add-commands
 '(:hidden)
 '(("<backtab>" "Cycle Width" org-cmenu-table-cycle-column-width :transient t))
 '(table table-row table-cell)
 'no-wrap)
(org-cmenu-add-commands
 '(:table "Table2")
 '(("a" "Align" org-table-align)
   ("e" "Export File" org-table-export)
   ("S" "Copy as S-Exp" org-cmenu-table-copy-as-sexp))
 '(table :forced t)
 'no-wrap)

;; Line (Row/Column)

(transient-define-prefix org-cmenu-table-move-line ()
  "Move table line left/right/up/down."
  ["Move Line"
   [("p" "Row Up" org-table-move-row-up :transient t)
    ("n" "Row Down" org-table-move-row-down :transient t)
    ("b" "Column Left" org-table-move-column-left :transient t)
    ("f" "Column Right" org-table-move-column-right :transient t)]
   [("C-p" "Row Up" org-table-move-row-up :transient t)
    ("C-n" "Row Down" org-table-move-row-down :transient t)
    ("C-b" "Column Left" org-table-move-column-left :transient t)
    ("C-f" "Column Right" org-table-move-column-right :transient t)]]
  (interactive)
  (when-let ((key-str (this-command-keys))
             (cmd (pcase (key-description (substring key-str -1))
                    ((or "p" "C-p") #'org-table-move-row-up)
                    ((or "n" "C-n") #'org-table-move-row-down)
                    ((or "b" "C-b") #'org-table-move-column-left)
                    ((or "f" "C-f") #'org-table-move-column-right))))
    (call-interactively cmd))
  (transient-setup 'org-cmenu-table-move-line))

;; Row

(transient-define-prefix org-cmenu-table-move-row ()
  "Move table row up/down."
  ["Move Row"
   [("p" "Up" org-table-move-row-up :transient t)
    ("n" "Down" org-table-move-row-down :transient t)]
   [("C-p" "Up" org-table-move-row-up :transient t)
    ("C-n" "Down" org-table-move-row-down :transient t)]]
  (interactive)
  (when-let ((key-str (this-command-keys))
             (cmd (pcase (key-description (substring key-str -1))
                    ((or "p" "C-p") #'org-table-move-row-up)
                    ((or "n" "C-n") #'org-table-move-row-down))))
    (call-interactively cmd))
  (transient-setup 'org-cmenu-table-move-row))

(org-cmenu-add-commands
 '(:table "Row")
 '(("rp" "Move Up" org-cmenu-table-move-row)
   ("rn" "Move Down" org-cmenu-table-move-row)
   ("rm" "Mark Fields" org-cmenu-table-mark-row :transient t)
   ("r+" "Sum" org-cmenu-table-sum-row :transient t)
   ("ri" "Insert" org-table-insert-row :transient t)
   ("rk" "Kill" org-table-kill-row :transient t)
   ("_" "HLine" org-table-insert-hline :transient t)
   ("-" "HLine & Down" org-table-hline-and-move :transient t))
 ;;("r=" "Set Formula" ;;@todo Set row formula @N=
 '(table-row table-cell)
 'no-wrap)
(org-cmenu-add-commands
 '(:hidden)
 '(("r C-p" "Move Up" org-cmenu-table-move-row)
   ("r C-n" "Move Down" org-cmenu-table-move-row)
   ("l C-p" "Move Up" org-cmenu-table-move-line)
   ("l C-n" "Move Down" org-cmenu-table-move-line)
   ("<M-up>" "Move Up" org-table-move-row-up :transient t)
   ("<M-down>" "Move Left" org-table-move-row-down :transient t)
   ("<M-S-up>" "Kill" org-table-kill-row :transient t)
   ("<M-S-down>" "Insert" org-table-insert-row :transient t))
 '(table-cell)
 'no-wrap)

;; Column

(transient-define-prefix org-cmenu-table-move-column ()
  "Move table column left/right."
  ["Move Column"
   [("b" "Left" org-table-move-column-left :transient t)
    ("f" "Right" org-table-move-column-right :transient t)]
   [("C-b" "Left" org-table-move-column-left :transient t)
    ("C-f" "Right" org-table-move-column-right :transient t)]]
  (interactive)
  (when-let ((key-str (this-command-keys))
             (cmd (pcase (key-description (substring key-str -1))
                    ((or "b" "C-b") #'org-table-move-column-left)
                    ((or "f" "C-f") #'org-table-move-column-right))))
    (call-interactively cmd))
  (transient-setup 'org-cmenu-table-move-column))

(org-cmenu-add-commands
 '(:table "Column")
 ;;NOTE: "c" key is not assigned to "comment" in table-cell.
 '(("cf" "Move Right" org-cmenu-table-move-column)
   ("cb" "Move Left" org-cmenu-table-move-column)
   ("cm" "Mark Fields" org-cmenu-table-mark-column :transient t)
   ("c+" "Sum" org-table-sum :transient t)
   ("ci" "Insert" org-table-insert-column :transient t)
   ("cd" "Delete" org-table-delete-column :transient t)
   ("TAB" "Toggle Width" org-table-toggle-column-width :transient t)
   ("c=" "Set Formula" org-table-eval-formula :transient t))
   ;; insert column width <r10>
 '(table-cell)
 'no-wrap)
(org-cmenu-add-commands
 '(:hidden)
 '(("c C-f" "Move Right" org-cmenu-table-move-column)
   ("c C-b" "Move Left" org-cmenu-table-move-column)
   ("l C-f" "Move Right" org-cmenu-table-move-line)
   ("l C-b" "Move Left" org-cmenu-table-move-line)
   ("<M-right>" "Move Right" org-table-move-column-right :transient t)
   ("<M-left>" "Move Left" org-table-move-column-left :transient t)
   ("<M-S-right>" "Insert" org-table-insert-column :transient t)
   ("<M-S-left>" "Delete" org-table-delete-column :transient t))
 '(table-cell)
 'no-wrap)

;; Field(Cell)

(transient-define-prefix org-cmenu-table-move-cell ()
  "Move table cell."
  ["Move Cell"
   [("b" "Left" org-table-move-cell-left :transient t)
    ("f" "Right" org-table-move-cell-right :transient t)
    ("p" "Up" org-table-move-cell-up :transient t)
    ("n" "Down" org-table-move-cell-down :transient t)]
   [("C-b" "Left" org-table-move-cell-left :transient t)
    ("C-f" "Right" org-table-move-cell-right :transient t)
    ("C-p" "Up" org-table-move-cell-up :transient t)
    ("C-n" "Down" org-table-move-cell-down :transient t)]]
  (interactive)
  (when-let ((key-str (this-command-keys))
             (cmd (pcase (key-description (substring key-str -1))
                    ((or "b" "C-b") #'org-table-move-column-left)
                    ((or "f" "C-f") #'org-table-move-column-right))))
    (call-interactively cmd))
  (transient-setup 'org-cmenu-table-move-cell))


(org-cmenu-add-commands
 '(:table "Field")
 '(("." "Move" org-cmenu-table-move-cell)
   ("m" "Mark Field" org-cmenu-table-mark-field :transient t)
   ("h" "Info" org-table-field-info :transient t)
   ("=" "Set Formula" (lambda () (interactive) (org-table-eval-formula '(4))) :transient t)
   (":" "Edit Formula" (lambda () (interactive) (org-table-eval-formula '(16))) :transient t)
   ("`" "Edit Field" org-table-edit-field)
   ;;@todo insert org-recalc-marks
   )
 '(table-cell)
 'no-wrap)

(org-cmenu-add-commands
 '(:hidden)
 '(("<S-right>" "Move Cell Right" org-table-move-cell-right :transient t)
   ("<S-left>" "Move CellLeft" org-table-move-cell-left :transient t)
   ("<S-up>" "Move Cell Up" org-table-move-cell-up :transient t)
   ("<S-down>" "Move Cell Down" org-table-move-cell-down :transient t))
 '(table-cell)
 'no-wrap)

;; Region

(org-cmenu-remove-group '(table-cell) '(:basic "Whole"))
(org-cmenu-add-commands
 '(:basic "Region/Field")
 '(("C-SPC" "Mark" set-mark-command :transient t)
   ("C-w" "Cut" org-cmenu-table-cut-region :transient t)
   ("M-w" "Copy" org-cmenu-table-copy-region :transient t)
   ("C-y" "Paste" org-table-paste-rectangle :transient t)
   ("+" "Sum" org-table-sum :transient t)
   ;; ("=" "Set Formula" ;; @todo Set range formula @N$M..@N$M= or current field
   )
 '(table-cell)
 'no-wrap)

;; Navigation

(org-cmenu-add-commands
 '(:table-move-point-hidden)
 '(("C-b" "Left" org-cmenu-table-previous-column :transient t)
   ("C-f" "Right" org-cmenu-table-next-column :transient t)
   ("C-p" "Up" org-cmenu-table-previous-row :transient t)
   ("C-n" "Down" org-cmenu-table-next-row :transient t)
   ("C-a" "First Column" org-cmenu-table-first-column-in-row :transient t)
   ("C-e" "Last Column" org-cmenu-table-last-column-in-row :transient t)
   ("M-<" "First Field" org-cmenu-table-first-field-in-table :transient t)
   ("M->" "Last Field" org-cmenu-table-last-field-in-table :transient t))
 '(table-cell)
 'no-wrap)
(org-cmenu-set-group-property
 '(table-cell)
 '(:table-move-point-hidden)
 :hide '(lambda () t))

(org-cmenu-add-string '(table-cell) '(:basic "Navigation")
                      "C-b,f,p,n,a,e M-<,>")

;;;; Object
;;;;; bold, underline, italic, verbatim, code, strike-through
;;;;; subscript, superscript

(org-cmenu-add-commands
 '(:subscript "Subscript/Superscript")
 '(("p" "Pretty" org-toggle-pretty-entities))
 '(subscript superscript)
 'no-wrap)

;;;;; line-break
;;;;; citation
;;;;; citation-reference
;;;;; entity

(org-cmenu-add-commands
 '(:entity "Entity")
 '(("p" "Pretty" org-toggle-pretty-entities)
   ("h" "List" org-entities-help))
 '(entity)
 'no-wrap)

;;;;; export-snippet
;;;;; footnote-reference
;;;;; inline-babel-call
(org-cmenu-add-commands
 '(:babel-call "Babel Call")
 '(("e" "Execute" org-babel-execute-maybe)
   ("k" "Remove Result" org-babel-remove-inline-result))
 '(inline-babel-call)
 'no-wrap)

;;;;; inline-src-block
(org-cmenu-add-commands
 '(:inline-src-block "Inline Src Block")
 '(("e" "Execute" org-babel-execute-maybe)
   ("k" "Remove Result" org-babel-remove-inline-result)
   ("i" "Info" org-babel-view-src-block-info)
   ("a" "SHA1 Hash" org-babel-sha1-hash)
 )
 '(inline-src-block)
 'no-wrap)

;;;;; link
(org-cmenu-add-commands
 '(:link "Link")
 '(("l" "Edit Path and Description" org-insert-link)
   ("o" "Open" org-cmenu-link-open-by-default)
   ("x" "Open by system" org-cmenu-link-open-by-system)
   ("i" "Open by emacs" org-cmenu-link-open-by-emacs)
   ("d" "Open Directory" org-cmenu-link-open-directory)
   ("cp" "Copy Path" org-cmenu-link-copy-path)
   ("cf" "Copy File Name" org-cmenu-link-copy-file-name)
   ("fi" "Info" org-cmenu-link-show-file-info))
 '(link)
 'no-wrap)

;;;;; latex-fragment
;;;;; macro

;;@todo search macro definition

;;;;; radio-target

;;@todo search (org-occur?) target text

;;;;; statistics-cookie

(org-cmenu-add-commands
 '(:statistics-cookie "Statistics Cookie")
 '(("u" "Update" org-update-statistics-cookies))
 '(statistics-cookie)
 'no-wrap)

;;;;; target

;;@todo search (org-occur?) target text

;;;;; timestamp

;;;; Insert

(transient-define-prefix org-cmenu-insert ()
  "Insert"
  [["Emphasis"
    ("b" "Bold" org-cmenu-insert-bold)
    ("u" "Underline" org-cmenu-insert-underline)
    ("i" "Italic" org-cmenu-insert-italic)
    ("v" "Verbatim" org-cmenu-insert-verbatim)
    ("c" "Code" org-cmenu-insert-code)
    ("+" "Strike" org-cmenu-insert-strike-through)]
   ["Super/Subscript"
    ("_" "Subscript" org-cmenu-insert-subscript)
    ("^" "Superscript" org-cmenu-insert-superscript)
    ""
    "Babel"
    ("C" "Inline Call" org-cmenu-insert-inline-babel-call)
    ("S" "Inline Src" org-cmenu-insert-inline-src-block)]
   ["Others"
    ("e" "Entity" org-cmenu-insert-entity)
    ("l" "Link" org-insert-link)
    ("t" "Target" org-cmenu-insert-target)
    ("r" "Radio Target" org-cmenu-insert-radio-target)
    ("m" "Macro" org-cmenu-insert-macro)
    ("@" "Export Snippet" org-cmenu-insert-export-snippet)
    ("f" "Footnote" org-footnote-new)
    ("RET" "Line Break" org-cmenu-insert-line-break) ;;exclude table-cell
    ]
   ["Elements"
    :if (lambda () (eq (org-element-type (org-cmenu-target-datum)) 'section))
    ("d" "Drawer" org-insert-drawer)
    ("," "#+BEGIN_?" org-insert-structure-template)
    ("a" "#+CALL" org-cmenu-insert-babel-call)
    ("M" "#+MACRO" org-cmenu-insert-macro-definition)
    (":" "Fixed Width" org-cmenu-insert-fixed-width)
    ("-" "Horizontal" org-cmenu-insert-horizontal-rule)]
   ])

(org-cmenu-add-commands
 '(:basic "Insert")
 '(("i" "Insert" org-cmenu-insert))
 '(section paragraph table-cell item
           center-block quote-block special-block dynamic-block
           drawer footnote-definition)
 'no-wrap)

;;@todo Add commands to decorate region. bold, italic, etc.

;;@todo Add commands to convert region to block.

(provide 'org-cmenu-setup)
;;; org-cmenu-setup.el ends here
