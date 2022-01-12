;;; org-cmenu-typedoc.el --- Org Syntax Type Help  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: Org

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

;; This file is a list of correspondence between each syntax element
;; type and the URL of the manual.

;; Usage:
;; (org-cmenu-browse-type-document 'timestamp)

;;; Code:

;; ref:
;; - https://orgmode.org/org.html
;; - https://orgmode.org/manual/Main-Index.html

(defconst org-cmenu-type-document-alist
  '(;; Elements
    (babel-call . "https://orgmode.org/manual/Evaluating-Code-Blocks.html")
    (center-block ."https://orgmode.org/manual/Paragraphs.html")
    (clock . "https://orgmode.org/manual/Clocking-commands.html")
    (comment . "https://orgmode.org/manual/Comment-Lines.html")
    (comment-block . "https://orgmode.org/manual/Comment-Lines.html")
    (diary-sexp . "https://orgmode.org/manual/Timestamps.html")
    (drawer . "https://orgmode.org/manual/Drawers.html")
    (dynamic-block . "https://orgmode.org/manual/Dynamic-Blocks.html")
    (example-block . "https://orgmode.org/manual/Literal-Examples.html")
    (export-block . "https://orgmode.org/manual/Quoting-HTML-tags.html")
    (fixed-width . "https://orgmode.org/manual/Literal-Examples.html")
    (footnote-definition . "https://orgmode.org/manual/Creating-Footnotes.html")
    (headline . "https://orgmode.org/manual/Headlines.html")
    (horizontal-rule . "https://orgmode.org/manual/Horizontal-Rules.html")
    (inlinetask . "https://git.savannah.gnu.org/cgit/emacs/org-mode.git/tree/lisp/org-inlinetask.el")
    (item . "https://orgmode.org/manual/Plain-Lists.html")
    (keyword
     . (lambda (datum)
         (pcase (org-element-property :key datum)
           ((or "ARCHIVE" "CATEGORY" "COLUMNS" "CONSTANTS" "FILETAGS" "LINK"
                "PRIORITIES" "PROPERTY" "SETUPFILE" "STARTUP"
                "TAGS" "TODO" "SEQ_TODO" "TYP_TODO")
            "https://orgmode.org/manual/In_002dbuffer-Settings.html")
           ((pred (lambda (kw) (string-prefix-p "HTML" kw)))
            "https://orgmode.org/manual/HTML-specific-export-settings.html")
           ("INFOJS_OPT"
            "https://orgmode.org/manual/JavaScript-support.html")
           (_ "https://orgmode.org/manual/Export-Settings.html"))))
    (latex-environment . "https://orgmode.org/manual/LaTeX-fragments.html")
    (node-property . "https://orgmode.org/manual/Property-Syntax.html")
    (paragraph . "https://orgmode.org/manual/Paragraphs.html")
    (plain-list . "https://orgmode.org/manual/Plain-Lists.html")
    (planning . "https://orgmode.org/manual/Deadlines-and-Scheduling.html")
    (property-drawer . "https://orgmode.org/manual/Drawers.html")
    (quote-block . "https://orgmode.org/manual/Paragraphs.html")
    (section . "https://orgmode.org/manual/Headlines.html")
    (special-block . "https://orgmode.org/worg/org-contrib/org-special-blocks.html")
    (src-block . "https://orgmode.org/manual/Working-with-Source-Code.html")
    (table . "https://orgmode.org/manual/Tables.html")
    (table-row . "https://orgmode.org/manual/Tables.html")
    (verse-block . "https://orgmode.org/manual/Paragraphs.html")
    ;; Objects
    (bold . "https://orgmode.org/manual/Emphasis-and-Monospace.html")
    (citation . "https://orgmode.org/manual/Citation-handling.html")
    (citation-reference . "https://orgmode.org/manual/Citation-handling.html")
    (code . "https://orgmode.org/manual/Emphasis-and-Monospace.html")
    (entity . "https://orgmode.org/manual/Special-Symbols.html")
    (export-snippet . "https://orgmode.org/manual/Quoting-HTML-tags.html")
    (footnote-reference . "https://orgmode.org/manual/Creating-Footnotes.html")
    (inline-babel-call . "https://orgmode.org/manual/Evaluating-Code-Blocks.html")
    (inline-src-block . "https://orgmode.org/manual/Structure-of-Code-Blocks.html")
    (italic . "https://orgmode.org/manual/Emphasis-and-Monospace.html")
    (line-break . "https://orgmode.org/manual/Paragraphs.html")
    (latex-fragment . "https://orgmode.org/manual/LaTeX-fragments.html")
    (link . "https://orgmode.org/manual/Hyperlinks.html")
    (macro . "https://orgmode.org/manual/Macro-Replacement.html")
    (radio-target . "https://orgmode.org/manual/Radio-Targets.html")
    (statistics-cookie . "https://orgmode.org/manual/Checkboxes.html")
    (strike-through . "https://orgmode.org/manual/Emphasis-and-Monospace.html")
    (subscript . "https://orgmode.org/manual/Subscripts-and-Superscripts.html")
    (superscript . "https://orgmode.org/manual/Subscripts-and-Superscripts.html")
    (table-cell . "https://orgmode.org/manual/Tables.html")
    (target . "https://orgmode.org/manual/Internal-Links.html")
    (timestamp . "https://orgmode.org/manual/Timestamps.html")
    (underline . "https://orgmode.org/manual/Emphasis-and-Monospace.html")
    (verbatim . "https://orgmode.org/manual/Emphasis-and-Monospace.html")))

(defconst org-cmenu-affiliated-keyword-document-alist
  '(("CAPTION" . "https://orgmode.org/manual/Captions.html")))

(put 'org-cmenu-browse-type-document 'org-cmenu
     '(:target all))
(defun org-cmenu-browse-type-document (type-or-datum)
  (interactive
   (list
    (intern (completing-read "Type: " org-cmenu-type-document-alist))))

  (let* ((datum (if (listp type-or-datum) type-or-datum))
         (type (if datum (org-element-type datum) type-or-datum))
         (url-or-fun
          (or
           ;; From Affiliated Keyword
           (when-let ((aff-kw (org-cmenu-current-affiliated-keyword-at-point datum)))
             (alist-get
              aff-kw
              org-cmenu-affiliated-keyword-document-alist
              nil nil #'string=))
           ;; From Type
           (alist-get type org-cmenu-type-document-alist))))
    ;; Open Browser
    (if url-or-fun
        (browse-url
         (if (functionp url-or-fun)
             (funcall url-or-fun datum)
           url-or-fun))
      (error "Unknown type %s" type))))

(defun org-cmenu-current-affiliated-keyword-at-point (&optional datum pos)
  (let* ((datum (or datum (org-element-at-point)))
         (datum-beg (org-element-property :begin datum))
         (datum-post-aff (org-element-property :post-affiliated datum))
         (pos (or pos (point))))
    (when (and datum-beg
               datum-post-aff
               (<= datum-beg pos)
               (< pos datum-post-aff))
      (save-excursion
        (forward-line 0)
        (when (looking-at
               "[ \t]*#\\+\\([a-zA-Z_-]+\\)")
          (upcase (match-string-no-properties 1)))))))

(provide 'org-cmenu-typedoc)
;;; org-cmenu-typedoc.el ends here
