;;; org-cmenu-tools.el --- Tools for Org Mode -*- lexical-binding: t; -*-

;; Copyright (C) 2021 AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: outline

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

;; Define useful commands that are missing in org-mode.

;; This file does not depend directly on org-cmenu.

;;; Code:

(require 'org)

;;;; Common

(defun org-cmenu-enclosing-element (element ignore-types)
  (seq-find
   (lambda (e) (and
                ;; ignore-types is used when you want to exclude a specific type.
                (not (memq (org-element-type e) ignore-types))
                ;; It seems that a inline thing becomes 'object and
                ;; becomes something like 'element that wraps
                ;; something.
                (eq (org-element-class e) 'element)))
   ;; A list of elements from ELEMENT to the root.
   ;; For example
   ;;'(link paragraph item plain-list).
   (org-element-lineage (or element (org-element-context)) nil t)))

(defun org-cmenu-narrow-to-datum (datum)
  (interactive (list (org-element-context)))
  (narrow-to-region
   (org-element-property :begin datum)
   (org-element-property :end datum)))

(defun org-cmenu-under-section-p (datum)
  (member
   (org-element-type (org-element-property :parent datum))
   '(nil section)))

;;;; Predicates

(defun org-cmenu-element-p (datum)
  "Return t if DATUM is a element (not an inline object)."
  (eq (org-element-class datum) 'element))

(defun org-cmenu-first-link-p (datum)
  "Return t if DATUM is the first link in the parent element."
  (and (eq (org-element-type datum) 'link)
        (if-let ((parent (org-element-property :parent datum)))
            (equal
             (+ (org-element-property :begin parent)
                (org-element-property
                 :begin
                 (seq-find
                  (lambda (e) (eq (org-element-type e) 'link))
                  (org-element-parse-secondary-string
                   (save-excursion
                     (save-restriction
                       (widen)
                       (buffer-substring
                        (org-element-property :begin parent)
                        (org-element-property :end parent))))
                   '(link)
                   (org-element-property :parent parent))))
                -1)
             (org-element-property :begin datum))
          t)))

(defun org-cmenu-element-or-first-link-p (datum)
  "Return t if DATUM is not a link or is the first link in the parent element."
  (or (org-cmenu-element-p datum)
      (org-cmenu-first-link-p datum)))

(defun org-cmenu-standalone-link-p (datum)
  "Return t if DATUM is a standalone link."
  (and (eq (org-element-type datum) 'link)
       (let ((parent (org-element-property :parent datum)))
         (or
          (null parent)
          (and
           parent
           (eq (org-element-type parent) 'paragraph)
           (save-excursion
             (save-restriction
               (widen)
               (not
                (or
                 (org-string-nw-p
                  (buffer-substring
                   (org-element-property :post-affiliated parent)
                   (org-element-property :begin datum)))
                 (org-string-nw-p
                  (buffer-substring
                   (org-element-property :end datum)
                   (org-element-property :end parent))))))))))))

(defun org-cmenu-element-or-standalone-link-p (datum)
  (or (not (eq (org-element-type datum) 'link))
      (org-cmenu-standalone-link-p datum)))

(defun org-cmenu-file-link-p (link)
  (and (eq (org-element-type link) 'link)
       (equal (org-element-property :type link) "file")))

(defun org-cmenu-exists-file-link-p (link)
  (and (org-cmenu-file-link-p link)
       (file-exists-p (org-element-property :path link))))

(defun org-cmenu-exists-image-file-first-link-p (link)
  (and
   (org-cmenu-first-link-p link);;see: "HACK:" comment in org-html-link
   (equal (org-element-property :type link) "file")
   (let ((path (org-element-property :path link)))
     (and
      (string-match-p (image-file-name-regexp) path)
      (file-exists-p path)))))

;;;; Affiliated Keywords

;; Add affiliated KEYWORD to the current ELEMENT.
;;
;; - Insertion point is :post-affilicated or :begin
;; - If there is something (item, etc.) to the left of the insertion point, insert KEYWORD after line-break and indent. Keywords must always start at the beginning of the line.
;; - Even if there is nothing, insert KEYWORD after indent.
;; - Some elements cannot have KEYWORD. item, table-row, table-cell.
;; - Objects cannot have KEYWORD.
;; - Some link objects can have KEYWORD. For example, the attributes for the first link in paragraph and the captions for the standalone image.

(defun org-cmenu-add-affiliated-keyword (keyword &optional element)
  "Add affiliated KEYWORD to ELEMENT."

  ;; Find the target element
  (setq element
        (if element
            ;; Skip objects (Object can't have keywords)
            (seq-find
             (lambda (e) (not (eq (org-element-class e) 'object)))
             (org-element-lineage element nil t))
          ;; Find element
          (org-cmenu-enclosing-element nil '(item table-row table-cell))))
  (when (null element)
    (error "Target element not found"))

  ;; Insert keyword before post-affiliated
  (let* ((begin (org-element-property :begin element))
         (end (org-element-property :end element))
         (post-aff (org-element-property :post-affiliated element))
         (downcase-p (and post-aff
                          (save-excursion
                            (goto-char begin)
                            (let ((case-fold-search nil))
                              (looking-at "[ \t]*#\\+[a-z]")))))
         (after-str ""))

    ;; Insert indent
    (goto-char begin)
    (if (and (not (equal begin post-aff))
             (re-search-forward "^\\([ \t]*\\)#\\+" (or post-aff end) t))
        ;; If affiliated keywords already exist, use it's indentation.
        (progn
          (goto-char post-aff)
          (insert (match-string 1)))
      (goto-char (or post-aff begin))
      (if (looking-back "^[ \t]*" (line-beginning-position))
          ;; There is only a blank before the insertion point.
          ;; e.g. . <Paragraph>
          ;; - Paragraph1
          ;;
          ;; . #+keyword: <=Insert (. means :begin of Paragraph2)
          ;;   Paragraph2
          (progn
            (setq after-str (match-string 0))
            (when (looking-at "[ \t]*")
              (insert (match-string 0))))
        ;; There is somthing before the insertion point.
        ;; e.g. <Item> here <Paragraph>
        ;; - .          <=:begein of Paragraph1
        ;;   #+keyword: <=Insert
        ;;   Paragraph1
        (let ((column-str (make-string (current-column) ? )))
          (insert "\n" column-str)
          (setq after-str column-str))))

    ;; Insert keyword
    (insert "#+" (if downcase-p (downcase keyword) keyword) ": ")
    (save-excursion
      (insert "\n" after-str))))

(put 'org-cmenu-add-attr-org 'org-cmenu
     '(:target (aff-elements link :pred org-cmenu-element-or-first-link-p)))
(defun org-cmenu-add-attr-org (&optional element)
  "Add #+ATTR_ORG: keyword to the element around point."
  (interactive)
  (org-cmenu-add-affiliated-keyword "ATTR_ORG" element)
  (insert ":"))

(put 'org-cmenu-add-attr-html 'org-cmenu
     '(:target (aff-elements link :pred org-cmenu-element-or-first-link-p)))
(defun org-cmenu-add-attr-html (&optional element)
  "Add #+ATTR_HTML: keyword to the element around point."
  (interactive)
  (org-cmenu-add-affiliated-keyword "ATTR_HTML" element)
  (insert ":"))

(put 'org-cmenu-add-attr-latex 'org-cmenu
     '(:target (aff-elements link :pred org-cmenu-element-or-first-link-p)))
(defun org-cmenu-add-attr-latex (&optional element)
  "Add #+ATTR_LATEX: keyword to the element around point."
  (interactive)
  (org-cmenu-add-affiliated-keyword "ATTR_LATEX" element)
  (insert ":"))

(put 'org-cmenu-add-caption 'org-cmenu
     '(:target (aff-elements link :pred org-cmenu-element-or-standalone-link-p)))
(defun org-cmenu-add-caption (&optional element)
  "Add #+CAPTION: keyword to the element around point."
  (interactive)
  (org-cmenu-add-affiliated-keyword "CAPTION" element))

(put 'org-cmenu-add-name 'org-cmenu '(:target aff-elements))
(defun org-cmenu-add-name (&optional element)
  "Add #+NAME: keyword to the element around point."
  (interactive)
  (org-cmenu-add-affiliated-keyword "NAME" element))

;;;; Comment

(defconst org-cmenu-types-can-comment-out
  (seq-difference org-element-all-elements '(item table-row)))

(defconst org-cmenu-types-cannot-comment-out
  (append org-element-all-objects '(item table-row)))

(put 'org-cmenu-comment-element 'org-cmenu
     `(:target ,org-cmenu-types-can-comment-out))
(defun org-cmenu-comment-element (&optional element)
  "Comment the element around point."
  (interactive (list (org-element-lineage (org-element-at-point)
                                          org-cmenu-types-can-comment-out t)))

  (unless element
    (error "No element"))

  (unless (memq (org-element-type element)
                org-cmenu-types-can-comment-out)
    ;; (error "%s is a type that cannot be commented out"
    ;;        (org-element-type element))
    (setq element
          (org-element-lineage element org-cmenu-types-can-comment-out t))
    (unless element
      (error "There are no elements that can be commented out")))

  (let ((begin (org-element-property :begin element))
        (end (org-element-property :end element)))
    ;; Insert line-break
    (save-excursion
      (goto-char begin)
      (unless (looking-back "^[ \t]*" (line-beginning-position))
        ;; e.g. - Paragraph
        (let ((column (current-column)))
          (insert "\n" (make-string column ? ))
          (setq begin (+ begin 1 column)
                end (+ end 1 column)))))

    ;; Comment
    (let ((comment-empty-lines t))
      (comment-region begin end))))

(put 'org-cmenu-comment-enclosing-element 'org-cmenu
     `(:target ,org-cmenu-types-cannot-comment-out))
(defun org-cmenu-comment-enclosing-element (datum)
  (org-cmenu-comment-element datum))

;;;; Region

(put 'org-cmenu-mark-datum 'org-cmenu '(:target all))
(defun org-cmenu-mark-datum (&optional datum)
  (interactive)
  (unless datum
    (setq datum (org-element-context)))
  (unless datum
    (error "No datum"))
  (let ((begin (org-element-property :begin datum))
        (end (org-element-property :end datum))
        (post-blank (org-element-property :post-blank datum)))
    (push-mark)
    (goto-char end)
    ;; :fixed-width returns too many post-blanks
    ;; (forward-line (- (or (org-element-property :post-blank datum) 0)))
    (cl-loop repeat (or post-blank 0)
             while (looking-back "^[ \t]*\n[ \t]*" begin)
             do (forward-line -1))
    (push-mark (point) nil t)
    (goto-char begin)))

(put 'org-cmenu-kill-datum 'org-cmenu '(:target all))
(defun org-cmenu-kill-datum (&optional datum)
  (interactive)
  (org-cmenu-mark-datum datum)
  (call-interactively #'kill-region))

(put 'org-cmenu-copy-datum 'org-cmenu '(:target all))
(defun org-cmenu-copy-datum (&optional datum)
  (interactive)
  (org-cmenu-mark-datum datum)
  (call-interactively #'kill-ring-save))

(put 'org-cmenu-toggle-narrow-datum 'org-cmenu '(:target all))
(defun org-cmenu-toggle-narrow-datum (&optional datum)
  (interactive)
  (if (buffer-narrowed-p)
      (widen)
    (org-cmenu-narrow-to-datum datum)))

;;;;; Contents

(defun org-cmenu-has-non-empty-contents-p (datum)
  (let* ((range (ignore-errors (org-cmenu-contents-range datum)))
         (begin (car range))
         (end (cdr range)))
    (and begin end (< begin end))))

(put 'org-cmenu-mark-contents 'org-cmenu
     '(:target (contents :pred org-cmenu-has-non-empty-contents-p)))
(defun org-cmenu-mark-contents (&optional datum)
  (interactive)
  (unless datum
    (setq datum (org-element-context)))
  (unless datum
    (error "No datum"))
  (let* ((range (org-cmenu-contents-range datum))
         (begin (car range))
         (end (cdr range)))
    ;; Some elements (e.g. comment, fixed-width) cannot only mark contents.
    (push-mark)
    (goto-char end)
    (push-mark (point) nil t)
    (goto-char begin)))

(put 'org-cmenu-kill-contents 'org-cmenu
     '(:target (contents :pred org-cmenu-has-non-empty-contents-p)))
(defun org-cmenu-kill-contents (&optional datum)
  (interactive)
  ;;@todo Support comment, fixed-width
  (org-cmenu-mark-contents datum)
  (call-interactively #'kill-region))

(put 'org-cmenu-copy-contents 'org-cmenu
     '(:target (contents :pred org-cmenu-has-non-empty-contents-p)))
(defun org-cmenu-copy-contents (&optional datum)
  (interactive)
  ;;@todo Support comment, fixed-width
  (org-cmenu-mark-contents datum)
  (call-interactively #'kill-ring-save))

(put 'org-cmenu-toggle-narrow-contents 'org-cmenu
     '(:target (contents :pred org-cmenu-has-non-empty-contents-p)))
(defun org-cmenu-toggle-narrow-contents (&optional datum)
  (interactive)
  (if (buffer-narrowed-p)
      (widen)
    (let* ((range (org-cmenu-contents-range datum))
           (begin (car range))
           (end (cdr range)))
      (narrow-to-region begin end))))

(defun org-cmenu-find-contents-range (datum begin-regexp end-regexp)
  (let ((begin (org-element-property :begin datum))
        (end (org-element-property :end datum))
        c-begin
        c-end)
    (save-excursion
      (goto-char (or (org-element-property :post-affiliated datum)
                     begin))
      (unless (re-search-forward begin-regexp end t)
        (error "No begin text"))
      (setq c-begin (point))
      (goto-char end)
      (unless (re-search-backward end-regexp begin t)
        (error "No end text"))
      (setq c-end (point)))
    (cons c-begin c-end)))

(defun org-cmenu-contents-range (datum)
  (if-let ((c-begin (org-element-property :contents-begin datum))
           (c-end (org-element-property :contents-end datum)))
      (cons c-begin c-end)
    (pcase (org-element-type datum)
      ('src-block
       (org-cmenu-find-contents-range datum
                                    "[ \t]*#\\+BEGIN_SRC[^\n]*\n"
                                    "^[ \t]*#\\+END_SRC"))
      ('comment-block
       (org-cmenu-find-contents-range datum
                                    "[ \t]*#\\+BEGIN_COMMENT[^\n]*\n"
                                    "^[ \t]*#\\+END_COMMENT"))
      ('example-block
       (org-cmenu-find-contents-range datum
                                    "[ \t]*#\\+BEGIN_EXAMPLE[^\n]*\n"
                                    "^[ \t]*#\\+END_EXAMPLE"))
      ('export-block
       (org-cmenu-find-contents-range datum
                                    "[ \t]*#\\+BEGIN_EXPORT[^\n]*\n"
                                    "^[ \t]*#\\+END_EXPORT"))
      ('code
       (org-cmenu-find-contents-range datum "~" "~"))
      ('verbatim
       (org-cmenu-find-contents-range datum "=" "="))
      ;;@todo support (single line) comment
      ;;@todo support (single line) fixed-width
      ;;@todo support inline-babel-call?
      ;;@todo support inline-src-block?
      ;;@todo support babel-call?

      ;;@todo support diary-sexp?
      ;;@todo support latex-environment?
      ;;@todo support node-property?
      ;;@todo support citation-reference?
      ;;@todo support latex-fragment?

      ;; Not support:
      ;; - Elements
      ;;   comment(multiline), fixed-width(multiline)
      ;;   keyword, planning, clock, horizontal-rule
      ;; - Objects
      ;;   entity, export-snippet, line-break, macro,
      ;;   statistics-cookie, target, timestamp
      (_
       (error "No contents range information (type=%s)"
              (org-element-type datum))))))

;;;; Expose Contents

(defun org-cmenu-back-before-spaces ()
  (while (member (char-before) '(?  ?\n ?\t))
    (backward-char)))

(defun org-cmenu-pos-before-spaces (pos)
  (save-excursion
    (goto-char pos)
    (org-cmenu-back-before-spaces)
    (point)))

(defun org-cmenu-can-expose-p (datum)
  (let* ((c-range (ignore-errors (org-cmenu-contents-range datum)))
         (c-begin (car c-range))
         (c-end (cdr c-range))
         (begin (org-element-property :begin datum))
         (end (org-element-property :end datum))
         (post-blank (org-element-property :post-blank datum)))
    (and
     c-begin
     c-end
     (or
      (< begin c-begin)
      (< c-end (- end post-blank))))))

(put 'org-cmenu-expose-contents 'org-cmenu
     '(:target (contents
                :pred org-cmenu-can-expose-p
                :exclude (table table-row table-cell))))
(defun org-cmenu-expose-contents (&optional datum)
  (interactive)
  (unless datum
    (setq datum (org-element-context)))
  (unless datum
    (error "No datum"))
  ;;@todo Support comment, fixed-width
  (let* ((c-range (org-cmenu-contents-range datum)) ;;error if no :contents-begin
         (c-begin (car c-range))
         (c-end (cdr c-range))
         (begin (org-element-property :begin datum))
         (end (org-element-property :end datum))
         (post-blank (org-element-property :post-blank datum)))
    (delete-region c-end (- end post-blank))
    (delete-region begin c-begin)))

;;;; Link

(put 'org-cmenu-link-open-by-default 'org-cmenu '(:target link))
(defun org-cmenu-link-open-by-default (link)
  (org-link-open link))

(put 'org-cmenu-link-open-by-emacs 'org-cmenu '(:target link))
(defun org-cmenu-link-open-by-emacs (link)
  (org-link-open link '(4)))

(put 'org-cmenu-link-open-by-system 'org-cmenu '(:target link))
(defun org-cmenu-link-open-by-system (link)
  (org-link-open link '(16)))

(put 'org-cmenu-link-open-directory 'org-cmenu
     '(:target (link :pred org-cmenu-file-link-p)));;@todo org-cmenu-exists-directory-p ?
(defun org-cmenu-link-open-directory (link)
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link)))
    (when (and (equal type "file")
               path
               (file-exists-p path))
      (setq path (expand-file-name path))
      (find-file (file-name-directory path))
      (when (eq major-mode 'dired-mode)
        (dired-goto-file path)))))

(put 'org-cmenu-link-copy-path 'org-cmenu '(:target link))
(defun org-cmenu-link-copy-path (link)
  (when-let ((path (org-element-property :path link)))
    (kill-new path)
    (message "%s" path)))

(put 'org-cmenu-link-copy-file-name 'org-cmenu
     '(:target (link :pred org-cmenu-file-link-p)))
(defun org-cmenu-link-copy-file-name (link)
  (when-let ((type (org-element-property :type link))
             (path (org-element-property :path link)))
    (when (equal type "file")
      (let ((filename (file-name-nondirectory path)))
        (kill-new filename)
        (message "%s" filename)))))

(defvar org-cmenu-file-info-function-map
  (nconc
   (when-let
       ((command (cond
                  ((executable-find "exiftool") "exiftool %s")
                  ((executable-find "identify") "identify -verbose %s"))))
     (list (cons (image-file-name-regexp) command)))
   (list (cons "" "stat %s"))))

(defun org-cmenu-show-file-info (path)
  (let ((func (cdr (seq-find
                    (lambda (item) (string-match-p (car item) path))
                    org-cmenu-file-info-function-map))))
    (cond
     ((functionp func)
      (funcall func path))
     ((stringp func)
      (org-cmenu-shell-command-popup
       (format func (shell-quote-argument path))
       "*File Info*" "*File Info Error*")))))

(put 'org-cmenu-link-show-file-info 'org-cmenu
     '(:target (link :pred org-cmenu-exists-file-link-p)))
(defun org-cmenu-link-show-file-info (link)
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link)))
    (unless (equal type "file")
      (error "type=%s" type))
    (unless (file-exists-p path)
      (error "File not exists : %s" path))

    (org-cmenu-show-file-info path)))

(put 'org-cmenu-link-rename-file 'org-cmenu
     '(:target (link :pred org-cmenu-exists-file-link-p)))
(defun org-cmenu-link-rename-file (link)
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         (abs-p (file-name-absolute-p path))
         (abs-path (expand-file-name path)))
    (unless (equal type "file")
      (error "type=%s" type))
    (unless (file-exists-p abs-path)
      (error "File not exists : %s" abs-path))

    (let* ((new-file
            (read-file-name "File Name: "
                            (file-name-directory abs-path)
                            nil
                            nil
                            (file-name-nondirectory abs-path)))
           (new-file
            (if (string-empty-p (file-name-nondirectory new-file))
                (concat new-file (file-name-nondirectory abs-path))
              new-file))
           (new-link-path
            (concat "file:"
                    (if abs-p
                        (expand-file-name new-file)
                      (file-relative-name new-file)))))
      (rename-file abs-path new-file)
      (org-cmenu-link-replace-at-point new-link-path 'path)
      (message "Changed the file name to %s\n(%s)"
               new-file new-link-path))))

(defun org-cmenu-link-replace-at-point (new-text part)
  (cond
   ((org-in-regexp org-link-bracket-re 1)
    (pcase part
      ('link (replace-match (org-link-escape new-text) t t nil 0))
      ('path (replace-match (org-link-escape new-text) t t nil 1))
      ('description (replace-match new-text t t nil 2)))
    t)
   ((org-in-regexp org-link-angle-re)
    (pcase part
      ('link (replace-match new-text t t nil 0) t)
      ('path (replace-match (concat "<" new-text ">") t t nil 0) t)
      ('description nil)))
   ((org-in-regexp org-link-plain-re)
    (pcase part
      ('link (replace-match new-text t t nil 0) t)
      ('path (replace-match new-text t t nil 0) t)
      ('description nil)))))

;;@todo impl
;; (defun org-cmenu-file-link-open-source (link)
;;   (interactive)
;;   )

;;@todo impl
;; (defun org-cmenu-file-link-open-map (link)
;;   (interactive)
;;   )

(defun org-cmenu-shell-command-popup (command output-buffer error-buffer)
  "Execute COMMAND and pop up the resulting buffer."
  (let* ((kill-buffers
          (lambda ()
            (when (get-buffer output-buffer) (kill-buffer output-buffer))
            (when (get-buffer error-buffer) (kill-buffer error-buffer))))
         (quit
          (lambda ()
            (interactive)
            (quit-window)
            (funcall kill-buffers)))
         (init-buffer
          (lambda (buffer-name)
            (when (get-buffer buffer-name)
              (with-current-buffer buffer-name
                (read-only-mode)
                (local-set-key "q" quit)))))
         (result-code
          (progn
            (funcall kill-buffers)
            (let ((max-mini-window-height 0))
              (shell-command command output-buffer error-buffer)))))
    (funcall init-buffer output-buffer)
    (funcall init-buffer error-buffer)
    (pop-to-buffer (if (equal result-code 0) output-buffer error-buffer))
    result-code))

;;;; Plain List

(put 'org-cmenu-plain-list-make-subtree 'org-cmenu
     '(:target (plain-list :pred org-cmenu-under-section-p)))
(defun org-cmenu-plain-list-make-subtree (datum)
  (save-excursion
    (save-restriction
      (org-list-make-subtree))))

(put 'org-cmenu-plain-list-repair 'org-cmenu '(:target plain-list))
(defun org-cmenu-plain-list-repair (datum)
  (save-excursion
    (save-restriction
      (org-cmenu-narrow-to-datum datum)
      (org-list-repair))))

(put 'org-cmenu-plain-list-copy-as-sexp 'org-cmenu '(:target plain-list))
(defun org-cmenu-plain-list-copy-as-sexp (datum)
  (save-excursion
    (save-restriction
      (org-cmenu-narrow-to-datum datum)
      (goto-char (org-element-property :post-affiliated datum))
      (kill-new (pp (org-list-to-lisp))))))

;;;; Table
;;;;; S-Exp
(put 'org-cmenu-table-copy-as-sexp 'org-cmenu
     '(:target (table table-row table-cell)))
(defun org-cmenu-table-copy-as-sexp (datum)
  (save-excursion
    (goto-char (or (org-element-property :post-affiliated datum)
                   (org-element-property :begin datum)))
    (kill-new (pp (org-cmenu-table-to-lisp-no-properties)))))

(defun org-cmenu-table-to-lisp-no-properties ()
  (interactive)
  (let ((rows (org-table-to-lisp)))
    (dolist (row rows)
      (when (listp row)
        (cl-loop for cell on row
                 do (when (stringp (car cell))
                      (setcar cell (substring-no-properties (car cell)))))))
    rows))

;;;;; Move Point (Internal)

;; goto row

(defun org-cmenu-table-goto-row-bol (n)
  "Go to the beginning (on |) of the Nth row.

Return t when the line exists, nil if it does not exist."
  (let ((saved-point (point))
        (end (org-table-end))
        (i 0))
    (goto-char (org-table-begin))
    (while (and (< i n)
                (re-search-forward org-table-dataline-regexp end t))
      (setq i (1+ i)))
    (backward-char 2) ;; |[^-]

    (if (= i n)
        t
      (goto-char saved-point)
      nil)))

(defun org-cmenu-table-goto-first-row-bol ()
  "Go to the beginning (on |) of the first row."
  (org-cmenu-table-goto-row-bol 1))

(defun org-cmenu-table-goto-first-non-empty-row-bol ()
  "Go to the beginning (on |) of the first non empty row."
  (let ((saved-point (point)))
    (goto-char (org-table-begin))
    (if (re-search-forward "^[ \t]*|[^-\n]" (org-table-end) t)
        (progn
          (backward-char 2) ;; |[^-\n]
          t)
      (goto-char saved-point)
      nil)))

(defun org-cmenu-table-goto-first-row-nth-column (n)
  (let ((saved-point (point))
        (end (org-table-end))
        found)
    (goto-char (org-table-begin))
    (while (and (re-search-forward "^[ \t]*|[^-\n]" end t)
                (not (setq found (org-cmenu-table-goto-column n)))))
    (unless found
      (goto-char saved-point))
    found))

(defun org-cmenu-table-goto-last-row-bol ()
  "Go to the beginning (on |) of the last row."
  (let ((saved-point (point)))
    (goto-char (org-table-end))
    (if (re-search-backward org-table-dataline-regexp (org-table-begin) t)
        t
      (goto-char saved-point)
      nil)))

(defun org-cmenu-table-goto-last-non-empty-row-bol ()
  "Go to the beginning (on |) of the last non empty row."
  (let ((saved-point (point)))
    (goto-char (org-table-end))
    (if (re-search-backward "^[ \t]*|[^-\n]" (org-table-begin) t)
        t
      (goto-char saved-point)
      nil)))

(defun org-cmenu-table-goto-last-row-nth-column (n)
  (let ((saved-point (point))
        (begin (org-table-begin))
        found)
    (goto-char (org-table-end))
    (while (and (re-search-backward "^[ \t]*|[^-\n]" begin t)
                (not (setq found (org-cmenu-table-goto-column n))))
      (beginning-of-line))
    (unless found
      (goto-char saved-point))
    found))

;; goto column

(defun org-cmenu-table-goto-column (n)
  "Move the cursor to the Nth column in the current table line."
  ;;NOTE: org-table-goto-column will move to place that is not table-cell,
  ;;      so do not use it.
  (beginning-of-line)
  (cl-loop repeat n
           always (re-search-forward "\\(|[ \t]?\\)[ \t]*[^ \t\n]"
                                    (line-end-position) t)
           do (goto-char (match-end 1))))

(defun org-cmenu-table-has-column-p (n)
  "Return t if the current row has the Nth column."
  (save-excursion
    (org-cmenu-table-goto-column n)))

;; goto in field

(defun org-cmenu-table-goto-end-of-field ()
  (when (re-search-forward "\\([ \t]?|\\|$\\)" (line-end-position) t)
    (goto-char (match-beginning 1))))

;; Predicate

(defun org-cmenu-table-empty-row-p ()
  (save-excursion
    (when (org-at-table-p)
      (beginning-of-line)
      (not (looking-at "^[ \t]*|[^-\n]")))))

;;;;; Move Point

(put 'org-cmenu-table-previous-column 'org-cmenu
     '(:target table-cell :call no-warp))
(defun org-cmenu-table-previous-column ()
  (interactive)
  (when (org-at-table-p)
    (when (re-search-backward "\\(|[ \t]?\\)[^|\n]*|"
                              (line-beginning-position) t)
      (goto-char (match-end 1)))))

(put 'org-cmenu-table-next-column 'org-cmenu
     '(:target table-cell :call no-warp))
(defun org-cmenu-table-next-column ()
  (interactive)
  (when (org-at-table-p)
    (when (re-search-forward "|[ \t]?\\([ \t]*[^\n]\\)" (line-end-position) t)
      (goto-char (match-beginning 1)))))

(put 'org-cmenu-table-previous-row 'org-cmenu
     '(:target table-cell :call no-warp))
(defun org-cmenu-table-previous-row ()
  (interactive)
  (when (org-at-table-p)
    (let ((col (org-table-current-column)))
      (when (re-search-backward "^[ \t]*|[^-\n][^\n]*\n[^\n]*"
                                (org-table-begin) t)
        (org-cmenu-table-goto-column col)))))

(put 'org-cmenu-table-next-row 'org-cmenu
     '(:target table-cell :call no-warp))
(defun org-cmenu-table-next-row ()
  (interactive)
  (when (org-at-table-p)
    (let ((col (org-table-current-column)))
      (when (re-search-forward org-table-dataline-regexp (org-table-end) t)
        (org-cmenu-table-goto-column col)))))

(put 'org-cmenu-table-first-column-in-row 'org-cmenu
     '(:target table-cell :call no-warp))
(defun org-cmenu-table-first-column-in-row ()
  (interactive)
  (when (org-at-table-p)
    (beginning-of-line)
    (org-cmenu-table-next-column)))

(put 'org-cmenu-table-last-column-in-row 'org-cmenu
     '(:target table-cell :call no-warp))
(defun org-cmenu-table-last-column-in-row ()
  (interactive)
  (while (org-cmenu-table-next-column)))

;;

(put 'org-cmenu-table-first-row 'org-cmenu
     '(:target table-cell :call no-warp))
(defun org-cmenu-table-first-row ()
  (interactive)
  (when (org-at-table-p)
    (let ((col (org-table-current-column)))
      (when (org-cmenu-table-goto-first-row-bol)
        (org-cmenu-table-goto-column col)))))

(put 'org-cmenu-table-last-row 'org-cmenu
     '(:target table-cell :call no-warp))
(defun org-cmenu-table-last-row ()
  (interactive)
  (when (org-at-table-p)
    (let ((col (org-table-current-column)))
      (when (org-cmenu-table-goto-last-row-bol)
        (org-cmenu-table-goto-column col)))))

(put 'org-cmenu-table-first-field-in-table 'org-cmenu
     '(:target table-cell :call no-warp))
(defun org-cmenu-table-first-field-in-table ()
  (interactive)
  (when (and (org-at-table-p)
             (org-cmenu-table-goto-first-non-empty-row-bol))
    (org-cmenu-table-goto-column 1)))

(put 'org-cmenu-table-last-field-in-table 'org-cmenu
     '(:target table-cell :call no-warp))
(defun org-cmenu-table-last-field-in-table ()
  (interactive)
  (when (and (org-at-table-p)
             (org-cmenu-table-goto-last-non-empty-row-bol))
    (org-cmenu-table-last-column-in-row)))

;;;;; Mark

(put 'org-cmenu-table-mark-all 'org-cmenu
     '(:target (table table-row table-cell) :call no-warp))
(defun org-cmenu-table-mark-all ()
  (interactive)
  (when (org-at-table-p)
    ;;@todo check the last row has a maximum column size.
    (org-cmenu-table-first-field-in-table)
    (push-mark (point) nil t)
    (org-cmenu-table-last-field-in-table)
    (org-cmenu-table-goto-end-of-field)
    t))

(put 'org-cmenu-table-mark-row 'org-cmenu
     '(:target (table-row table-cell) :call no-warp))
(defun org-cmenu-table-mark-row ()
  (interactive)
  (when (org-at-table-p)
    (when (org-cmenu-table-empty-row-p)
      (error "The current row is empty"))

    (org-cmenu-table-first-column-in-row)
    (push-mark (point) nil t)
    (org-cmenu-table-last-column-in-row)
    (org-cmenu-table-goto-end-of-field)
    t))

(put 'org-cmenu-table-mark-column 'org-cmenu
     '(:target table-cell :call no-warp))
(defun org-cmenu-table-mark-column ()
  (interactive)
  (when (org-at-table-p)
    (let ((col (org-table-current-column)))
      (when (org-cmenu-table-goto-first-row-nth-column col)
        (push-mark (point) nil t)
        (org-cmenu-table-goto-last-row-nth-column col)
        (org-cmenu-table-goto-end-of-field)
        t))))

(put 'org-cmenu-table-mark-field 'org-cmenu
     '(:target table-cell :call no-warp))
(defun org-cmenu-table-mark-field ()
  (interactive)
  (when (org-at-table-p)
    (org-cmenu-table-goto-end-of-field)
    (push-mark (point) nil t)
    (when (re-search-backward "| ?" (line-beginning-position) t)
      (goto-char (match-end 0)))))


;;;;; Region

(put 'org-cmenu-table-cut-region 'org-cmenu
     '(:target table-cell :call no-warp))
(defun org-cmenu-table-cut-region ()
  (interactive)
  (org-table-save-field
   (call-interactively #'org-table-cut-region)))

(put 'org-cmenu-table-copy-region 'org-cmenu
     '(:target table-cell :call no-warp))
(defun org-cmenu-table-copy-region ()
  (interactive)
  (org-table-save-field
   (call-interactively #'org-table-copy-region))
  (setq deactivate-mark t))

;;;;; Sum

(defun org-cmenu-table-sum-all ()
  (interactive)
  (when (org-at-table-p)
    ;;@todo check the last row has a maximum column size.
    ;;@todo There are many cases where it cannot be calculated correctly.
    (save-mark-and-excursion
      (org-cmenu-table-mark-all)
      (call-interactively #'org-table-sum))))

(defun org-cmenu-table-sum-row ()
  (interactive)
  (when (org-at-table-p)
    (save-mark-and-excursion
      (org-cmenu-table-mark-row)
      (call-interactively #'org-table-sum))))

(defun org-cmenu-table-sum-column ()
  (interactive)
  (when (org-at-table-p)
    (save-mark-and-excursion
      (org-cmenu-table-mark-column)
      (call-interactively #'org-table-sum))))

;;;;; Shrink/Expand

(defvar org-cmenu-table-cycle-column-width--count 0)
(defun org-cmenu-table-cycle-column-width ()
  (interactive)
  ;;(message "last=%s this=%s" last-command this-command)
  (setq org-cmenu-table-cycle-column-width--count
        (if (eq last-command this-command)
            (1+ org-cmenu-table-cycle-column-width--count)
          0))
  (pcase (% org-cmenu-table-cycle-column-width--count 4)
    (0 (if (org-cmenu-table-has-column-width-spec-p)
           (org-table-shrink)
         (org-cmenu-table-shrink-all-column)))
    (1 (org-table-expand))
    (2 (org-cmenu-table-shrink-all-column))
    (3 (org-table-expand))))

(defun org-cmenu-table-has-column-width-spec-p ()
  (when (org-at-table-p)
    (save-excursion
      (goto-char (org-table-begin))
      (re-search-forward "|[ \t]*<[lrc]?[0-9]+>[ \t]*\\(|\\|$\\)" (org-table-end) t))))

(defun org-cmenu-table-shrink-all-column ()
  (interactive)
  (when (org-at-table-p)
    (org-table-expand)
    (org-table-toggle-column-width "-")))

;;;; Insert
;;;;; Insert Objects

(defun org-cmenu-insert-begin-end (beg-str &optional end-str)
  (unless end-str
    (setq end-str beg-str))
  (insert beg-str end-str)
  (backward-char (length end-str)))

(defun org-cmenu-insert-bold ()
  (interactive)
  (org-emphasize ?*))

(defun org-cmenu-insert-underline ()
  (interactive)
  (org-emphasize ?_))

(defun org-cmenu-insert-italic ()
  (interactive)
  (org-emphasize ?/))

(defun org-cmenu-insert-verbatim ()
  (interactive)
  (org-emphasize ?=))

(defun org-cmenu-insert-code ()
  (interactive)
  (org-emphasize ?~))

(defun org-cmenu-insert-strike-through ()
  (interactive)
  (org-emphasize ?+))

(defun org-cmenu-insert-subscript ()
  (interactive)
  (org-cmenu-insert-begin-end "_{" "}"))

(defun org-cmenu-insert-superscript ()
  (interactive)
  (org-cmenu-insert-begin-end "^{" "}"))

(defun org-cmenu-insert-inline-babel-call (fname)
  (interactive "sFunction Name: ")
  (insert (format "call_%s()" fname)))

(defun org-cmenu-insert-inline-src-block (lang)
  (interactive "sLanguage: ")
  (insert (format "src_%s[]{}" lang)) ;;@todo remove [] after implementing the command to add header arguments to inline-src-block. org-babel-insert-header-arg does not support insertion into inline-src-block.
  (backward-char 1))

(defun org-cmenu-insert-line-break ()
  (interactive)
  (insert "\\\\\n"))

(defun org-cmenu-entities-name-and-utf8 (entities)
  "Convert ENTITIES (org-entities or org-entities-user) to list
of name and utf8."
  (cl-loop for e in entities
           when (listp e)
           nconc (list (car e)
                       (nth 6 e)))) ;;utf8

(defun org-cmenu-entities-rfind (entities name-or-utf8)
  "Find the entity by NAME-OR-UTF8 from ENTITIES (org-entities or
org-entities-user)."
  (seq-some
   (lambda (e)
     (when (listp e)
       (cond
        ((string= (nth 0 e) name-or-utf8) name-or-utf8) ;;name
        ((string= (nth 6 e) name-or-utf8) (nth 0 e))))) ;;utf8
   entities))

(defun org-cmenu-insert-entity (name)
  (interactive
   (list
    (let ((name-or-utf8
           (completing-read
            "Entity name or symbol: "
            (nconc
             (org-cmenu-entities-name-and-utf8 org-entities)
             (org-cmenu-entities-name-and-utf8 org-entities-user)))))
      (or (org-cmenu-entities-rfind org-entities name-or-utf8)
          (org-cmenu-entities-rfind org-entities-user name-or-utf8)))))
  (insert (format "\\%s{}" name))
  (message "%s" (nth 6 (org-entity-get name))))

;; Use org-insert-link
;; (defun org-cmenu-insert-link ()
;;   (interactive)
;;   (org-cmenu-insert-begin-end "[[" "]]"))

(defun org-cmenu-insert-target ()
  (interactive)
  (org-cmenu-insert-begin-end "<<" ">>"))

(defun org-cmenu-insert-radio-target ()
  (interactive)
  (org-cmenu-insert-begin-end "<<<" ">>>"))

(defun org-cmenu-insert-macro ()
  (interactive)
  ;;@todo find macro definition
  (org-cmenu-insert-begin-end "{{{" "name(arg)}}}"))

(defun org-cmenu-insert-export-snippet (backend)
  (interactive
   (list
    (completing-read "Backend: "
                     (mapcar #'symbol-name org-export-backends))))
  (org-cmenu-insert-begin-end (format "@@%s:" backend) "@@"))


;;;;; Insert Elements

;; org-insert-drawer
;; org-footnote-new
;; org-insert-structure-template

(defun org-cmenu-insert-babel-call ()
  (interactive)
  (org-cmenu-insert-begin-end "#+call: " "name(arg)"))

(defun org-cmenu-insert-macro-definition ()
  (interactive)
  (org-cmenu-insert-begin-end "#+MACRO: " "name string-or-sexp $1 $2\n"))

(defun org-cmenu-insert-fixed-width ()
  (interactive)
  (insert ": "))

(defun org-cmenu-insert-horizontal-rule ()
  (interactive)
  (insert "-----\n"))

;;@todo Add statistics cookie

;;@todo Add functions to insert the following keywords.
;; #+TEXT:
;; #+TEXT: [TABLE-OF-CONTENTS]
;; #+TEXT:
;; #+TITLE, AUTHOR, DATE, EMAIL, DESCRIPTION, KEYWORDS, LANGUAGE:
;; #+LINK_UP, LINK_HOME: url
;; #+BIND:
;; #+OPTIONS:
;; #+CONSTANTS: c=299792458. pi=3.14 eps=2.4e-6
;; #+STARTUP:
;; #+LINK: google http://www.google.com/search?q=%s
;; #+TODO: TODO FEEDBACK VERIFY | DONE CANCELED
;; #+PRIORITIES: A C B
;; #+FILETAGS: :Peter:Boss:Secret:
;; #+TAGS: { @work(w)  @home(h)  @tennisclub(t) }  laptop(l)  pc(p)
;; #+PROPERTY: NDisks_ALL 1 2 3 4
;; #+COLUMNS: %25ITEM %TAGS %PRIORITY %TODO
;; #+DRAWERS: LOGBOOK PROPERTIES FEEDSTATUS
;; #+ARCHIVE: %s_done::
;; #+CATEGORY: Holiday
;; #+INCLUDE: "~/example.el" :lines "5-10" :prefix1 "" :prefix "" :minlevel 1 src elisp
;; #+INDEX: Application!CV
;; LATEX_HEADER
;; EXPORT_SELECT_TAGS
;; EXPORT_EXCLUDE_TAGS
;; XSLT
;; MATHJAX
;; STYLE
;; INFOJS_OPT
;;(defun org-cmenu-insert-options ()
;;  )



(provide 'org-cmenu-tools)
;;; org-cmenu-tools.el ends here
