;;; org-cmenu.el --- Context Menu for Org-Mode    -*- lexical-binding: t; -*-

;; Copyright (C) 2021  AKIYAMA Kouhei

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

;;

;;; Code:

(require 'subr-x)
(require 'cl-lib)
(require 'transient)
(require 'org-element)

;;;; Customize

(defgroup org-cmenu nil
  "Context Menu for Org-Mode."
  :prefix "org-cmenu-"
  :group 'org)

(defvar org-cmenu-update-transient-prefix-everytime t)

;;;; Reset

(defun org-cmenu-reset ()
  (interactive)
  (org-cmenu-reset-type-aliases)
  (org-cmenu-reset-types))

;;;; Syntax Element Type Aliases

(defvar org-cmenu-type-aliases nil)

(defun org-cmenu-reset-type-aliases ()
  (setq org-cmenu-type-aliases nil)
  (org-cmenu-define-standard-type-aliases))

(defun org-cmenu-type-alias-to-ids (alias)
  (alist-get alias org-cmenu-type-aliases))

(defun org-cmenu-define-type-alias (alias type-ids)
  (setf (alist-get alias org-cmenu-type-aliases) type-ids))

(defun org-cmenu-define-standard-type-aliases ()
  ;; Each elements (see: org-element-all-elements)
  (dolist (type org-element-all-elements)
    (org-cmenu-define-type-alias type (list type)))
  ;; Each objects (see: org-element-all-objects)
  (dolist (type org-element-all-objects)
    (org-cmenu-define-type-alias type (list type)))
  ;; All elements and all objects (all)
  (org-cmenu-define-type-alias 'all (append org-element-all-elements
                                          org-element-all-objects))
  ;; All elements (elements)
  (org-cmenu-define-type-alias 'elements org-element-all-elements)
  ;; All objects (objects)
  (org-cmenu-define-type-alias 'objects org-element-all-objects)
  ;; Elements that can have affiliated keywords (aff-elements)
  (org-cmenu-define-type-alias 'aff-elements
                             (seq-difference
                              org-element-all-elements
                              '(item table-row table-cell)
                              #'eq))
  ;; Elements that can be comment (com-elements)
  (org-cmenu-define-type-alias 'com-elements
                             (seq-difference
                              org-element-all-elements
                              '(item table-row table-cell)
                              #'eq))
  ;; Elements that have contents (contents)
  (org-cmenu-define-type-alias
   'contents
   '(;; Special Support (see: org-cmenu-contents-range)
     code verbatim
          src-block comment-block example-block export-block
          ;; Elements
          center-block  drawer
          dynamic-block
          footnote-definition headline inlinetask item
          paragraph plain-list
          property-drawer quote-block section
          special-block table table-row verse-block
          ;; Objects
          bold citation 
          footnote-reference  italic
          link radio-target strike-through
          subscript superscript table-cell underline )))

(org-cmenu-define-standard-type-aliases)

;;;; Syntax Element Type List

(defvar org-cmenu-types nil)

(defun org-cmenu-reset-types ()
  (setq org-cmenu-types nil))

(defun org-cmenu-get-type (type-id)
  (assq type-id org-cmenu-types))

(defun org-cmenu-add-type (type-id)
  (let ((type (org-cmenu-type-create type-id)))
    (push type org-cmenu-types)
    type))

(defun org-cmenu-get-type-create (type-id)
  (or (org-cmenu-get-type type-id)
      (org-cmenu-add-type type-id)))

(defun org-cmenu-add-command-to-types (group-path command target-types)
  (dolist (type-id target-types)
    (org-cmenu-type-add-command
     (org-cmenu-get-type-create type-id)
     group-path command)))

;;;; Syntax Element Type

(defun org-cmenu-type-create (type-id)
  ;; (type-id:symbol groups:list)
  (list type-id nil))

(defmacro org-cmenu-type-id (type-sym)
  `(car ,type-sym))

(defmacro org-cmenu-type-groups (type-sym)
  `(cadr ,type-sym))

(defun org-cmenu-type-find-group (type group-id)
  (seq-find
   (lambda (group)
     (org-cmenu-group-equal-id group group-id))
   (org-cmenu-type-groups type)))

(defun org-cmenu-type-add-group (type group-id)
  (let ((group (org-cmenu-group-create group-id)))
    ;; push back
    (setf (org-cmenu-type-groups type)
          (nconc (org-cmenu-type-groups type)
                 (list group)))
    group))

(defun org-cmenu-type-remove-group (type group-id)
  (setf (org-cmenu-type-groups type)
        (seq-remove
         (lambda (group)
           (org-cmenu-group-equal-id group group-id))
         (org-cmenu-type-groups type))))

(defun org-cmenu-type-get-group (type group-path)
  (unless (consp group-path)
    (setq group-path (list group-path)))

  (let* ((group (org-cmenu-type-find-group type (car group-path))))
    (pop group-path)

    (while (and group group-path)
      (setq group (org-cmenu-group-find-subgroup group (car group-path)))
      (pop group-path))

    group))

(defun org-cmenu-type-get-group-create (type group-path)
  (unless (consp group-path)
    (setq group-path (list group-path)))

  (let* ((group-id (car group-path))
         (group (or (org-cmenu-type-find-group type group-id)
                    (org-cmenu-type-add-group type group-id))))
    (pop group-path)

    (while group-path
      (setq group-id (car group-path))
      (setq group (or (org-cmenu-group-find-subgroup group group-id)
                      (org-cmenu-group-add-subgroup group group-id)))
      (pop group-path))

    group))

(defun org-cmenu-type-add-command (type group-path command)
  (org-cmenu-group-add-command
   (org-cmenu-type-get-group-create type group-path)
   command))

(defun org-cmenu-type-remove-command (type group-path key)
  (when-let ((group (org-cmenu-type-get-group type group-path)))
    (org-cmenu-group-remove-command group key)))

(defun org-cmenu-type-remove-group-by-path (type group-path)
  (when group-path
    (let* ((rpath (reverse group-path))
           (last-group-id (car rpath))
           (path-to-parent (nreverse (cdr rpath))))

      (if (null path-to-parent)
          (org-cmenu-type-remove-group type last-group-id)
        (when-let ((parent-group (org-cmenu-type-get-group type path-to-parent)))
          (org-cmenu-group-remove-subgroup parent-group last-group-id))))))


;;;; Group

(defun org-cmenu-group-create (group-id)
  ;; (:group group-id:string props:plist elements:list)
  (list :group group-id nil nil))

(defun org-cmenu-group-p (object)
  (eq (car-safe object) :group))

(defmacro org-cmenu-group-id (group-sym)
  `(cadr ,group-sym))

(defmacro org-cmenu-group-props (group-sym)
  `(caddr ,group-sym))

(defmacro org-cmenu-group-elements (group-sym)
  `(cadddr ,group-sym))

(defun org-cmenu-group-equal-id (object group-id)
  (and (org-cmenu-group-p object)
       (equal (org-cmenu-group-id object) group-id)))

(defun org-cmenu-group-find-subgroup (group subgroup-id)
  (seq-find
   (lambda (elm)
     (org-cmenu-group-equal-id elm subgroup-id))
   (org-cmenu-group-elements group)))

(defun org-cmenu-group-add-subgroup (group subgroup-id)
  (let ((subgroup (org-cmenu-group-create subgroup-id)))
    ;; push back
    (setf (org-cmenu-group-elements group)
          (nconc (org-cmenu-group-elements group)
                 (list subgroup)))
    subgroup))

(defun org-cmenu-group-add-command (group command)
  (setf (org-cmenu-group-elements group)
        (nconc
         ;; Remove commands with duplicate keys from GROUP
         (seq-remove (lambda (elm)
                       (org-cmenu-command-equal-key
                        elm
                        (org-cmenu-command-get-key command)))
                     (org-cmenu-group-elements group))
         ;; Add COMMAND to the end
         (cons command nil)))
  command)

(defun org-cmenu-group-add-string (group str)
  (setf (org-cmenu-group-elements group)
        (nconc (org-cmenu-group-elements group)
               (list str)))
  str)

(defun org-cmenu-group-remove-subgroup (group subgroup-id)
  (setf (org-cmenu-group-elements group)
        (seq-remove
         (lambda (elm)
           (org-cmenu-group-equal-id elm subgroup-id))
         (org-cmenu-group-elements group))))

(defun org-cmenu-group-remove-command (group key)
  (setf (org-cmenu-group-elements group)
        (seq-remove
         (lambda (elm)
           (org-cmenu-command-equal-key elm key))
         (org-cmenu-group-elements group))))

(defun org-cmenu-group-remove-string (group str)
  (setf (org-cmenu-group-elements group)
        (seq-remove
         (lambda (elm)
           (and (stringp elm)
                (string= elm str)))
         (org-cmenu-group-elements group))))

(defun org-cmenu-group-set-property (group key value)
  (setf (org-cmenu-group-props group)
        (plist-put (org-cmenu-group-props group) key value)))

(defun org-cmenu-group-to-transient-spec (group)
  (apply
   #'vector
   ;; https://magit.vc/manual/transient.html#Group-Specifications
   (delq
    nil
    (append
     ;; Description (string)
     (let ((group-id (org-cmenu-group-id group)))
       (when (stringp group-id) (list group-id)))
     ;; Properties
     (org-cmenu-group-props group)
     ;; Elements
     (mapcar
      (lambda (elm)
        (cond
         ((org-cmenu-group-p elm)
          (org-cmenu-group-to-transient-spec elm))
         ((org-cmenu-command-p elm)
          elm)
         ((stringp elm)
          elm)))
      (org-cmenu-group-elements group))))))

;;;; Command

(defun org-cmenu-command-create (key description func &rest properties)
  ;; (key:string description:string func:function . properties:plist)
  (nconc (list key description func) properties))

(defun org-cmenu-command-p (object)
  (and (listp object)
       (stringp (car object))))

(defmacro org-cmenu-command-key (command-sym)
  `(car ,command-sym))

(defmacro org-cmenu-command-description (command-sym)
  `(cadr ,command-sym))

(defmacro org-cmenu-command-function (command-sym)
  `(caddr ,command-sym))

(defmacro org-cmenu-command-properties (command-sym)
  `(cdddr ,command-sym))

(defun org-cmenu-command-equal-key (object key)
  (and (org-cmenu-command-p object)
       (equal (org-cmenu-command-key object) key)))

(defun org-cmenu-command-get-key (command)
  (org-cmenu-command-key command))

;;;; Modify transient.el

;; ;; Add pre-exit-hook feature

;; (defvar org-cmenu-transient-pre-exit-hook nil)

;; (defun org-cmenu-transient-pre-exit (old-fun &rest rest)
;;   (prog1 (apply old-fun rest)
;;     (run-hooks 'org-cmenu-transient-pre-exit-hook)))

;; (advice-add #'transient--pre-exit
;;             :around
;;             #'org-cmenu-transient-pre-exit)

;;;; Menu

(defvar org-cmenu-saved-point nil)
(defvar org-cmenu-saved-mark nil)
(defvar org-cmenu-pointed-datum nil)
(defvar org-cmenu-pointed-path nil)
(defvar org-cmenu-target-datum nil)
(defvar org-cmenu-open-p nil)
(defvar org-cmenu-mark-active-p nil)

(defun org-cmenu-pointed-path-string ()
  "Return the current path string for menu display."
  (mapconcat
   (lambda (d)
     (propertize
      (format "%s" (org-element-type d))
      'face
      (if (eq d org-cmenu-target-datum)
          'success
        'transient-heading)))
   (reverse org-cmenu-pointed-path)
   " > "))

(defun org-cmenu-next-element (elt lst)
  (cl-loop for p on lst
           do (when (eq (car p) elt)
                (cl-return (cadr p)))))

(defun org-cmenu-target-parent ()
  "Return the parent of the current datum."
  (org-cmenu-next-element org-cmenu-target-datum org-cmenu-pointed-path))

(defun org-cmenu-target-child ()
  "Return the child of the current datum."
  (org-cmenu-next-element org-cmenu-target-datum (reverse org-cmenu-pointed-path)))

(defun org-cmenu-select-parent ()
  (interactive)
  (if-let ((parent (org-cmenu-target-parent)))
      (org-cmenu-open-internal parent)
    (org-cmenu-open-internal org-cmenu-target-datum)))

(defun org-cmenu-select-child ()
  (interactive)
  (if-let ((child (org-cmenu-target-child)))
      (org-cmenu-open-internal child)
    (org-cmenu-open-internal org-cmenu-target-datum)))

(defun org-cmenu-define-transient-prefix-for-type (type-id)
  (let ((type (org-cmenu-get-type type-id))
        (prefix-name (intern
                      (format "org-cmenu-transient-prefix-%s" type-id))))
    (unless type
      (error "Unknown type %s" type-id))
    (eval
     `(transient-define-prefix ,prefix-name ()
        ,(format "Operations for %s" type-id)
        [:description
         org-cmenu-on-setup ;;HACK!! I want to callback when returned from subprefix!
         [("q" "Quit" transient-quit-one)]
         [("^" "Parent" org-cmenu-select-parent :if org-cmenu-target-parent)]
         [("\\" "Child" org-cmenu-select-child :if org-cmenu-target-child)]
         ]
        ;; Groups
        ,@(cl-loop for group in (org-cmenu-type-groups type)
                   collect (org-cmenu-group-to-transient-spec group))
        ;; ;; Body
        ;; (interactive)
        ;; (transient-setup ',prefix-name)
        ))))

(defun org-cmenu-update-transient-prefixes ()
  "Define transient prefixes for all elements and all objects."
  (mapc #'org-cmenu-define-transient-prefix-for-type
        (org-cmenu-type-alias-to-ids 'all)))

;; Save/Restore Mark/Point

(defun org-cmenu-save-mark-and-point ()
  (setq org-cmenu-saved-point (point))
  (setq org-cmenu-saved-mark
        ;;(save-mark-and-excursion--save)
        (cons
         (let ((mark (mark-marker)))
           (and (marker-position mark) (copy-marker mark)))
         (and mark-active (not deactivate-mark)))) ;;If deactivate-mark is t, turn off mark-active in commandn loop.
  ;;(message "Save mark(%s) and point(%s)" org-cmenu-saved-mark org-cmenu-saved-point)
  )

(defun org-cmenu-save-mark-and-point--save ()
  )


(defun org-cmenu-restore-mark-and-point ()
  ;;(message "Restore mark(%s) and point(%s)" org-cmenu-saved-mark org-cmenu-saved-point)
  (goto-char org-cmenu-saved-point)
  (save-mark-and-excursion--restore org-cmenu-saved-mark))

;; Highlight

(defun org-cmenu-highlight-datum (datum)
  (goto-char (org-element-property :end datum))
  ;; :fixed-width returns too many post-blanks
  ;; (forward-line (- (or (org-element-property :post-blank datum) 0)))
  (cl-loop repeat (or (org-element-property :post-blank datum) 0)
           while (looking-back "^[ \t]*\n[ \t]*"
                               (save-excursion (forward-line -1) (point)))
           do (forward-line -1))
  (push-mark (point) t t) ;;@todo Avoid using mark
  (goto-char (org-element-property :begin datum)))

(defun org-cmenu-unhighlight-datum ()
  (pop-mark))

;; Open/Close

(defun org-cmenu-on-setup ()
  ;;(message "on-setup org-cmenu-open-p=%s" org-cmenu-open-p)
  (unless org-cmenu-open-p
    (when (not (<= (org-element-property :begin org-cmenu-pointed-datum)
                   (point)
                   (org-element-property :end org-cmenu-pointed-datum)))
      (let ((current-datum (org-element-context)))
        (unless (eq (org-element-type current-datum)
                    (org-element-type org-cmenu-pointed-datum))
          (error "The type of element currently pointing has changed"))

        ;; If it is the same type, it can be continued.
        (org-cmenu-reset-pointed-datum current-datum)))

    (setq org-cmenu-open-p t)
    ;; Save mark and point.
    (org-cmenu-save-mark-and-point)
    ;; Hilight datum
    (setq org-cmenu-mark-active-p (and mark-active (not deactivate-mark)))
    (unless org-cmenu-mark-active-p
      (org-cmenu-highlight-datum org-cmenu-target-datum))
    ;; Add hook
    ;;(add-hook 'org-cmenu-transient-pre-exit-hook #'org-cmenu-on-pre-exit))
    (add-hook 'pre-command-hook #'org-cmenu-on-pre-command))
  ;; Return path string.
  (org-cmenu-pointed-path-string))

(defun org-cmenu-on-pre-command ()
  ;; (message "on-pre-command")
  (when org-cmenu-open-p
    (unless org-cmenu-mark-active-p
      (org-cmenu-unhighlight-datum))
    (setq org-cmenu-mark-active-p nil)
    (setq org-cmenu-open-p nil)
    (org-cmenu-restore-mark-and-point)
    ;;(remove-hook 'org-cmenu-transient-pre-exit-hook #'org-cmenu-on-pre-exit)
    (remove-hook 'pre-command-hook #'org-cmenu-on-pre-command)))

(defun org-cmenu-open-internal (datum)
  (let ((type-id (org-element-type datum)))
    ;; Update transient prefix
    (when org-cmenu-update-transient-prefix-everytime
      (org-cmenu-define-transient-prefix-for-type type-id))

    ;; Set target datum
    (setq org-cmenu-target-datum datum)

    ;; Invoke transient prefix
    (call-interactively
     (intern (format "org-cmenu-transient-prefix-%s" type-id)))))

;; Beginning of Menu

(defun org-cmenu (&optional datum)
  "Open a menu for the syntax element pointed by the current point or DATUM."
  (interactive)

  (unless datum
    (setq datum (org-element-context)))
  (unless datum
    (error "No elements or objects"))

  (org-cmenu-reset-pointed-datum datum)

  (org-cmenu-open-internal datum))

(defun org-cmenu-reset-pointed-datum (datum)
  ;; Set current point information.
  (setq org-cmenu-pointed-datum datum)
  (setq org-cmenu-pointed-path (org-element-lineage datum nil t)) ;;@todo support sections
  (setq org-cmenu-target-datum datum))

;;;; Wrap Command

(defun org-cmenu-target-datum ()
  "Return the current target datum (element or object)."
  org-cmenu-target-datum)

(defun org-cmenu-make-wrap-command-function (original func)
  ;; Avoid transient.el problem.
  ;; transient.el defines a function with description as the function name.
  ;; (transient:<prefix>:<description>)
  ;; If there are multiple commands with the exact same description,
  ;; they will collide and only the last command will be valid.
  ;; e.g.
  ;; (transient-define-prefix talk ()
  ;;   "Talk"
  ;;   ["Dog" ("d" "Talk" (lambda () (interactive) (message "bowwow")))]
  ;;   ["Cat" ("c" "Talk" (lambda () (interactive) (message "meow")))])
  ;; (talk) => d => meow
  ;; see: "(format "transient:%s:%s"" part in transient--parse-suffix.
  (let ((fname (intern (format "org-cmenu-wrap:%s"
                               (if (symbolp original) original (gensym))))))
    (fset fname func)
    fname))

(defun org-cmenu-wrap-command-with-datum (func)
  (org-cmenu-make-wrap-command-function
   func
   (lambda () (interactive) (funcall func (org-cmenu-target-datum)))))

(defun org-cmenu-wrap-command-at-begin (func)
  (org-cmenu-make-wrap-command-function
   func
   (lambda ()
     (interactive)
     (goto-char (org-element-property :begin (org-cmenu-target-datum)))
     (funcall func))))

(defun org-cmenu-wrap-command-at-post-affiliated (func)
  (org-cmenu-make-wrap-command-function
   func
   (lambda ()
     (interactive)
     (goto-char (org-element-property :post-affiliated (org-cmenu-target-datum)))
     (funcall func))))

(defun org-cmenu-no-wrap-command (func)
  func)

(defun org-cmenu-wrap-command (command wrapping-method)
  (funcall
   (pcase wrapping-method
     ('no-wrap #'org-cmenu-no-wrap-command)
     ('with-datum #'org-cmenu-wrap-command-with-datum)
     ('nil #'org-cmenu-wrap-command-with-datum)
     ('at-begin #'org-cmenu-wrap-command-at-begin)
     ('at-post-affiliated #'org-cmenu-wrap-command-at-post-affiliated)
     ((pred functionp) wrapping-method)
     (_ #'org-cmenu-no-wrap-command))
   command))

;;;; Target Types

(defun org-cmenu-resolve-target-spec (target-spec)
  (let (target-types
        target-props)

    ;; target-spec:
    ;;   type
    ;;   (type ... :keyword value ...)
    (cond
     ((null target-spec))
     ((symbolp target-spec)
      (setq target-types (list target-spec)))
     ((listp target-spec)
      (setq target-types
            (seq-take-while (lambda (e) (not (keywordp e))) target-spec))
      (setq target-props
            (seq-drop-while (lambda (e) (not (keywordp e))) target-spec))))

    ;; Resolve aliases
    (setq target-types
          (delq nil
                (seq-uniq
                 (seq-mapcat #'org-cmenu-type-alias-to-ids target-types)
                 #'eq)))

    ;; Exclude spec
    (when-let ((exclude (plist-get target-props :exclude)))
      (setq target-types (seq-difference
                          target-types
                          (if (listp exclude)
                              exclude
                            (list exclude)))))

    (cons target-types target-props)))

(defun org-cmenu-add-command (group-path command &optional
                                         target-spec wrapping-method)
  (let* ((command (copy-sequence command))
         (func (org-cmenu-command-function command))
         (func-props (and (symbolp func) (get func 'org-cmenu)))
         (target-spec (if func-props
                          (plist-get func-props :target)
                        target-spec))
         (target (org-cmenu-resolve-target-spec target-spec))
         (target-types (car target))
         (target-props (cdr target)))

    (unless target-types
      (error "No target types command %s" func))

    ;; Wrap Command
    (setf (org-cmenu-command-function command)
          (org-cmenu-wrap-command
           func
           (if func-props (or (plist-get func-props :call)
                              'with-datum)
             wrapping-method)))

    ;; Condition
    (when-let ((pred (plist-get target-props :pred)))
      (setf (org-cmenu-command-properties command)
            (nconc
             (list :if (lambda () (funcall pred org-cmenu-target-datum)))
             (org-cmenu-command-properties command))))

    (org-cmenu-add-command-to-types group-path command target-types)))

(defun org-cmenu-add-commands (group-path commands &optional
                                          target-spec wrapping-method)
  (dolist (command commands)
    (org-cmenu-add-command group-path command target-spec wrapping-method)))

(defun org-cmenu-apply-target-types (target-spec func)
  (let* ((target (org-cmenu-resolve-target-spec target-spec))
         (target-types (car target)))
    (unless target-types
      (error "No target types %s" target-spec))
    (dolist (type-id target-types)
      (funcall func type-id))))

(defun org-cmenu-remove-command (target-spec group-path key)
  (org-cmenu-apply-target-types
   target-spec
   (lambda (type-id)
     (org-cmenu-type-remove-command (org-cmenu-get-type type-id)
                                    group-path
                                    key))))

(defun org-cmenu-add-group (target-spec group-path &rest props)
  "Add all groups on GROUP-PATH to all types specified by TARGET-SPEC."
  (org-cmenu-apply-target-types
   target-spec
   (lambda (type-id)
     (let ((group (org-cmenu-type-get-group-create
                   (org-cmenu-get-type-create type-id)
                   group-path)))
       (cl-loop for (key value) on props by 'cddr
                do (org-cmenu-group-set-property group key value))))))

(defun org-cmenu-remove-group (target-spec group-path)
  (org-cmenu-apply-target-types
   target-spec
   (lambda (type-id)
     (org-cmenu-type-remove-group-by-path (org-cmenu-get-type type-id)
                                          group-path))))

(defun org-cmenu-set-group-property (target-spec group-path key value)
  (org-cmenu-apply-target-types
   target-spec
   (lambda (type-id)
     (when-let ((type (org-cmenu-get-type type-id))
                (group (org-cmenu-type-get-group type group-path)))
       (org-cmenu-group-set-property group key value)))))

(defun org-cmenu-add-string (target-spec group-path str)
  "Add the string STR to the groups specified by GROUP-PATH of
all types specified by TARGET-SPEC."
  (org-cmenu-apply-target-types
   target-spec
   (lambda (type-id)
     (let ((group (org-cmenu-type-get-group-create
                   (org-cmenu-get-type-create type-id)
                   group-path)))
       (org-cmenu-group-add-string group str)))))

(provide 'org-cmenu)
;;; org-cmenu.el ends here
