;;; spaceline.el --- Modeline configuration library for powerline

;; Copyright (C) 2015 TheBB
;;
;; Author: Eivind Fonn <evfonn@gmail.com>
;; URL: https://github.com/TheBB/spaceline
;; Version: 1.1.2
;; Keywords: mode-line powerline spacemacs
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5") (powerline "2.3") (dash "2.11.0") (s "1.10.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Spaceline is a modular mode-line library built on the powerline package,
;; designed to make it easy to build beautiful mode-lines.
;;
;; It was originally used in the Spacemacs distribution, but has since been
;; extracted as a stand-alone package.

;;; Code:

(require 'dash)
(require 'gv)
(require 'powerline)
(require 'cl-lib)

(defvar evil-previous-state)
(defvar evil-state)
(defvar evil-visual-selection)

(defvar spaceline-left nil
  "A list of modeline segments to render on the left side of the modeline.

See `spaceline--eval-segment' for what constitutes a segment.")

(defvar spaceline-right nil
  "List of modeline segments to render on the right side of the modeline.

See `spaceline--eval-segment' for what constitutes a segment.")

(defvar spaceline-pre-hook nil
  "Hook run before the modeline is rendered.")

(defvar spaceline--global-excludes nil
  "List of symbols that will be excluded from `global-mode-string'.

This list is populated by `spacemacs-install' by investigating the
`:global-override' properties of all the included segments.")

(defvar spaceline-separator-dir-left nil
  "The separator directions to use for the left side.
Cons of the form (DIR . DIR) where DIR is one of left and right, or nil, in
which case the best separators are chosen depending on the separator style.")

(defvar spaceline-separator-dir-right nil
  "The separator directions to use for the right side.
Cons of the form (DIR . DIR) where DIR is one of left and right, or nil, in
which case the best separators are chosen depending on the separator style.")

(defvar spaceline-directed-separators '(arrow arrow-fade brace butt curve roundstub utf-8)
  "List of separators for which spaceline will choose different separator
directions on the left and right side, if not explicitly set in
`spaceline-separator-dir-left' or `spaceline-separator-dir-right'.")

(defvar spaceline-highlight-face-func 'spaceline-highlight-face-default
  "The function that decides the highlight face.
Superseded by `spaceline-face-func' if that variable is set.")

(defvar spaceline-face-func nil
  "The function that defines all faces.

Must be a function that accepts two arguments: FACE and ACTIVE, where
FACE is `face1', `face2' `line' or `highlight', and ACTIVE determines whether
the window in question is active. It should return a face to use.

This variable supersedes `spaceline-highlight-face-func' if set.")

(defun spaceline--get-face (face active)
  "Universal function to get the right face.
FACE and ACTIVE have the same meanings as in `spaceline-face-func'. Delegates
the work to `spaceline-face-func' if it is given, otherwise falls back to
default configuration."
  (if spaceline-face-func
      (funcall spaceline-face-func face active)
    (cond
     ((eq 'face1 face) (if active 'powerline-active1 'powerline-inactive1))
     ((eq 'face2 face) (if active 'mode-line 'mode-line-inactive))
     ((eq 'line face) (if active 'powerline-active2 'powerline-inactive2))
     ((eq 'highlight face) (if active
                               (funcall spaceline-highlight-face-func)
                             'powerline-inactive1)))))

(defface spaceline-highlight-face
  `((t (:background "DarkGoldenrod2"
        :foreground "#3E3D31"
        :inherit 'mode-line)))
  "Default highlight face for spaceline."
  :group 'spaceline)

;; Define various other highlight faces
(dolist (s '((spaceline-evil-normal "DarkGoldenrod2" "Evil normal state face.")
             (spaceline-evil-insert "chartreuse3" "Evil insert state face.")
             (spaceline-evil-emacs "SkyBlue2" "Evil emacs state face.")
             (spaceline-evil-replace "chocolate" "Evil replace state face.")
             (spaceline-evil-visual "gray" "Evil visual state face.")
             (spaceline-evil-motion "plum3" "Evil motion state face.")
             (spaceline-unmodified "DarkGoldenrod2" "Unmodified buffer face.")
             (spaceline-modified "SkyBlue2" "Modified buffer face.")
             (spaceline-read-only "plum3" "Read-only buffer face.")))
  (eval `(defface ,(nth 0 s)
           `((t (:background ,(nth 1 s)
                 :foreground "#3E3D31"
                 :inherit 'mode-line)))
           ,(nth 2 s)
           :group 'spaceline)))

(defun spaceline-highlight-face-default ()
  "The default highlight face function.

Set `spaceline-highlight-face-func' to `spaceline-highlight-face-default' to use
this."
  'spaceline-highlight-face)

(defvar spaceline-evil-state-faces
  '((normal . spaceline-evil-normal)
    (insert . spaceline-evil-insert)
    (emacs . spaceline-evil-emacs)
    (replace . spaceline-evil-replace)
    (visual . spaceline-evil-visual)
    (motion . spaceline-evil-motion))
  "Association list mapping evil states to their corresponding highlight faces.
Is used by `spaceline-highlight-face-evil-state'.")

(defun spaceline-highlight-face-evil-state ()
  "Set the highlight face depending on the evil state.

Set `spaceline-highlight-face-func' to `spaceline-highlight-face-evil-state' to
use this."
  (if (bound-and-true-p evil-local-mode)
      (let* ((state (if (eq 'operator evil-state) evil-previous-state evil-state))
             (face (assq state spaceline-evil-state-faces)))
        (if face (cdr face) (spaceline-highlight-face-default)))
    (spaceline-highlight-face-default)))

(defun spaceline-highlight-face-modified ()
  "Set the highlight face depending on the buffer modified status.

Set `spaceline-highlight-face-func' to `spaceline-highlight-face-modified' to
use this."
  (cond
   (buffer-read-only 'spaceline-read-only)
   ((buffer-modified-p) 'spaceline-modified)
   (t 'spaceline-unmodified)))

(defun spaceline--imagep (object)
  "Test whether the given OBJECT is an image.

An image is a list whose first element is the symbol `image'."
  (and (listp object)
       object
       (eq 'image (car object))))

(defun spaceline--intersperse (separator seq)
  "Intersperses SEPARATOR between each element of SEQ."
  (cond
   ((not seq) nil)
   ((not (cdr seq)) seq)
   (t (append (list (car seq) separator)
              (spaceline--intersperse separator (cdr seq))))))

(defun spaceline--mode-line-nonempty (seg)
  "Check whether a modeline segment SEG (classical Emacs style) is nonempty."
  (let ((val (format-mode-line seg)))
    (cond ((listp val) val)
          ((stringp val) (< 0 (length val)))
          (t))))

(defmacro spaceline--parse-segment-spec (spec &rest body)
  "Destructure the segment specification SPEC and then run BODY.

The following bindings are available in BODY:

- `segment': The segment itself, either a symbol or a literal value, or a list
  of such.
- `segment-symbol': The function that evaluates `segment', if it is a symbol.
- `input-props': The property list part of SPEC, if present.
- `props': The full property list (including those bound to `segment-symbol', if
  applicable)."
  (declare (indent 1))
  `(let* ((input (if (and (listp ,spec)
                          (cdr ,spec)
                          (keywordp (cadr ,spec)))
                     ,spec
                   (cons ,spec nil)))
          (segment (car input))
          (segment-symbol (when (symbolp segment)
                            (intern (format "spaceline--segment-%S" segment))))
          (input-props (cdr input))
          (props (append input-props
                         (when (symbolp segment)
                           (symbol-plist segment-symbol)))))
     ,@body))

(defun spaceline--update-global-excludes-from-list (segments)
  "Add global overrides from the segment list SEGMENTS."
  (when segments
    (spaceline--parse-segment-spec (car segments)
      (let* ((exclude (plist-get props :global-override))
             (excludes (if (listp exclude) exclude (list exclude))))
        (dolist (e excludes)
          (add-to-list 'spaceline--global-excludes e))))
    (spaceline--update-global-excludes-from-list (cdr segments))))

(defun spaceline--update-global-excludes ()
  "Populate the list `spacemacs--global-excludes'.

Depends on the values of `spaceline-left' and `spaceline-right',"
  (setq spaceline--global-excludes nil)
  (spaceline--update-global-excludes-from-list spaceline-left)
  (spaceline--update-global-excludes-from-list spaceline-right))

(defun spaceline-install (left right)
  "Install a modeline given by the lists of segment specs LEFT and RIGHT."
  (setq spaceline-left left)
  (setq spaceline-right right)
  (spaceline--update-global-excludes)
  (setq-default mode-line-format '("%e" (:eval (spaceline--prepare spaceline-left spaceline-right)))))

(defmacro spaceline-define-segment (name value &rest props)
  "Define a modeline segment called NAME with value VALUE and properties PROPS.

Its value is computed by the form VALUE.  The optional keyword argument `:when'
defines a condition required for the segment to be shown.

This macro defines a function `spaceline--segment-NAME' which returns a list of
modeline objects (strings or images).  If the form VALUE does not result in a
list, the return value will be wrapped as a singleton list.

Also defined is a variable `spaceline--NAME-p' whose value can be used to switch
the segment on or off.  Its initial value is given by the optional keyword
argument `:enabled', which defaults to true.

If the segment is intended as a replacement for data which is otherwise inserted
into `global-mode-string' (typically by another package), you can use the
keyword argument `GLOBAL-OVERRIDE' to disable that.

All properties listed in `spaceline--eval-segment' are also accepted here.  They
are stored in a plist attached to the symbol, to be inspected at evaluation time
by `spaceline--eval-segment'."
  (declare (indent 1)
           (doc-string 2))
  (let* ((wrapper-func (intern (format "spaceline--segment-%S" name)))
         (toggle-var (intern (format "spaceline-%S-p" name)))
         (toggle-func (intern (format "spaceline-toggle-%S" name)))
         (toggle-func-on (intern (format "spaceline-toggle-%S-on" name)))
         (toggle-func-off (intern (format "spaceline-toggle-%S-off" name)))
         (docstring (when (stringp value)
                      (prog1 value
                        (setq value (car props)
                              props (cdr props)))))
         (docstring (concat "A modeline segment generated by `spaceline-define-segment'.\n\n"
                            docstring))
         (enabled (if (plist-member props :enabled)
                      (plist-get props :enabled)
                    t))
         (condition `(and ,toggle-var
                          ,(if (plist-member props :when)
                               (plist-get props :when)
                             t))))
    `(progn
       (defvar ,toggle-var ,enabled
         ,(format "True if modeline segment %S is enabled." name))
       (defun ,toggle-func () (interactive) (setq ,toggle-var (not ,toggle-var)))
       (defun ,toggle-func-on () (interactive) (setq ,toggle-var t))
       (defun ,toggle-func-off () (interactive) (setq ,toggle-var nil))
       (defun ,wrapper-func (&optional props active default-face other-face
                                       highlight-face line-face)
         ,docstring
         (when ,condition
           (let ((separator (eval (or (plist-get props :separator) " ")))
                 (value ,value))
             (cond ((spaceline--imagep value) (list value))
                   ((listp value)
                    (spaceline--intersperse separator value))
                   ((and (stringp value)
                         (= 0 (length value)))
                    nil)
                   (t (list value))))))
       (setplist ',wrapper-func ',props))))

(defun spaceline--global ()
  "Return `global-mode-string' with the excluded segments removed."
  (cond
   ((listp global-mode-string)
    (-difference global-mode-string spaceline--global-excludes))
   (t global-mode-string)))
(spaceline-define-segment global
  (powerline-raw (spaceline--global))
  :when (spaceline--mode-line-nonempty (spaceline--global)))

(cl-defstruct spaceline--seg
  objects
  face-left
  face-right
  tight-left
  tight-right
  skip-alternate)

(defun spaceline--eval-segment (segment-spec &optional outer-props
                                             active default-face other-face
                                             highlight-face line-face)
  "Evaluate SEGMENT-SPEC with additional properties OUTER-PROPS.

SEGMENT-SPEC may be either:
- A literal value (number or string, for example)
- A symbol previously defined by `spaceline-define-segment'
- A list whose car is a segment-spec and whose cdr is a plist of properties
- A list of segment-specs

The properties applied are, in order of priority:
- Those given by SEGMENT-SPEC, if applicable
- The properties attached to the segment symbol, if applicable
- OUTER-PROPS

Valid properties are:
- `:tight-left' => if true, the segment should be rendered with no padding or
  separator on its left side
- `:tight-right' => corresponding option for the right side
- `:tight' => shorthand option to set both `:tight-left' and `:tight-right'
- `:when' => condition that determines whether this segment is shown
- `:fallback' => segment to evaluate if this segment produces no output
- `:separator' => string with which to separate nested segments
- `:face' => the face with which to render the segment

When calling nested or fallback segments, the full property list is passed as
`OUTER-PROPS', with the exception of `:fallback'.  This means that more deeply
specified properties, as a rule, override the higher level ones.  The exception
is `:when', which must be true at all levels.

The return value is a `segment' struct.  Its OBJECTS list may be nil."

  ;; We get a property list from SEGMENT-SPEC if it's a list with more than
  ;; one element whose second element is a keyword symbol
  (spaceline--parse-segment-spec segment-spec
    (let* (;; Assemble the properties in the correct order
           (props (append props outer-props))

           ;; Property list to be passed to nested or fallback segments
           (nest-props (append '(:fallback nil) input-props outer-props))

           ;; Parse property list
           (condition (if (plist-member props :when)
                          (eval (plist-get props :when))
                        t))
           (face (let ((face-spec (or (plist-get props :face) 'default-face)))
                   (if (facep face-spec) face-spec (eval face-spec))))
           (separator (powerline-raw (eval (or (plist-get props :separator) " ")) face))
           (tight-left (or (plist-get props :tight)
                           (plist-get props :tight-left)))
           (tight-right (or (plist-get props :tight)
                            (plist-get props :tight-right)))

           ;; Final output
           (result (make-spaceline--seg
                    :objects nil
                    :face-left face
                    :face-right face
                    :tight-left tight-left
                    :tight-right tight-right
                    :skip-alternate (plist-get props :skip-alternate))))

      ;; Evaluate the segment based on its type
      (when condition
        (cond

         ;; A list of segments
         ((listp segment)
          (let ((results (cl-remove-if-not
                          'spaceline--seg-objects
                          (mapcar (lambda (s)
                                    (spaceline--eval-segment
                                     s nest-props active
                                     default-face other-face
                                     highlight-face line-face))
                                  segment))))
            (when results
              (setf (spaceline--seg-objects result)
                    (apply 'append (spaceline--intersperse
                                    (list separator)
                                    (mapcar 'spaceline--seg-objects results))))
              (setf (spaceline--seg-face-left result)
                    (spaceline--seg-face-left (car results)))
              (setf (spaceline--seg-face-right result)
                    (spaceline--seg-face-right (car (last results))))
              (setf (spaceline--seg-tight-left result)
                    (spaceline--seg-tight-left (car results)))
              (setf (spaceline--seg-tight-right result)
                    (spaceline--seg-tight-right (car (last results)))))))

         ;; A single symbol
         ((symbolp segment)
          (when (fboundp segment-symbol)
            (setf (spaceline--seg-objects result)
                  (mapcar (lambda (s)
                            (if (spaceline--imagep s) s (powerline-raw s face)))
                          (funcall segment-symbol
                                   props active default-face other-face
                                   highlight-face line-face)))))

         ;; A literal value
         (t (setf (spaceline--seg-objects result)
                  (list (powerline-raw (format "%s" segment) face))))))

      (cond
       ;; This segment produced output, so return it
       ((spaceline--seg-objects result) result)

       ;; Return the fallback segment, if any
       ((plist-get props :fallback)
        (spaceline--eval-segment (plist-get props :fallback)
                                 nest-props active default-face other-face
                                 highlight-face line-face))

       ;; No output (objects = nil)
       (t result)))))

(defun spaceline--get-separator-dirs (side)
  "Gets the preconfigured separator directions for SIDE, or the \"best\" ones,
if not specified."
  (or (if (eq 'l side)
          spaceline-separator-dir-left
        spaceline-separator-dir-right)
      (cond
       ((memq powerline-default-separator spaceline-directed-separators)
        (if (eq 'l side) '(left . left) '(right . right)))
       (t '(left . right)))))

(defun spaceline--prepare-any (spec side active line-face)
  "Prepare one side of the modeline.

SPEC is a list of segment specs (see `spaceline--eval-segment'), and SIDE is
either of the symbols l or r.

ACTIVE is true if the current window is active.  LINE-FACE is the face used to
render the empty space in the middle of the mode-line."
  (let* ((default-face (spaceline--get-face 'face1 active))
         (other-face (spaceline--get-face 'face2 active))
         (highlight-face (spaceline--get-face 'highlight active))

         ;; Loop through the segments and collect the results
         (segments (cl-loop with result
                            for s in spec
                            do (setq result (spaceline--eval-segment
                                             s nil active
                                             default-face other-face
                                             highlight-face line-face))
                            if (spaceline--seg-objects result)
                            collect result
                            and do (unless (spaceline--seg-skip-alternate result)
                                     (cl-rotatef default-face other-face))))

         (dummy (make-spaceline--seg :face-left line-face :face-right line-face))
         (separator-style (format "powerline-%S" powerline-default-separator))
         (dirs (spaceline--get-separator-dirs side))
         (default-separator (intern (format "%s-%S" separator-style (car dirs))))
         (other-separator (intern (format "%s-%S" separator-style (cdr dirs)))))

    ;; Collect all segment values and add separators
    (apply 'append
           (mapcar
            (lambda (pair)
              (let* ((lhs (car pair))
                     (rhs (cdr pair))
                     (objs (if (eq 'l side) lhs rhs))
                     (add-sep (not (or (spaceline--seg-tight-right lhs)
                                       (spaceline--seg-tight-left rhs)))))
                (cl-rotatef default-separator other-separator)
                (append
                 (when (and (eq 'r side) add-sep)
                   (list (funcall default-separator
                                  (spaceline--seg-face-right lhs)
                                  (spaceline--seg-face-left rhs))))
                 (unless (spaceline--seg-tight-left objs)
                   (list (powerline-raw " " (spaceline--seg-face-left objs))))
                 (spaceline--seg-objects objs)
                 (unless (spaceline--seg-tight-right objs)
                   (list (powerline-raw " " (spaceline--seg-face-right objs))))
                 (when (and (eq 'l side) add-sep)
                   (list (funcall default-separator
                                  (spaceline--seg-face-right lhs)
                                  (spaceline--seg-face-left rhs)))))))
            (-zip (if (eq 'l side) segments (cons dummy segments))
                  (if (eq 'l side) (append (cdr segments) (list dummy)) segments))))))

(defun spaceline--prepare (left right)
  "Prepare the modeline."
  (run-hooks 'spaceline-pre-hook)
  (let* ((active (powerline-selected-window-active))
         (line-face (spaceline--get-face 'line active))
         (lhs (spaceline--prepare-any left 'l active line-face))
         (rhs (spaceline--prepare-any right 'r active line-face)))
    (concat (powerline-render lhs)
            (powerline-fill line-face (powerline-width rhs))
            (powerline-render rhs))))

(provide 'spaceline)

;;; spaceline.el ends here
