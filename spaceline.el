;;; spaceline.el --- Modeline configuration library for powerline

;; Copyright (C) 2015 TheBB
;;
;; Author: Eivind Fonn <evfonn@gmail.com>
;; Keywords: modeline powerline
;; Created: 21 Sep 2015
;; Version: 0.1

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

;;; Code:

(require 'dash)
(require 'cl)
(require 'gv)
(require 'powerline)

(defvar spaceline-left nil
  "List of modeline segments to render on the left side of the modeline. See
`spaceline--eval-segment' for what constitutes a segment.")

(defvar spaceline-right nil
  "List of modeline segments to render on the right side of the modeline. See
`spaceline--eval-segment' for what constitutes a segment.")

(defvar spaceline-pre-hook nil
  "Hook run before the modeline is rendered.")

(defvar spaceline--global-excludes nil
  "List of symbols that will be excluded from `global-mode-string' upon
rendering. This list is populated by `spacemacs-install' by investigating the
`:global-override' properties of all the included segments.")

(defvar spaceline-highlight-face-func 'spaceline-highlight-face-default
  "The function that decides the highlight face.")

(defface spaceline-highlight-face
  `((t (:background "DarkGoldenrod2"
        :foreground ,(face-background 'mode-line)
        :box ,(face-attribute 'mode-line :box)
        :inherit 'mode-line)))
  "Default highlight face for spaceline."
  :group 'spaceline)

(dolist (s '((normal . "DarkGoldenrod2")
             (insert . "chartreuse3")
             (emacs . "SkyBlue2")
             (replace . "chocolate")
             (visual . "gray")
             (motion . "plum3")))
  (eval `(defface ,(intern (format "spaceline-evil-%S" (car s)))
           `((t (:background ,(cdr s)
                             :foreground ,(face-background 'mode-line)
                             :box ,(face-attribute 'mode-line :box)
                             :inherit 'mode-line)))
           ,(format "Evil %S state face." (car s))
           :group 'spaceline)))

(defun spaceline-highlight-face-default ()
  "The default highlight face function. Set `spaceline-highlight-face-func' to
`spaceline-highlight-face-default' to use it."
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
  "Sets the highlight face depending on the evil state. Set
`spaceline-highlight-face-func' to `spaceline-highlight-face-evil-state' to use
it."
  (if (bound-and-true-p evil-local-mode)
      (let* ((state (if (eq 'operator evil-state) evil-previous-state evil-state))
             (face (assq state spaceline-evil-state-faces)))
        (if face (cdr face) (spaceline-highlight-face-default)))
    (spaceline-highlight-face-default)))

(defun spaceline--imagep (object)
  "Tests whether the given object is an image (a list whose first element is the
symbol `image')."
  (and (listp object)
       object
       (eq 'image (car object))))

(defun spaceline--intersperse (seq separator)
  "Returns a list with `SEPARATOR' added between each element of the list
`SEQ'."
  (cond
   ((not seq) nil)
   ((not (cdr seq)) seq)
   (t (append (list (car seq) separator)
              (spaceline--intersperse (cdr seq) separator)))))

(defun spaceline--mode-line-nonempty (seg)
  "Checks whether a modeline segment (classical Emacs style) is nonempty."
  (let ((val (format-mode-line seg)))
    (cond ((listp val) val)
          ((stringp val) (< 0 (length val)))
          (t))))

(defmacro spaceline--parse-segment-spec (spec &rest body)
  "Destructures the segment specification `SPEC' and then runs `BODY'. The
following bindings are available:

- `segment': The segment itself, either a symbol or a literal value, or a list
  of such.
- `segment-symbol': Equal to `segment' if it is a symbol, nil otherwise.
- `input-props': The property list part of `SPEC', if present.
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
  "Runs through the list of segment specs `SEGMENTS', finds any global overrides
and adds them to `spaceline--global-excludes.'"
  (when segments
    (spaceline--parse-segment-spec (car segments)
      (let* ((exclude (plist-get props :global-override))
             (excludes (if (listp exclude) exclude (list exclude))))
        (dolist (e excludes)
          (add-to-list 'spaceline--global-excludes e))))
    (spaceline--update-global-excludes-from-list (cdr segments))))

(defun spaceline--update-global-excludes ()
  "Populates the list `spacemacs--global-excludes' according to the values of
`spaceline-left' and `spaceline-right',"
  (setq spaceline--global-excludes nil)
  (spaceline--update-global-excludes-from-list spaceline-left)
  (spaceline--update-global-excludes-from-list spaceline-right))

(defun spaceline-install (left right)
  "Installs a modeline given by the lists of segment specs `LEFT' and `RIGHT'."
  (setq spaceline-left left)
  (setq spaceline-right right)
  (spaceline--update-global-excludes)
  (setq-default mode-line-format '("%e" (:eval (spaceline--prepare)))))

(defmacro spaceline-define-segment (name value &rest props)
  "Defines a modeline segment called `NAME' whose value is computed by the form
`VALUE'. The optional keyword argument `WHEN' defines a condition required for
the segment to be shown.

This macro defines a function `spaceline--segment-NAME' which returns a list of
modeline objects (strings or images). If the form `VALUE' does not result in a
list, the return value will be wrapped as a singleton list.

Also defined is a variable `spaceline--NAME-p' whose value can be used to switch
the segment on or off. Its initial value is given by the optional keyword
argument `ENABLED', which defaults to `t'.

If the segment is intended as a replacement for data which is otherwise inserted
into `global-mode-string' (typically by another package), you can use the
keyword argument `GLOBAL-OVERRIDE' to disable that.

All properties listed in `spaceline--eval-segment' are also accepted here. They
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
    (when (stringp value)
      (setq docstring value)
      (setq value (car props)))
    `(progn
       (defvar ,toggle-var ,enabled
         ,(format "True if modeline segment %S is enabled." name))
       (defun ,toggle-func () (interactive) (setq ,toggle-var (not ,toggle-var)))
       (defun ,toggle-func-on () (interactive) (setq ,toggle-var t))
       (defun ,toggle-func-off () (interactive) (setq ,toggle-var nil))
       (defun ,wrapper-func (&optional props)
         ,docstring
         (when ,condition
           (let ((separator (eval (or (plist-get props :separator) " ")))
                 (value ,value))
             (cond ((spaceline--imagep value) (list value))
                   ((listp value)
                    (spaceline--intersperse value separator))
                   ((and (stringp value)
                         (= 0 (length value)))
                    nil)
                   (t (list value))))))
       (setplist ',wrapper-func ',props))))

(defun spaceline--global ()
  "Returns `global-mode-string' with the excluded segments removed."
  (-difference global-mode-string spaceline--global-excludes))
(spaceline-define-segment global
  (powerline-raw (spaceline--global))
  :when (spaceline--mode-line-nonempty (spaceline--global)))

(defstruct sl--seg
  objects
  face-left
  face-right
  tight-left
  tight-right)

(defun spaceline--eval-segment (segment-spec &rest outer-props)
  "Evaluates a modeline segment given by `SEGMENT-SPEC' with additional
properties given by `OUTER-PROPS'.

`SEGMENT-SPEC' may be either:
- A literal value (number or string, for example)
- A symbol previously defined by `spaceline-define-segment'
- A list whose car is a segment-spec and whose cdr is a plist of properties
- A list of segment-specs

The properties applied are, in order of priority:
- Those given by `SEGMENT-SPEC', if applicable
- The properties attached to the segment symbol, if applicable
- `OUTER-PROPS'

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
`OUTER-PROPS', with the exception of `:fallback'. This means that more deeply
specified properties, as a rule, override the higher level ones. The exception
is `:when', which must be true at all levels.

The return vaule is a `segment' struct. Its `OBJECTS' list may be nil."

  ;; We get a property list from `SEGMENT-SPEC' if it's a list with more than
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
           (result (make-sl--seg
                    :objects nil
                    :face-left face
                    :face-right face
                    :tight-left tight-left
                    :tight-right tight-right)))

      ;; Evaluate the segment based on its type
      (when condition
        (cond

         ;; A list of segments
         ((listp segment)
          (let ((results (remove-if-not
                          'sl--seg-objects
                          (mapcar (lambda (s)
                                    (apply 'spaceline--eval-segment
                                           s nest-props))
                                  segment))))
            (when results
              (setf (sl--seg-objects result)
                    (apply 'append (spaceline--intersperse
                                    (mapcar 'sl--seg-objects results)
                                    (list separator))))
              (setf (sl--seg-face-left result)
                    (sl--seg-face-left (car results)))
              (setf (sl--seg-face-right result)
                    (sl--seg-face-right (car (last results))))
              (setf (sl--seg-tight-left result)
                    (sl--seg-tight-left (car results)))
              (setf (sl--seg-tight-right result)
                    (sl--seg-tight-right (car (last results)))))))

         ;; A single symbol
         ((symbolp segment)
          (setf (sl--seg-objects result)
                (mapcar (lambda (s)
                          (if (spaceline--imagep s) s (powerline-raw s face)))
                        (funcall segment-symbol props))))

         ;; A literal value
         (t (setf (sl--seg-objects result)
                  (list (powerline-raw (format "%s" segment) face))))))

      (cond
       ;; This segment produced output, so return it
       ((sl--seg-objects result) result)

       ;; Return the fallback segment, if any
       ((plist-get props :fallback)
        (apply 'spaceline--eval-segment
               (plist-get props :fallback) nest-props))

       ;; No output (objects = nil)
       (t result)))))

(defun spaceline--prepare-any (spec side)
  "Prepares one side of the modeline. `SPEC' is a list of segment specs (see
`spaceline--eval-segment'), and `SIDE' is either `l' or `r'."
  (let* ((default-face (if active 'powerline-active1 'powerline-inactive1))
         (other-face (if active 'mode-line 'mode-line-inactive))
         (highlight-face (funcall spaceline-highlight-face-func))

         ;; Loop through the segments and collect the results
         (segments (loop with result
                         for s in spec
                         do (setq result (spaceline--eval-segment s))
                         if (sl--seg-objects result)
                           collect result
                           and do (rotatef default-face other-face)))

         (dummy (make-sl--seg :face-left line-face :face-right line-face))
         (separator-style (format "powerline-%S" powerline-default-separator))
         (default-separator (intern (format "%s-%S" separator-style
                                            (car powerline-default-separator-dir))))
         (other-separator (intern (format "%s-%S" separator-style
                                          (cdr powerline-default-separator-dir)))))

    ;; Collect all segment values and add separators
    (apply 'append
           (mapcar
            (lambda (pair)
              (let* ((lhs (car pair))
                     (rhs (cdr pair))
                     (objs (if (eq 'l side) lhs rhs))
                     (add-sep (not (or (sl--seg-tight-right lhs)
                                       (sl--seg-tight-left rhs)))))
                (rotatef default-separator other-separator)
                (append
                 (when (and (eq 'r side) add-sep)
                   (list (funcall default-separator
                                  (sl--seg-face-right lhs)
                                  (sl--seg-face-left rhs))))
                 (unless (sl--seg-tight-left objs)
                   (list (powerline-raw " " (sl--seg-face-left objs))))
                 (sl--seg-objects objs)
                 (unless (sl--seg-tight-right objs)
                   (list (powerline-raw " " (sl--seg-face-right objs))))
                 (when (and (eq 'l side) add-sep)
                   (list (funcall default-separator
                                  (sl--seg-face-right lhs)
                                  (sl--seg-face-left rhs)))))))
            (-zip (if (eq 'l side) segments (cons dummy segments))
                  (if (eq 'l side) (append (cdr segments) (list dummy)) segments))))))

(defun spaceline--prepare-left ()
  "Prepares the left side of the modeline."
  (spaceline--prepare-any spaceline-left 'l))

(defun spaceline--prepare-right ()
  "Prepares the left side of the modeline."
  (spaceline--prepare-any spaceline-right 'r))

(defun spaceline--prepare ()
  "Prepares the modeline."
  (run-hooks 'spaceline-pre-hook)
  (let* ((active (powerline-selected-window-active))
         (line-face (if active 'powerline-active2 'powerline-inactive2))
         (lhs (spaceline--prepare-left))
         (rhs (spaceline--prepare-right)))
    (concat (powerline-render lhs)
            (powerline-fill line-face (powerline-width rhs))
            (powerline-render rhs))))

(provide 'spaceline)

;;; spaceline.el ends here
