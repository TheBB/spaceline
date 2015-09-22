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

(defvar spaceline-left nil)
(defvar spaceline-right nil)
(defvar spaceline-pre-hook nil)
(defvar spaceline-evil-state-faces nil)

(defun spaceline--evil-state-face (&optional default)
  (if (bound-and-true-p evil-local-mode)
      (let* ((state (if (eq 'operator evil-state) evil-previous-state evil-state))
             (face (assq state spaceline-evil-state-faces)))
        (if face (cdr face) default))
    default))

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

(defmacro spaceline-define-segment (name value &rest props)
  "Defines a modeline segment called `NAME' whose value is computed by the form
`VALUE'. The optional keyword argument `WHEN' defines a condition required for
the segment to be shown.

This macro defines a function `spaceline--segment-NAME' which returns a list of
modeline objects (strings or images). If the form `VALUE' does not result in a
list, the return value will be wrapped as a singleton list.

All properties are stored in a plist attached to the symbol, to be inspected at
evaluation time by `spaceline--eval-segment'."
  (declare (indent 1))
  (let* ((wrapper-func (intern (format "spaceline--segment-%S" name)))
         (condition (if (plist-member props :when)
                        (plist-get props :when)
                      t)))
    `(progn
       (defun ,wrapper-func (&optional props)
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
  (let* ((input (if (and (listp segment-spec)
                         (cdr segment-spec)
                         (keywordp (cadr segment-spec)))
                    segment-spec
                  (cons segment-spec nil)))

         ;; The actual segment
         (segment (car input))
         (segment-symbol (when (symbolp segment)
                           (intern (format "spaceline--segment-%S" segment))))

         ;; Assemble the properties in the correct order
         (props (append (cdr input)
                        (when (symbolp segment) (symbol-plist segment-symbol))
                        outer-props))

         ;; Property list to be passed to nested or fallback segments
         (nest-props (append '(:fallback nil) (cdr input) outer-props))

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
     (t result))))

(defun spaceline--prepare-any (spec side)
  "Prepares one side of the modeline."
  (let* ((default-face (if active 'powerline-active1 'powerline-inactive1))
         (other-face (if active 'mode-line 'mode-line-inactive))
         (evil-state-face (if active
                              (spaceline--evil-state-face default-face)
                            default-face))

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
  (spaceline--prepare-any spaceline-left 'l))

(defun spaceline--prepare-right ()
  (spaceline--prepare-any spaceline-right 'r))

(defun spaceline--prepare ()
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
