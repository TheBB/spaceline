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

(defvar spaceline-byte-compile t
  "Whether to byte-compile the modeline.")

(defvar spaceline-mode-lines nil
  "Alist of modelines.")

(defvar spaceline-pre-hook nil
  "Hook run before the modeline is rendered.")

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
          (sym (when (symbolp segment) (intern (format "spaceline-%s-p" segment))))
          (sym-form (when (symbolp segment) (get sym :code)))
          (input-props (cdr input))
          (props (append input-props
                         (when (symbolp segment) (symbol-plist sym)))))
     ,@body))

(defun spaceline--gen-separator (face side)
  `((when needs-separator
      ,(if (eq 'l side)
           `(push (funcall default-sep separator-face ,face) result)
         `(push (funcall default-sep ,face separator-face) result))
      (cl-rotatef default-sep other-sep)
      (setq needs-separator nil))))

(defun spaceline--gen-produce (face side)
  `(,@(spaceline--gen-separator face side)
    (when prior
      (push prior result))
    (setq prior next-prior)
    (setq separator-face ,face)))

(defun spaceline--gen-segment (segment-spec side &optional outer-props deep)
  (spaceline--parse-segment-spec segment-spec
    (let* ((props (append props outer-props))
           (fallback (plist-get props :fallback))
           (nest-props (append '(:fallback nil) input-props outer-props))
           (condition (if (plist-member props :when)
                          (plist-get props :when) t))
           (face (or (plist-get props :face) 'default-face))
           (face (if (memq face '(default-face other-face highlight-face))
                     face `(quote ,face)))
           (separator `(powerline-raw ,(or (plist-get props :separator) " ") ,face))
           (tight-left (or (plist-get props :tight)
                           (plist-get props :tight-left)))
           (tight-right (or (plist-get props :tight)
                            (plist-get props :tight-right)))

           (prev-res-var (make-symbol "prev-res"))
           clean-up-code)

      (when (eq 'r side) (cl-rotatef tight-left tight-right))
      (setq clean-up-code
            `(,@(unless tight-right `((push (propertize " " 'face ,face) result)))
              (cl-rotatef default-face other-face)
              (setq needs-separator ,(not tight-right))))

      `(,@(when tight-left `((setq needs-separator nil)))
        (let ((,prev-res-var result))
          ,@(unless (or deep tight-left)
              `((setq prior (propertize " " 'face ,face))))
          (when ,condition
            ,@(cond
               ((listp segment)
                `((let ((next-prior ,separator))
                    ,@(apply 'append
                             (mapcar (lambda (s)
                                       (spaceline--gen-segment s side nest-props 'deep))
                                     (if (eq 'r side) (reverse segment) segment))))
                  (setq prior next-prior)))
               ((symbolp segment)
                `((-when-let (value ,sym-form)
                    ,@(spaceline--gen-produce face side)
                    (cond
                     ((spaceline--imagep value) (push value result))
                     ((listp value)
                      (dolist (r ,(if (eq 'l side)
                                      `(spaceline--intersperse ,separator value)
                                    `(reverse (spaceline--intersperse ,separator value))))
                        (push (if (spaceline--imagep r) r (powerline-raw r ,face)) result)))
                     ((and (stringp value) (= 0 (length value))))
                     (t (push (powerline-raw value ,face) result))))))
               (t
                `(,@(spaceline--gen-produce face side)
                  (push (powerline-raw (format "%s" ,segment) ,face) result)))))
          ,@(cond
             ((and fallback deep)
              `((unless (eq ,prev-res-var result)
                  ,@(spaceline--gen-segment fallback side nest-props deep))))
             ((and fallback (not deep))
              `((if (eq ,prev-res-var result)
                    (progn ,@(spaceline--gen-segment fallback side nest-props deep))
                  ,@clean-up-code)))
             ((and (not fallback) (not deep))
              `((unless (eq ,prev-res-var result)
                  ,@clean-up-code)))))))))

(defun spaceline-install (&rest args)
  (interactive)
  (if (and (eq 1 (length args)) (eq 'all (car args)))
      (dolist (target spaceline-mode-lines)
        (spaceline-install (car target)))
    (let* ((nargs (length args))
           (target (if (cl-oddp nargs) (pop args) 'main))
           (left-segs (if (> nargs 1) (pop args)
                        (cadr (assq target spaceline-mode-lines))))
           (right-segs (if (> nargs 1) (pop args)
                         (cddr (assq target spaceline-mode-lines))))
           (target-func (intern (format "spaceline-ml-%s" target)))
           (global-excludes (append (spaceline--global-excludes left-segs)
                                    (spaceline--global-excludes right-segs)))
           (sep-style (format "powerline-%s" powerline-default-separator))

           (sep-dirs (spaceline--get-separator-dirs 'l))
           (left-code
            `(let ((default-face face1) (other-face face2)
                   (default-sep ',(intern (format "%s-%s" sep-style (car sep-dirs))))
                   (other-sep ',(intern (format "%s-%s" sep-style (cdr sep-dirs))))
                   (global-excludes ',global-excludes)
                   prior next-prior produced needs-separator separator-face result)
               ,@(apply 'append (mapcar (lambda (s) (spaceline--gen-segment s 'l)) left-segs))
               ,@(spaceline--gen-separator 'line-face 'l)
               (reverse result)))

           (sep-dirs (spaceline--get-separator-dirs 'r))
           (right-code
            `(let ((default-face face1) (other-face face2)
                   (default-sep ',(intern (format "%s-%s" sep-style (car sep-dirs))))
                   (other-sep ',(intern (format "%s-%s" sep-style (cdr sep-dirs))))
                   (global-excludes ',global-excludes)
                   prior next-prior needs-separator separator-face result)
               ,@(apply 'append (mapcar (lambda (s) (spaceline--gen-segment s 'r)) (reverse right-segs)))
               ,@(spaceline--gen-separator 'line-face 'r)
               result)))

      (unless (assq target spaceline-mode-lines)
        (push `(,target) spaceline-mode-lines))
      (setcdr (assq target spaceline-mode-lines) `(,left-segs . ,right-segs))

      (eval `(defun ,target-func ()
               (run-hooks 'spaceline-pre-hook)
               (let* ((active (powerline-selected-window-active))
                      (line-face (spaceline--get-face 'line active))
                      (highlight-face (spaceline--get-face 'highlight active))
                      (face1 (spaceline--get-face 'face1 active))
                      (face2 (spaceline--get-face 'face2 active))
                      (lhs ,left-code)
                      (rhs ,right-code))
                 (concat (powerline-render lhs)
                         (powerline-fill line-face (powerline-width rhs))
                         (powerline-render rhs)))))
      (when spaceline-byte-compile
        (let ((byte-compile-warnings nil))
          (byte-compile target-func))))))

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
         (value `(when ,toggle-var ,value))
         (enabled (if (plist-member props :enabled)
                      (plist-get props :enabled)
                    t))
         (global-override (plist-get props :global-override))
         (global-override (if (listp global-override)
                              global-override
                            (list global-override))))
    `(progn
       (defvar ,toggle-var ,enabled
         ,(format "True if modeline segment %S is enabled." name))
       (defun ,toggle-func () (interactive) (setq ,toggle-var (not ,toggle-var)))
       (defun ,toggle-func-on () (interactive) (setq ,toggle-var t))
       (defun ,toggle-func-off () (interactive) (setq ,toggle-var nil))
       (let ((doc (get ',toggle-var 'variable-documentation)))
         (setplist ',toggle-var ',props)
         (put ',toggle-var 'variable-documentation doc))
       (put ',toggle-var :code ',value)
       (put ',toggle-var :global-override ',global-override))))

(defun spaceline--global-excludes (segments)
  "Compute global overrides from the segment list SEGMENTS."
  (let (excludes)
    (dolist (s-spec segments)
      (spaceline--parse-segment-spec s-spec
        (setq excludes (append (plist-get props :global-override) excludes))
        (when (listp segment)
          (setq excludes (append (spaceline--global-excludes segment) excludes)))))
    excludes))

(spaceline-define-segment global
  (let* ((global-excludes (bound-and-true-p global-excludes))
         (global (if (listp global-mode-string)
                     (-difference global-mode-string global-excludes)
                   global-mode-string)))
    (when (spaceline--mode-line-nonempty global)
      (powerline-raw global))))

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

(provide 'spaceline)

;;; spaceline.el ends here
