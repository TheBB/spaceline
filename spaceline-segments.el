;;; spaceline-segments.el --- Segments for spaceline

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

;; This file contains a variety of segments that may be of general interest in
;; many people's modelines. It contains both "stock" segments which are usable
;; without any additional packages, as well as a number of segments which depend
;; on optional third-party packages.

;; Note: The `global' segment is defined in spaceline.el, not here. It's the
;; only exception.

;;; Code:

(require 'spaceline)
(require 's)

;; Stock segments - no optional dependencies
;; =========================================

(defvar spaceline-minor-modes-separator "|")
(spaceline-define-segment minor-modes
  "A list of minor modes. Configure the separator with
`spaceline-minor-modes-separator'."
  (progn
   (mapcar
    (lambda (m)
      (propertize m
                  'mouse-face 'mode-line-highlight
                  'help-echo (concat "Minor mode\n"
                                     "mouse-1: Display minor mode menu\n"
                                     "mouse-2: Show help for minor mode\n"
                                     "mouse-3: Toggle minor mode")
                  'local-map (let ((map (make-sparse-keymap)))
                               (define-key map
                                 [mode-line down-mouse-1]
                                 (powerline-mouse 'minor 'menu m))
                               (define-key map
                                 [mode-line down-mouse-2]
                                 (powerline-mouse 'minor 'help m))
                               (define-key map
                                 [mode-line down-mouse-3]
                                 (powerline-mouse 'minor 'menu m))
                               (define-key map
                                 [header-line down-mouse-3]
                                 (powerline-mouse 'minor 'menu m))
                               map)))
   (split-string (format-mode-line minor-mode-alist))))
  :separator spaceline-minor-modes-separator)

(spaceline-define-segment buffer-modified
  "Buffer modified marker."
  "%*")

(spaceline-define-segment buffer-size
  "Size of buffer."
  (powerline-buffer-size))

(spaceline-define-segment buffer-id
  "Name of buffer."
  (powerline-buffer-id))

(spaceline-define-segment remote-host
  "Hostname for remote buffers."
  (concat "@" (file-remote-p default-directory 'host))
  :when (file-remote-p default-directory 'host))

(spaceline-define-segment major-mode
  "The name of the major mode."
  (powerline-major-mode))

(spaceline-define-segment process
  "The process associated with this buffer, if any."
  (powerline-raw mode-line-process)
  :when (spaceline--mode-line-nonempty mode-line-process))

(spaceline-define-segment version-control
  "Version control information."
  (s-trim (powerline-vc))
  :when (powerline-vc))

(spaceline-define-segment buffer-encoding
  "The full `buffer-file-coding-system'."
  (format "%s" buffer-file-coding-system))

(spaceline-define-segment buffer-encoding-abbrev
  "The line ending convention used in the buffer."
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
        (match-string 1 buf-coding)
      buf-coding)))

(spaceline-define-segment point-position
  "The value of `(point)'."
  (format "%d" (point))
  :enabled nil)

(spaceline-define-segment line
  "The current line number."
  "%l")

(spaceline-define-segment column
  "The current column number."
  "%l")

(spaceline-define-segment line-column
  "The current line and column numbers."
  "%l:%2c")

(spaceline-define-segment buffer-position
  "The current approximate buffer position, in percent."
  "%p")

(defun spaceline--column-number-at-pos (pos)
  "Column number at POS.  Analog to `line-number-at-pos'."
  (save-excursion (goto-char pos) (current-column)))

(spaceline-define-segment selection-info
  "Information about the size of the current selection, when applicable.
Supports both Emacs and Evil cursor conventions."
  (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
         (chars (- (1+ (region-end)) (region-beginning)))
         (cols (1+ (abs (- (spaceline--column-number-at-pos (region-end))
                           (spaceline--column-number-at-pos (region-beginning))))))
         (evil (and (bound-and-true-p evil-state) (eq 'visual evil-state)))
         (rect (or (bound-and-true-p rectangle-mark-mode)
                   (and evil (eq 'block evil-visual-selection))))
         (multi-line (or (> lines 1) (and evil (eq 'line evil-visual-selection)))))
    (cond
     (rect (format "%d×%d block" lines (if evil cols (1- cols))))
     (multi-line (format "%d lines" lines))
     (t (format "%d chars" (if evil chars (1- chars))))))
  :when (or mark-active
            (and (bound-and-true-p evil-local-mode)
                 (eq 'visual evil-state))))

(spaceline-define-segment hud
  "A HUD that shows which part of the buffer is currently visible."
  (powerline-hud 'highlight 'default)
  :tight t
  :when (string-match "\%" (format-mode-line "%p")))

;; Segments requiring optional dependencies
;; ========================================

(spaceline-define-segment anzu
  "Show the current match number and the total number of matches.  Requires anzu
to be enabled."
  (anzu--update-mode-line)
  :when (and active (bound-and-true-p anzu--state)))

(spaceline-define-segment erc-track
  "Show the ERC buffers with new messages.  Requires `erc-track-mode' to be
enabled."
  (mapcar (lambda (b) (buffer-name (car b)))
          erc-modified-channels-alist)
  :when (bound-and-true-p erc-track-mode))

(spaceline-define-segment battery
  "Show battery information.  Requires `fancy-battery-mode' to be enabled.

This segment overrides the modeline functionality of `fancy-battery-mode'."
  (powerline-raw (s-trim (fancy-battery-default-mode-line))
                 (fancy-battery-powerline-face))
  :when (bound-and-true-p fancy-battery-mode)
  :global-override fancy-battery-mode-line)

(defvar spaceline-org-clock-format-function
  'org-clock-get-clock-string
  "The function called by the `org-clock' segment to determine what to show.")

(spaceline-define-segment org-clock
  "Show information about the current org clock task.  Configure
`spaceline-org-clock-format-function' to configure. Requires a currently running
org clock.

This segment overrides the modeline functionality of `org-mode-line-string'."
  (substring-no-properties (funcall spacline-org-clock-format-function))
  :when (and (fboundp 'org-clocking-p)
             (org-clocking-p))
  :global-override org-mode-line-string)

(spaceline-define-segment org-pomodoro
  "Shows the current pomodoro.  Requires `org-pomodoro' to be active.

This segment overrides the modeline functionality of `org-pomodoro' itself."
  (nth 1 org-pomodoro-mode-line)
  :when (and (fboundp 'org-pomodoro-active-p)
             (org-pomodoro-active-p))
  :global-override org-pomodoro-mode-line)

(spaceline-define-segment nyan-cat
  "Shows the infamous nyan cat.  Requires `nyan-mode' to be enabled."
  (powerline-raw (nyan-create) default-face)
  :when (bound-and-true-p nyan-mode))

(defun spaceline--unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (cond
   ((string= "1" str) "➊")
   ((string= "2" str) "➋")
   ((string= "3" str) "➌")
   ((string= "4" str) "➍")
   ((string= "5" str) "➎")
   ((string= "6" str) "❻")
   ((string= "7" str) "➐")
   ((string= "8" str) "➑")
   ((string= "9" str) "➒")
   ((string= "0" str) "➓")))

(defvar spaceline-window-numbers-unicode nil
  "Set to true to enable unicode display in the `window-number' segment.")

(spaceline-define-segment window-number
  "The current window number. Requires `window-numbering-mode' to be enabled."
  (let* ((num (window-numbering-get-number))
         (str (when num (int-to-string num))))
    (if spaceline-window-numbers-unicode
        (spaceline--unicode-number str)
      str))
  :when (bound-and-true-p window-numbering-mode))

(defvar spaceline-workspace-numbers-unicode nil
  "Set to true to enable unicode display in the `workspace-number' segment.")

(spaceline-define-segment workspace-number
  "The current workspace number.  Requires `eyebrowse-mode' to be enabled."
  (let* ((num (eyebrowse--get 'current-slot))
         (str (when num (int-to-string num))))
    (if spaceline-workspace-numbers-unicode
        (spaceline--unicode-number str)
      str))
  :when (bound-and-true-p eyebrowse-mode))

(eval-after-load 'flycheck
  '(progn
     (defmacro spaceline--flycheck-face (state)
       "Generate a face for the given flycheck error type STATE."
       (let* ((fname (intern (format "spaceline-flycheck-%S-face" state)))
              (foreground (face-foreground (intern (format "flycheck-fringe-%S" state)) nil t)))
         `(progn
            (defface ,fname '((t ()))
              ,(format "Face for flycheck %S feedback in modeline." state)
              :group 'spaceline)
            (set-face-attribute ',fname nil
                                :foreground ,foreground
                                :box (face-attribute 'mode-line :box)))))

     (spaceline--flycheck-face error)
     (spaceline--flycheck-face warning)
     (spaceline--flycheck-face info)))

(defmacro spaceline--flycheck-lighter (state)
  "Return flycheck information for the given error type STATE."
  `(let* ((counts (flycheck-count-errors flycheck-current-errors))
          (errorp (flycheck-has-current-errors-p ',state))
          (err (or (cdr (assq ',state counts)) "?"))
          (running (eq 'running flycheck-last-status-change)))
     (if (or errorp running) (format "•%s" err))))

(dolist (state '(error warning info))
  (let ((segment-name (intern (format "flycheck-%S" state)))
        (face (intern (format "spaceline-flycheck-%S-face" state))))
    (eval
     `(spaceline-define-segment ,segment-name
        ,(format "Information about flycheck %Ss. Requires `flycheck-mode' to be enabled" state)
        (let ((lighter (spaceline--flycheck-lighter ,state)))
          (when lighter (powerline-raw (s-trim lighter) ',face)))
        :when (and (bound-and-true-p flycheck-mode)
                   (or flycheck-current-errors
                       (eq 'running flycheck-last-status-change)))))))

(spaceline-define-segment evil-state
  "The current evil state.  Requires `evil-mode' to be enabled."
  (s-trim (evil-state-property evil-state :tag t))
  :when (bound-and-true-p evil-local-mode))

(provide 'spaceline-segments)

;;; spaceline-segments.el ends here
