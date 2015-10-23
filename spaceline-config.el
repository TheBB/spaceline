;;; spaceline-config.el --- Spaceline themes

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

;; This file contains ready-to-use modeline themes for powerline.

;;; Code:

(require 'spaceline-segments)

(defvar spaceline--spacemacs-left
  '((workspace-number window-number)
    :fallback evil-state
    :separator "|"
    :face highlight-face)
  "Default leftmost segment for the Spacemacs theme.")

(defvar spaceline--emacs-left
  '((((workspace-number window-number) :separator "|")
     buffer-modified
     buffer-size)
    :face highlight-face)
  "Default leftmost segment for the Emacs theme.")

(defun spaceline--theme (left additional-segments)
  "Auxiliary function for installing the two basic themes.

LEFT is the leftmost segment, and ADDITIONAL-SEGMENTS are inserted on the right,
between `global' and `buffer-position'."

  (spaceline-install
   `(,left
     anzu
     auto-compile
     (buffer-modified buffer-size buffer-id remote-host)
     major-mode
     ((flycheck-error flycheck-warning flycheck-info)
      :when active)
     (((minor-modes :separator spaceline-minor-modes-separator)
       process)
      :when active)
     (erc-track :when active)
     (version-control :when active)
     (org-pomodoro :when active)
     (org-clock :when active)
     nyan-cat)
   `((battery :when active)
     selection-info
     ((buffer-encoding-abbrev
       point-position
       line-column)
      :separator " | ")
     (global :when active)
     ,@additional-segments
     buffer-position
     hud)))

(defun spaceline-spacemacs-theme (&rest additional-segments)
  "Install the modeline used by Spacemacs.

ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
  (spaceline--theme spaceline--spacemacs-left additional-segments))

(defun spaceline-emacs-theme (&rest additional-segments)
  "Install the modeline used by Spacemacs.

ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
  (spaceline--theme spaceline--emacs-left additional-segments))

;; Header line
;; ===========

(defcustom spaceline-header-invert-separators t
  "Set to true if the header should have inverted separators."
  :group 'spaceline)

(define-minor-mode spaceline-header-mode
  "Header line."
  :init-value nil
  :global t
  (setq-default
   header-line-format
   (when spaceline-header-mode
     '("%e" (:eval (spaceline--prepare spaceline-header-left spaceline-header-right
                                (when spaceline-header-invert-separators
                                  'invert-separators)))))))

(defun spaceline--header-install (left)
  "Install a header theme. LEFT is the leftmost segment."
  (setq spaceline-header-left
        `(,left
          ((buffer-modified
            buffer-full-path
            remote-host)))
        spaceline-header-right nil)
  (spaceline-header-mode t))

(defun spaceline-spacemacs-header-theme ()
  "Install a header theme similar to the Spacemacs theme."
  (spaceline--header-install spaceline--spacemacs-left))

(defun spaceline-emacs-header-theme ()
  "Install a header theme similar to the Emacs theme."
  (spaceline--header-install spaceline--emacs-left))

;; Helm custom mode
;; ================

(defun spaceline-helm (source &optional force)
  "Set up a custom helm modeline."
  (setq spaceline--helm-current-source source
        mode-line-format '("%e" (:eval (spaceline--prepare
                                        '(helm-buffer-id
                                          helm-number
                                          helm-follow
                                          helm-prefix-argument)
                                        '(helm-help)))))
  (when force (force-mode-line-update)))

(define-minor-mode spaceline-helm-mode
  "Customize the mode-line in helm."
  :init-value nil
  :global t
  (if spaceline-helm-mode
      (advice-add 'helm-display-mode-line :after 'spaceline-helm)
    (advice-remove 'helm-display-mode-line 'spaceline-helm)))

(provide 'spaceline-config)

;;; spaceline-config.el ends here
