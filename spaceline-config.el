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

;;; Code:

(require 'spaceline-segments)

(defun spaceline-spacemacs-theme (&rest additional-segments)
  (setq spaceline-left
        '(((workspace-number window-number)
           :fallback evil-state
           :separator "|"
           :face evil-state-face)
          anzu
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
          nyan-cat))

  (setq spaceline-right
        `((battery :when active)
          selection-info
          ((buffer-encoding-abbrev
            point-position
            line-column)
           :separator " | ")
          (global :when active)
          ,@additional-segments
          buffer-position
          hud))

  (setq-default mode-line-format '("%e" (:eval (spaceline--prepare)))))

(provide 'spaceline-config)

;;; spaceline-config.el ends here
