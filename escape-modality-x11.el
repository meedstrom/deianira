;; escape-modality-x11.el
;; Copyright (C) 2019 Martin Erik Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; to disable all ubuntu / gnome hotkeys, use
;; https://github.com/fatso83/dotfiles/blob/master/utils/scripts/gnome-key-bindings

;; TODO: Switch to not shell-command so you can output to esm-debug without
;; overwriting it.

(require 'escape-modality-common)
(eval-when-compile (require 'subr-x)) ;; string-join

(defun esm-run (x)
  (start-process-shell-command x nil x))

(defun esm-xmodmap-reload ()
  (interactive)
  (let* ((shell-command-dont-erase-buffer t)
         (rules (string-join esm-xmodmap-rules "' -e '"))
         (cmd (concat "xmodmap -e '" rules "'")))
    (when (executable-find "xmodmap")
      (shell-command cmd (or (esm-debug-buffer) "*Messages*")))))

(defun esm-xcape-reload (&optional output-buffer)
  (interactive)
  (let* ((shell-command-dont-erase-buffer t)
         (rules (string-join esm-xcape-rules ";"))
         (cmd (concat "xcape -e '" rules "'")))
    (when (executable-find "xcape")
      ;; (esm-xcape-kill) ;; seems to kill the following process!
      (start-process-shell-command "xcape"
                                   (or (esm-debug-buffer) output-buffer)
                                   cmd))))

(defun esm-xcape-kill-command ()
  (if (executable-find "pkill")
      "pkill xcape"
    (if (executable-find "killall")
        "killall -9 xcape")))

(defun esm-xcape-kill ()
  (interactive)
  (if (executable-find "pkill")
      (esm-run "pkill xcape")
    (if (executable-find "killall")
        (esm-run "killall -9 xcape")
      (warn "Not killing previous instances of xcape."))))

(defun esm-xkbset-enable-sticky-keys ()
  (interactive)
  (when (executable-find "xkbset")
    (shell-command "xkbset sticky -twokey -latchlock; xkbset exp =sticky;")))

(provide 'escape-modality-x11)
