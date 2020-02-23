;; escape-modality-x11.el  -*- lexical-binding: t; -*-
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

;; to disable ubuntu / gnome hotkeys, use
;; https://github.com/fatso83/dotfiles/blob/master/utils/scripts/gnome-key-bindings

(require 'escape-modality-common)
(eval-when-compile (require 'subr-x)) ;; string-join

(defun esm-xmodmap-reload (&optional output-buffer)
  (interactive)
  (let* ((shell-command-dont-erase-buffer t)
         (rules (string-join esm-xmodmap-rules "' -e '"))
         (cmd (concat "xmodmap -e '" rules "'")))
    (when (executable-find "xmodmap")
      (start-process-shell-command cmd
                                   (or output-buffer (esm-debug-buffer) "*Messages*")
                                   cmd))))

(defvar esm-xcape-process)

(defun esm-xcape-reload ()
  (interactive)
  (let* ((shell-command-dont-erase-buffer t)
         (rules (string-join esm-xcape-rules ";")))
    (when (executable-find "xcape")
      (and (boundp 'esm-xcape-process)
           (process-live-p esm-xcape-process)
           (kill-process esm-xcape-process))
      (setq esm-xcape-process
            (start-process "xcape" "*xcape*" "xcape" "-d" "-e" rules)))))

(defun esm-xkbset-enable-sticky-keys ()
  (interactive)
  (when (executable-find "xkbset")
    (shell-command "xkbset sticky -twokey -latchlock; xkbset exp =sticky;")))

(provide 'escape-modality-x11)
