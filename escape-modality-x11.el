;; escape-modality-x11.el
;; Copyright (C) 2019 Martin Edstrom

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

(eval-when-compile (require 'subr-x)) ;; string-join

;; Idea: Make C-m send Return and C-h send Backspace, via xmodmap? Cleaner?
;; Or use key-translation-map for C-h, and use the same method to make
;; Backspace send something else so you don't waste a key.
(defvar esm-xmodmap-rules
  '(;; necessary for xcape to send them
    "keycode any = F13"
    "keycode any = F14"
    "keycode any = F15"
    "keycode any = F16"
    "keycode any = F17"
    "keycode any = F23"
    "keycode any = F24"
    "keycode any = F25"))

(defvar esm-xcape-rules
  '("Control_L=F13"
    "Control_R=F13"
    "Meta_L=F14"
    "Meta_R=F14"
    "Alt_L=F14"
    "Alt_R=F14"
    "Super_L=F15"
    "Super_R=F15"))

(defun esm-xmodmap-reload ()
  (interactive)
  (when (executable-find "xmodmap")
    (message "Loading esm-xmodmap-rules.")
    (shell-command
     (concat "xmodmap -e '" (string-join esm-xmodmap-rules "' -e '") "'"))))

(defun esm-xcape-reload ()
  (interactive)
  (when (executable-find "xcape")
    (if (executable-find "pkill")
        (shell-command "pkill xcape")
      (if (executable-find "killall")
          (shell-command "killall -9 xcape")
        (message "Program pkill not found, so not killing previous instances of xcape.")))
    (shell-command
     (concat "xcape -e '" (string-join esm-xcape-rules ";") "'"))))

(defun esm-xkbset-enable-sticky-keys ()
  (interactive)
  (when (executable-find "xkbset")
    (shell-command "xkbset sticky -twokey -latchlock; xkbset exp =sticky;")))

(provide 'escape-modality-x11)
