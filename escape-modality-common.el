;; escape-modality-common.el
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

(defvar esm-all-keys-on-keyboard
  (append
   (split-string
    "`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./~!@#$%^&*()_+{}|:\"<>?"
    "" t)
   (split-string
    "<left> <right> <up> <down> <SPC> <RET> <backspace> <delete>
     TAB <f1> <f2> <f3> <f4> <f5> <f6> <f7> <f8> <f9> <f10> <f11>
     <f12> <print> <insert> <next> <prior> <home> <end>"
    "\s\\|\n" t))
  "If you want to customize this to your local layout, try
copying the source.")

(defvar esm-all-keys-on-keyboard-without-shift
  (seq-difference esm-all-keys-on-keyboard
                  (split-string "~!@#$%^&*()_+{}|:\"<>?" "" t)))

(defvar esm-hydra-keys "1234567890qwertyuiopasdfghjkl;zxcvbnm,./")

(defun esm-get-hydra-keys ()
  (split-string esm-hydra-keys "" t))

(provide 'escape-modality-common)
