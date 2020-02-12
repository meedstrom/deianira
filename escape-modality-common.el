;; escape-modality-common.el
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

(require 'seq)

(defvar esm-debug nil
  "A buffer name or nil.")

(defun esm-echo (x)
  (when esm-debug
    (print x (esm-debug-buffer))))

(defun esm-debug-buffer ()
  (when esm-debug
    (let ((buf (get-buffer-create esm-debug)))
      (with-current-buffer buf
        (setq-local truncate-lines t)
        buf))))

(defun esm-get-leaf (key)
  (if (string-match "^<" key)
      key
    (car (last (split-string (esm-normalize key) "[ -]+")))))

;; Is there a library for this kind of thing?
(defun esm-normalize (key)
  "Typical example to fix: C-<M-return> should be C-M-<return>."
  (let* ((pieces (split-string key "[- ]" nil "[<>]"))
         (big-pieces (seq-remove
                      (lambda (p) (= 1 (string-bytes p))) pieces))
         (op-needed (= 1 (length big-pieces)))
         (last-piece (when op-needed
                       (concat "<" (car big-pieces) ">"))))
    (if (not op-needed)
        key
      (string-join (append (butlast pieces) (list last-piece))
                   "-"))))

(defvar esm-all-keys-on-keyboard-except-shifted-symbols
  (append
   (split-string
    "`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./"
    "" t)
   (split-string
    "<left> <right> <up> <down> <SPC> <RET> <backspace> <delete>
     TAB <f1> <f2> <f3> <f4> <f5> <f6> <f7> <f8> <f9> <f10> <f11>
     <f12> <print> <insert> <next> <prior> <home> <end>")))

(defvar esm-all-shifted-symbols
  (split-string
   "~!@#$%^&*()_+{}|:\"<>?"
   "" t))

(defvar esm-all-keys-on-keyboard
  (append
   esm-all-keys-on-keyboard-except-shifted-symbols
   esm-all-shifted-symbols))

;; unused
;; TODO: use it
(defun esm-get-all-keys-on-keyboard ()
  (append
   esm-all-keys-on-keyboard-except-shifted-symbols
   esm-all-shifted-symbols))

(defvar esm-hydra-keys "1234567890qwertyuiopasdfghjkl;zxcvbnm,./")

(defun esm-get-hydra-keys ()
  (split-string esm-hydra-keys "" t))

;; "We've not found a keyboard with more than 35 function keys total."
;; -- /usr/include/X11/keysymdef.h
;; i.e. F35 is the highest defined in upstream keysymdef.h.
(defvar esm-xmodmap-rules
  '(;; necessary for xcape to send them
    "keycode any = F35"
    "keycode any = F34"
    "keycode any = F33"
    "keycode any = F32"
    "keycode any = F31"
    "keycode any = F30"
    "keycode any = F29"
    "keycode any = F28"))

(defvar esm-xcape-rules
  '(
    "Control_L=F35"
    "Control_R=F35"
    "Meta_L=F34"
    "Meta_R=F34"
    "Alt_L=F34"
    "Alt_R=F34"
    "Super_L=F33"
    "Super_R=F33"))

(provide 'escape-modality-common)
