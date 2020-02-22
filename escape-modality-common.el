;; escape-modality-common.el  -*- lexical-binding: t; -*-
;; Copyright (C) 2019-2020 Martin Erik Edström

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

;; Allows cmd to be a keymap, this is intended
(defun esm-of-interest (cmd)
  (not (member cmd '(self-insert-command
                     nil
                     ignore
                     ignore-event
                     company-ignore))))

(defun esm-get-leaf (key)
    (if (string-match "-$" key)
      "-"
    (car (last (split-string (esm-normalize key) "[ -]+")))))

;; (esm-test-keydesc-handling)

;; TODO: Split this up into functions that can be reused by esm-get-leaf,
;; esm-drop-last-chord-in-seq, and esm-dub-from-key
(defun esm-normalize (keydesc)
  (cl-letf* ((segments (split-string keydesc (rx space)))
             (trim-segment (lambda (x)
                             (if (and (string-match (rx "<") x)
                                      (string-match (rx ">" eol) x))
                                 (replace-regexp-in-string (rx (any "<>")) "" x)
                               x)))
             (trimmed (mapcar trim-segment segments))
             (get-atoms (lambda (x)
                          (if (string-match (rx "-" eol) x)
                              (append (split-string x "-" t) (list "-"))
                            (split-string x "-"))))
             (atoms (mapcar get-atoms trimmed))
             (wrap-leaf-maybe (lambda (x)
                                (let* ((leaf (car (last x)))
                                       (corrected-leaf (if (string= "TAB" leaf)
                                                           leaf
                                                         (if (< 1 (length leaf))
                                                             (concat "<" leaf ">")
                                                           leaf)))
                                       (corrected (append (butlast x) (list corrected-leaf))))
                                  corrected)))
             (corrected-atoms (mapcar wrap-leaf-maybe atoms))
             (build-segments (lambda (x) (string-join x "-")))
             (corrected-segments (mapcar build-segments corrected-atoms))
             (corrected-keydesc (string-join corrected-segments " ")))
    corrected-keydesc))

;; TODO: include DEL, <return>, F13 thru F35, <iso-lefttab>, xf86 keys etc?
(defvar esm-all-keys-on-keyboard-except-shifted-symbols
  (append
   (split-string
    "`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./"
    "" t)
   (split-string
    "<left> <right> <up> <down> <SPC> <RET> <backspace> <delete>
     TAB <f1> <f2> <f3> <f4> <f5> <f6> <f7> <f8> <f9> <f10> <f11>
     <f12> <print> <insert> <next> <prior> <home> <end>"))
  "All keys, except where a held-down Shift is implied. Prefer this variable
over `esm-all-keys-on-keyboard' for most purposes if you don't like Shift.")

(defvar esm-all-shifted-symbols
  (if (>= emacs-major-version 27)
      (split-string
       "~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?"
       "" t)
    (split-string
     "~!@#$%^&*()_+{}|:\"<>?"
     "" t)))

(defvar esm-all-keys-on-keyboard
  (append
   esm-all-keys-on-keyboard-except-shifted-symbols
   esm-all-shifted-symbols))

;; unused
;; TODO: use it
(defun esm-all-keys-on-keyboard ()
  (append
   esm-all-keys-on-keyboard-except-shifted-symbols
   esm-all-shifted-symbols))

(defvar esm-hydra-keys "1234567890qwertyuiopasdfghjkl;zxcvbnm,./")

(defun esm-hydra-keys-in-a-list ()
  (split-string esm-hydra-keys "" t))

(defun esm-hydra-keys-nonum ()
    (replace-regexp-in-string (rx num) "" esm-hydra-keys))

(defvar esm-hydra-keys-nonum (esm-hydra-keys-nonum))


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
    ;; "Alt_L=F34"
    ;; "Alt_R=F34"
    "Control_L=F35"
    "Control_R=F35"
    ;; "Hyper_L=F34"
    ;; "Hyper_R=F34"
    "Meta_L=F34"
    "Meta_R=F34"
    "Super_L=F33"
    "Super_R=F33"))

(provide 'escape-modality-common)
