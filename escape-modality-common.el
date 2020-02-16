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

;; Allows cmd to be a keymap, this is intended
(defun esm-of-interest (cmd)
  (member cmd '(self-insert-command
                nil
                ignore
                ignore-event
                company-ignore)))

(defun esm-get-leaf (key)
    (if (string-match "-$" key)
      "-"
    (car (last (split-string (esm-normalize key) "[ -]+")))))

(defun esm-test-keydesc-handling ()
  (let ((errors 0)
        (problematic-key-descriptions
         '(;; unaltered    normalized       squashed           leaf
           ("C-x 8 RET"    "C-x 8 <RET>"    "esm-Cx8<RET>"     "<RET>")
           ("<f2> 8 RET"   "<f2> 8 <RET>"   "esm-<f2>8<RET>"   "<RET>")
           ("<f2> f r"     "<f2> f r"       "esm-<f2>fr"       "r")
           ("<f2> <f2>"    "<f2> <f2>"      "esm-<f2><f2>"     "<f2>")
           ("ESC <C-down>" "<ESC> C-<down>" "esm-<ESC>C<down>" "<down>")
           ("C-x RET C-\\" "C-x <RET> C-\\" "esm-Cx<RET>C\\"   "\\")
           ;; (kbd "TAB")  (kbd "<TAB>") are different
           ;; ("<TAB>"        "<TAB>"          "esm-<TAB>"        "<TAB>")
           ("TAB"          "TAB"            "esm-TAB"          "TAB")
           ("A-T A-B"      "A-T A-B"        "esm-ATAB"         "B")
           ("C-<M-return>" "C-M-<return>"   "esm-CM<return>"   "<return>")
           ("<C-M-return>" "C-M-<return>"   "esm-CM<return>"   "<return>")
           )))
    (dolist (x problematic-key-descriptions)
      (seq-let (raw normalized squashed leaf) x
        (unless (and (string= normalized (esm-normalize raw))
                     (string= squashed (esm-dub-from-key normalized))
                     (string= leaf (esm-get-leaf normalized)))
          (error (concat "Keydesc handling failed for test case: " raw)))))))

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
