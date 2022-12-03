;;; deianira-lib.el -- Library for deianira -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Martin Edström

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

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'dash)
(require 'cl-lib)

(defconst dei--ignore-keys-regexp
  (regexp-opt
   '(;; Don't bother handling control characters well, it's a
     ;; nightmare the edge cases they introduce, especially ESC
     ;; because it's also Meta!  See `dei--nightmare-p'.
     ;;
     ;; Suppose someone wants C-x C-m cloned to C-x m, what to
     ;; do? Or suppose C-x i is bound to something useful but we clone
     ;; from C-x C-i (aka C-x TAB) and destroy it? And going the
     ;; other way instead destroys C-x TAB instead. Kill me
     ;;
     ;; By putting them in this regexp, we've doomed any sequence
     ;; ending with them, C-x C-i will be overwritten by C-x i.  Put
     ;; a blurb in the readme about it.
     ;;
     ;; Actually, even with them in the regexp, C-x \[ will STILL be
     ;; cloned to C-x C-\[...  maybe we need a check in
     ;; homogenize-key. Now done.
     "ESC" "C-["
     "RET" "C-m"
     "TAB" "C-i"
     ;; declutter a bit (unlikely someone wants)
     "help" "key-chord" "kp-" "iso-" "wheel" "menu" "redo" "undo"
     "again" "XF86"
     ;; extremely unlikely someone wants
     "compose" "Scroll_Lock" "drag-n-drop"
     "mouse" "remap" "scroll-bar" "select" "switch" "state"
     "which-key" "corner" "divider" "edge" "header" "mode-line"
     "vertical-line" "frame" "open" "chord" "tool-bar" "fringe"
     "touch" "margin" "pinch" "tab-bar" ))
  "Regexp for some key bindings that don't interest us.")

(defun dei--raw-keymap (map)
  "If MAP is a keymap, return it; if a symbol, evaluate first."
  (if (keymapp map)
      map
    (if (symbolp map)
        (let ((evaluated (symbol-value map)))
          (if (keymapp evaluated)
              evaluated
            (error "Doesn't evaluate to a keymap: %s" map)))
      (error "Not a keymap or keymap name: %s" map))))

;; REVIEW: Test <ctl> x 8 with Deianira active.
(defvar dei--unnest-avoid-prefixes
  (cons "C-x 8"
        (mapcar #'key-description
                (where-is-internal #'iso-transl-ctl-x-8-map)))
  "List of prefixes to avoid looking up.")


(defun dei--unnest-keymap (map &optional with-commands extra-filters)
  "Return MAP as a flat list of key seqs instead of a tree.
These key seqs are strings satisfying `key-valid-p'.

Optional argument WITH-COMMANDS makes the return value an alist
where the cars are key seqs and the cdrs are the associated
commands.

Optional argument EXTRA-FILTERS is a list of functions with which
to test the key seq, such as `dei--key-has-more-than-one-chord'.
If any function returns non-nil, exclude this key seq.  Aside
from functions, nil is also a permitted member, and ignored."
  (cl-loop
   ;; REVIEW: do we need to set case-fold-search? test with and without it.
   with case-fold-search = nil
   for x being the key-seqs of (dei--raw-keymap map) using (key-bindings cmd)
   as key = (key-description x)
   unless (or (member cmd '(nil
                            self-insert-command
                            ignore
                            ignore-event
                            company-ignore))
              (string-match-p dei--ignore-keys-regexp key)
              (string-search "backspace" key)
              (string-search "DEL" key)
              ;; (-any-p #'dei--not-on-keyboard (split-string key " "))
              (cl-loop for filter in extra-filters
                       when (and filter (funcall filter key))
                       return t)
              (cl-loop for prefix in dei--unnest-avoid-prefixes
                       when (string-prefix-p prefix key)
                       return t))
   if with-commands collect (cons key cmd)
   else collect key))
;; (dei--unnest-keymap 'projectile-mode-map nil '((lambda (x) (string-search "s-4" x))))
;; (dei--unnest-keymap 'projectile-mode-map t '((lambda (x) (string-search "s-4" x))))


;;;; Handlers for key descriptions

(defun dei--last-key (single-key-or-chord)
  "Return the last key in SINGLE-KEY-OR-CHORD.
USE WITH CARE.  Presupposes that the input has no spaces and has
been normalized by (key-description (kbd KEY))!"
  (declare (pure t) (side-effect-free t))
  (let ((s single-key-or-chord))
    (cond ((or (string-match-p "--" s)
               (string-match-p "-$" s))
           "-")
          ((string-match-p "<$" s)
           "<")
          ((string-match-p "<" s)
           (save-match-data
             (string-match (rx "<" (* nonl) ">") s)
             (match-string 0 s)))
          (t
           (car (last (split-string s "-" t)))))))

(defun dei--key-contains-multi-chord (keydesc)
  "Check if KEYDESC has C-M- or such simultaneous chords.
Assumes KEYDESC was normalized by (key-description (kbd KEY))."
  (declare (pure t) (side-effect-free t))
  ;; assume keydesc was already normalized.
  (string-match-p (rx (= 2 (regexp dei--modifier-regexp)))
                  keydesc))

(defun dei--key-contains (symlist keydesc)
  "Check if any key in KEYDESC matches a member of SYMLIST.
Ignores modifier keys.

To check for shifted symbols such as capital letters, pass
`dei--all-shifted-symbols-list' as SYMLIST."
  (declare (pure t) (side-effect-free t))
  (->> (split-string keydesc " ")
       (-map #'dei--last-key)
       (-intersection symlist)))

(defun dei--key-has-more-than-one-chord (keydesc)
  "Return nil if KEYDESC has exactly one or zero chords.
otherwise always return t, even if the additional chords use the
same modifier.  In other words:

C-c C-o returns t
C-c M-o returns t
C-c o returns nil
<f1> o returns nil
<f1> C-f returns nil

Does not check for shifted symbols, such as capital letters.  For
that, see `dei--key-contains-any'."
  (declare (pure t) (side-effect-free t))
  (when-let ((first-modifier-pos
              (string-match-p dei--modifier-regexp-safe keydesc)))
    ;; We use +1 and not +2 b/c of the peculiarities of this regexp, but it
    ;; gets the job done.
    (string-match-p dei--modifier-regexp-safe keydesc (1+ first-modifier-pos))))

(defun dei--key-starts-with-modifier (keydesc)
  "Return t if kEYDESC starts with a modifier."
  (declare (pure t) (side-effect-free t))
  (when-let ((first-modifier-pos
              (string-match-p dei--modifier-regexp-safe keydesc)))
    (zerop first-modifier-pos)))

(defun dei--key-mixes-modifiers (keydesc)
  "Return t if KEYDESC has more than one kind of modifier.
Does not catch shiftsyms such as capital letters; to check for
those, see `dei--key-contains-any'.  Does catch e.g. C-S-<RET>."
  (declare (pure t) (side-effect-free t))
  (let ((case-fold-search nil)
        (first-match-pos (string-match-p dei--modifier-regexp-safe keydesc)))
    (when first-match-pos
      (let* (;; Compensate if `dei--modifier-regexp-safe' matched a dash or
             ;; space preceding the actual modifier.
             (first-match-pos (if (zerop first-match-pos)
                                  first-match-pos
                                (1+ first-match-pos)))
             (caught (substring keydesc first-match-pos (1+ first-match-pos)))
             (mods '("A" "C" "H" "M" "S" "s"))
             (now-forbidden-mods-regexp
              (concat "\\(-\\|^\\| \\)\\(["
                      (string-join (remove caught mods))
                      "]-\\)")))
        (string-match-p now-forbidden-mods-regexp keydesc)))))

(defun dei--key-seq-steps=1 (keydesc)
  "Does KEYDESC represent a single event rather than a sequence?
Tiny function, but useful for `mapcar' and friends."
  (declare (pure t) (side-effect-free t))
  (not (string-search " " keydesc)))

(defun dei--key-seq-steps-length (keydesc)
  "Length of key sequence KEYDESC.
Useful predicate for `seq-sort-by' or `cl-sort'."
  (declare (pure t) (side-effect-free t))
  (length (split-string keydesc " ")))

(defun dei--key-seq-is-permachord (keydesc)
  "If sequence KEYDESC has a chord on every step, return t.
Note that it does not check if it's the same chord every time.
For that, see `dei--key-mixes-modifiers'."
  (declare (pure t) (side-effect-free t))
  (when (> (length keydesc) 2)
    (let* ((first-2-chars (substring keydesc 0 2))
           (root-modifier (when (string-suffix-p "-" first-2-chars)
                            first-2-chars)))
      (when root-modifier
        (not (cl-loop
              for step in (split-string keydesc " ")
              unless (and
                      (> (length step) 2)
                      (string-prefix-p root-modifier
                                       (substring step 0 2))
                      (not (string-match-p dei--modifier-regexp-safe
                                           (substring step 2))))
              return t))))))

(defun dei--stem-to-parent-keydesc (stem)
  "Return a valid key by trimming STEM from the right.
In practice, you'd use this to figure out the prefix key that
maps to the keymap implied by there being a stem here.  For
example, inputting a STEM of \"C-x \" returns \"C-x\"."
  (declare (pure t) (side-effect-free t))
  (if (string-suffix-p " " stem)
      (substring stem 0 -1)
    (if (string-match-p (concat dei--modifier-regexp "$") stem)
        (replace-regexp-in-string (rx " " (+ (regexp dei--modifier-regexp)) eol)
                                  ""
                                  stem)
      (error "Non-stem passed to `dei--stem-to-parent-keydesc': %s" stem))))

(defun dei--get-leaf (keydesc)
  "Return leaf key of KEYDESC."
  (declare (pure t) (side-effect-free t))
  (->> (split-string keydesc " ")
       (-last-item)
       (dei--last-key)))

(defun dei--drop-leaf (keydesc)
  "Chop the leaf off KEYDESC and return the resulting stem."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (rx (literal (dei--get-leaf keydesc)) eol)
                            ""
                            keydesc))

(defun dei--parent-stem (stem)
  "Return a parent stem to STEM.
Note what is considered a parent.  The stem \"C-s C-\" is in some
information-theoretic sense nested more deeply than \"C-s \", but
speaking in terms of keymap nesting, they refer to the same
sub-keymap.  As such, both of these return the same parent: \"C-\"."
  (declare (pure t) (side-effect-free t))
  (if (or (= 2 (length stem))
          (dei--key-seq-steps=1 stem))
      nil
    (dei--drop-leaf (dei--stem-to-parent-keydesc stem))))

(defun dei--parent-key (keydesc)
  "Return immediate prefix of KEYDESC, or nil if it hasn't one."
  (declare (pure t) (side-effect-free t))
  (let ((steps (split-string keydesc " ")))
    (if (= 1 (length steps))
        ;; Return nil so functions like `-keep' can take advantage.
        nil
      (string-join (butlast steps) " "))))

(defun dei--prefix-to-stem (keydesc)
  "Add a space to the end of KEYDESC.
Trivial function, but useful for `mapcar' and friends."
  (declare (pure t) (side-effect-free t))
  (concat keydesc " "))

(provide 'deianira-lib)
