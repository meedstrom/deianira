;;; deianira.el --- Modifier-free pseudo-modal input -*- lexical-binding: t -*-

;; (read-symbol-shorthands . '("dei-" . "deianira-"))

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

;; Author:  <meedstrom@teknik.io>
;; Created: 2018-08-03
;; Version: 0.1.0
;; Keywords: abbrev convenience
;; Homepage: https://github.com/meedstrom/deianira
;; Package-Requires: ((emacs "28.1") (hydra "0.15.0") (named-timer "0.1") (dash "2.19.1"))

;;; Commentary:

;;; Code:

(defgroup deianira nil
  "Hydra everywhere."
  :link '(info-link "(deianira)")
  :group 'keyboard)

;; builtin dependencies
(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'help-fns)

;; external dependencies
(require 'dash)
(require 'hydra)
(require 'named-timer) ;; emacs core when? just 70 lines

;; muffle the compiler
(declare-function #'dei-A/body "deianira" nil t)
(declare-function #'dei-C/body "deianira" nil t)
(declare-function #'dei-H/body "deianira" nil t)
(declare-function #'dei-M/body "deianira" nil t)
(declare-function #'dei-s/body "deianira" nil t)


;;;; Constants

(defconst dei--modifier-regexp
  (regexp-opt '("A-" "C-" "H-" "M-" "S-" "s-"))
  "Regexp for any of the modifiers: \"C-\", \"M-\" etc.
Upon match, the string segment it matched is always two characters long.

Beware that it will also match a few obscure named function keys,
such as <ns-drag-n-drop>, where it will match against s- in the
ns- part.  To guard this, use `dei--modifier-regexp-safe'
instead, although that has its own flaws.")

(defconst dei--modifier-regexp-safe
  (rx (or "<" "-" bol " ")
      (or "A-" "C-" "H-" "M-" "S-" "s-"))
  "Like `dei--modifier-regexp', but check a preceding character.
Benefit: it will always match if there's at least one modifier,
and not count spurious modifier-looking things such as
<ns-drag-n-drop> which contains an \"s-\".

Drawback: you can't match twice on the same string.  Look at the
case of M-s-<down>: it'll match M-, but if the search continues
from there, it will fail to match s- since it's only looking for
s- preceded by a dash (i.e. \"-s-\"), and our second search
starts past the first dash.  However, it's fine if you cut the
string and start a new search on the cut string.")

(defconst dei--ignore-keys-regexp
  (regexp-opt '(;; Don't bother handling control characters well, it's a
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


;;;; User settings

(defcustom dei-debug t
  "Whether to show debug buffers by default."
  :group 'deianira
  :type 'boolean)

(defun dei--debug-buffer ()
  (let ((bufname (concat (unless dei-debug " ") "*Deianira debug*")))
    (or (get-buffer bufname)
        (with-current-buffer (get-buffer-create bufname)
          (setq-local truncate-lines t)
          (setq-local buffer-read-only nil)
          (setq-local tab-width 12)
          (current-buffer)))))

(defun dei--echo (&rest args)
  "Write a message to the debug buffer.
Arguments same as for `format'."
  (with-current-buffer (dei--debug-buffer)
    (goto-char (point-min))
    (insert (apply #'format
                   (cons (concat (format-time-string "%T: ")
                                 (car args))
                         (cdr args))))
    (newline)))

(defun dei-debug-show ()
  "Show debug buffers."
  (interactive)
  (with-current-buffer (dei--debug-buffer)
    ;; Unhide them if they were hidden
    (rename-buffer "*Deianira debug*")
    (display-buffer (current-buffer)))
  ;; (with-current-buffer " *Deianira remaps*"
  ;;   (rename-buffer "*Deianira remaps*"))
  (setq dei-debug t))

(defun dei-xmodmap-reload ()
  "(Re-)apply the `dei-xmodmap-rules'."
  (interactive)
  (let* ((shell-command-dont-erase-buffer t)
         (rules (string-join dei-xmodmap-rules "' -e '"))
         (cmd (concat "xmodmap -e '" rules "'")))
    (if (executable-find "xmodmap")
        (start-process-shell-command cmd (dei--debug-buffer) cmd)
      (message "(deianira) Executable not found: xmodmap"))))

(defcustom dei-xmodmap-rules
  '("keycode any = F20"
    "keycode any = F21"
    "keycode any = F22"
    "keycode any = F23"
    "keycode any = F24")
  "Rules to pass to xmodmap.
The Linux kernel defines KEY_F24 as scancode 194 and doesn't
define higher function keys (i.e. there is no F25 or above).
However, at kernel level these names don't mean anything and
could equally as well be called KEY_FNORD.  We only need to know
them because of how they match up to Xorg: Xorg adds 8 to the
number.  So when Linux emits scancode 194, Xorg calls that
keycode 202.  Depending on your Xkb keymap table it may not be
mapped to what Xorg calls the keysym F24 but something like
XF86Launch7.

See also https://unix.stackexchange.com/questions/364641/mapping-scancodes-to-keycodes

Incidentally, /usr/include/X11/keysymdef.h tells us Xorg has
keysyms up to F35: \"We've not found a keyboard with more than 35
function keys total.\" So if you do have actual physical function
keys up to F24, you could migrate to F31-F35 instead for our
purposes.  The kernel codes are found here (don't forget to add 8):
https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes"
  :type '(repeat string)
  :group 'deianira
  :set (lambda (var new)
         (set-default var new)
         (dei-xmodmap-reload)))

(defcustom dei-hydra-keys
  "1234567890qwertyuiopasdfghjkl;zxcvbnm,./"
  "Keys to show in hydra hint\; default reflects an US keyboard.
Length should be divisible by `dei-columns'.  In other words, if
you have 10 columns this can be 30 or 40 characters, if you
want 11 columns this can be 33 or 44 characters, and so on.

The order matters\; it determines the order in which the keys are
displayed."
  :type 'string
  :group 'deianira)

(defcustom dei-all-shifted-symbols
  "~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?"
  "Characters that imply Shift pressed\; default reflects an US keyboard."
  :type 'string
  :group 'deianira)

(defcustom dei-columns 10
  "Amount of columns to display in the hydra hint.
When customizing, please also customize `dei-hydra-keys'."
  :type 'number
  :group 'deianira)

(defcustom dei-quasiquitter-keys
  '("C-c c"
    "C-x l" ;; for testing
    )
  "Keys that send you to the root hydra.

Note that if you use mass remapping (see manual), the hydras are
generated afterwards, consulting this list then.  So it is safe
to only refer to e.g. \"C-c c\" even if it's going to be a clone
of \"C-c C-c\".  In fact, only \"C-c c\" will have an effect,
probably."
  :type '(repeat key)
  :group 'deianira
  :set (lambda (var new)
         (set-default var (--map (key-description (kbd it)) new))))

(defcustom dei-quasiquitter-commands
  '(set-mark-command
    rectangle-mark-mode)
  "Commands that send you to the root hydra."
  :type '(repeat symbol)
  :group 'deianira)

(defcustom dei-inserting-quitters
  '("SPC"
    "RET")
  "Keys guaranteed to slay the hydra as well as self-insert.
Note that you do not need to specify shift symbols, as
`dei-all-shifted-symbols' is basically added to this."
  :type '(repeat key)
  :group 'deianira
  :set (lambda (var new)
         (set-default var (--map (key-description (kbd it)) new))))

(defcustom dei-stemless-quitters
  '("<menu>"
    "C-g")
  "Keys guaranteed to behave like themselves, instead of the key plus a prefix.
Also guaranteed to slay the hydra.

This can be used to ensure that when you're in any hydra, let's
say the hydra for the \"C-x a\" keymap, typing C-g will not send
to Emacs \"C-x a C-g\", but simply C-g."
  :type '(repeat key)
  :group 'deianira
  :set (lambda (var new)
         (set-default var (--map (key-description (kbd it)) new))))

(defcustom dei-quitter-keys
  '()
  "Key sequences guaranteed to slay the hydra.

Note that if you use mass remapping (see manual), the hydras are
generated afterwards, consulting this list then.  So it is safe
to only refer to e.g. \"C-c c\" even if it's going to be a clone
of \"C-c C-c\".  In fact, only \"C-c c\" will have an effect,
probably."
  :type '(repeat key)
  :group 'deianira
  :set (lambda (var new)
         (set-default var (--map (key-description (kbd it)) new))))

(defcustom dei-quitter-commands
  '(keyboard-quit
    minibuffer-keyboard-quit
    keyboard-escape-quit
    dei--call-and-return-to-root
    dei--call-and-return-to-parent
    doom/escape
    isearch-forward
    isearch-backward
    doom/restart
    doom/restart-and-restore
    abort-recursive-edit
    execute-extended-command
    counsel-M-x
    helm-M-x
    magit-status
    dired
    dired-jump
    re-builder
    ffap-other-frame
    make-frame-command
    other-frame
    kill-emacs
    +lookup/online
    +doom-dashboard/open
    org-noter
    org-agenda
    org-roam-capture
    org-capture)
  "Commands guaranteed to slay the hydra.
Note that you usually don't need to add commands that focus the
minibuffer, as we slay the hydra automatically when something
calls `completing-read' or the Ivy/Ido/Helm equivalents."
  :type '(repeat symbol)
  :group 'deianira)

(defcustom dei-extra-heads
  '()
  "Heads to add to every hydra.  See `defhydra' for the format."
  :type '(repeat sexp)
  :group 'deianira)

(defcustom dei-invisible-leafs
  (append
   (split-string
    "`-=[]\\'"
    "" t)
   (split-string
    (concat
     " <left> <right> <up> <down> SPC"
     " <print> <insert> <next> <prior> <home> <end> <menu>"
     " TAB <tab> <iso-lefttab>")))
  "Keys that should not behave as foreign keys.

By default, typing a key not in `dei-hydra-keys' nor this list
will result in calling that key's normal binding, as if there was
no active hydra (in Hydra jargon, it behaves as a foreign key).

Inclusion in this list means that key will be prepended with a
prefix.

Example:

If TAB is not a member of this list, typing the three keystrokes
<Ctrl> x TAB calls the binding of TAB.

If TAB is a member of this list, it's taken as a leaf grafted
onto the hydra's corresponding stem.  So typing the three
keystrokes <Ctrl> x TAB calls the binding of C-x TAB."
  :type '(repeat key)
  :group 'deianira
  :set (lambda (var new)
         (set-default var (--map (key-description (kbd it)) new))))


;;;; Background facts

(defun dei--all-shifted-symbols-list-recalc ()
  "."
  (split-string dei-all-shifted-symbols "" t))

(defvar dei--all-shifted-symbols-list (dei--all-shifted-symbols-list-recalc)
  "Cache variable, not to be modified directly.
Customize `dei-all-shifted-symbols' instead.")

(defun dei--hydra-keys-list-recalc ()
  "."
  (split-string dei-hydra-keys "" t))

(defvar dei--hydra-keys-list (dei--hydra-keys-list-recalc)
  "Cache variable, not to be modified directly.
Customize `dei-hydra-keys' instead.")

(defun dei--colwidth-recalc ()
  "Recalculate `dei--colwidth' based on frame width."
  ;; Minus four because of the legend (i.e. space reserved for a,b,c in
  ;; "a: COMMAND b: COMMAND c: COMMAND ...".
  (- (floor (frame-width) 10) 4))

(defvar dei--colwidth (dei--colwidth-recalc)
  "Cache variable, not to be modified directly.")

(defun dei--filler-recalc ()
  "Recalculate `dei--filler' based on `dei--colwidth'."
  (make-string dei--colwidth (string-to-char " ")))

(defvar dei--filler (dei--filler-recalc)
  "Cache variable, not to be modified directly.")


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
  (declare (pure t) (side-effect-free t))
  (not (string-search " " keydesc)))

(defun dei--key-seq-split (keydesc)
  (declare (pure t) (side-effect-free t))
  (split-string keydesc " "))

(defun dei--key-seq-steps-length (keydesc)
  "Length of key sequence KEYDESC.
Useful predicate for `seq-sort-by' or `cl-sort', which take a
function but won't pass extra arguments to it."
  (declare (pure t) (side-effect-free t))
  (length (dei--key-seq-split keydesc)))

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
              for step in (dei--key-seq-split keydesc)
              unless (and
                      (> (length step) 2)
                      (string-prefix-p root-modifier
                                       (substring step 0 2))
                      (not (string-match-p dei--modifier-regexp-safe
                                           (substring step 2))))
              return t))))))

;; This is used by dei--head-arg-cmd
(defun dei--corresponding-hydra (keydesc-or-stem &optional leaf)
  (intern (concat
           (dei--dub-hydra-from-key-or-stem (concat keydesc-or-stem leaf))
           "/body")))

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

;; TODO: wrap RET SPC DEL etc in <> for the squashed version
(defun dei--dub-hydra-from-key-or-stem (keydesc)
  "Example: if KEYDESC is \"C-x a\", return \"dei-Cxa\"."
  (declare (pure t) (side-effect-free t))
  (let ((squashed (string-join (split-string keydesc (rx (any " -"))))))
    (if (string-match (rx "-" eol) keydesc)
        (if (= 2 (length keydesc))
            (concat "dei-" squashed) ;; C-, M-, s-
          (concat "dei-" squashed "-")) ;; leaf is -
      (concat "dei-" squashed))))

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

;; heh, when i made this a separate function, i found my code never did what i
;; wanted.  now fixed.  score one for small testable functions.
(defun dei--ensure-chordonce (keydesc)
  "Strip chords from most of key sequence KEYDESC.
Leave alone the first step of the key sequence."
  (declare (pure t) (side-effect-free t))
  (let* ((steps (dei--key-seq-split keydesc)))
    (string-join (cons (car steps)
                       (-map #'dei--get-leaf (cdr steps)))
                 " ")))

(defun dei--ensure-permachord (keydesc)
  "Return KEYDESC as perma-chord.
If it's already that, return it unmodified."
  (declare (pure t) (side-effect-free t))
  (when (> (length keydesc) 2)
    (let* ((steps (dei--key-seq-split keydesc))
           (first-2-chars (substring keydesc 0 2))
           (root-modifier (when (string-suffix-p "-" first-2-chars)
                            first-2-chars)))
      (if root-modifier
          (string-join (cl-loop
                        for step in steps
                        if (string-prefix-p root-modifier step)
                        do (when (string-match-p dei--modifier-regexp-safe
                                                 (substring step 1))
                             (warn "Maybe found bastard sequence: %s" keydesc))
                        and collect step
                        else collect (concat root-modifier step))
                       " ")
        (warn "dei--ensure-permachord probably shouldn't be called on: %s"
              keydesc)
        keydesc))))


;;;; Library of random stuff

;; REVIEW: write test for it with a key-simulator
(defun dei-universal-argument (arg)
  "enter a nonum hydra and activate the universal argument."
  (interactive "p")
  (prefix-command-preserve-state)
  (call-interactively
   (intern (concat
            (substring (symbol-name hydra-curr-body-fn) 0 -5) ;; chop "/body"
            "-nonum/body")))
  (hydra--universal-argument arg))

(defun dei-negative-argument (arg)
  (interactive "p")
  (prefix-command-preserve-state)
  (call-interactively
   (intern (concat
            (substring (symbol-name hydra-curr-body-fn) 0 -5) ;; chop "/body"
            "-nonum/body")))
  (hydra--negative-argument arg))

;; quasiquit
;; unused
(defun dei--call-and-return-to-root (keydesc)
  "Nice in some cases, like C-c C-c for which it's often
desirable to end up in the Control root hydra rather than exit
altogether. Say you want to call it for each item in a list of
Org headings, and `next-line' is bound to the standard C-n, then
you want to be able to type nccnccnccncc."
  (interactive)
  (call-interactively (key-binding (kbd keydesc)))
  (let ((init (substring keydesc 0 2)))
    ;; REVIEW: is it better to use call-interactively?
    (cond ((string-search "C-" init) (dei-C/body))
          ((string-search "M-" init) (dei-M/body))
          ((string-search "s-" init) (dei-s/body))
          ((string-search "H-" init) (dei-H/body))
          ((string-search "A-" init) (dei-A/body)))))

;; unused; another sense of quasiquit
;; TODO: come up with a good name and implement a defcustom
(defun dei--call-and-return-to-parent (keydesc)
  (interactive)
  (call-interactively (key-binding (kbd keydesc)))
  (if-let ((parent (dei--corresponding-hydra
                    (dei--parent-stem (dei--drop-leaf keydesc)))))
      (call-interactively parent)
    (hydra-keyboard-quit)))


;;;; Main

(defvar dei--old-hydra-cell-format nil
  "Backup for `hydra-cell-format'.")

(defvar dei--old-hydra-C-u nil
  "Backup for key binding of \"C-u\" in `hydra-base-map'.")

(declare-function #'ido-read-internal "ido")
(declare-function #'ivy-read "ivy")
(declare-function #'helm "helm-core")

(defvar dei--interrupt-counter 0)
(defun dei--interrupt-decrement-ctr ()
  "Decrement `dei--interrupt-counter' until zero."
  (unless (zerop dei--interrupt-counter)
    (cl-decf dei--interrupt-counter)))

(defun dei--hydra-active-p ()
  "Return t if a hydra is active and awaiting input."
  (not (null hydra-curr-map)))

(defun dei--slay (&rest args)
  "Slay active hydra and return ARGS."
  (when (dei--hydra-active-p)
    (setq hydra-deactivate t)
    (call-interactively #'hydra-keyboard-quit))
  args)

;;;###autoload
(define-minor-mode deianira-mode
  "Set switch-window hooks to make hydras and in the darkness bind them."
  :global t
  :lighter " dei"
  :group 'deianira
  :keymap (make-sparse-keymap)
  (if deianira-mode
      (progn
        (setq dei--async-running nil)
        (setq dei--async-chain nil)
        (setq dei--interrupt-counter 0)
        (named-timer-run 'deianira-b 300 300 #'dei--interrupt-decrement-ctr)
        (setq dei--old-hydra-cell-format hydra-cell-format)
        (setq dei--old-hydra-C-u (lookup-key hydra-base-map (kbd "C-u")))
        (setq hydra-cell-format "% -20s %% -11`%s")
        (if (version<= "29" emacs-version)
            (define-key hydra-base-map (kbd "C-u") nil t)
          (define-key hydra-base-map (kbd "C-u") nil))
        (advice-add #'completing-read :before #'dei--slay)
        (advice-add #'ido-read-internal :before #'dei--slay) ;; REVIEW UNTESTED
        (advice-add #'ivy-read :before #'dei--slay) ;; REVIEW UNTESTED
        (advice-add #'helm :before #'dei--slay) ;; REVIEW UNTESTED
        (add-hook 'window-buffer-change-functions #'dei--record-keymap-maybe -68)
        (add-hook 'window-buffer-change-functions #'dei--react 56)
        (add-hook 'window-selection-change-functions #'dei--react)
        (add-hook 'after-change-major-mode-hook #'dei--react)
        (add-variable-watcher 'local-minor-modes #'dei--react))
    (named-timer-cancel 'deianira-at-work)
    (named-timer-cancel 'deianira-b)
    (named-timer-cancel 'deianira-c)
    (setq hydra-cell-format (or dei--old-hydra-cell-format "% -20s %% -8`%s"))
    (define-key hydra-base-map (kbd "C-u") dei--old-hydra-C-u)
    (remove-hook 'window-buffer-change-functions #'dei--record-keymap-maybe)
    (remove-hook 'window-buffer-change-functions #'dei--react)
    (remove-hook 'window-selection-change-functions #'dei--react)
    (remove-hook 'after-change-major-mode-hook #'dei--react)
    (remove-variable-watcher 'local-minor-modes #'dei--react)
    (advice-remove #'completing-read #'dei--slay)
    (advice-remove #'ido-read-internal #'dei--slay)
    (advice-remove #'ivy-read #'dei--slay)
    (advice-remove #'helm #'dei--slay)))

(defconst dei--ersatz-keys-alist
  '((dei-ersatz-alt . dei-A/body)
    (dei-ersatz-control . dei-C/body)
    (dei-ersatz-hyper . dei-H/body)
    (dei-ersatz-meta . dei-M/body)
    (dei-ersatz-super . dei-s/body))
  "Table associating the ersatz keys with root hydras.
Never expected to change.  Customize `dei-ersatz-alt' & co
instead.")

(defvar dei--hidden-obarray (obarray-make))

(defun dei--set-ersatz-key (var newkey)
  "Bind VAR to NEWKEY, and help other code cope with the change."
  (require 'map)
  ;; Reset all hydras because value gets hardcoded by `dei--specify-extra-heads'
  (setq dei--flocks nil)
  (obarray-remove dei--hidden-obarray "dei--flocks")
  ;; Unbind last key
  (when (boundp var)
    (if (version<= "29" emacs-version)
        (define-key deianira-mode-map (kbd (symbol-value var)) nil t)
      (define-key deianira-mode-map (kbd (symbol-value var)) nil)))
  ;; Bind new key
  (define-key deianira-mode-map (kbd newkey) (map-elt dei--ersatz-keys-alist var))
  (set-default var newkey))

(defcustom dei-ersatz-alt "<hiragana-katakana>"
  "Key that represents Alt."
  :type 'key
  :group 'deianira
  :set #'dei--set-ersatz-key)

(defcustom dei-ersatz-control "<katakana>"
  "Key that represents Control."
  :type 'key
  :group 'deianira
  :set #'dei--set-ersatz-key)

(defcustom dei-ersatz-hyper "<hiragana>"
  "Key that represents Hyper."
  :type 'key
  :group 'deianira
  :set #'dei--set-ersatz-key)

(defcustom dei-ersatz-meta "<muhenkan>"
  "Key that represents Meta."
  :type 'key
  :group 'deianira
  :set #'dei--set-ersatz-key)

(defcustom dei-ersatz-super "<henkan>"
  "Key that represents Super."
  :type 'key
  :group 'deianira
  :set #'dei--set-ersatz-key)

(defvar dei--hydrable-prefix-keys nil)


;;;; Hydra blueprinting

;; Head arguments

(defun dei--head-arg-cmd (stem leaf)
  "See `dei--head'."
  (let ((key (concat stem leaf)))
    (cond
     ;; Sub-hydra
     ((member (concat stem leaf) dei--hydrable-prefix-keys)
      (dei--corresponding-hydra stem leaf))
     ;; Quasi-quitter, meaning call key and return to root hydra
     ((or (member (key-binding (kbd key)) dei-quasiquitter-commands)
          (member key dei-quasiquitter-keys))
      `(lambda ()
         (interactive)
         (call-interactively (key-binding (kbd ,key)))
         (,(let ((init (substring key 0 2)))
             (cond
              ((string-search "C-" init) (dei--corresponding-hydra "C "))
              ((string-search "M-" init) (dei--corresponding-hydra "M "))
              ((string-search "s-" init) (dei--corresponding-hydra "s "))
              ((string-search "H-" init) (dei--corresponding-hydra "H "))
              ((string-search "A-" init) (dei--corresponding-hydra "A ")))))))
     ;; Regular key
     (t
      `(call-interactively (key-binding (kbd ,key)))))))

(defun dei--head-arg-cmd-will-bind-to-hydra-p (stem leaf)
  (let ((binding (dei--head-arg-cmd stem leaf)))
    (when (symbolp binding)
      (string-suffix-p "/body" (symbol-name binding)))))

(defun dei--head-arg-hint (stem leaf)
  "See `dei--head'."
  (let* ((sym (if (member (concat stem leaf) dei--hydrable-prefix-keys)
                  (dei--corresponding-hydra stem leaf)
                (key-binding (kbd (concat stem leaf)))))
         ;; REVIEW: when is sym ever not a symbol?
         (name (if (symbolp sym)
                   (symbol-name sym)
                 (concat stem leaf))))
    (if (null sym)
        dei--filler
      (if (> (length name) dei--colwidth)
          (substring name 0 dei--colwidth)
        name))))

(defun dei--head-arg-exit (stem leaf)
  "See `dei--head'."
  (let ((key (concat stem leaf)))
    (when (or (member (key-binding (kbd key)) dei-quasiquitter-commands)
              (member (key-binding (kbd key)) dei-quitter-commands)
              (member key dei-quasiquitter-keys)
              (member key dei-quitter-keys)
              (member key dei--hydrable-prefix-keys)
              ;; Extra safety measure which could be upstreamed to Hydra
              (dei--head-arg-cmd-will-bind-to-hydra-p stem leaf))
      '(:exit t))))

;; Different types of full heads

(defun dei--head (stem leaf)
  "Return a hydra head specification (sexp), see `defhydra'.
Strings STEM and LEAF concatenate to form a key description
satisfying `key-valid-p' (Emacs 29 function), and string LEAF is
a single unchorded key, typically one character but can also be a
named event such as <return>."
  `( ,leaf
     ,(dei--head-arg-cmd stem leaf)
     ,(dei--head-arg-hint stem leaf)
     ,@(dei--head-arg-exit stem leaf)))

(defun dei--head-invisible (stem leaf)
  `( ,leaf
     ,(dei--head-arg-cmd stem leaf)
     ,@(dei--head-arg-exit stem leaf)))

(defun dei--head-invisible-self-inserting-stemless (_stem leaf)
  `( ,leaf self-insert-command :exit t))

(defun dei--head-invisible-exiting-stemless (_stem leaf)
  `( ,leaf ,(dei--head-arg-cmd "" leaf) :exit t))

(defun dei--head-invisible-stemless (_stem leaf)
  `( ,leaf ,(dei--head-arg-cmd "" leaf)))

;; Lists of full heads

;; unused
(defun dei--specify-heads-to-subhydra (stem leaf-list)
  (cl-loop for leaf in leaf-list
           collect
           (let ((cmd (dei--corresponding-hydra stem leaf)))
             (list leaf
                   cmd
                   (when (member leaf dei--hydra-keys-list)
                     (symbol-name cmd))
                   :exit t))))

(defun dei--specify-visible-heads (stem &optional verboten-leafs)
  "Return a list of heads that will be shown in the hydra hint.
These are for the hydra imaged by STEM, which is combined with
various leafs, see function body.

Optional argument VERBOTEN-LEAFS, a list of strings such as
'(\"1\" \"z\"), prevents including heads for these leafs."
  (let ((leaf-list (-difference dei--hydra-keys-list verboten-leafs)))
    (cl-loop for leaf in leaf-list
             collect `(,leaf
                       ,(dei--head-arg-cmd stem leaf)
                       ,(dei--head-arg-hint stem leaf)
                       ,@(dei--head-arg-exit stem leaf)))))

(defun dei--specify-invisible-heads (stem &optional verboten-leafs)
  "Return a list of heads not to be shown in the hydra hint.
These are for the hydra imaged by STEM, which is combined with
various leafs, see function body.

Optional argument VERBOTEN-LEAFS, a list of strings such as
'(\"1\" \"z\"), prevents including heads for these leafs."
  (let ((x (append
            (cl-loop for leaf in dei-invisible-leafs
                     collect (dei--head-invisible stem leaf))
            ;; For these keys, we want the hydra to exit, so we need to add
            ;; heads so that we can set :exit t. (We design hydras with default
            ;; :exit nil for foreign keys, because otherwise we'd need a lot
            ;; more heads in order to set :exit nil for them.)  As for the
            ;; command within these heads, the naive approach is bind to nil,
            ;; but we prefer to self-insert at the same time otherwise user has
            ;; to press twice.  So we bind directly to self-insert-command.  An
            ;; alternative, our usual head-arg-cmd, is not necessary if we've
            ;; anyway decided to self-insert --- although if all shiftsyms were
            ;; unbound in all keymaps, it'd work the same.
            (cl-loop for leaf in (append dei--all-shifted-symbols-list
                                         dei-inserting-quitters)
                     collect (dei--head-invisible-self-inserting-stemless stem leaf))
            ;; NOTE: This doesn't overlap with dei-quitter-keys; have to be stemless
            (cl-loop for leaf in dei-stemless-quitters
                     collect (dei--head-invisible-exiting-stemless stem leaf)))))
    (-remove (lambda (head)
               (member (car head) verboten-leafs))
             x)))

(defun dei--convert-head-for-nonum (head)
  "Ensure that hydra head HEAD suits a nonum hydra.
Basically, if HEAD binds `dei-universal-argument', return a
different head that instead binds `hydra--universal-argument' and
drops any :exit keyword.  Same for `dei-negative-argument'.

This is necessary for the nonum hydras to stay active, as the
wrapper `dei-universal-argument' exists to spawn the nonum hydra to
start with, and should only be called once."
  (cond ((eq (cadr head) 'dei-universal-argument)
         (list (car head) 'hydra--universal-argument))
        ((eq (cadr head) 'dei-negative-argument)
         (list (car head) 'hydra--negative-argument))
        (t
         head)))

(defun dei--specify-extra-heads (stem &optional nonum-p)
  "For a hydra imaged by STEM, return some bonus heads for it.
These are mostly the kinds of heads shared by all of Deianira's
hydras.  With NONUM-P non-nil, return a different set of extra
heads suited for a nonum hydra, see `dei--convert-head-for-nonum'."
  (let ((self-poppers
         (cond ((equal "C-" stem) (list dei-ersatz-control
                                        (concat "C-" dei-ersatz-control)))
               ((equal "M-" stem) (list dei-ersatz-meta
                                        (concat "M-" dei-ersatz-meta)))
               ((equal "s-" stem) (list dei-ersatz-super
                                        (concat "s-" dei-ersatz-super)))
               ((equal "H-" stem) (list dei-ersatz-hyper
                                        (concat "H-" dei-ersatz-hyper)))
               ((equal "A-" stem) (list dei-ersatz-alt
                                        (concat "A-" dei-ersatz-alt)))))
        (extras (if nonum-p
                    (mapcar #'dei--convert-head-for-nonum dei-extra-heads)
                  dei-extra-heads)))
    (-non-nil
     `(,(if nonum-p
            `("<backspace>" ,(dei--corresponding-hydra stem) :exit t)
          `("<backspace>" ,(dei--corresponding-hydra (dei--parent-stem stem)) :exit t))
       ,@(when self-poppers
           (cl-loop for key in self-poppers
                    collect `(,key nil :exit t)))
       ,@extras))))

;; Massive list of heads

(defun dei--specify-hydra (stem name &optional nonum-p)
  "Return a list of hydra heads, with NAME as first element.
You can pass the output to `dei--try-birth-hydra' like:

   (let ((spec (dei--specify-hydra ...)))
     (funcall #'dei--try-birth-hydra (car spec) (cdr spec)))

String STEM is a stem on which the resulting list of heads will
be based.

Boolean NONUM-P determines if this should be a numberless hydra,
i.e. one where the numeric keys do numeric prefix arguments
instead of anything else they may have been bound to."
  (let* ((extra-heads (dei--specify-extra-heads stem nonum-p))
         (verboten-leafs (append (mapcar #'car extra-heads)
                                 (when nonum-p
                                   (split-string "1234567890" "" t))))
         (heads (append
                 extra-heads
                 (dei--specify-visible-heads stem verboten-leafs)
                 (dei--specify-invisible-heads stem verboten-leafs))))
    (cons name heads)))

;; TODO: Make it even faster.  It's a bit slow because defhydra does a lot of
;; heavy lifting.  Approaches: 1. give up on the self-inserting quitters
;; (principally capital keys) since there are many of those heads. 2. Give up
;; on nonum hydras. 3. Make a focused variant of defhydra (possible?).
(defun dei--try-birth-hydra (name list-of-heads)
  "Create a hydra named NAME with LIST-OF-HEADS.
This will probably be called by `dei--generate-hydras',
which see."
  ;; An old error check
  (if (eq hydra-curr-body-fn (intern name))
      (error "This hydra active, skipped redefining: %s" name)
    (eval `(defhydra ,(intern name)
             (:columns ,dei-columns
              :exit nil
              :hint nil
              :foreign-keys run)
             ,name
             ,@list-of-heads)
          t)))


;;;; Keymap scanner and mess-up-er

;;; Unbinding ugly keys

(defvar dei--cleaned-maps nil)

(defvar dei--clean-actions nil)

;; Since `or' calls the slow `regexp-opt', save output in variable.
(defconst dei--shift-regexp (rx (or bol "-" " ") "S-")
  "Match explicit \"S-\" chords in key descriptions.")

(defun dei--key-is-illegal (keydesc)
  "Non-nil if KEYDESC would be unbound in a purist scheme."
  (let ((case-fold-search nil))
    (or
     (string-match-p dei--shift-regexp keydesc)
     (dei--key-contains dei--all-shifted-symbols-list keydesc)
     (dei--key-contains-multi-chord keydesc)
     (dei--key-mixes-modifiers keydesc)

     ;; Drop bastard sequences.
     (and (dei--key-has-more-than-one-chord keydesc)
          (not (dei--key-seq-is-permachord keydesc)))

     ;; Drop e.g. <f1> C-f.
     (and (not (dei--key-starts-with-modifier keydesc))
          (string-match-p dei--modifier-regexp-safe keydesc)))))

(defun dei--unbind-illegal-keys ()
  "Push keys to unbind onto `dei--clean-actions'."
  (cl-loop
   for map in (-difference dei--known-keymaps dei--cleaned-maps)
   with doom = (and (bound-and-true-p doom-leader-alt-key)
                    (bound-and-true-p doom-localleader-alt-key))
   do (push
       (cons map
             (cl-sort
              (cl-loop
               for x being the key-seqs of (dei--raw-keymap map)
               as key = (key-description x)
               when (and
                     (not (string-match-p dei--ignore-keys-regexp key))
                     (or (dei--key-is-illegal key)
                         ;; I don't want to touch these, I want to see what
                         ;; Doom does with them.
                         (when doom
                           (or (string-prefix-p doom-localleader-alt-key key)
                               (string-prefix-p doom-leader-alt-key key)))))
               collect key)
              #'> :key #'dei--key-seq-steps-length))
       dei--clean-actions)))

;; REVIEW: Test <ctl> x 8 with Deianira active.
(defvar dei--unnest-avoid-prefixes
  (cons "C-x 8"
        (mapcar #'key-description
                (where-is-internal #'iso-transl-ctl-x-8-map)))
  "List of prefixes to avoid looking up.")

(defun dei--unnest-keymap-for-hydra-to-eat (map &optional avoid-prefixes)
  "Return MAP as a list of key seqs instead of a tree of keymaps.
These key seqs are strings satisfying `key-valid-p'.

Optional argument AVOID-PREFIXES is a list of prefixes to leave
out of the result, defaulting to the value of
`dei--unnest-avoid-prefixes'."
  (cl-loop for x being the key-seqs of (dei--raw-keymap map)
           using (key-bindings cmd)
           as key = (key-description x)
           with cleaned = (member map dei--cleaned-maps)
           with case-fold-search = nil
           unless
           (or (member cmd '(nil
                             self-insert-command
                             ignore
                             ignore-event
                             company-ignore))
               (string-match-p dei--ignore-keys-regexp key)
               (string-search "backspace" key)
               (string-search "DEL" key)
               ;; (-any-p #'dei--not-on-keyboard (dei--key-seq-split key))
               (dei--key-has-more-than-one-chord key)
               (unless cleaned
                 (dei--key-is-illegal key))
               (cl-loop
                for prefix in (or avoid-prefixes dei--unnest-avoid-prefixes)
                when (string-prefix-p prefix key)
                return t))
           collect key))

(defun dei--get-filtered-bindings ()
  "List the current buffer keys appropriate for hydra."
  ;; NOTE Read below comment if 1. we decide to return an alist that includes
  ;; the bound commands, not just keys, or 2. if a conflict does occur in the
  ;; "Just in case" check below.
  ;;
  ;; You know that the same key can be found several times in multiple keymaps.
  ;; Fortunately we can know which binding is correct for the current buffer,
  ;; as `current-active-maps' seems to return maps in the order in which Emacs
  ;; selects them.  So what comes earlier in the list is what would in fact be
  ;; used.  Then we run `-uniq' to declutter the list, which likewise keeps
  ;; the first instance of each set of duplicates.
  (let ((T (current-time))
        (keys
         (-uniq
          (cl-loop
           for map in (current-active-maps)
           with case-fold-search = nil
           append (dei--unnest-keymap-for-hydra-to-eat map)))))
    ;; Just in case: if a key is bound to a simple command in one keymap, but
    ;; to a subkeymap in another keymap, so we record both the single command
    ;; and the children that take it as prefix, it may break lots of things, so
    ;; just signal an error and I'll think about how to fail gracefully later.
    ;; I've not got an error yet.
    ;; NOTE: this check costs some performance
    (let ((conflicts (cl-loop
                      for parent in (-uniq (-keep #'dei--parent-key keys))
                      when (assoc parent keys)
                      collect parent)))
      (when conflicts
        (error "Bound variously to commands or prefix maps: %s" conflicts)))
    (and dei-debug
         (> (float-time (time-since T)) 1)
         (warn "Took way too long getting bindings"))
    keys))

;; TODO: dedup
(defun dei--get-filtered-bindings-with-commands ()
  "List the current buffer keys appropriate for hydra."
  ;; You know that the same key can be found several times in multiple keymaps.
  ;; Fortunately we can know which binding is correct for the current buffer,
  ;; as `current-active-maps' seems to return maps in the order in which Emacs
  ;; selects them.  So what comes earlier in the list is what would in fact be
  ;; used.  Then we run `-uniq' to declutter the list, which likewise keeps
  ;; the first instance of each set of duplicates.
  (let ((alist
         (-uniq
          (cl-loop
           for map in (current-active-maps)
           with case-fold-search = nil
           with avoid-prefixes =
           (cons "C-x 8"
                 (mapcar #'key-description
                         (where-is-internal #'iso-transl-ctl-x-8-map)))
           append (cl-loop
                   for x being the key-seqs of (dei--raw-keymap map)
                   using (key-bindings cmd)
                   as key = (key-description x)
                   with cleaned = (member map dei--cleaned-maps)
                   unless
                   (or (member cmd '(nil
                                     self-insert-command
                                     ignore
                                     ignore-event
                                     company-ignore))
                       (string-match-p dei--ignore-keys-regexp key)
                       (string-search "backspace" key)
                       (string-search "DEL" key)
                       ;; (-any-p #'dei--not-on-keyboard (dei--key-seq-split key))
                       (dei--key-has-more-than-one-chord key)
                       (unless cleaned
                         (dei--key-is-illegal key))
                       (cl-loop
                        for prefix in avoid-prefixes
                        when (string-prefix-p prefix key)
                        return t))
                   collect (cons key cmd))))))
    ;; Just in case: if a key is bound to a simple command in one keymap, but
    ;; to a subkeymap in another keymap, so we record both the single command
    ;; and the children that take it as prefix, it may break lots of things, so
    ;; just signal an error and I'll think about how to fail gracefully later.
    ;; I've not got an error yet.
    ;; NOTE: this check costs some performance
    (let ((conflicts (cl-loop
                      with keys = (mapcar #'car alist)
                      for parent in (-uniq (-keep #'dei--parent-key keys))
                      when (assoc parent keys)
                      collect parent)))
      (when conflicts
        (error "Bound variously to commands or prefix maps: %s" conflicts)))
    alist))


;;;; Async worker
;; Because it would cause noticable lag to do everything at once, we slice up
;; the work into pieces and run them only while the user is not operating Emacs.
;; That's not true async, but it makes no difference to the user and it's easier
;; to debug than true async.

;; Persistent variables helpful for debugging
(defvar dei--stems nil)
(defvar dei--hydra-blueprints nil)

(defvar dei--buffer-under-analysis nil)
(defvar dei--flock-under-operation nil)
(defvar dei--async-chain nil)

;; REVIEW: Maybe just presuppose that the user sets things with Emacs 29's
;; `setopt'?  Lots of users won't, but we could detect a non-default value and
;; warn that `setopt' wasn't used.  Ehh, I already wrote this.
(defvar dei--last-settings-alist
  '((dei-hydra-keys)
    (dei-extra-heads)
    (dei-invisible-leafs)
    (dei-stemless-quitters)
    (dei-all-shifted-symbols)
    (dei-inserting-quitters))
  "User settings to record and watch for changes.
When they change, we want to carry out some re-calculations, but
we don't want to do them super-often so we skip doing them as
long as they don't change.")

;; Sidenote: If we had a deterministic hash function (`sxhash' is not
;; deterministic across restarts), we could even restore this variable from
;; disk.  Cool, eh?  It won't be necessary now, as the package is finally so
;; fast it doesn't even slow down my 2009 Eee PC.  Never thought it possible.
(defvar dei--flocks nil
  "Alist relating keymap composites with root hydras.
Actually it doesn't look at major or minor modes, but the
composite of all enabled keymaps in the buffer, which is nearly
the same thing.")

(defvar dei--async-chain-last-idle-value 0)
(defvar dei--async-chain-template
  #'(dei--async-2-check-settings
     dei--async-1-model-the-world
     dei--async-3-draw-blueprint
     dei--async-4-birth-hydra
     dei--async-5-register))

(defvar dei--async-running nil)

(defun dei--async-chomp (&optional politely)
  "Pop the next function off `dei--async-chain' and call it.
Rinse and repeat until user does something, in which case defer
to a short idle timer so that user is free to use Emacs.  Stop
only when `dei--async-chain' is empty or a keyboard quit occurs
during execution.

To start, see `dei-async-make-hydras'."
  (if (buffer-live-p dei--buffer-under-analysis)
      (let ((fun (car-safe dei--async-chain)))
        (condition-case err
            (progn
              ;; In case something called this function twice and it wasn't the
              ;; timer that did it (if the timer ran the function, it won't be
              ;; among the active timers while this body is executing, so the
              ;; error isn't tripped).
              (when (member (named-timer-get 'deianira-at-work) timer-list)
                (error "Timer was not cancelled before `dei--async-chomp'"))
              (setq dei--async-running t)
              (dei--echo "Running: %s" fun)
              (funcall fun t) ;; Real work happens here
              ;; Now that we know it exited without error, pop it off the queue
              (pop dei--async-chain)
              (when dei--async-chain
                (let ((polite-delay 1.0)
                      (idled-time (or (current-idle-time) 0)))
                  (if (or (and politely
                               (time-less-p polite-delay idled-time))
                          (and (not politely)
                               (time-less-p idled-time polite-delay) ;; unneccessary?
                               (time-less-p dei--async-chain-last-idle-value idled-time)))
                      ;; If user hasn't done anything since last chomp, go go go.
                      (named-timer-run 'deianira-at-work
                        0 nil #'dei--async-chomp)
                    ;; Otherwise allow time for user input.
                    ;; (cl-incf dei--async-chain-last-idle-value .7)
                    (named-timer-idle-run 'deianira-at-work
                      polite-delay nil #'dei--async-chomp 'politely))
                  (setq dei--async-chain-last-idle-value idled-time))))
          ((error quit)
           (dei--echo "Chain interrupted because: %s" err)
           (setq dei--async-running nil)
           (when (eq (car err) 'error)
             (error "Function %s failed: %s" fun (cdr err))))))
    (dei--echo "Canceling because buffer killed: %s" dei--buffer-under-analysis)
    (setq dei--async-chain nil))
  ;; The chain fully finished, so set nil.
  (unless dei--async-chain
    (setq dei--async-running nil)))

(defvar dei--current-hash 0)
(defvar dei--last-hash 0)

(cl-defstruct (dei--flock
               (:constructor dei--flock-record)
               (:copier nil))
  hash
  width
  funs
  vars
  bindings)

(defun dei--first-flock-by-hash (hash)
  "Return a flock with :hash HASH, among `dei--flocks'."
  (cl-loop for flock in dei--flocks
           when (eq hash (dei--flock-hash flock))
           return flock))

;; Note that this could be sped up with a hash table (faster for lists longer
;; than about 32 items; give each flock object an unique key contrived from
;; combining hash and width), but I doubt we have a bottleneck here.
(defun dei--flock-by-hash-and-width (hash width)
  "Return the flock with :hash HASH and :width WIDTH."
  (cl-loop for flock in dei--flocks
           when (and (eq hash (dei--flock-hash flock))
                     (eq width (dei--flock-width flock)))
           return flock))

(defvar dei--current-width (frame-width))
(defvar dei--current-bindings nil)
(defvar dei--last-bindings nil)

;; TODO: Since even 100ms seems not enough, maybe just set a variable akin to
;; dei--async-running.
(defun dei--react (&rest _)
  "Wait a moment, then call `dei-async-make-hydras' once.
Calling this function repeatedly within the wait period will
still only result in one call.  You can add this wrapper to
however many potentially co-occurring hooks you like, such as
`window-buffer-change-functions' and
`window-selection-change-functions'."
  (named-timer-run 'deianira-c .1 nil #'dei-async-make-hydras))

;; TODO: check if last hash&width are the same, and do nothing
(defun dei-async-make-hydras ()
  "Set hydras to something appropriate for the buffer.
If no such hydras exist, start asynchronously making them."
  (interactive)
  ;; Briefly reveal this monster variable.  (We keep it hidden in a separate
  ;; obarray most of the time because eldoc chokes for minutes if the person
  ;; reading this code accidentally places point on the symbol `dei--flocks'.
  ;; Written 1995, eldoc is overdue for a timeout mechanism.)
  (setq dei--flocks
        (symbol-value (obarray-get dei--hidden-obarray "dei--flocks")))
  ;; NOTE: Do not use the OLP argument of `current-active-maps'. It would look
  ;; up hydra's own uses of `set-transient-map', may risk mutual recursion.
  (let* ((hash (sxhash (current-active-maps)))
         (some-flock (dei--first-flock-by-hash hash)))
    ;; (if (and (equal dei--last-hash hash)
    ;;          (equal dei--last-width (frame-width)))
    ;;     (dei--echo "Same hash and frame width, doing nothing: %s" hash))
    ;; If we already made hydras for this keymap composite, restore from cache.
    (if some-flock
        (let ((found (dei--flock-by-hash-and-width hash (frame-width))))
          (if found
              (dei--echo "Flock exists, making it current: %s" hash)
            (dei--echo "Converting to %s chars wide: %s" (frame-width) hash)
            (setq found (dei--rehint-flock some-flock))
            (push found dei--flocks))
          ;; Point names to already made values.  It's fucking beautiful.
          (cl-loop for x in (dei--flock-funs found) do (fset (car x) (cdr x)))
          (cl-loop for x in (dei--flock-vars found) do (set (car x) (cdr x)))
          (setq dei--last-bindings (dei--flock-bindings found))
          (setq dei--last-hash hash))
      (if dei--async-running
          (dei--echo "Already running chain, letting it continue")
        (if (and dei--async-chain
                 (not (equal dei--async-chain dei--async-chain-template))
                 (buffer-live-p dei--buffer-under-analysis))
            (if (<= dei--interrupt-counter 3)
                (progn
                  ;; NOTE: The counter decays via `dei--interrupt-decrement-ctr'
                  (cl-incf dei--interrupt-counter)
                  (dei--echo "Chain had been interrupted, resuming")
                  (dei--async-chomp 'politely))
              (deianira-mode 0)
              (dei--echo
               (message "3 interrupts last 5 min, disabling deianira-mode")))
          (dei--echo "Launching new chain.  Previous value of `dei--async-chain': %s"
                     dei--async-chain)
          (named-timer-cancel 'deianira-at-work)
          ;; TODO: Use the recorded frame width everywhere so we're
          ;; consistent all the way to the last async stage.
          (setq dei--current-width (frame-width))
          (setq dei--buffer-under-analysis (current-buffer))
          (setq dei--current-hash hash)
          (setq dei--async-chain dei--async-chain-template)
          (setq dei--async-chain-last-idle-value 0) ;; so chomp won't wait
          (dei--async-chomp)))
      (unless dei--async-running
        (set (obarray-put dei--hidden-obarray "dei--flocks") dei--flocks)
        (obarray-remove obarray "dei--flocks")))))

(defun dei--prefix-to-stem (keydesc)
  "Add a space to the end of KEYDESC.
Trivial function, but useful for `mapcar' and friends."
  (declare (pure t) (side-effect-free t))
  (concat keydesc " "))

(defvar dei--changed-stems nil)

(defun dei--async-1-model-the-world (&optional _)
  "Calculate things."
  (with-current-buffer dei--buffer-under-analysis
    ;; Cache settings
    (setq dei--all-shifted-symbols-list (dei--all-shifted-symbols-list-recalc)
          dei--hydra-keys-list (dei--hydra-keys-list-recalc)
          dei--colwidth (dei--colwidth-recalc)
          dei--filler (dei--filler-recalc))

    ;; Figure out dei--hydrable-prefix-keys, to lookup when making hydra-heads.
    ;; Also used to figure out dei--new-or-changed-stems.
    (setq dei--hydrable-prefix-keys
          (-uniq
           (cl-loop
            ;; Since `dei--get-filtered-bindings' returns no prefixes, infer them
            ;; by cutting the last key off each sequence.
            for key in (-uniq (-keep #'dei--parent-key (dei--get-filtered-bindings)))
            ;; Add ancestors (if we found C-c c p, count C-c c and C-c too).
            as n = (length (split-string key " "))
            append (-iterate #'dei--parent-key key n))))

    ;; Figure out which stems have changed, so we can exploit the previous
    ;; flock's work and skip running defhydra for results we know will be
    ;; identical.  This has a TREMENDOUS performance boost in most buffers,
    ;; cutting 30 seconds of computation down to 0-2 seconds.
    ;;
    ;; Visualize a Venn diagram, with two circles for the LAST and CURRENT
    ;; bindings. The defunct are somewhere in LAST, the new somewhere in CURRENT,
    ;; but neither are in the circles' intersection (overlapping area), which
    ;; should never be relevant to update, as they represent cases where the
    ;; key's definition didn't change.

    ;; TODO: have it return subkeymaps too just to be sure we really update.
    ;; In emacs-lisp-mode, I'm getting a Cxn stem that comes from org-mode,
    ;; because I have no equivalent prefix in emacs-lisp-mode currently.
    ;; Although that issue should anyway not happen if C-x n is bound to a
    ;; command -- there is an issue there (it was bound to an empty keymap, now
    ;; fixed).  Still, it casts light on how this algo really works, and I
    ;; think we need a flowchart before we remove this comment, just to ensure
    ;; that all edge cases will be taken care of.
    (setq dei--current-bindings (dei--get-filtered-bindings-with-commands))
    ;; The price of rigor: even if only C-c c p 4 changed, we'll have to
    ;; rebuild hydras for C-c, C-c c, and C-c c p, because it may be that each
    ;; of these levels previously didn't exist but for that last binding (rare,
    ;; but happens). TODO: Figure out a way to skip that most of the time
    (setq dei--changed-stems
          (cl-loop for stem in (->> dei--hydrable-prefix-keys
                                    (-map #'dei--prefix-to-stem)
                                    (append '("C-" "M-" "s-" "H-" "A-"))
                                    (-uniq))
                   with new-bindings =
                   (-uniq (append (-difference dei--current-bindings
                                               dei--last-bindings)
                                  (-difference dei--last-bindings
                                               dei--current-bindings)))
                   with new-stems = '()
                   as it = (cl-loop
                            for binding in new-bindings
                            when (string-prefix-p stem (car binding))
                            return stem)
                   when it collect it into new-stems
                   ;; Sort to avail most relevant hydras soonest.  Longest
                   ;; first now means shortest first once we get to stage 6.
                   finally return
                   (seq-sort-by #'dei--key-seq-steps-length #'> new-stems)))

    ;; Clear the workbench from a previous done or half-done iteration.
    (setq dei--hydra-blueprints nil)))

(defun dei--async-2-check-settings (&optional _)
  "Signal error if any two user settings overlap.
Otherwise we could end up with two heads in one hydra both bound
to the same key, no bueno."
  (unless (--all-p (equal (symbol-value (car it)) (cdr it))
                   dei--last-settings-alist)
    (let ((vars
           (list (cons 'dei-hydra-keys dei--hydra-keys-list)
                 (cons 'dei-all-shifted-symbols dei--all-shifted-symbols-list)
                 (cons 'dei-invisible-leafs dei-invisible-leafs)
                 (cons 'dei-stemless-quitters dei-stemless-quitters)
                 (cons 'dei-inserting-quitters dei-inserting-quitters)
                 (cons 'dei-extra-heads (mapcar #'car dei-extra-heads)))))
      (while vars
        (let ((var (pop vars)))
          (cl-loop
           for remaining-var in vars
           do (when-let ((overlap (-intersection (cdr var)
                                                 (cdr remaining-var))))
                (error "Found %s in both %s and %s"
                       overlap (car var) (car remaining-var)))))))
    ;; Record the newest setting values, so we can skip the relatively
    ;; expensive calculations next time if nothing changed.
    (setq dei--last-settings-alist
          (cl-loop for cell in dei--last-settings-alist
                   collect (cons (car cell) (symbol-value (car cell)))))))

(defun dei--async-3-draw-blueprint (&optional chainp)
  "Draw blueprint for one of `dei--stems'.
In other words, we compute all the arguments we'll later pass to
`defhydra'.  Pop a stem off the list `dei--stems', transmute it into
a blueprint pushed onto the list `dei--hydra-blueprints'.

With CHAINP non-nil, add another invocation of this function to
the front of `dei--async-chain', until `dei--stems' is empty."
  (unwind-protect
      (when dei--changed-stems
        (with-current-buffer dei--buffer-under-analysis
          (let* ((stem (car dei--changed-stems))
                 (name (dei--dub-hydra-from-key-or-stem stem))
                 (h1 (dei--specify-hydra stem (concat name "-nonum") t))
                 (h2 (dei--specify-hydra stem name)))
            (push h1 dei--hydra-blueprints)
            (push h2 dei--hydra-blueprints)
            ;; Now we can pop the list. In the unlikely case a C-g interrupts
            ;; execution before here, it'll lead to redundant computation but
            ;; not break stuff.
            (pop dei--changed-stems))))
    ;; Run again if more to do
    (and chainp
         dei--changed-stems
         (push #'dei--async-3-draw-blueprint dei--async-chain))))

(defun dei--async-4-birth-hydra (&optional chainp)
  "Pass a blueprint to `defhydra', turning dry-run into wet-run.
Each invocation pops one blueprint off `dei--hydra-blueprints'.

With CHAINP non-nil, add another invocation of this function to
the front of `dei--async-chain', so that we repeat until
`dei--hydra-blueprints' empty."
  (unwind-protect
      (when dei--hydra-blueprints
        (with-current-buffer dei--buffer-under-analysis
          (let ((blueprint (car dei--hydra-blueprints)))
            (unless blueprint
              (error "Blueprint should not be nil"))
            (dei--try-birth-hydra (car blueprint) (cdr blueprint))
            ;; Turn the dynamic sexp into a static string, since there is
            ;; nothing dynamic in the input anyway.
            (let ((hint-sym (intern-soft (concat (car blueprint) "/hint"))))
              (set hint-sym (eval (eval hint-sym))))
            (pop dei--hydra-blueprints))))
    ;; Run again if more to do
    (and chainp
         dei--hydra-blueprints
         (push #'dei--async-4-birth-hydra dei--async-chain))))

;; TODO: figure out if it's necessary to respecify heads due to changed
;; dei--filler and if so how to get the effect here (theory: we don't need filler
;; to be more than one char, I think it was for gracefully degrading from a
;; weird issue) (theory: we see no effect when starting with a narrow frame and
;; rehint for a big frame, but the other way around could get messy)
(defun dei--rehint-flock (flock)
  "Return copy of FLOCK with hints updated to match framewidth."
  (let* ((dei--colwidth (dei--colwidth-recalc))
         (dei--filler (dei--filler-recalc))
         (new (copy-sequence flock)))
    (setf (dei--flock-width new) (frame-width))
    (setf (dei--flock-vars new)
          (cl-loop
           for pair in (dei--flock-vars flock)
           when (string-suffix-p "/hint" (symbol-name (car pair)))
           collect
           (let ((basename (substring (symbol-name (car pair)) 0 -5)))
             (cons (car pair)
                   ;; NOTE: here we actually eval the output of hydra--format
                   ;; to get a static string instead of a sexp, in hopes of
                   ;; ameliorating the performance issue in `lv-message'.
                   (eval (hydra--format
                          ;; These arguments will be identical to what `defhydra'
                          ;; passed to `hydra--format' the first time around. The
                          ;; difference comes from let-binding `dei--colwidth'.
                          basename
                          (eval (intern-soft (concat basename "/params")))
                          (eval (intern-soft (concat basename "/docstring")))
                          (eval (intern-soft (concat basename "/heads")))))))
           else collect pair))
    new))

;; Test
;; (--filter (string-suffix-p "/hint" (symbol-name (car it)))
;;           (caddar dei--flocks))
;; (setq bar (dei--rehint-flock (car dei--flocks)))
;; (--filter (string-suffix-p "/hint" (symbol-name (car it)))
;;           (caddr bar))

(defun dei--async-5-register (&optional _)
  "Record the hydras made under this keymap combination."
  ;; (when (assoc dei--current-hash dei--flocks)
    ;; (error "Hash already recorded: %s" dei--current-hash))
  (let (funs vars)
    ;; All symbols generated by `defhydra' contain a slash in the name, so just
    ;; collect all such names from the global obarray.  Note that this will
    ;; also catch syms no longer used by the current flock, which is slightly
    ;; inefficient as this extra \"cream on top\" per flock builds up the more
    ;; flocks we make.  I expect it won't add up to even one doubling of work,
    ;; so it's fine.  An alternative is patching `defhydra' to list for us what
    ;; it made.
    (mapatoms
     (lambda (sym)
       (when (string-match-p "dei-.*?/" (symbol-name sym))
         (when (fboundp sym)
           (push (cons sym (symbol-function sym)) funs))
         (when (boundp sym)
           (push (cons sym (symbol-value sym)) vars)))))
    ;; (frame-parameter (window-frame
    ;;                   (get-buffer-window
    ;;                    dei--buffer-under-analysis)) 'width)
    (push (dei--flock-record
           :hash dei--current-hash
           :width dei--current-width
           :funs funs
           :vars vars
           :bindings dei--current-bindings)
          dei--flocks)
    (dei--echo "Flock #%s born: %s" (length dei--flocks) dei--current-hash)
    (setq dei--last-bindings dei--current-bindings)
    (setq dei--last-hash dei--current-hash)
    ;; Now hide this monster variable.  See other use of `dei--hidden-obarray'.
    (set (obarray-put dei--hidden-obarray "dei--flocks") dei--flocks)
    (obarray-remove obarray "dei--flocks")))


;;;; Bonus functions for mass remaps

(defcustom dei-keymap-found-hook nil
  "Run after adding one or more keymaps to `dei--known-keymaps'.
See `dei--record-keymap-maybe', which runs this hook.

You may be interested in adding some of these functions:

- `dei-homogenize-all-keymaps'

and one, but obviously ONLY ONE, of:

- `dei-define-super-like-ctl-everywhere'
- `dei-define-super-like-ctlmeta-everywhere'"
  :type 'hook
  :group 'deianira)

(defun dei--raw-keymap (map)
  "If MAP is a keymap, return it, and if a symbol, evaluate it."
  (if (keymapp map)
      map
    (if (symbolp map)
        (symbol-value map)
      (error "Not a keymap or keymap name: %s" map))))

(defvar dei--known-keymaps '(global-map)
  "List of named keymaps seen while `deianira-mode' was active.")

(defvar dei--buffer-keymaps (make-hash-table))

(defun dei--record-keymap-maybe (&optional _)
  "If Emacs has seen new keymaps, record them in a variable.
This simply evaluates `current-active-maps' and adds to
`dei--known-keymaps' anything not already there."
  (let ((maps (current-active-maps))
        (remembered-maps (gethash (buffer-name) dei--buffer-keymaps)))
    ;; Make sure we only run the expensive `help-fns-find-keymap-name' once for
    ;; this buffer, unless the keymaps actually have changed.  In addition, use
    ;; the buffer name as a reference (instead of a buffer-local variable as
    ;; might be more logical), because some packages work by creating and
    ;; destroying the same buffer repeatedly.
    (unless (and remembered-maps
                 (equal maps remembered-maps))
      (puthash (buffer-name) maps dei--buffer-keymaps)
      (when-let* ((named-maps (-uniq (-keep #'help-fns-find-keymap-name maps)))
                  (new-maps (-difference named-maps dei--known-keymaps)))
        (setq dei--known-keymaps (append new-maps dei--known-keymaps))
        (run-hooks 'dei-keymap-found-hook)))))

;;; Reflecting one stem in another

(defvar dei--reflect-actions nil
  "List of actions to pass to `define-key'.")

(defvar dei--super-reflected-keymaps nil
  "List of keymaps worked on by `dei-define-super-like-ctl-everywhere'.")

(defvar dei--ctlmeta-reflected-keymaps nil
  "List of keymaps worked on by `dei-define-super-like-ctlmeta-everywhere'.")

;; NOTE: Below function creates many superfluous mixed bindings like
;; C-x s-k C-t
;; C-x s-k s-t
;; s-x s-k C-t
;; s-x C-k C-t
;; s-x C-k s-t
;;
;; The reason: it's because s-x is bound simply to Control-X-prefix, a keymap.
;; In addition, C-x is also bound to that keymap. Inside that keymap you can
;; find the key C-k and now also s-k...
;;
;; The way keymaps are designed, it's not possible to bind only C-x C-k C-t and
;; s-x s-k s-t, at least when the prefixes within these key descriptions bind
;; to the same subkeymaps.  Binding both of these keys means binding every
;; possible combination.  It's annoying in describe-keymap output, but it is
;; possible to hide the strange combinations from the which-key popup, see
;; README.  It may be possible to use `copy-keymap' in order to avoid this, but
;; it has no impact on the user so I'll let it be.
(defun dei--define-super-like-ctl-in-keymap* (map)
  (map-keymap
   (lambda (event definition)
     (let ((key (key-description (vector event))))
       (when (string-search "C-" key)
         (unless (string-search "s-" key)
           (push (list map (string-replace "C-" "s-" key) definition)
                 dei--reflect-actions)
           (when (keymapp definition)
             ;; Recurse!
             ;; TODO: use `copy-keymap'
             (dei--define-super-like-ctl-in-keymap definition))))))
   (dei--raw-keymap map)))

;; (defun dei--define-super-like-ctl-in-keymap (map)
;;   (cl-loop
;;    for key being the key-seqs of (dei--raw-keymap map)
;;    do (if (string-search " " key)
;;           )
;;            ))

;; A variant that uses `copy-keymap' trick.
;; Q: Should we use map inheritance? See `copy-keymap'.
(defun dei--define-super-like-ctl-in-keymap (map &optional preserve)
  (map-keymap
   (lambda (event definition)
     (let ((key (key-description (vector event))))
       (when (string-search "C-" key)
         (unless (string-search "s-" key)
           (let ((ctlkey key)
                 (superkey (string-replace "C-" "s-" key)))
             (if (keymapp definition)
                 (let ((new-submap (copy-keymap definition)))
                   ;; Recurse!
                   (dei--define-super-like-ctl-in-keymap new-submap)
                   (push (list map superkey new-submap) dei--reflect-actions))
               (push (list map superkey definition) dei--reflect-actions))
             (unless preserve
               (push (list map ctlkey nil) dei--reflect-actions)))))))
   (dei--raw-keymap map)))

;; (dei--define-super-like-ctl-in-keymap vertico-map t)
;; (dei--reflect-actions-execute)
;; (setq dei--reflect-actions nil)

;; Variant that uses map inheritance.
(defun dei--define-super-like-ctl-in-keymap (map &optional preserve)
  (map-keymap
   (lambda (event definition)
     (let ((key (key-description (vector event))))
       (when (string-search "C-" key)
         (unless (string-search "s-" key)
           (let ((ctlkey key)
                 (superkey (string-replace "C-" "s-" key)))
             (if (keymapp definition)
                 (let ((new-submap (make-sparse-keymap))
                       (map-name (help-fns-find-keymap-name definition)))
                   (set-keymap-parent new-submap definition)
                   ;; Give it a new name
                   ;;(when map-name
                   ;;  (set (intern (concat "dei-super-" (symbol-name map-name)))
                   ;;       new-submap))
                   ;; Recurse!
                   (dei--define-super-like-ctl-in-keymap new-submap)
                   (push (list map superkey new-submap) dei--reflect-actions))
               (push (list map superkey definition) dei--reflect-actions))
             (unless preserve
               (push (list map ctlkey nil) dei--reflect-actions)))))))
   (dei--raw-keymap map)))

(defun dei--define-super-like-ctlmeta-in-keymap (map)
  (map-keymap
   (lambda (event definition)
     (let ((key (key-description (vector event))))
       (when (string-search "C-M-" key)
         (unless (string-search "s-" key)
           (push (list map (string-replace "C-M-" "s-" key) definition)
                 dei--reflect-actions)
           (when (keymapp definition)
             ;; Recurse!
             (dei--define-super-like-ctlmeta-in-keymap definition))))))
   (dei--raw-keymap map)))

;; TODO: Insert customizable exceptions such that we don't bring over
;; TAB/RET/ESC/g as s-i/s-m/s-[/s-g unless user chooses it. Ideally, we would
;; also react to every package that binds e.g. RET, and relocate that binding
;; to <return>, but it's impossible to see after the fact whether the package
;; author wrote it as RET or as C-m and therefore how they thought of it.  I
;; guess we can count both as intended for the Return key.  Then, on
;; keymap-found-hook, we always move this binding to <return>.  Only if
;; <return> already exists within the same keymap, and C-m is bound to
;; something different, we might consider duplicating C-m to s-m.  Otherwise,
;; whatever the user chooses for C-m or s-m in the global map should tend to
;; stick.
;;
;; Note that using Super also has the comes-for-free benefit of allowing you to
;; homogenize s-x g to s-x s-g without making you go woops i typed C-g
;; (keyboard-quit), ditto for m, i and [.  Mind you that homogenizing should be
;; done directly on the Super bindings /after/ making them reflect the Control
;; bindings.
(defun dei-define-super-like-ctl-everywhere ()
  (cl-loop for map in (-difference dei--known-keymaps
                                   dei--super-reflected-keymaps)
           do (progn
                (dei--define-super-like-ctl-in-keymap map t)
                (dei--reflect-actions-execute)
                (push map dei--super-reflected-keymaps))))

(defun dei-define-super-like-ctlmeta-everywhere ()
  (cl-loop for map in (-difference dei--known-keymaps
                                   dei--ctlmeta-reflected-keymaps)
           do (progn
                (dei--define-super-like-ctlmeta-in-keymap map)
                (push map dei--ctlmeta-reflected-keymaps))
           finally (dei--reflect-actions-execute)))

(defun dei--reflect-actions-execute ()
  (cl-loop for arglist in dei--reflect-actions
           do (seq-let (map key def) arglist
                (apply #'define-key
                       (dei--raw-keymap map)
                       (kbd key)
                       def
                       ;; Remove the binding entirely
                       (and (null def)
                            (version<= "29" emacs-version)
                            t))

                ;; Known working in Emacs 29
                ;; (define-key (dei--raw-keymap map)
                ;;   (kbd key)
                ;;   def
                ;;   (when (null def)
                ;;     t))

                )
           finally (setq dei--reflect-actions nil)))

;; (dei-define-super-like-ctl-everywhere)
;; (dei--update-super-reflection-2)
;; (dei--record-keymap-maybe nil)

;;; Homogenizing

(defcustom dei-permachord-wins-homogenizing nil
  "Non-nil if perma-chord should win over chord-once.
This means that the behavior of a perma-chorded sequence such as
C-x C-k C-e will be kept as is, and cloned to the chord-once
sequence C-x k e, overwriting any binding there, so they both do
what the former always did.  If the setting is nil, both will
instead do what C-x k e always did.

PLEASE NOTE that any calls to `define-key' or similar in your
initfiles have to take this into account!  If this is t, all
sequences have to be written in perma-chord style to be sure the
choice sticks.  If this is nil, they have to be written in the
chord-once style to be sure the choice sticks.  (Of course, you
can always bind both while you experiment with this setting.)

This variable sets the default approach, but you can override
specific cases in `dei-homogenizing-winners'; and if only one
variety is bound but not the other, the binding will be cloned no
matter the direction since there is no contest."
  :type 'boolean
  :group 'deianira)

(defcustom dei-homogenizing-winners '()
  "Alist of keys that always win the homogenizing battle.
See `dei-permachord-wins-homogenizing' for an explanation of
homogenizing.

Each item has the format (KEY-OR-COMMAND . KEYMAP).  See the
package readme for how a full alist may look.

KEY-OR-COMMAND can be either a `kbd'-compatible key description
or a symbol which is assumed to refer to a command.
In the event that you add e.g. both (\"C-x C-f\") and
(set-fill-column), normally a binding of C-x f, the first item
wins, so C-x f will be bound to find-file regardless.

If KEYMAP is nil, apply the winner in whichever keymap where it
or its (perma-chord or chord-once) sibling is found.  If non-nil,
it should be a major or minor mode map.  It will likely have no
effect if it is a named so-called prefix command such as
Control-X-prefix or kmacro-keymap (you can find these with
describe-function, whereas you can't find org-mode-map, as that's
a proper mode map)."
  :type '(repeat (cons sexp symbol))
  :group 'deianira
  :set
  (lambda (var new)
    (set-default
     var (cl-loop for cell in new
                  collect (cons (if (stringp (car cell))
                                    (key-description (kbd (car cell)))
                                  (car cell))
                                (cdr cell))))))

(defun dei--key-seq-has-non-prefix-in-prefix (keymap keydesc)
  "Does KEYDESC contain a bound command in its prefix?
For example: C-x C-v is bound to a command by default, so if you
attempt to bind C-x C-v C-x to a command, you get an error.  So
here we check for that.  If KEYDESC is C-x C-v C-x, we check if
either C-x or C-x C-v are bound to anything.

Does not additionally check that KEYDESC is not itself a prefix
map with children bound: that's another thing that can make KEYDESC
unbindable.

KEYMAP is the keymap in which to look."
  (let ((steps (dei--key-seq-split keydesc))
        (ret nil))
    ;; Don't proceed if no subseqs
    (when (> (length steps) 1)
      ;; TODO: don't use dotimes, but some "until" structure
      (dotimes (i (- (length steps) 1))
        (let ((subseq (string-join (-take (1+ i) steps) " ")))
          (when (lookup-key-ignore-too-long keymap (kbd subseq)))
            (push subseq ret)))
      (car ret))))
;; (dei--key-seq-has-non-prefix-in-prefix global-map "C-x C-v C-x")

(defvar dei--homogenized-keymaps nil)

(defvar dei--remap-record nil
  "Record of work done.")

(defvar dei--remap-actions nil
  "List of actions to pass to `define-key'.")

(defun dei--nightmare-p (keydesc)
  "Non-nil if homogenizing KEYDESC would cause bugs.
This has to do with e.g. C-x \[ becoming C-x C-\[, which is the
same as C-x ESC, which is the same as C-x M-anything.  You do
this, then Magit tries to bind C-x M-g and complains it's not a
prefix key and you can't even load Magit.  That's mild issue.  A
more silent bug is C-x i becoming C-x C-i which goes ahead and
overrides your C-x TAB binding without signaling anything even to
the developer.

The problem is anachronistic Unix control character behavior,
which Emacs could deprecate but has so far chosen not to, for the
sake of functioning inside basic terminal emulators.  I'm not
saying they should deprecate it, we have a clean solution in
`dei-define-super-like-ctl-everywhere' and never typing another
control character.

If you don't, it pays to know this: always bind <tab> instead of
TAB, <return> instead of RET, and <escape> instead of ESC.  GUI
Emacs always looks up these if bound, and only falls back to the
control character if these function keys are unbound.  They do
not work on the terminal/TTY, but neither does Super or many
other niceties.  This will still not let you use C-\[ as anything
other than an ESC shorthand, but you'll be able to bind C-m and
C-i without clobbering the Tab or Return keys.  You may still
encounter issues with packages either binding RET/TAB, ruining
your C-m/C-i, and/or not thinking to bind <return>/<tab> so that
you have to add them yourself.

As an additional nicety, we avoid touching key sequences
involving Control and \"g\" so as to prevent clobbering the
meaning of \"C-g\".  Otherwise, homogenizing C-h g binds C-h C-g
to the same, creating a situation when C-g is not available for
`keyboard-quit'."
  (declare (pure t) (side-effect-free t))
  (and (string-prefix-p "C-" keydesc)
       (string-match-p (rx (any "[" "m" "i" "g")) keydesc)))

;; TODO: Maybe just return the action, and let the caller push onto
;; external variables if they wish.
(defun dei--homogenize-key-in-keymap (this-key keymap)
  "In KEYMAP, homogenize THIS-KEY.
See `dei-permachord-wins-homogenizing' for explanation.

Actually just pushes an action onto `dei--remap-actions', which
you can preview with \\[dei-remap-actions-preview] and execute
with \\[dei-remap-actions-execute]."
  (unless (stringp this-key)
    (error "String not passed: %s" this-key))
  (and
   (not (dei--key-seq-steps=1 this-key)) ;; nothing to homogenize if length 1
   (not (dei--nightmare-p this-key))
   (dei--key-starts-with-modifier this-key)
   (let* ((raw-keymap (dei--raw-keymap keymap))
          (this-cmd (lookup-key-ignore-too-long raw-keymap (kbd this-key))))
      ;; REVIEW: what do if this-cmd is another keymap?
      (when (functionp this-cmd)
        (let* (;; NOTE: we are assuming there exist no "bastard sequences",
               ;; so we don't bother to ensure the alternative is a chordonce.
               (this-is-permachord (dei--key-seq-is-permachord this-key))
               (permachord-key
                (if this-is-permachord
                    this-key
                  (dei--ensure-permachord this-key)))
               (permachord-cmd
                (if this-is-permachord
                    this-cmd
                  (lookup-key-ignore-too-long raw-keymap (kbd permachord-key))))
               (chordonce-key
                (if this-is-permachord
                    (dei--ensure-chordonce this-key)
                  this-key))
               (chordonce-cmd
                (if this-is-permachord
                    (lookup-key-ignore-too-long raw-keymap (kbd chordonce-key))
                  this-cmd))
               (sibling-keydesc (if this-is-permachord
                                    chordonce-key
                                  permachord-key))
               (sibling-cmd (if this-is-permachord
                                chordonce-cmd
                              permachord-cmd))
               (winners (->> dei-homogenizing-winners
                             (-remove #'cdr) ;; drop items with a keymap
                             (-map #'car)))
               (winners-for-this-keymap (->> dei-homogenizing-winners
                                             (-filter #'cdr)
                                             (--filter (equal keymap (cdr it)))
                                             (-map #'car)))
               (action nil))

          (cond
           ;; Simple case: This key or the sibling key has already been dealt
           ;; with.  Then we just no-op.  This approach is
           ;; simpler than naively populating `dei--remap-actions' and teasing out
           ;; duplicates and conflicts afterwards.
           ;; ((or (when-let ((found
           ;;                  (assoc (kbd this-key) dei--remap-record)))
           ;;        (equal (nth 2 found) keymap))
           ;;      (when-let ((found
           ;;                  (assoc (kbd sibling-keydesc) dei--remap-record)))
           ;;        (equal (nth 2 found) keymap))))
           ((equal keymap (nth 2 (or (assoc sibling-keydesc dei--remap-record)
                                     (assoc this-key dei--remap-record)))))

           ;; Complex case #1: both keys have a command, which do we choose?
           ;; Eeny meny miny moe...?  No.  Let's start by checking if one of
           ;; them is a specified winner, then fall back on a rule.
           ((functionp sibling-cmd)
            (cond ((< 1 (length
                         (-non-nil
                          (list (member sibling-keydesc winners-for-this-keymap)
                                (member sibling-cmd winners-for-this-keymap)
                                (member this-key winners-for-this-keymap)
                                (member this-cmd winners-for-this-keymap)))))
                   (warn "Found a contradiction in dei-homogenizing-winners."))
                  ((or (member sibling-keydesc winners-for-this-keymap)
                       (member sibling-cmd winners-for-this-keymap))
                   (setq action
                         (list this-key
                               sibling-cmd
                               keymap
                               "Clone winning sibling to overwrite this   "
                               this-cmd)))
                  ((or (member this-key winners-for-this-keymap)
                       (member this-cmd winners-for-this-keymap))
                   (setq action
                         (list sibling-keydesc
                               this-cmd
                               keymap
                               "Clone this winner to overwrite sibling    "
                               sibling-cmd)))
                  ((< 1 (length
                         (-non-nil
                          (list (member sibling-keydesc winners)
                                (member sibling-cmd winners)
                                (member this-key winners)
                                (member this-cmd winners)))))
                   ;; Leave it on the user to fix this mess.
                   (warn "Found a contradiction in dei-homogenizing-winners."))
                  ((or (member sibling-keydesc winners)
                       (member sibling-cmd winners))
                   (setq action
                         (list this-key
                               sibling-cmd
                               keymap
                               "Clone winning sibling to overwrite this   "
                               this-cmd)))
                  ((or (member this-key winners)
                       (member this-cmd winners))
                   (setq action
                         (list sibling-keydesc
                               this-cmd
                               keymap
                               "Clone this winner to overwrite sibling    "
                               sibling-cmd)))
                  ;; Neither key and neither command is rigged to win, so fall
                  ;; back on some simple rule.
                  ;;
                  ;; NOTE that here is the only place where this user option
                  ;; comes into play!  You'd think it'd affect more code, but
                  ;; this monster defun would be necessary even without the
                  ;; user option.
                  (dei-permachord-wins-homogenizing
                   (setq action
                         (list chordonce-key
                               permachord-cmd
                               keymap
                               "Clone perma-chord to chord-once           "
                               chordonce-cmd)))
                  (t
                   (setq action
                         (list permachord-key
                               chordonce-cmd
                               keymap
                               "Clone chord-once to perma-chord           "
                               permachord-cmd)))))

           ;; ;; We ended up here due to this type of situation: there exists a key
           ;; ;; C-x v x, and there exists a key C-x C-v (this-key).  Meet
           ;; ;; failure if cloning C-x C-v to the sibling C-x v.
           ;;  ((or (keymapp sibling-cmd)
           ;;            (boundp sibling-cmd)
           ;;       (and (symbolp sibling-cmd)
           ;;            (keymapp (symbol-value sibling-cmd))))
           ;;   )

           ;; ;; We ended up here due to this type of situation: there exists a key
           ;; ;; C-x v x (this-key), and there exists key C-x C-v bound directly
           ;; ;; to a command.  Meet failure if cloning to the sibling C-x C-v C-x.
           ;; ((dei--key-seq-has-non-prefix-in-prefix sibling-keydesc)
           ;;  )

           ;; Simple case: only one of the two is bound, so just duplicate.  We
           ;; don't need to do it both directions, b/c this invocation is
           ;; operating on this-key which must be the one known to have a
           ;; command.
           ((null sibling-cmd)
            (setq action (list sibling-keydesc
                               this-cmd
                               keymap
                               "Clone to unbound sibling                  "
                               sibling-cmd)))
           ;; Default.  This will also, via dei-remap-actions-execute, unbind the
           ;; key seqs that would've blocked us from proceeding.
           (t
            (setq action
                  (list permachord-key
                        chordonce-cmd
                        keymap
                        "Clone chord-once to perma-chord           "
                        permachord-cmd))))
          (when (and action
                     (not (member action dei--remap-record)))
            (push action dei--remap-actions)
            (push action dei--remap-record)))))))

;; TODO: Merge with the other unnest function
(defun dei--unnest-keymap-for-homogenizing (map &optional avoid-prefixes)
  "Return MAP as a list of key seqs instead of a tree of keymaps.
These key seqs are strings satisfying `key-valid-p'.
AVOID-PREFIXES is a list of prefixes to leave out of the result."
  (cl-loop for x being the key-seqs of (dei--raw-keymap map)
           using (key-bindings cmd)
           as key = (key-description x)
           with cleaned = (member map dei--cleaned-maps)
           unless
           (or (member cmd '(nil
                             self-insert-command
                             ignore
                             ignore-event
                             company-ignore))
               (string-match-p dei--ignore-keys-regexp key)
               (string-search "backspace" key)
               (string-search "DEL" key)
               (unless cleaned
                 (dei--key-is-illegal key))
               (cl-loop for prefix in (or avoid-prefixes
                                          dei--unnest-avoid-prefixes)
                        when (string-prefix-p prefix key)
                        return t))
           collect key))

;; TODO: some way to catch when this doesn't do anything and it should
(defun dei--homogenize-keymap (map)
  "Homogenize most of keymap MAP."
  (message "Keys rebound: %s"
           (cl-loop for key in (dei--unnest-keymap-for-homogenizing map)
                    when (dei--homogenize-key-in-keymap key map)
                    count key)))

(defun dei--echo-remap-actions ()
  (let ((buf (get-buffer-create "*Deianira remaps*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (goto-char (point-max))
      (let ((sorted-actions
             (cl-sort dei--remap-actions #'string-lessp
                      :key (lambda (x) (nth 0 x)))))
        (dolist (item sorted-actions)
          (seq-let (keydesc cmd map hint old) item
            (insert
             (if (symbolp map)
                 (concat "(" (symbol-name map) ")\t\t ")
               "")
             hint
             " Bind  " keydesc
             "\tto  " (if (symbolp cmd)
                          (symbol-name cmd)
                        (if (keymapp cmd)
                            (if-let ((named (help-fns-find-keymap-name cmd)))
                                (symbol-name named)
                              "(some sub-keymap)")
                          cmd))
             (if (and old (symbolp old))
                 (concat "\t ... was " (symbol-name old))
               "")))
          (newline))))))

(defun dei-homogenize-all-keymaps ()
  (cl-loop for map in (-difference dei--known-keymaps dei--homogenized-keymaps)
           do (progn
                (dei--homogenize-keymap map)
                (if (member map dei--homogenized-keymaps)
                    (warn "Keymap already homogenized, doing again: %s" map)
                  (push map dei--homogenized-keymaps)))
           finally (progn
                     (dei-remap-actions-execute dei--remap-actions)
                     (dei--echo-remap-actions)
                     (setq dei--remap-actions nil))))

(defun dei-homogenize-all-keymaps-dry-run ()
  (interactive)
  (cl-loop for map in (-difference dei--known-keymaps dei--homogenized-keymaps)
           do (dei--homogenize-keymap map)
           finally (message "%s %s"
                            "Inspect with dei-remap-actions-preview and"
                            "make a wet-run with dei-remap-actions-execute.")))

;; (define-key doom-leader-map (kbd "o b j") #'self-insert-command)
(defun dei-remap-actions-execute (actions)
  "Carry out remaps specified by ACTIONS.
Interactively, use the value of `dei--remap-actions'."
  (interactive (list dei--remap-actions))
  (dolist (action actions)
    (seq-let (keydesc cmd map _ _) action
      (let* ((raw-keymap (dei--raw-keymap map))
             (old-def (lookup-key-ignore-too-long raw-keymap (kbd keydesc))))
        ;; DEPRECATED (causes bug) Unbind things that are in the way of the new definition
        ;; (when (keymapp old-def)
        ;;   (dei--destroy-keymap old-def))
        (when-let* ((conflict-prefix (dei--key-seq-has-non-prefix-in-prefix raw-keymap keydesc))
                    (conflict-def (lookup-key-ignore-too-long raw-keymap (kbd conflict-prefix))))
          (unless (keymapp conflict-def)
            ;; If it's a keymap, we'll be perfectly able to bind our key. If
            ;; not a keymap, we must unbind it. (prevent error "Key sequence
            ;; starts with non-prefix key")
            (apply #'define-key raw-keymap (kbd conflict-prefix) nil (version<= "29" emacs-version))
            ;; known working in emacs 29
            ;; (define-key raw-keymap (kbd conflict-prefix) nil t)
            ))
        (define-key raw-keymap (kbd keydesc) cmd)))))


;;;; Debug toolkit

;; FIXME
(defun dei-regenerate-hydras-for-this-buffer ()
  (interactive)
  (let ((hash (sxhash (current-active-maps))))
    (setq dei--flocks (map-delete dei--flocks hash))
    (dei-async-make-hydras)))

;; FIXME
(defun dei--reset ()
  (interactive)
  (setq dei--async-chain nil)
  (setq dei--async-running nil)
  (named-timer-cancel 'deianira-at-work))

(defun dei-remap-actions-preview (&optional silently)
  "For convenience while debugging."
  (interactive)
  (let ((buf (get-buffer-create "*Deianira remaps*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (goto-char (point-min))
      (newline)
      (insert "Remap actions planned as of " (current-time-string))
      (newline 2)
      (let ((sorted-actions (cl-sort dei--remap-actions #'string-lessp
                                     :key (lambda (x)
                                            (if (symbolp (nth 2 x))
                                                (symbol-name (nth 2 x))
                                              "unknown keymap")))))
        (dolist (item sorted-actions)
          (seq-let (keydesc cmd map hint) item
            (insert hint
                    " Bind  " keydesc
                    "\tto  " (if (symbolp cmd)
                                 (symbol-name cmd)
                               (if (keymapp cmd)
                                   (if-let ((named (help-fns-find-keymap-name cmd)))
                                       (symbol-name named)
                                     "(some sub-keymap)")
                                 cmd))
                    (if (symbolp map)
                        (concat "\t\t  (" (symbol-name map) ")")
                      "")))
          (newline)))
      (goto-char (point-min)))
    (unless silently
      (display-buffer buf))
    (when-let ((window (get-buffer-window buf)))
      (with-selected-window window
        (recenter 0)
        (view-mode)))))

(defun dei-remap-actions-wipe ()
  "For convenience while debugging."
  (interactive)
  (setq dei--remap-actions nil)
  (setq dei--homogenized-keymaps nil)
  (message "remap actions wiped"))

(provide 'deianira)
;;; deianira.el ends here
