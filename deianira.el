;;; deianira.el --- Modifier-free pseudo-modal input -*- lexical-binding: t -*-

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
  "Modifier-free pseudo-modal input."
  :link '(info-link "(deianira)")
  :group 'keyboard)

;; builtin dependencies
(require 'seq)
(require 'subr-x)
(require 'cl-lib)

;; external dependencies
(require 'dash)
(require 'hydra)
(require 'named-timer)

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

Beware that if there's a named function key <ns-drag-n-drop>, it
will match against s- in the ns- there.  To guard this, use
`dei--modifier-regexp-safe', although that has its own flaws.")

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
                ;; By putting them in this regexp, we've effectively doomed any
                ;; sequence ending with them, they will be overwritten by C-x i.
                ;; Put a blurb in the readme about it.
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

(defcustom dei-hydra-keys "1234567890qwertyuiopasdfghjkl;zxcvbnm,./"
  "Keys to show in hydra hint.  Length should be divisible by `dei-columns'.
In other words, if you have 10 columns this can be 30
characters (or 40)\; if you want 11 columns this can be 33
characters (or 44), and so on ..."
  :type 'string
  :group 'deianira)

(defcustom dei-all-shifted-symbols
  "~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?"
  "Characters that imply Shift pressed\; default reflects an US keyboard."
  :type 'string
  :group 'deianira)

(defcustom dei-colwidth-override
  nil
  "Character width of each head hint. If nil, determine from frame."
  :type 'sexp
  :group 'deianira)

(defcustom dei-columns 10
  "Amount of columns to display in the hydra hint."
  :type 'number
  :group 'deianira)

(defcustom dei-quasiquitter-keys
  '("C-c c"
    "C-x l" ;; for testing
    )
  "Keys that send you to the root hydra.

Note that if you use the mass remapper, the hydras are generated
afterwards, consulting this list then.  So it is safe to only
refer to e.g. \"C-c c\" even if it's a clone of \"C-c C-c\"."
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
`dei-all-shifted-symbols' is taken into account."
  :type '(repeat key)
  :group 'deianira
  :set (lambda (var new)
         (set-default var (--map (key-description (kbd it)) new))))

(defcustom dei-stemless-quitters
  '("<menu>"
    "C-g")
  "Keys guaranteed to behave like the literal key, instead of the key plus a prefix.
Also guaranteed to slay the hydra."
  :type '(repeat key)
  :group 'deianira
  :set (lambda (var new)
         (set-default var (--map (key-description (kbd it)) new))))

(defcustom dei-quitter-keys
  '()
  "Key sequences guaranteed to slay the hydra.

If you use the homogenizer, the hydras are made afterwards, so
you do not need to refer to both e.g. \"C-c c\" and \"C-c C-c\".
In fact, only \"C-c c\" will have an effect, probably."
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
    doom/restart
    doom/restart-and-restore
    abort-recursive-edit
    execute-extended-command
    counsel-m-x
    helm-m-x
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
minibuffer, as we slay the hydra automatically when
`completing-read' is called."
  :type '(repeat symbol)
  :group 'deianira)

(defcustom dei-extra-heads
  nil
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

By default, typing a key not in `dei-hydra-keys' or this list will
result in calling that key's normal binding, as if there was no
active hydra.

(It's worth noting that the aforementioned default behavior winds
up just applying to chorded keys, since the defaults for
`dei--hydra-keys' plus this list make a near-complete coverage of an
US keyboard.)

If TAB is not a member of this list, typing the sequence
<Control> x TAB calls the binding of TAB.

With this list, if TAB is a member, it's taken as a leaf
grafted onto the hydra's corresponding stem.

So typing <Ctrl> x TAB calls the binding of C-x TAB."
  :type '(repeat key)
  :group 'deianira
  :set (lambda (var new)
         (set-default var (--map (key-description (kbd it)) new))))


;;;; Background facts

(defun dei--all-shifted-symbols-list-recalc ()
  "."
  (split-string dei-all-shifted-symbols "" t))

(defun dei--hydra-keys-list-recalc ()
  "."
  (split-string dei-hydra-keys "" t))

(defun dei--colwidth-recalc ()
  "Recalculate `dei--colwidth' based on frame width."
  (or dei-colwidth-override
      (let ((optimal (- (round (frame-parameter nil 'width) 10) 4)))
        (max optimal 8))))

(defun dei--filler-recalc ()
  "Recalculate `dei--filler' based on `dei--colwidth'."
  (make-string dei--colwidth (string-to-char " ")))

(defvar dei--all-shifted-symbols-list (dei--all-shifted-symbols-list-recalc)
  "Cache variable, not to be modified directly.
Customize `dei-all-shifted-symbols' instead.")

(defvar dei--hydra-keys-list (dei--hydra-keys-list-recalc)
  "Cache variable, not to be modified directly.
Customize `dei-hydra-keys' instead.")

(defvar dei--colwidth (dei--colwidth-recalc)
  "Cache variable, not to be modified directly.
Customize `dei-colwidth-override' instead.")

(defvar dei--filler (dei--filler-recalc)
  "Cache variable, not to be modified directly.")


;;;; Handlers for key descriptions

(defun dei--last-key (single-key-or-chord)
  "Return the last key in SINGLE-KEY-OR-CHORD.
Presupposes that the input has no spaces and has been normalized
by (key-description (kbd KEY))!"
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
Assumes KEYDESC is normalized."
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
    (string-match-p dei--modifier-regexp-safe keydesc (+ 1 first-modifier-pos))))

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
             (now-forbidden-mods (concat "\\(-\\|^\\| \\)\\(["
                                         (string-join (remove caught mods))
                                         "]-\\)")))
        (string-match-p now-forbidden-mods keydesc)))))

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

(defun dei--number-base36 (num len)
  "Return NUM as base 36 and ensure it's LEN characters wide.
Copy-pasted from the message library, no idea how it works."
  (declare (pure t) (side-effect-free t))
  (if (if (< len 0)
          (<= num 0)
        (= len 0))
      ""
    (concat (dei--number-base36 (/ num 36) (1- len))
            (char-to-string (aref "zyxwvutsrqponmlkjihgfedcba9876543210"
                                  (% num 36))))))

(defun dei--hash-current-modes ()
  (let ((hash (sxhash local-minor-modes)))
    (concat "/"
            (substring (symbol-name major-mode) 0 -4)
            (dei--number-base36 (if (< hash 0)
                                 (- hash)
                               hash)
                             5))))

;; This is used by dei--head-arg-cmd
(defun dei--corresponding-hydra (keydesc-or-stem &optional leaf)
  (intern (concat
           (dei--dub-hydra-from-key-or-stem (concat keydesc-or-stem leaf))
           dei--flock-under-operation
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

(defun dei--stem-to-hydra-sym (stem)
  (declare (pure t) (side-effect-free t))
  (when stem
    (intern (concat (dei--dub-hydra-from-key-or-stem stem) "/body"))))

(defun dei--parent-stem (stem)
  (declare (pure t) (side-effect-free t))
  (if (or (= 2 (length stem))
          (dei--key-seq-steps=1 stem))
      nil
    (dei--drop-leaf (dei--stem-to-parent-keydesc stem))))

;; Used by specify-extra-heads for backspace
(defun dei--parent-hydra (stem)
  (dei--corresponding-hydra (dei--parent-stem stem)))

(defun dei--parent-key (keydesc)
  "Return parent prefix of KEYDESC, or nil if no parent."
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

(defvar dei-debug nil
  "A buffer name or nil.")

(defun dei--echo (&rest args)
  (when dei-debug
    (print (apply #'format (cons (concat (format-time-string "%T: ") (car args)) (cdr args)))
           (dei--debug-buffer))))

(defun dei--debug-buffer ()
  (when dei-debug
    (let ((buf (get-buffer-create dei-debug)))
      (with-current-buffer buf
        (setq-local truncate-lines t)
        buf))))

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

;; unused
(defun dei--call-and-return-to-parent (keydesc)
  (interactive)
  (call-interactively (key-binding (kbd keydesc)))
  (if-let ((parent (dei--corresponding-hydra
                    (dei--parent-stem (dei--drop-leaf keydesc)))))
      (call-interactively parent)
    (hydra-keyboard-quit)))


;;;; Hydra blueprinting

(defvar dei--hydrable-prefix-keys nil)

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
         ,(let ((init (substring key 0 2)))
            (cond
             ((string-search "C-" init) (dei--corresponding-hydra "C "))
             ((string-search "M-" init) (dei--corresponding-hydra "M "))
             ((string-search "s-" init) (dei--corresponding-hydra "s "))
             ((string-search "H-" init) (dei--corresponding-hydra "H "))
             ((string-search "A-" init) (dei--corresponding-hydra "A "))))))
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
heads suited for a nonum hydra."
  (let ((self-poppers
         (cond ((string= "C-" stem) '("<katakana>"
                                      "C-<katakana>"))
               ((string= "M-" stem) '("<muhenkan>"
                                      "M-<muhenkan>"))
               ((string= "s-" stem) '("<henkan>"
                                      "s-<henkan>"))
               ((string= "H-" stem) '("<f32>"
                                      "H-<f32>"))
               ((string= "A-" stem) '("<f31>"
                                      "A-<f31>"))))
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

(defun dei--specify-dire-hydra (stem name &optional nonum-p)
  "Return a list of three items.
The first two items are STEM and NAME unmodified, and the third
is a list of hydra heads.  You can call
`dei--try-birth-dire-hydra' directly with the second and third
items as input.

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
    (-cons* stem name heads)))

(defun dei--try-birth-dire-hydra (name list-of-heads)
  "Create a hydra named NAME with LIST-OF-HEADS.
This will probably be called by `dei--generate-hydras',
which see."
  ;; TODO: test running generate-hydras-async while in open hydra
  (if (eq hydra-curr-body-fn (intern name))
      (progn
        (message "This hydra active, skipped redefining: %s" name)
        ;; Return nil because caller uses the return value
        nil)
    (eval `(defhydra ,(intern name)
             (:columns ,dei-columns
              :exit nil
              :hint nil
              :foreign-keys run)
             ,name
             ,@list-of-heads)
          t)))


;;;; Keymap scanner

;; REVIEW: Test <ctl> x 8 with Deianira active.
(defvar dei--unnest-avoid-prefixes
  (cons "C-x 8"
        (mapcar #'key-description
                (where-is-internal #'iso-transl-ctl-x-8-map)))
  "List of prefixes to avoid looking up.
Do not set directly, rarely has an effect.")

(defun dei--unnest-keymap-for-hydra-to-eat (map &optional avoid-prefixes)
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
               (string-match-p "backspace" key)
               (string-match-p "DEL" key)
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
  ;; NOTE Below comment is useful info if 1. we decide to return an alist
  ;; that includes the bound commands like `which-key--get-current-bindings',
  ;; or 2. if a conflict does occurs in the "Just in case" check below.
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
           with avoid-prefixes =
           (cons "C-x 8"
                 (mapcar #'key-description
                         (where-is-internal #'iso-transl-ctl-x-8-map)))
           append (dei--unnest-keymap-for-hydra-to-eat map avoid-prefixes)))))
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



;;;; Async worker

;; Persistent variables helpful for debugging
(defvar dei--new-or-changed-stems nil)
(defvar dei--hydra-blueprints nil)

(defvar dei--buffer-under-operation nil)
(defvar dei--flock-under-operation nil)
(defvar dei-lazy-p nil)
(defvar dei--async-chain nil)

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

(defun dei--stage-4b-check-settings (&optional _)
  (unless (--all? (equal (cdr it) (symbol-value (car it)))
                  dei--last-settings-alist)
    ;; Check for overlap between any two settings: that's user error.  We could
    ;; end up with two heads in one hydra both bound to the same key, no bueno.
    (let ((vars
           (list (cons 'dei-hydra-keys dei--hydra-keys-list)
                 (cons 'dei-all-shifted-symbols dei--all-shifted-symbols-list)
                 (cons 'dei-invisible-leafs dei-invisible-leafs)
                 (cons 'dei-stemless-quitters dei-stemless-quitters)
                 (cons 'dei-inserting-quitters dei-inserting-quitters)
                 (cons 'dei-extra-heads (mapcar #'car dei-extra-heads)))))
      ;; REVIEW: make sure this version of the code works
      (cl-loop for sublist on vars
               for y in (cdr sublist)
               as x = (car sublist)
               as overlap = (-intersection (cdr x) (cdr y))
               when overlap
               do (error "Found %s in both %s and %s" overlap (car x) (car y)))
      ;; (while vars
      ;;   (let ((var (pop vars)))
      ;;     (cl-loop for remaining-var in vars
      ;;              do (when-let ((overlap (-intersection (cdr var)
      ;;                                                    (cdr remaining-var))))
      ;;                   (error "Found %s in both %s and %s"
      ;;                          overlap (car var) (car remaining-var))))))
      )
    ;; Record the newest setting values, so we can skip the relatively
    ;; expensive calculations next time if nothing changed.
    (setq dei--last-settings-alist
          (cl-loop for cell in dei--last-settings-alist
                   collect (cons (car cell) (symbol-value (car cell)))))))


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

(defun dei--destroy-keymap (map)
  "Unbind all keys in keymap MAP recursively."
  (map-keymap
   (lambda (ev def)
     (when (keymapp def)
       (dei--destroy-keymap def))
     (define-key map (vector ev) nil t))
   map))

(defun dei--bind (keymap key def &optional remove)
  "Bind a key, but recursively unbind the previous binding.
Arguments KEYMAP, KEY, DEF and REMOVE as in `define-key'."
  (cl-assert (keymapp keymap))
  (let ((old-def (lookup-key-ignore-too-long keymap key)))
    (when (keymapp old-def)
      ;; NOTE: Here be a destructive action!
      (dei--destroy-keymap old-def)))
  (define-key keymap key def remove))

;;; Unbinding ugly keys

(defvar dei--cleaned-maps nil)

(defvar dei--clean-actions nil)

(defconst dei--shift-regexp (rx (or bol "-" " ") "S-")
  "Match explicit \"S-\" chords in key descriptions.")

(defun dei--key-is-illegal (keydesc)
  "Non-nil if KEYDESC would be unbound in a purist scheme."
  (let ((case-fold-search nil))
    (or
     ;; TODO: it seems rx will call regexp-opt indirectly, when we use its `or'
     ;; syntax. So this part is causing a full 30% of the lag. Does rx have an
     ;; implicit eval-when-compile (Seems not)? If not, tell upstream. And I
     ;; like interpreted performance to be passable, so let's replace this.
     ;;
     ;; The biggest question I have is why sometimes it does NOT lag.  Maybe it
     ;; was using the compiled artifact of this code?  No, I guess it's fast
     ;; enough when generate-hydras is not running repeatedly.
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
                         ;; I don't want to touch these, they constitute an
                         ;; alternative form of menu-bar to me.  I want to see
                         ;; Doom, Spacemacs and friends crystallize a leader
                         ;; key standard that one day lives as its own package.
                         (when doom
                           (or (string-prefix-p doom-localleader-alt-key key)
                               (string-prefix-p doom-leader-alt-key key)))))
               collect key)
              #'> :key #'dei--key-seq-steps-length))
       dei--clean-actions)))

;;; Reflecting one stem in another

(defvar dei--reflect-actions nil
  "List of actions to pass to `define-key'.")

(defvar dei--known-keymaps '(global-map)
  "List of named keymaps seen while `deianira-mode' was active.")

(defvar dei--super-reflected-keymaps nil
  "List of keymaps worked on by `dei-define-super-like-ctl-everywhere'.")

(defvar dei--ctlmeta-reflected-keymaps nil
  "List of keymaps worked on by `dei-define-super-like-ctlmeta-everywhere'.")

(defun dei--record-keymap-maybe (&optional _)
  "If Emacs has seen new keymaps, record them in a variable.
This simply evaluates `current-active-maps' and adds to
`dei--known-keymaps' anything not already there."
  (require 'help-fns)
  (when-let* ((current-named-maps (-uniq (-keep #'help-fns-find-keymap-name
                                                (current-active-maps))))
              (new-maps (-difference current-named-maps dei--known-keymaps)))
    (setq dei--known-keymaps (append new-maps dei--known-keymaps))
    (run-hooks 'dei-keymap-found-hook)))

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
;; s-x s-k s-t, at least when the prefixes within these key descriptions
;; involve named keymaps.  Binding both of these keys means binding every
;; possible combination.  It's annoying in describe-keymap output, but it is
;; possible to hide the strange combinations from the which-key popup, see
;; README.
(defun dei--define-super-like-ctl-in-keymap (map)
  (map-keymap
   (lambda (event definition)
     (let ((key (key-description (vector event))))
       (when (string-search "C-" key)
         (unless (string-search "s-" key)
           (push (list map (string-replace "C-" "s-" key) definition)
                 dei--reflect-actions)
           (when (keymapp definition)
             ;; Recurse!
             (dei--define-super-like-ctl-in-keymap definition))))))
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

(defun dei-define-super-like-ctl-everywhere ()
  (cl-loop for map in (-difference dei--known-keymaps
                                   dei--super-reflected-keymaps)
           do (progn
                (dei--define-super-like-ctl-in-keymap map)
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
                (define-key (dei--raw-keymap map)
                  (kbd key)
                  def
                  (when (null def)
                    t)))
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

(defcustom dei-homogenizing-winners
  '(("C-c C-c")
    ("C-x C-f") ;; FIXME: does not apply .. ??
    ("C-x C-s")
    ("C-x C-;")
    ("C-x a")
    ("C-x g")
    ("C-x b")
    ("C-x n")
    ("C-x p")
    ("C-g")
    ("C-h C-g")
    ;; ("C-x r")
    ;; ("C-x v")
    ("M-g")
    ("C-c C-c" . org-mode-map))
  "Alist of keys that always win the homogenizing battle.
See `dei-permachord-wins-homogenizing' for explanation.

Each item has the format (KEY-OR-COMMAND . KEYMAP).

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

The problem is that control characters should be deprecated.  I
recommend a solution like `dei-define-super-like-ctl-everywhere' and
never typing another control character.

If you don't, it pays to know to always bind <tab> instead of
TAB, <return> instead of RET, and <escape> instead of ESC.  GUI
Emacs always looks up these if bound, and only falls back to the
control character if these function keys are unbound.  They do
not work on the terminal/TTY, but neither does Super or many
other niceties."
  (cl-assert (dei--key-starts-with-modifier keydesc))
  (and (string-search "C-" (substring keydesc 0 2))
       (string-match-p (rx (any "[" "m" "i")) keydesc)))

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
   (dei--key-starts-with-modifier this-key)
   (dei--nightmare-p this-key)
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
                               "Clone winning sibling to overwrite this   ")))
                  ((or (member this-key winners-for-this-keymap)
                       (member this-cmd winners-for-this-keymap))
                   (setq action
                         (list sibling-keydesc
                               this-cmd
                               keymap
                               "Clone this winner to overwrite sibling    ")))
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
                               "Clone winning sibling to overwrite this   ")))
                  ((or (member this-key winners)
                       (member this-cmd winners))
                   (setq action
                         (list sibling-keydesc
                               this-cmd
                               keymap
                               "Clone this winner to overwrite sibling    ")))
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
                               "Clone perma-chord to chord-once           ")))
                  (t
                   (setq action
                         (list permachord-key
                               chordonce-cmd
                               keymap
                               "Clone chord-once to perma-chord           ")))))

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
                               "Clone to unbound sibling                  ")))
           ;; Default.  This will also, via dei-remap-actions-execute, unbind the
           ;; key seqs that would've blocked us from proceeding.
           (t
            (setq action
                  (list permachord-key
                        chordonce-cmd
                        keymap
                        "Clone chord-once to perma-chord           "))))
          ;; FIXME: no seq ending in g should ever overwrite the one ending in
          ;; C-g.  Check if the leaf is g and then decline to duplicate it to
          ;; C-g.
          ;; Currently, magit-status gets bound to C-x C-g.
          (when (and action
                     (not (member action dei--remap-record)))
            (push action dei--remap-actions)
            (push action dei--remap-record)))))))

;; (dei--homogenize-key-in-keymap "C-c C-;" 'vertico-map)
;; (dei--homogenize-key-in-keymap "C-x C-k C-e" 'global-map)
;; (dei--homogenize-keymap 'global-map)

;; TODO: some way to catch when this doesn't do anything and it should
(defun dei--homogenize-keymap (map)
  "Homogenize most of keymap MAP."
  (message "Keys rebound: %s"
           (cl-loop for key in (dei--unnest-keymap-for-hydra-to-eat map)
                    when (dei--homogenize-key-in-keymap key map)
                    count key)))

(defun dei-homogenize-all-keymaps ()
  "."
  (cl-loop for map in (-difference dei--known-keymaps dei--homogenized-keymaps)
           do (progn (dei--homogenize-keymap map)
                     (if (member map dei--homogenized-keymaps)
                         (warn "Keymap already homogenized, doing again: %s" map)
                       (push map dei--homogenized-keymaps)))
           finally (progn
                     (dei-remap-actions-execute dei--remap-actions)
                     (setq dei--remap-actions nil))))

(defun dei-homogenize-all-keymaps-dry-run ()
  "."
  (interactive)
  (cl-loop for map in (-difference dei--known-keymaps dei--homogenized-keymaps)
           do (dei--homogenize-keymap map)
           finally (message "%s %s"
                            "Inspect with dei-remap-actions-preview and"
                            "make a wet-run with dei-remap-actions-execute.")))

(defun dei-remap-actions-execute (actions)
  "Carry out remaps specified by ACTIONS.
Interactively, use the value of `dei--remap-actions'."
  (interactive (list dei--remap-actions))
  (dolist (action actions)
    (seq-let (keydesc cmd map _) action
      (let* ((raw-keymap (dei--raw-keymap map))
             (old-def (lookup-key-ignore-too-long raw-keymap (kbd keydesc))))
        ;; Unbind things that are in the way of the new definition
        (when (keymapp old-def)
          (dei--destroy-keymap old-def))
        (when-let* ((conflict-prefix (dei--key-seq-has-non-prefix-in-prefix raw-keymap keydesc))
                    (conflict-def (lookup-key-ignore-too-long raw-keymap (kbd conflict-prefix))))
          (unless (keymapp conflict-def)
            ;; if it's a keymap, we'll be perfectly able to bind our key
            (define-key raw-keymap (kbd conflict-prefix) nil t)))
        (define-key raw-keymap (kbd keydesc) cmd)))))

(defun dei-remap-actions-preview ()
  "For convenience while debugging."
  (interactive)
  (require 'help-fns)
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
    (display-buffer buf)
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

;; An alternative that may do away with the need for a static :exit keyword
(defun dei--head-arg-cmd* (stem leaf)
  "See `dei--head'."
  (cond ((member (concat stem leaf) dei--hydrable-prefix-keys)
         (dei--corresponding-hydra stem leaf))
        ((or (member (key-binding (kbd (concat stem leaf))) dei-quasiquitter-commands)
             (member (concat stem leaf) dei-quasiquitter-keys))
         `(dei--call-and-return-to-root ,(concat stem leaf)))
        (t
         `(let* ((key ,(concat stem leaf))
                 (cmd (key-binding ,(kbd (concat stem leaf)))))
            ;; TODO: Maybe just call dei--head-arg-exit directly
            (when (or (member cmd dei-quasiquitter-commands)
                      (member cmd dei-quitter-commands)
                      (member key dei-quasiquitter-keys)
                      (member key dei-quitter-keys)
                      (member key dei--hydrable-prefix-keys))
              (dei--slay))
            (call-interactively cmd)))))


;;;; Main
;; Things that summon and slay the hydras we've made.

(defun dei--hydra-active-p ()
  "Return t if a hydra is active and awaiting input."
  (not (null hydra-curr-map)))

(defun dei--slay (&rest args)
  "Slay active hydra and return ARGS."
  (when (dei--hydra-active-p)
    (setq hydra-deactivate t)
    (call-interactively #'hydra-keyboard-quit))
  args)

(defvar dei--old-hydra-cell-format nil
  "Backup for `hydra-cell-format'.")

(defvar dei--old-hydra-C-u nil
  "Backup for key binding of \"C-u\" in `hydra-base-map'.")

(declare-function #'ido-read-internal "ido")
(declare-function #'ivy-read "ivy")
(declare-function #'helm "helm-core")

(defcustom dei-ersatz-control "<katakana>"
  "Key that represents Control."
  :type 'key
  :group 'deianira)

(defcustom dei-ersatz-meta "<muhenkan>"
  "Key that represents Meta."
  :type 'key
  :group 'deianira)

(defcustom dei-ersatz-super "<henkan>"
  "Key that represents Super."
  :type 'key
  :group 'deianira)

(defcustom dei-ersatz-hyper "<f30>"
  "Key that represents Hyper."
  :type 'key
  :group 'deianira)

(defcustom dei-ersatz-alt "<f31>"
  "Key that represents Alt."
  :type 'key
  :group 'deianira)

;;;###autoload
(define-minor-mode deianira-mode
  "Configure switch-window hooks to make hydras and in the darkness bind them."
  :global t
  :lighter " dei"
  :group 'deianira
  ;; :keymap `((,(kbd "<f31>") . dei-A/body)
  ;;           (,(kbd dei-ersatz-control) . dei-C/body)
  ;;           (,(kbd "<f24>") . dei-H/body)
  ;;           (,(kbd dei-ersatz-meta) . dei-M/body)
  ;;           (,(kbd "<henkan>") . dei-s/body)
  ;;           ;; in case of sticky keys
  ;;           (,(kbd "A-<f31>") . dei-A/body)
  ;;           (,(kbd "C-<katakana>") . dei-C/body)
  ;;           (,(kbd "H-<f24>") . dei-H/body)
  ;;           (,(kbd "M-<muhenkan>") . dei-M/body)
  ;;           (,(kbd "s-<henkan>") . dei-s/body)

  ;;           ;; theoretically, we can get a third hydra by chording two modifiers and releasing. Cannot allow sticky keys then, as it'd destroy being able to switch hydra with one press.

  ;;           ;; (,(kbd "M-<katakana>") . dei-CM/body)
  ;;           ;; (,(kbd "C-<muhenkan>") . dei-CM/body)

  ;;           ;; (,(kbd "s-<muhenkan>") . dei-sM/body) ;; nonstandard modifier order required in name to disambig. from M-s
  ;;           ;; (,(kbd "M-<henkan>")   . dei-sM/body)

  ;;           ;; (,(kbd "C-<henkan>")   . dei-sC/body)
  ;;           ;; (,(kbd "s-<katakana>") . dei-sC/body)

  ;;           ;; (,(kbd "C-<muhenkan>") . dei-CH/body)
  ;;           ;; (,(kbd "C-<muhenkan>") . dei-CH/body)
  ;;           ;; (,(kbd "C-<muhenkan>") . dei-AC/body)
  ;;           ;; (,(kbd "C-<muhenkan>") . dei-AC/body)
  ;;           ;; (,(kbd "C-<muhenkan>") . dei-AH/body)
  ;;           ;; (,(kbd "C-<muhenkan>") . dei-AH/body)
  ;;           ;; (,(kbd "C-<muhenkan>") . dei-AM/body)
  ;;           ;; (,(kbd "C-<muhenkan>") . dei-AM/body)
  ;;           ;; (,(kbd "C-<muhenkan>") . dei-HM/body)
  ;;           ;; (,(kbd "C-<muhenkan>") . dei-HM/body)
  ;;           ;; (,(kbd "C-<muhenkan>") . dei-sH/body)
  ;;           ;; (,(kbd "C-<muhenkan>") . dei-sH/body)
  ;;           ;; (,(kbd "C-<muhenkan>") . dei-sA/body)
  ;;           ;; (,(kbd "C-<muhenkan>") . dei-sA/body)
  ;;          )

  (if deianira-mode
      (progn
        (setq dei--old-hydra-cell-format hydra-cell-format)
        (setq dei--old-hydra-C-u (lookup-key hydra-base-map (kbd "C-u")))
        (setq hydra-cell-format "% -20s %% -11`%s")
        (define-key hydra-base-map (kbd "C-u") nil t)
        (advice-add #'completing-read :before #'dei--slay)
        (advice-add #'ido-read-internal :before #'dei--slay) ;; REVIEW UNTESTED
        (advice-add #'ivy-read :before #'dei--slay) ;; REVIEW UNTESTED
        (advice-add #'helm :before #'dei--slay) ;; REVIEW UNTESTED
        (add-hook 'window-buffer-change-functions #'dei--record-keymap-maybe -48)
        (add-hook 'window-buffer-change-functions #'dei-async-make-hydrasd-maybe 56)
        ;; (add-variable-watcher 'local-minor-modes #'dei-generate-hydras-restart)
        ;; NOTE: Horrid performance penalty!  For some reason the hook re-runs
        ;; on every hydra head call? May be a Doom thing.
        ;; (add-hook 'after-change-major-mode-hook #'dei-generate-hydras-restart)
        )

    (named-timer-cancel 'deianira-at-work)
    (setq hydra-cell-format (or dei--old-hydra-cell-format "% -20s %% -8`%s"))
    (define-key hydra-base-map (kbd "C-u") dei--old-hydra-C-u)
    ;; (remove-variable-watcher 'local-minor-modes #'dei--generate-hydras)
    (remove-hook 'window-buffer-change-functions #'dei--record-keymap-maybe)
    (remove-hook 'window-buffer-change-functions #'dei-async-make-hydras)
    (advice-remove #'completing-read #'dei--slay)
    (advice-remove #'ido-read-internal #'dei--slay)
    (advice-remove #'ivy-read #'dei--slay)
    (advice-remove #'helm #'dei--slay)))

;;; User config

;; lv is being terribly laggy for some reason
(setopt hydra-hint-display-type 'message)
(setopt dei-debug "*Deianira Debug*")
(setopt dei-invisible-leafs
      (seq-difference dei-invisible-leafs '("-" "=" "<menu>")))
(setopt dei-extra-heads
      '(("=" dei-universal-argument nil :exit t)
        ("-" dei-negative-argument nil :exit t)
        ("<f5>" hydra-repeat nil)))

(add-hook 'dei-keymap-found-hook #'dei-homogenize-all-keymaps)
;; (add-hook 'dei-keymap-found-hook #'dei-define-super-like-ctlmeta-everywhere)

;; Don't show permacahords in which-key (unnecessary), only chord-onces
(after! which-key
  (push '((" .-." . nil) . t) which-key-replacement-alist))

;; dev settings
(general-auto-unbind-keys 'undo) ;; we shouldn't rely on general
;; (remove-hook 'after-save-hook 'my-compile-and-drop)


;;;; WIP area

;; TODO: show only minimal hint to indicate that the hydra is on
(defun dei--make-static-hydras ()
  "Make simple fallback hydras.
Model them on the bindings of `emacs-lisp-mode', to be most
useful when developing the package or fixing initfiles, but the
hint may not reflect the truth in other modes."
  (with-current-buffer (get-buffer-create "deianira-temp-buffer")
    (emacs-lisp-mode)
    (let ((stems (->> (dei--get-filtered-bindings)
                      (-map #'dei--drop-leaf)
                      (-remove #'string-empty-p)
                      (-uniq))))
      (setq dei--hydrable-prefix-keys
            (-difference (-map #'dei--stem-to-parent-keydesc stems)
                         '("C-" "M-" "s-" "H-" "A-")))
      (dei--stage-4b-check-settings)
      (cl-loop for stem in stems
               as bp1 = (dei--specify-dire-hydra
                          stem
                          (concat (dei--dub-hydra-from-key-or-stem stem) "-nonum")
                          t)
               as bp2 = (dei--specify-dire-hydra
                          stem (dei--dub-hydra-from-key-or-stem stem))
               do
               (dei--try-birth-dire-hydra (cadr bp1) (cddr bp1))
               (dei--try-birth-dire-hydra (cadr bp2) (cddr bp2))))
    (kill-buffer (current-buffer))))

(defvar dei--buffer-hydras nil
  "Alist relating major plus minor modes with root hydras.")

(make-variable-buffer-local (defvar deianira-local-map))
(make-variable-buffer-local (defvar dei--all-done nil))

(defvar dei--async-chain-last-idle-value)
(defvar dei--async-chain-template
  #'(dei--async-1-check-predone
     dei--stage-4b-check-settings
     dei--async-4-model-the-world
     dei--async-5-draw-blueprint
     dei--async-6-birth-dire-hydra
     dei--async-7-register
     dei--async-8-bind-hydras))

;; TODO: if we're gonna use an idle timer, we really need fallback hydras per
;; major mode, that'll always be bound in that mode map.
;;
;; REVIEW: When it was laggy as shit due to unnecessary re-running, the lag was
;; in such a pattern it seemed to be as if it's just using the named-timer-run
;; and doesn't switch to named-timer-idle-run after user input.  I've hopefully
;; fixed it now. TODO: Verify it switches to the idle timer.
(defun dei--async-chomp ()
  "Pop the next function off `dei--async-chain' and call it.
Rinse and repeat until user does something, in which case defer
to a short idle timer so that user is free to use Emacs.  Stop
only when `dei--async-chain' is empty or a keyboard quit occurs
during execution.

To start, see `dei-async-make-hydras'."
  (when (member (named-timer-get 'deianira-at-work) timer-list)
    (error "Timer was not cancelled before `dei--async-chomp'"))
  (when dei--async-chain
    (funcall (pop dei--async-chain) t) ;; work happens here
    (let ((polite-delay .321)
          (idled-time (or (current-idle-time) 0)))
      (if (and (time-less-p idled-time polite-delay)
               (time-less-p dei--async-chain-last-idle-value idled-time))
          ;; If user hasn't done anything since last chomp, go go go.
          (named-timer-run 'deianira-at-work 0 nil #'dei--async-chomp)
        ;; Otherwise go slow and polite.
        ;; (cl-incf dei--async-chain-last-idle-value .321)
        (named-timer-idle-run 'deianira-at-work polite-delay nil #'dei--async-chomp-polite))
      (setq dei--async-chain-last-idle-value idled-time))))

(defun dei--async-chomp-polite ()
  (when dei--async-chain
    (funcall (pop dei--async-chain) t) ;; work happens here
    (let ((polite-delay .321)
          (idled-time (or (current-idle-time) 0)))
      (if (time-less-p polite-delay idled-time)
          ;; Switch back to aggressive
          (named-timer-run 'deianira-at-work 0 nil #'dei--async-chomp)
        (named-timer-idle-run 'deianira-at-work polite-delay nil #'dei--async-chomp-polite))
      (setq dei--async-chain-last-idle-value idled-time))))

(defun dei-async-make-hydras (&rest _)
  "Drop any running chain and start a new one."
  (interactive)
  (dei--echo "Launching new chain.  Previous value of `dei--async-chain': %s"
          dei--async-chain)
  (named-timer-cancel 'deianira-at-work)
  (setq dei--async-chain dei--async-chain-template)
  (setq dei--async-chain-last-idle-value 0) ;; yes, 0
  (dei--async-chomp))

;; TODO: unset all-done if frame-width etc changed. go back in git-timemachine
;; for the old dei--stage-4-model-the-world, grab some of its framewidth checks
(defun dei--async-1-check-predone (&optional _)
  "If worked on this buffer before, drop the chain."
  (setq dei--buffer-under-operation (current-buffer))
  (setq dei--flock-under-operation (dei--hash-current-modes))
  (if dei--all-done
      ;; This specific buffer worked on before
      (progn
        (setq dei--async-chain nil)
        (unless (bound-and-true-p deianira-local-map)
          (warn "Unexpected state! Grateful for bug report")))
    ;; This mode combo worked on before
    (when (rassoc dei--flock-under-operation dei--buffer-hydras)
      (setq dei--async-chain nil)
      (unless (bound-and-true-p deianira-local-map)
        (dei--async-8-bind-hydras))
      (dei--echo "Already created flock: %s."
              dei--flock-under-operation))))

(defun dei--async-4-model-the-world (&optional _)
  "Calculate things."
  (with-current-buffer dei--buffer-under-operation
    ;; Infer which submaps exist by cutting the leafs off every key.
    (setq dei--new-or-changed-stems
          (->> (dei--get-filtered-bindings)
               ;; Sort to avail the most relevant hydras to the user soonest.
               (seq-sort-by #'dei--key-seq-steps-length #'>)
               (-map #'dei--drop-leaf)
               (-remove #'string-empty-p)
               (-uniq)))

    ;; Figure out dei--hydrable-prefix-keys, important in several
    ;; functions indirectly called from the next stage.
    (setq dei--hydrable-prefix-keys
          (-difference (-map #'dei--stem-to-parent-keydesc dei--all-stems)
                       ;; Subtract the root stems since they are still
                       ;; invalid keydescs, which is ok bc no hydra
                       ;; refers to them.
                       '("C-" "M-" "s-" "H-" "A-")))

    ;; Clear the workbench from a previous done or half-done iteration.
    (setq dei--hydra-blueprints nil)

    (when (null dei--new-or-changed-stems)
      (error "Something is fucky"))))

(defun dei--async-5-draw-blueprint (&optional chainp)
  "Draw blueprint for one of `dei--new-or-changed-stems'.
In other words, we compute all the arguments we'll later pass to
defhydra.  Pop a stem off the list `dei--new-or-changed-stems',
transmute it into a blueprint, and push that onto the list
`dei--hydra-blueprints'.

With CHAINP non-nil, the function will add another invocation of
itself to the front of `dei--async-chain', until
`dei--new-or-changed-stems' is empty."
  (require 'message)
  (when dei--new-or-changed-stems
    (with-current-buffer dei--buffer-under-operation
      (let* ((flock dei--flock-under-operation)
             (stem (pop dei--new-or-changed-stems))
             (name (dei--dub-hydra-from-key-or-stem stem)))
        (push (dei--specify-dire-hydra stem (concat name flock "-nonum") t)
              dei--hydra-blueprints)
        (push (dei--specify-dire-hydra stem (concat name flock))
              dei--hydra-blueprints))))
  ;; Run again if more to do
  (and chainp
       dei--new-or-changed-stems
       (push #'dei--async-5-draw-blueprint dei--async-chain)))

(defun dei--async-6-birth-dire-hydra (&optional chainp)
  "Pass a blueprint to `defhydra', turning dry-run into wet-run.
Each invocation pops one blueprint off `dei--hydra-blueprints'.
With CHAINP non-nil, the function will add another invocation of
itself to the front of `dei--async-chain', until
`dei--hydra-blueprints' is empty."
  (when dei--hydra-blueprints
    (with-current-buffer dei--buffer-under-operation
      (let ((blueprint (pop dei--hydra-blueprints)))
        (or blueprint
            (error "Blueprint should not be nil"))
        (dei--try-birth-dire-hydra (cadr blueprint) (cddr blueprint)))))
  ;; Run again if more to do
  (and chainp
       dei--hydra-blueprints
       (push #'dei--async-6-birth-dire-hydra dei--async-chain)))

(defun dei--async-7-register (&optional _)
  "Register this mode combination."
  (with-current-buffer dei--buffer-under-operation
    (let ((mode-combn (cons major-mode local-minor-modes)))
      (unless (assoc mode-combn dei--buffer-hydras)
        (push (cons mode-combn dei--flock-under-operation)
              dei--buffer-hydras)))))

;; TODO: There is no way to have a buffer-local map -- not without running code
;; on every buffer switch to turn it on and off somehow (set-transient-map). So
;; we may as well just bind it in (current-local-map) i.e. the major mode
;; map. Be aware that the minor mode maps take priority over it, so don't bind
;; stuff in deianira-mode-map.
(defun dei--async-8-bind-hydras (&optional chainp)
  "Buffer-locally bind appropriate root hydras for the buffer.
Optional argument CHAINP should only be used by `dei--async-chomp'."
  (with-current-buffer (if chainp
                           dei--buffer-under-operation
                         (current-buffer))
    (if (bound-and-true-p deianira-local-map)
        (dei--echo "Already bound")
      (let ((map (make-sparse-keymap)))
        (define-key map
          (kbd dei-ersatz-control) (dei--corresponding-hydra "C- "))
        (define-key map
          (kbd dei-ersatz-meta) (dei--corresponding-hydra "M- "))
        (define-key map
          (kbd dei-ersatz-super) (dei--corresponding-hydra "s- "))
        (define-key map
          (kbd dei-ersatz-hyper) (dei--corresponding-hydra "H- "))
        (define-key map
          (kbd dei-ersatz-alt) (dei--corresponding-hydra "A- "))
        (setq-local deianira-local-map map))
      (dei--echo "Made new bindings in buffer %s" (buffer-name)))
    (setq-local dei--all-done t)))

;; test
;; (add-hook 'window-buffer-change-functions #'dei-async-make-hydras)
;; (remove-hook 'window-buffer-change-functions #'dei-make-hydras-maybe)

(provide 'deianira)
;;; deianira.el ends here
