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
;; Keywords: convenience emulations help
;; Homepage: https://github.com/meedstrom/deianira
;; Package-Requires: ((emacs "28.1") (hydra "0.15.0") (deferred "0.5.0") (concurrent "0.5.0") (named-timer "0.1") (dash) (s))

;;; Commentary:

;;; Code:

;; builtin dependencies
(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'help)

;; external dependencies
(require 'deferred)
(require 'concurrent)
(require 'dash)
(require 's)
(require 'hydra)
(require 'named-timer)


;;;; User settings

(defgroup deianira
  "Modifier-free pseudo-modal input."
  :link '(info-link "(deianira)"))

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

(defcustom dei-colwidth-override nil
  "Character width of hydra hint. If nil, determine from frame."
  :type 'sexp
  :group 'deianira)

(defcustom dei-columns 10
  "Amount of columns to display in the hydra hint."
  :type 'number
  :group 'deianira)

;; TODO: use it
(defcustom dei-pseudo-quitter-keys
  '("C-c c")
  "Keys that send you to the root hydra.
Unused for now."
  :type '(repeat string)
  :group 'deianira)

;; TODO: use it
(defcustom dei-pseudo-quitter-commands
  '(set-mark-command
    rectangle-mark-mode)
  "Commands that send you to the root hydra.
Unused for now."
  :type '(repeat symbol)
  :group 'deianira)

(defcustom dei-quitter-keys
 '("<menu>"
    "C-g")
  "Keys guaranteed to kill the hydra."
  :type '(repeat string)
  :group 'deianira)

(defcustom dei-quitter-commands
  '(keyboard-quit
    minibuffer-keyboard-quit
    keyboard-escape-quit
    doom/escape
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
  "Commands guaranteed to kill the hydra.
Note that you don't need to add commands that focus the
minibuffer, as we slay the hydra automatically when minibuffer
gets focus."
  :type '(repeat symbol)
  :group 'deianira)

(defcustom dei-extra-heads
  '(("<print>" dei-universal-argument nil :exit t)
    ("-" dei-negative-argument nil :exit t)
    ("<f5>" hydra-repeat nil))
  "Heads to add to every hydra."
  :type '(repeat sexp)
  :group 'deianira)


;;;; Background facts

;; NOTE: let this be a true const so it can be used in pure functions
(defconst dei--modifier-regexp
  (regexp-opt '("A-" "C-" "H-" "M-" "S-" "s-"))
  "Regexp for any of the modifiers: \"C-\", \"M-\" etc.
Upon match, the string segment it matched is always two characters long.

Beware that if there's a named function key <ns-drag-n-drop>, it
will match against s- in the ns- there.  To guard this, use
`dei--modifier-regexp-safe', although that has its own flaws.")

(defconst dei--modifier-regexp-safe
  (rx (or "<" "-" bol space)
      (regexp (regexp-opt '("A-" "C-" "H-" "M-" "S-" "s-"))))
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

;; unused
(defconst dei-all-keys-on-keyboard
  (append
   (split-string
    "`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./"
    "" t)
   (split-string
    "<left> <right> <up> <down> <SPC> <RET> <backspace> <delete>
     TAB <f1> <f2> <f3> <f4> <f5> <f6> <f7> <f8> <f9> <f10> <f11>
     <f12> <print> <insert> <next> <prior> <home> <end> <menu>")
   ;; untested
   ;; (split-string
   ;; "DEL <return> <f13> <f14> <f15> <f16> <f17> <f18> <f19> <f20>
   ;; <iso-lefttab> <XF86MonBrightnessUp>") ;; and so on...
   )
  "All keys, except where a held Shift is implied.")

(defun dei--all-shifted-symbols-list-recalc ()
  (split-string dei-all-shifted-symbols "" t))

(defun dei--hydra-keys-list-recalc ()
  (split-string dei-hydra-keys "" t))

(defun dei--colwidth-recalc ()
  (or dei-colwidth-override
      (let ((optimal (- (round (frame-parameter nil 'width) 10) 4)))
        (max optimal 8))))

(defun dei--filler-recalc ()
  (make-string dei--colwidth (string-to-char " ")))

(defvar dei--all-shifted-symbols-list (dei--all-shifted-symbols-list-recalc))
(defvar dei--hydra-keys-list (dei--hydra-keys-list-recalc))
(defvar dei--colwidth (dei--colwidth-recalc))
(defvar dei--filler (dei--filler-recalc))


;;;; Handlers for key descriptions
;; If it's not a pure function, it probably doesn't belong in this section.

(defun dei--key-contains-ctl (keydesc)
  (declare (pure t) (side-effect-free t))
  (string-match-p "C-" keydesc))

(defun dei--key-contains-multi-chords (keydesc)
  "
Assumes KEYDESC is normalized."
  (declare (pure t) (side-effect-free t))
  ;; Assume keydesc was already normalized.
  (string-match-p (rx (= 2 (regexp dei--modifier-regexp)))
                  keydesc))

(defun dei--contains-upcase (keydesc)
  (declare (side-effect-free t)) ;; NOTE: can't be pure
  (let* ((steps (dei--key-seq-split keydesc))
         (steps-without-modifier (mapcar #'dei--get-leaf steps)))
    (--any-p (member it dei--all-shifted-symbols-list) steps-without-modifier)))

(defun dei--key-seq-involves-shiftsym (keydesc)
  (declare (side-effect-free t)) ;; NOTE: can't be pure
  (->> keydesc
       (s-split (rx space))
       (-map #'dei--normalize-trim-segment)
       (-map #'dei--normalize-get-atoms)
       (-map #'dei--normalize-wrap-leaf-maybe)
       (-map #'-last-item)
       (-intersection dei--all-shifted-symbols-list)))

;; May be faster than `dei--key-is-permachord'
(defun dei--key-seq-is-allchord (keydesc)
  "If sequence KEYDESC has a chord on every step. return t.
Note that it does not check if it's the same chord every time.
For that, see `dei--key-seq-mixes-modifiers'."
  (declare (pure t) (side-effect-free t))
  (--all-p (string-match-p dei--modifier-regexp-safe it)
           (dei--key-seq-split keydesc)))

;; TODO: give it a more precise name
(defun dei--key-has-more-than-one-modifier (keydesc)
  "Return nil if KEYDESC has one or zero chords.
Behave the same even if the additional chords are made with the same modifier, i.e.

C-c C-o return t
C-c M-o return t
C-c o returns nil
<f1> o returns nil
<f1> C-f returns nil

Does not check for shiftsyms, for that see `dei--key-seq-involves-shiftsym'."
  (declare (pure t) (side-effect-free t))
  (when-let ((first-modifier-pos
              (string-match-p dei--modifier-regexp-safe keydesc)))
    ;; we use +1 b/c of the peculiarities of this regexp, but gets the job done
    (string-match-p dei--modifier-regexp-safe keydesc (+ 1 first-modifier-pos))))

;; NOTE: Assumes normalized keydesc.
(defun dei--key-starts-with-modifier (keydesc)
  (declare (pure t) (side-effect-free t))
  (when-let ((first-modifier-pos
              (string-match-p dei--modifier-regexp-safe keydesc)))
    (= 0 first-modifier-pos)))

(defun dei--key-seq-mixes-modifiers (keydesc)
  "
Does not catch shiftsyms such as capital letters; to check for
those, see `dei--key-seq-involves-shiftsym'.  Does catch e.g. C-S-<RET>."
  (declare (pure t) (side-effect-free t))
  (let ((case-fold-search nil)
        (mods '("A-" "C-" "H-" "M-" "S-" "s-")))
    (when-let* ((first-match-pos (string-match-p dei--modifier-regexp keydesc))
                (caught-mod (substring keydesc first-match-pos (+ 2 first-match-pos)))
                (now-verboten (-difference mods (list caught-mod))))
      (string-match-p (eval `(rx (or ,@now-verboten)) t) keydesc))))

(defun dei--get-leaf (keydesc)
  (declare (pure t) (side-effect-free t))
  (->> keydesc
       (s-split (rx space))
       (-last-item)
       (dei--normalize-trim-segment)
       (dei--normalize-get-atoms)
       (dei--normalize-wrap-leaf-maybe)
       (-last-item)))

;; NOTE: Not strictly idiot-proof in theory when the leaf is an < or > key.  At
;; least that'll only affect sequences involving that and a named function key,
;; so it hasn't affected me so far.
(defun dei--normalize-trim-segment (x)
  (declare (pure t) (side-effect-free t))
  (if (and (string-match-p (rx "<") x)
           (string-match-p (rx ">" eol) x))
      (replace-regexp-in-string (rx (any "<>")) "" x)
    x))

;; REVIEW: Is it sane to flag it pure? Compare source of `s-match'.
(defun dei--normalize-get-atoms (step)
  (declare (pure t) (side-effect-free t))
  (save-match-data
    (string-match (rx (group (* (regexp dei--modifier-regexp)))
                      (group (* nonl)))
                  step)
    (let ((modifiers (match-string 1 step))
          (last-atom (match-string 2 step)))
      (append (split-string modifiers "-" t)
              (list last-atom)))))

(defun dei--normalize-wrap-leaf-maybe (x)
  (declare (pure t) (side-effect-free t))
  (let* ((leaf (car (last x)))
         (corrected-leaf (if (string= "TAB" leaf)
                             leaf
                           (if (< 1 (length leaf))
                               (concat "<" leaf ">")
                             leaf))))
    (-snoc (butlast x) corrected-leaf)))

(defun dei--normalize-build-segments (x)
  (declare (pure t) (side-effect-free t))
  (string-join x "-"))

(defun dei--normalize (keydesc)
  (declare (pure t) (side-effect-free t))
  (if (dei--dangling-stem-p keydesc)
      (error "Dangling stem passed to `dei--normalize'")
    (->> keydesc
         (s-split " ")
         (-map #'dei--normalize-trim-segment)
         (-map #'dei--normalize-get-atoms)
         (-map #'dei--normalize-wrap-leaf-maybe)
         (-map #'dei--normalize-build-segments)
         (s-join " "))))

(defun dei--dangling-stem-p (keydesc)
  "Check if KEYDESC is actually a dangling stem, not a keydesc.
A proper key description is something you'd pass to `kbd', and a
stem is not.  Return t if true, else nil."
  (declare (pure t) (side-effect-free t))
  (if (or (string-match-p (rx "--" eol) keydesc)
          (string-match-p (rx " -" eol) keydesc)
          (string-match-p (rx (not (or "-" " ")) eol) keydesc))
      nil
    t))

(defun dei--dub-from-key (keydesc)
  "Example: If KEY is the string \"C-x a\", return \"dei-Cxa\"."
  (declare (pure t) (side-effect-free t))
  ;; (if (member keydesc '("C-" "M-" "s-" "H-" "A-"))
  ;;     (concat "dei-" (cond ((string= keydesc "C-") "control")
  ;;                          ((string= keydesc "M-") "meta")
  ;;                          ((string= keydesc "s-") "super")
  ;;                          ((string= keydesc "H-") "hyper")
  ;;                          ((string= keydesc "A-") "alt")))
  ;; else
  (let ((squashed (string-join (split-string keydesc (rx (any " -"))))))
    (if (string-match (rx "-" eol) keydesc)
        (if (= 2 (length keydesc))
            (concat "dei-" squashed) ;; C-, M-, s-
          (concat "dei-" squashed "-")) ;; leaf is -
      (concat "dei-" squashed))))

(defun dei--key-seq-steps=1 (keydesc)
  (declare (pure t) (side-effect-free t))
  (not (string-match-p " " keydesc)))

(defun dei--key-seq-split (keydesc)
  (declare (pure t) (side-effect-free t))
  (split-string keydesc " "))

(defun dei--corresponding-hydra (keydesc-or-stem &optional leaf)
  (declare (pure t) (side-effect-free t))
  (intern (concat
           (dei--dub-from-key (dei--normalize
                               (concat keydesc-or-stem leaf)))
           "/body")))

(defun dei--are-keymaps (keys)
  (declare (side-effect-free t)) ;; NOTE: Can't be pure
  (let* ((foo (--filter (keymapp (key-binding (kbd it)))
                        keys))
         ;; Special case to include the root maps if they need updating, since
         ;; the above won't catch them
         (roots (list (when (--find (s-starts-with-p "C-" it) keys)
                        "C-")
                      (when (--find (s-starts-with-p "M-" it) keys)
                        "M-")
                      (when (--find (s-starts-with-p "s-" it) keys)
                        "s-")))
         (all (-non-nil (append foo roots))))
    ;; Sort by length to avail the most relevant hydras to the user soonest.
    (when all
      (seq-sort-by #'dei--key-seq-steps-length #'< all))))

;; tests
;; (dei--are-keymaps '("C-x a" "C-M-s-f" "M-c C-o" "M-g"))

(defun dei--key-seq-steps-length (keydesc)
  (declare (pure t) (side-effect-free t))
  (length (dei--key-seq-split keydesc)))

(defun dei--stem-to-keydesc (stem)
  "Trim a space off STEM.
Would be engineered to cover stems like C-c C-, but we assume
those will never be passed.  Otherwise this could not be an
invertible function."
  (declare (pure t) (side-effect-free t))
  (cl-assert (s-ends-with-p " " stem))
  (substring stem 0 -1))

;; unused
(defun dei--keydesc-to-stem (keydesc)
  "Inverse of `dei--stem-to-keydesc'."
  (declare (pure t) (side-effect-free t))
  (concat keydesc " "))

(defun dei--drop-leaf (keydesc)
  "Chop the leaf off KEYDESC and return the resulting stem."
  (declare (pure t) (side-effect-free t))
  (s-replace-regexp (rx (literal (dei--get-leaf keydesc)) eol)
                    ""
                    keydesc))

(defun dei--hydra-from-stem (stem)
  (declare (pure t) (side-effect-free t))
  (when stem
    (intern (concat (dei--dub-from-key stem) "/body"))))

(defun dei--parent-stem (stem)
  (declare (pure t) (side-effect-free t))
  (if (or (= 2 (length stem))
          (dei--key-seq-steps=1 stem))
      nil
    (dei--drop-leaf (dei--stem-to-keydesc stem))))

;; unused
(defun dei--parent-key (keydesc)
  (declare (pure t) (side-effect-free t))
  (dei--stem-to-keydesc (dei--drop-leaf keydesc)))

(defun dei--parent-hydra (stem)
  (declare (pure t) (side-effect-free t))
  (dei--hydra-from-stem (dei--parent-stem stem)))

;; unused
;; Roughly the inverse of dei--dub-from-key (but that one does more than squash)
(defun dei--from-squashed-to-proper-keydesc (squashed-key)
  "Unsquash a squashed key description back into kbd-compatible.
SQUASHED-KEY may look like \"Cxp\", \"sxp\", \"<f12>a<RET>\".
Does not fully handle the TAB case."
  (declare (pure t) (side-effect-free t))
  (let* ((foo squashed-key)
         (foo (s-split "<" foo t))
         (foo (--mapcat (s-split ">" it t) foo))
         (foo (--map (if (and (> (length it) 1)
                              (not (equal it "TAB")))
                         (concat "<" it ">")
                       it) foo))
         (foo (cons (if (s-matches-p "[ACHMSs]" (car foo))
                        (concat (car foo) "-")
                      (concat (car foo) " "))
                    (cdr foo)))
         (foo (concat (car foo) (s-join " " (cdr foo)))))
    foo))

;; Heh, when I made this a separate function, I found my code never did what I
;; wanted.  Now fixed.  Score one for small testable functions.
(defun dei--ensure-chordonce (keydesc)
  "Strip chords from most of key sequence KEYDESC.
Leaves alone the first step of the key sequence."
  (declare (pure t) (side-effect-free t))
  (let* ((steps (dei--key-seq-split keydesc)))
    (s-join " "
            (cons (car steps) (-map #'dei--get-leaf (cdr steps))))))

(defun dei--ensure-permachord (keydesc)
  (declare (pure t) (side-effect-free t))
  (save-match-data
    (let* ((steps (dei--key-seq-split keydesc))
           (first-step (car steps))
           (root-modifier (when (dei--chord-match first-step)
                            (match-string 0 first-step))))
      (if root-modifier
          (let* ((chord-found nil)
                 (butfirst-steps (cdr steps))
                 (butfirst-steps-corrected
                  (-map (lambda (step)
                         (if (dei--chord-match step)
                             (progn
                               (setq chord-found t)
                               step)
                           (concat root-modifier step)))
                       butfirst-steps)))
            (and chord-found
                 (not (dei--key-is-permachord keydesc))
                 (warn "Maybe found bastard sequence: %s" keydesc))
            (s-join " " (cons first-step
                              butfirst-steps-corrected)))
        (warn "dei--ensure-permachord probably shouldn't be called on: %s"
              keydesc)
        keydesc))))

;; could be written simpler (I just banged it out)
(defun dei--key-is-permachord (keydesc)
  "Return t if KEYDESC looks like a permachord.
Allows only one chord, such as C- or M- but not C-M-."
  (declare (pure t) (side-effect-free t))
  (save-match-data
    (let* ((steps (dei--key-seq-split keydesc))
           (first-step (car steps))
           (root-modifier (when (dei--chord-match first-step)
                            (match-string 0 first-step))))
      (when (and root-modifier
                 (--all-p (>= (length it) 2) steps))
        (-none-p #'null
                 (cl-loop for step in steps
                          collect
                          (let ((first-bit (substring step 0 2))
                                (rest (substring step 2)))
                            (and (string-match-p root-modifier
                                                 first-bit)
                                 (not (string-match-p dei--modifier-regexp-safe
                                                      rest))))))))))



;;;; Keyboard scanning

;; Think this function is hairy and unnecessary?  We have it because the C
;; function `describe-buffer-bindings' seems the only builtin way to get this
;; information efficiently.  Inspired by `which-key--get-current-bindings' (as
;; it was in 2020, they use a different mechanism as of 2022).  Thanks!
(defun dei--current-bindings (&optional keep flush)
  "Get the list of all currently active bindings.
This ignores those masked by other keymaps, returning only the
binding in the winning keymap.

Optional argument KEEP is a regexp describing keys to keep. Can
be left at nil to keep everything.

Optional argument FLUSH is a regexp describing keys to discard
after the above filter has been applied (even if KEEP was nil).
You should pass a regexp that will catch \"ESC\" because a lot of
functions in the library aren't really ESC-aware."
  (let ((result nil)
        (ignore-keys (eval-when-compile
                       (regexp-opt '("mouse-" "remap" "scroll-bar" "select-"
                                     "switch-" "help" "-state" "which-key-"
                                     "-corner" "-divider" "-edge" "header-"
                                     "mode-line" "tab-" "vertical-line"
                                     "frame" "wheel-"))))
        (ignore-bindings (eval-when-compile
                           (regexp-opt '("self-insert-command" "ignore"
                                         "ignore-event" "company-ignore"))))
        (ignore-sections (eval-when-compile
                           (regexp-opt '("Key translations"
                                         "Function key map translations"
                                         "Input decoding map translations"))))
        (that-buffer (current-buffer))) ;; since we will go to a temp buffer
    (with-temp-buffer
      (setq-local indent-tabs-mode t)
      (describe-buffer-bindings that-buffer)
      (goto-char (point-min))
      (flush-lines ignore-bindings)
      (flush-lines (rx (regexp ignore-sections)
                       (* (not ""))))
      (flush-lines (rx "---"))
      (flush-lines (rx bol "key" (* nonl) "binding"))
      (flush-lines (rx bol (* nonl) ":" eol))
      (flush-lines (rx ""))
      (flush-lines (rx " .. "))
      (while (search-forward "\n\t" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward (rx "<" (group (regexp dei--modifier-regexp)))
                                nil t)
        (replace-match "\\1<")
        (goto-char (point-min)))
      (flush-lines (rx (regexp ignore-keys) (* nonl) "	"))
      (when keep (keep-lines keep))
      (when flush (flush-lines flush))
      (while (re-search-forward (rx (group (+? nonl)) (+ "	") (group (+ nonl)))
                                nil t)
        (push (cons (match-string 1) (match-string 2)) result)))
    result))



;;;; Library

(defvar dei-debug nil
  "A buffer name or nil.")

(defun dei--echo (x)
  (when dei-debug
    (print x (dei--debug-buffer))))

(defun dei--debug-buffer ()
  (when dei-debug
    (let ((buf (get-buffer-create dei-debug)))
      (with-current-buffer buf
        (setq-local truncate-lines t)
        buf))))

(defun dei--of-interest-p (cmd)
  "Return t if CMD is worth carrying over to another key.
It does not fail if CMD is a keymap, check that separately."
  (declare (pure t) (side-effect-free t))
  (not (member cmd '(self-insert-command
                     nil
                     ignore
                     ignore-event
                     company-ignore))))

;; REVIEW: Write test for it with a key-simulator
(defun dei-universal-argument (arg)
  "Enter a nonum hydra and activate the universal argument."
  (interactive "P")
  (prefix-command-preserve-state)
  (call-interactively
   (intern (concat
            (substring (symbol-name hydra-curr-body-fn) 0 -5) ;; chop "/body"
            "-nonum/body")))
  (hydra--universal-argument arg))

(defun dei-negative-argument (arg)
  (interactive "P")
  (prefix-command-preserve-state)
  (call-interactively
   (intern (concat
            (substring (symbol-name hydra-curr-body-fn) 0 -5) ;; chop "/body"
            "-nonum/body")))
  (hydra--negative-argument arg))

;; unused
(defun dei--call-and-return-to-root (keydesc)
  "Nice in some cases, like C-c C-c for which it's often
desirable to end up in the Control root hydra rather than exit
altogether. Say you want to call it for each item in a list of
Org headings, and next-line is bound to the standard C-n, then
you want to be able to type nccnccnccncc."
  (interactive)
  (call-interactively (key-binding (kbd keydesc)))
  (cond ((string-match-p "^C-" keydesc)
         (dei-C/body))
        ((string-match-p "^M-" keydesc)
         (dei-M/body))
        ((string-match-p "^s-" keydesc)
         (dei-s/body))
        ((string-match-p "^H-" keydesc)
         (dei-H/body))
        ((string-match-p "^A-" keydesc)
         (dei-A/body))))

;; unused
(defun dei--call-and-return-to-root* (keydesc)
  "Nice in some cases, like C-c C-c for which it's often
desirable to end up in the Control root hydra rather than exit
altogether. Say you want to call it for each item in a list of
Org headings, and next-line is bound to the standard C-n, then
you want to be able to type nccnccnccncc."
  (interactive)
  (call-interactively (key-binding (kbd keydesc)))
  (string-match (rx bol (regexp dei--modifier-regexp)) keydesc)
  (when-let ((root-modifier (match-string 0 keydesc)))
    (intern (concat (dei--dub-from-key root-modifier) "/body"))))

;; unused
(defun dei--call-and-return-to-parent (keydesc)
  (interactive)
  (call-interactively (key-binding (kbd keydesc)))
  (if-let ((parent (dei--parent-hydra (dei--drop-leaf keydesc))))
      (funcall parent)
    (hydra-keyboard-quit)))

(defun dei-cmd (stem leaf)
  (key-binding (kbd (concat stem leaf))))

(defun dei--is-unbound (stem leaf)
  (null (dei-cmd stem leaf)))

(defalias #'dei--is-bound #'dei-cmd)

;; (defun dei--is-bound (stem leaf)
;;  (dei-cmd stem leaf))

;; REVIEW: This may be slow
(defun dei--sort-like (index alist)
  "Sort ALIST by `car' element according to elements in INDEX.
That is, using INDEX as a master reference, sort ALIST to follow
it as well as possible, appending unknown members to the end.

Example: With INDEX being '(\"b\" \"c\" \"d\" \"a\"),
an example ALIST transformation may look like this:

'((\"a\" nil)           '((\"b\" nil)
  (\"b\" nil)     -->     (\"c\" nil)
  (\"z\" nil)             (\"a\" nil)
  (\"c\" nil))            (\"z\" nil))"
  (declare (pure t) (side-effect-free t))
  (append (-non-nil (cl-loop for leaf in index
                             collect (--find (equal leaf (car it))
                                             alist)))
          (->> (-difference (-map #'car alist) index)
               (--map (assoc it alist)))))

;; (dei--sort-like '("b" "c" "d" "a") '(("a") ("b") ("z") ("c")))


;;;; Hydra blueprinting

(defun dei--head-arg-cmd (stem leaf)
  "See `dei--head'."
  (cond ((member (concat stem leaf) dei--keys-that-are-hydras)
         (dei--corresponding-hydra stem leaf))
        (t
         `(call-interactively (key-binding (kbd ,(concat stem leaf)))))))

(defun dei--head-arg-hint (stem leaf)
  "See `dei--head'."
  (let* ((sym (if (member (concat stem leaf) dei--keys-that-are-hydras)
                  (dei--corresponding-hydra stem leaf)
                (key-binding (kbd (concat stem leaf)))))
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
  (when (or (member (concat stem leaf) dei--keys-that-are-hydras)
            (member (concat stem leaf) dei-quitter-keys)
            (member (key-binding (kbd (concat stem leaf))) dei-quitter-commands))
    '(:exit t)))

(defun dei--head (stem leaf)
  "Return a hydra head specification (sexp), see `defhydra'."
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

;; Lists of heads

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
  (let ((leaf-list (-difference dei--hydra-keys-list
                                verboten-leafs)))
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
            ;; TODO: Use all-keys-on-keyboard ... But not the function keys
            (cl-loop for leaf in  '("<left>" "<right>" "<up>" "<down>"
                                    "=" "\\" "'" "`")
                     collect (dei--head-invisible stem leaf))
            (cl-loop for leaf in (append dei--all-shifted-symbols-list
                                         '("<SPC>" "<RET>"))
                     collect (dei--head-invisible-self-inserting-stemless stem leaf))
            (cl-loop for leaf in '("<menu>" "C-g") ;; use dei-quitter-keys
                     collect (dei--head-invisible-exiting-stemless stem leaf)))))
    (-remove (lambda (head)
               (member (car head) verboten-leafs))
             x)))

(defun dei--convert-head-for-nonum (head)
  "If hydra head HEAD involves a keyboard prefix command, fix it for use in a nonum hydra.
Basically, change `dei-universal-argument' to
`hydra--universal-argument' and drop any :exit keyword."
  (cond ((eq (cadr head) 'dei-universal-argument)
         (list (car head) 'hydra--universal-argument))
        ((eq (cadr head) 'dei-negative-argument)
         (list (car head) 'hydra--negative-argument))
         (t
          head)))

(defun dei--specify-extra-heads (stem &optional nonum-p)
  "For a hydra imaged by STEM, return some bonus heads for it.
These are mostly the kinds of heads shared by all of Deianira's
hydras."
  (let ((self-poppers
         (cond ((string= "C-" stem) '("<katakana>" "C-<katakana>"))
               ((string= "M-" stem) '("<muhenkan>" "M-<muhenkan>"))
               ((string= "s-" stem) '("<henkan>" "s-<henkan>"))
               ((string= "H-" stem) '("<f32>" "H-<f32>"))
               ((string= "A-" stem) '("<f31>" "A-<f31>"))))
        (extras (if nonum-p
                    (mapcar #'dei--convert-head-for-nonum dei-extra-heads)
                  dei-extra-heads)))
    (-non-nil
     `(,(if nonum-p
            `("<backspace>" ,(dei--hydra-from-stem stem) :exit t)
          `("<backspace>" ,(dei--parent-hydra stem) :exit t))
       ,@(when self-poppers
           (cl-loop for key in self-poppers
                    collect `(,key nil :exit t)))
       ,@extras))))

;; Test
;; (dei--specify-extra-heads "C-x ")
;; (dei--specify-dire-hydra "C-x " (dei--dub-from-key "C-x "))

(defun dei--specify-dire-hydra (stem name &optional nonum-p)
  "The real magic kinda happens here."
  (let* ((extra-heads (dei--specify-extra-heads stem nonum-p))
         (verboten-leafs (append (-map #'car extra-heads)
                                 (when nonum-p
                                   (split-string "1234567890" "" t))))
         (heads (append
                 extra-heads
                 (dei--specify-visible-heads stem verboten-leafs)
                 (dei--specify-invisible-heads stem verboten-leafs))))
    (-cons* stem name heads)))

(defun dei--specify-dire-hydras ()
  "Call `dei--specify-dire-hydra' lots of times."
  (setq dei--live-stems (-difference dei--live-stems
                                     dei--defunct-stems))
  (setq dei--all-stems (-uniq (append dei--new-or-changed-stems
                                      dei--live-stems)))
  (setq dei--keys-that-are-hydras
        (-difference (-map #'s-trim-right dei--all-stems)
                     ;; subtract the root stems since they are still invalid
                     ;; keydescs, which is ok bc no hydra refers to them
                     '("C-" "M-" "s-" "H-" "A-")))
  (setq dei--hydra-blueprints
        (cl-loop
         for stem in dei--new-or-changed-stems
         append (list (dei--specify-dire-hydra stem
                                            (dei--dub-from-key stem))
                      (dei--specify-dire-hydra stem
                                            (concat (dei--dub-from-key stem) "-nonum")
                                            t)))))

;; Final boss
(defun dei--call-defhydra (name list-of-heads)
  "Create a hydra named after STEM with LIST-OF-HEADS.
This will probably be called by `dei--generate-hydras-async',
which see."
  (eval `(defhydra ,(intern name)
           (:columns ,dei-columns
            :exit nil
            :hint nil
            :foreign-keys run
            :post (dei-generate-hydras-async))
           ,name
           ,@list-of-heads)
        t))


;;;; Async worker

(defvar dei--after-scan-bindings-hook nil
  "Things to do after updating `dei--current-bindings'.
Use this to unbind new keys that may have appeared due to a
buffer change or major mode change. You should remove those
records from dei--current-bindings when you do that as we will not
rescan.")

(defvar dei--after-rebuild-hydra-hook nil
  "Hook run after updating hydras to match the local map.")

;; TODO: refactor to not operate on car, makes weird name
(defun dei--car-is-illegal-key (cell)
  "Filter for keys that should be unbound.
CELL comes in the form returned by `dei--get-relevant-bindings'."
  ;; (declare (pure t) (side-effect-free t))
  (let ((keydesc (car cell))
        (case-fold-search nil))
    (or
     ;; (dei--key-contains-ctl keydesc)
     (s-contains-p "S-" keydesc)
     ;;(s-matches-p (rx nonl (or "DEL" "<backspace>")) keydesc)
     ;; (s-matches-p (rx nonl (or "SPC" "RET" "<return>")) keydesc)
     ;; (s-matches-p (rx (or "SPC" "RET" "<return>") nonl) keydesc)
     (dei--key-contains-multi-chords keydesc)
     (dei--key-seq-mixes-modifiers keydesc)

     ;; Unbind bastard sequences
     (and (not (dei--key-seq-is-allchord keydesc))
          (dei--key-has-more-than-one-modifier keydesc))

     ;; unbind e.g. <f1> C-f.
     (and (not (dei--key-starts-with-modifier keydesc))
          (string-match-p dei--modifier-regexp-safe keydesc))

     ;; TODO: fuck, wrote same function twice... which is faster?
     (dei--key-seq-involves-shiftsym keydesc)
     (dei--contains-upcase keydesc))))

(defun dei--unbind-illegal-keys ()
  "Unbind keys that match `dei--car-is-illegal-key'."
  (let* ((illegal-keys (->> dei--current-bindings
                            (-filter #'dei--car-is-illegal-key)
                            (-map #'car)
                            (seq-sort-by #'dei--key-seq-steps-length #'>))))
    (dolist (key illegal-keys)
      ;; Find the map in which to unbind it.
      (let ((map (help--binding-locus (kbd key) nil)))
        ;; there's a bug that leaves some keys in `describe-bindings' but not
        ;; in the apparently active map, and they get a null map. Check
        ;; dei--current-bindings, C-M-q and C-M-@ are in there
        (unless (null map)
          (define-key (eval map t) (kbd key) nil))))))

(defun dei--foreign-to-keyboard (step)
  "Return t if STEP is not among `all-keys-on-keyboard'."
  ;; NOTE: A change to dei-all-keys-on-keyboard will NOT affect us compiled.
  ;; This is fine as long as we take it as a constant anyway.
  (declare (pure t) (side-effect-free t))
  (not (member (dei--get-leaf step) dei-all-keys-on-keyboard)))

(defun dei--filter-for-hydra-making (cell)
  "Filter for keys that are irrelevant when we make a hydra."
  (let ((keydesc (car cell))
        (case-fold-search nil))
    (or
     (dei--key-has-more-than-one-modifier keydesc)
     (-any-p #'dei--foreign-to-keyboard
             (dei--key-seq-split keydesc))
     ;; Attempt to find iso-transl-ctl-x-8-map, usually C-x 8 but user could
     ;; have relocated it (which is prolly smart). TODO: save in external
     ;; variable for perf
     (member keydesc
             (-map #'dei--parent-key
                   ;; necessary to get intended effec from `dei--parent-key'
                   (-remove #'dei--key-has-more-than-one-modifier
                            (dei--where-is #'insert-char)))))))

(defun dei--get-relevant-bindings ()
  (->> (dei--current-bindings
        nil
        (rx (or
             (regexp (eval-when-compile
                       (regexp-opt
                        '("ESC" ;; critical to filter out, represents M-combos
                          "TAB" "DEL" "backspace" ;; one day, I will let unbind-illegal-keys handle these
                          "<key-chord>" "<compose-last-chars>" "Scroll_Lock"
                          "-margin>" "-fringe>" "iso-leftab" "iso-lefttab"
                          "drag-n-drop" "wheel-"
                          ))))
             ;; "C-"
             (seq string-start
                  (or "<f1>"
                      "C-h"  ;; user should fix this keymap emself
                      ;; TODO: make hydras still, without unbinding violators
                      (literal doom-leader-alt-key) ;; fulla Shift
                      (literal doom-localleader-alt-key))))))
       (-map (lambda (x)
               (cons (dei--normalize (car x))
                     (cdr x))))
       (seq-sort-by #'car #'string-lessp)
       (seq-sort-by #'dei--binding-key-seq-steps-length #'>)
       (setq dei--current-bindings)
       ;; Remove submaps, we infer their existence later by cutting the leafs
       ;; off every actual key via (-uniq (-map #'dei--drop-leaf KEYS)).
       (-remove (lambda (x)
                  (or (string= "Prefix Command" (cdr x))
                      (null (intern (cdr x)))
                      (keymapp (intern (cdr x))))))
       (-remove #'dei--car-is-illegal-key)
       (-remove #'dei--filter-for-hydra-making)
       (setq dei--current-filtered-bindings)))


;; Dafuq was I doing here?

;; (advice-add #'read-key-sequence :around #'dei--kill-ctl)
;; (advice-add #'read-event :override #'dei--kill-ctl)
;; (advice-remove #'read-event #'dei--kill-ctl)
;; (advice-add #'read-key-sequence :after #'dei--restore-ctl)

;; (defun my-kill-ctl (&rest _)
;;   (define-key input-decode-map (kbd "C-p") (kbd "p"))
;;   _)

;; (defun my-restore-ctl (&rest _)
;;   (define-key input-decode-map (kbd "C-p") nil)
;;   _)

;; (defun dei--sort-bindings (bindings)
;;   (declare (pure t) (side-effect-free t))
;;   (seq-sort-by #'dei--binding-key-seq-steps-length #'<
;;                (seq-sort-by #'car #'string-lessp bindings)))

(defun dei--binding-key-seq-steps-length (x)
  (declare (pure t) (side-effect-free t))
  (dei--key-seq-steps-length (car x)))

(add-hook 'dei--after-scan-bindings-hook #'dei--unbind-illegal-keys -5)
(add-hook 'dei--after-scan-bindings-hook #'dei--mass-remap 5)

;; NOTE: Visualize a Venn diagram. The defunct and the new are the last and
;;      current minus their intersection (cases where the key's definition
;;      didn't change), which should never be relevant to look at.
(defun dei--target-stems ()
  (setq dei--defunct-bindings (-difference dei--last-filtered-bindings
                                           dei--current-filtered-bindings))
  (setq dei--new-or-changed-bindings (-difference dei--current-filtered-bindings
                                                  dei--last-filtered-bindings))
  (setq dei--defunct-stems
        (->> dei--defunct-bindings
             (-map #'car)
             (-map #'dei--drop-leaf)
             (-remove #'string-empty-p) ;; keys like <insertchar> have "" stem
             (-uniq)))
  (setq dei--new-or-changed-stems
        (->> dei--new-or-changed-bindings
             (-map #'car)
             ;; Sort to avail the most relevant hydras to the user soonest.
             ;; Only matters if we run defhydra from an async loop.
             ;; REVIEW: Is the direction correct, considering we'll use push?
             (seq-sort-by #'dei--key-seq-steps-length #'>)
             (-map #'dei--drop-leaf)
             (-remove #'string-empty-p) ;; keys like <insertchar> have "" stem
             (-uniq))))

;; Persistent variables helpful for debugging
(defvar dei--keys-that-are-hydras nil)
(defvar dei--current-bindings nil)
(defvar dei--current-filtered-bindings nil)
(defvar dei--last-filtered-bindings nil)
(defvar dei--new-or-changed-bindings nil)
(defvar dei--new-or-changed-stems nil)
(defvar dei--defunct-bindings nil)
(defvar dei--defunct-stems nil)
(defvar dei--live-stems nil)
(defvar dei--all-stems nil)
(defvar dei--changed-keymaps nil)
(defvar dei--hydra-blueprints nil)
;; (setq dei--hydra-blueprints nil)

(defvar dei--last-thread (cc:thread 0 nil))

;; TODO: the cc:thread sets an input variable x which the compiler then cries
;; about for unuse. can I contrive a way to use it?
;; FIXME: Deosnt' work
(defun dei-generate-hydras-async ()
  "Regenerate hydras to match the local map."
  (interactive)
  ;; Not sure if the cancellation is instant, so we'll safety-wrap with a timer.
  (deferred:cancel dei--last-thread)
  (if (null (deferred:status dei--last-thread)) ;; i think null means still running
      ;; Wait and try again.
      (named-timer-run :deianira 1 nil #'dei-generate-hydras-async)
    (setq dei--last-thread
          (cc:thread 50
            #'dei--get-rexclevant-bindings
            (run-hooks 'dei--after-scan-bindings-hook)
            #'dei--get-relevant-bindings
            #'dei--target-stems
            ;; Cache settings
            (setq dei--colwidth (dei--colwidth-recalc)
                  dei--filler (dei--filler-recalc)
                  dei--all-shifted-symbols-list (dei--all-shifted-symbols-list-recalc)
                  dei--hydra-keys-list (dei--hydra-keys-list-recalc))
            ;; idk
            (progn
              (setq dei--live-stems (-difference dei--live-stems
                                              dei--defunct-stems))
              (setq dei--all-stems (-uniq (append dei--new-or-changed-stems
                                               dei--live-stems)))
              (setq dei--keys-that-are-hydras
                    (-difference (-map #'s-trim-right dei--all-stems)
                                 ;; subtract the root stems since they are still invalid
                                 ;; keydescs, which is ok bc no hydra refers to them
                                 '("C-" "M-" "s-" "H-" "A-"))))
            ;; Draw blueprints
            (deferred:loop (prog1 dei--new-or-changed-stems
                             (setq dei--hydra-blueprints nil))
              (lambda (stem)
                (push (dei--specify-dire-hydra stem
                                            (concat (dei--dub-from-key stem) "-nonum")
                                            t)
                      dei--hydra-blueprints)))
            ;; Draw more blueprints
            (deferred:loop dei--new-or-changed-stems
              (lambda (stem)
                (push (dei--specify-dire-hydra stem
                                            (dei--dub-from-key stem))
                      dei--hydra-blueprints)))
            ;; Build hydras
            ;; TODO: deferred loop maybe here too
            ;; REVIEW: is this condition reliable?
            ;; TODO: bake the condition into call-defhydra so it refuses only to
            ;;       redefine the very hydra that's active
            (unless (dei--hydra-active-p)
              (cl-loop
               for x in dei--hydra-blueprints
               do (progn (dei--call-defhydra (cadr x) (cddr x))
                         (push (car x) dei--live-stems)))
              (setq dei--last-filtered-bindings dei--current-filtered-bindings))
            (run-hooks 'dei--after-rebuild-hydra-hook)))))


;;;; Bonus functions for mass remaps

(defcustom dei-permachord-wins-homogenizing nil
  "Non-nil if perma-chord should win over chord-once.
This means that the behavior of a perma-chorded sequence such as
C-x C-k C-e will be kept as is, and cloned to the chord-once
sequence C-x k e, overwriting any binding there, so they both do
what the former always did.  If the setting is nil, both will
instead do what C-x k e always did.

PLEASE NOTE that any calls to `define-key' in your initfiles have
to take this into account!  If this is t, all sequences have to
be written in perma-chord style to be sure the choice sticks.  If
this is nil, they have to be written in the chord-once style to
be sure the choice sticks.  (Of course, you can always bind both
while you experiment with this setting.)

This variable sets the default approach, but you can override
specific cases in `dei-homogenizing-winners'; and if only one
variety is bound but not the other, the binding will be cloned no
matter the direction since there is no contest."
  :type 'boolean
  :group 'deianira)

(defcustom dei-homogenizing-winners
  '(("C-c C-c")
    ("C-x C-f")
    ("C-x C-s")
    ("C-x C-;")
    ("C-x a")
    ("C-x g")
    ("C-x b")
    ("C-x n")
    ("C-x p")
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
  :type '(repeat sexp)
  :group 'deianira)

(defvar dei--remap-actions nil
  "List of actions to pass to `define-key'.")

;; dev settings
(general-auto-unbind-keys 'undo) ;; we shouldn't rely on general
(remove-hook 'after-save-hook 'my-compile-and-drop)

(defconst dei--ignore-keys
  (regexp-opt '("mouse" "remap" "scroll-bar" "select" "switch" "help" "state"
                "which-key" "corner" "divider" "edge" "header" "mode-line"
                "tab" "vertical-line" "frame" "open" "menu" "kp-" "iso-"
                "wheel-"))
  "Regexp for some key bindings that don't interest us.")

(defun dei--where-is (command &optional keymap)
  "Find to which key COMMAND is bound.
Optional argument KEYMAP means look only in that keymap."
  (->> (where-is-internal command (when keymap (list keymap)))
       (-map #'key-description)
       (--remove (string-match-p dei--ignore-keys it))
       (-map #'dei--normalize)))

;; Weird function, but named after string-match...
;; TODO: use something else
(defun dei--chord-match (keydesc)
  (cl-assert (dei--key-seq-steps=1 keydesc))
  (string-match (rx bol nonl "-") keydesc))

;; to debug, eval each in turn, btw after-scan-bindings-hook won't run
;; (dei--get-relevant-bindings)
;; (dei--unbind-illegal-keys)
;; (dei--get-relevant-bindings)
;; (dei--mass-remap)
;; (dei-reset)
(defun dei--mass-remap ()
  (setq dei--remap-actions nil)
  (setq dei--remap-record nil)
  (let ((winners-sin-keymaps (-remove #'cdr dei-homogenizing-winners))
        (winners-with-keymaps (-filter #'cdr dei-homogenizing-winners)))
    (dolist (x (-remove (lambda (y)
                          (or (not (dei--key-starts-with-modifier (car y)))
                              (string= "Prefix Command" (cdr y))
                              (null (intern (cdr y)))
                              (keymapp (intern (cdr y)))))
                        dei--current-bindings))
      ;; Observe that this loop only operates on keys known to have a binding.
      ;; Suppose the user has specifications in the dei-homogenizing-winners without
      ;; any binding.  We'll never consider those, and that's good.
      ;; In fact, we only need to specially run dei--homogenize-binding on those
      ;; winners that also specify keymap, since the normal invocations will
      ;; notice if a key is a member of the winners anyway.
      (when-let (winner (assoc (car x) winners-with-keymaps))
        (dei--homogenize-binding-in-keymap (car winner) (cdr winner)))
      ;; (when-let (winner (assoc (intern (cdr x)) winners-with-keymaps))
      ;;  (dei--homogenize-binding-in-keymap (car winner) (cdr winner)))
      (dei--homogenize-binding (car x))))
  (dei--execute-remap-actions dei--remap-actions)
  )
;; (dei--homogenize-binding "C-c C-c" 'org-mode-map)
;; (dei--get-relevant-bindings)
;; (dei--mass-remap)

(defun dei--as-raw-keymap (keymap-or-keymapsym)
  "If KEYMAP-OR-KEYMAPSYM is a symbol, return its global value.
Otherwise return it as it was input.

This can be an useful wrapper when you pass keymap variables,
because some keymaps are named symbols (meeting `symbolp') and
some aren't (meeting `keymapp')."
  (if (symbolp keymap-or-keymapsym)
      (symbol-value keymap-or-keymapsym)
    keymap-or-keymapsym))

(defun dei--key-seq-has-non-prefix-in-prefix (keydesc)
  "Return t if KEYDESC contains a bound command in its prefix.
For example: C-x C-v is bound to a command by default, so if you
attempt to bind C-x C-v C-x to a command, you get an error.  So
here we check for that.  If KEYDESC is C-x C-v C-x, we check if
either C-x or C-x C-v are bound to anything.

Does not additionally check that KEYDESC is not itself a prefix
map with children bound, another thing that makes KEYDESC
unbindable."
  (let ((steps (dei--key-seq-split keydesc))
        (subsequences nil))
    (when (> (length steps) 1)
      (dotimes (i (- (length steps) 1))
        ;; uhh
        (push (key-binding (kbd (s-join " " (-take (+ 1 i) steps))))
              subsequences))
      (-any-p #'functionp subsequences))))

(defvar dei--remap-record nil
  "Record of work done by `dei--homogenize-binding-in-keymap'.
We use it to avoid clobbering by successive rounds of
remappings.")

(defun dei--homogenize-binding-in-keymap (key-or-cmd keymap)
  (let* ((this-command
          (if (functionp key-or-cmd)
              key-or-cmd
            (lookup-key (symbol-value keymap) (kbd key-or-cmd))))
         (this-keydesc
          (if (stringp key-or-cmd)
              key-or-cmd
            (dei--where-is key-or-cmd (symbol-value keymap))))
         (steps (dei--key-seq-split this-keydesc)))
    ;; REVIEW: what do if this-command is another keymap? This really can
    ;; happen b/c this function is run taking user input as is.
    (when (and this-command
               this-keydesc
               (/= 1 (length steps)) ;; nothing to homogenize if length 1
               (dei--chord-match (car steps)) ;; only proceed if seq starts w a chord
               (functionp this-command))
      (let* (;; NOTE: we are assuming there exist no "bastard sequences",
             ;; they've been filtered out by `dei--unbind-illegal-keys'. In
             ;; addition, if it's an unchorded seq we're not here.  So we only
             ;; have chord-once and perma-chord.
             (this-is-permachord-p (dei--key-is-permachord this-keydesc))
             (permachord-keydesc
              (if this-is-permachord-p
                  this-keydesc
                (dei--ensure-permachord this-keydesc)))
             (permachord-command
              (if this-is-permachord-p
                  this-command
                (lookup-key (symbol-value keymap) (kbd permachord-keydesc))))
             (chordonce-keydesc
              (if this-is-permachord-p
                  (dei--ensure-chordonce this-keydesc)
                this-keydesc))
             (chordonce-command
              (if this-is-permachord-p
                  (lookup-key (symbol-value keymap) (kbd chordonce-keydesc))
                this-command))
             (sibling-keydesc (if this-is-permachord-p
                                  chordonce-keydesc
                                permachord-keydesc))
             (sibling-command (if this-is-permachord-p
                                  chordonce-command
                                permachord-command))
             (action nil))

        (cond
         ;; We ended up here due to this type of situation: there exists a key
         ;; C-x v x, and there exists a key C-x C-v (this-keydesc).  Meet
         ;; failure if cloning to the sibling C-x v.
         ((or (keymapp sibling-command)
              (and (symbolp sibling-command)
                   (boundp sibling-command)
                   (keymapp (dei--as-raw-keymap sibling-command))))
          (warn "Sibling key %s binds to a keymap" sibling-keydesc))

         ;; We ended up here due to this type of situation: there exists a key
         ;; C-x v x (this-keydesc), and there exists key C-x C-v that is not a
         ;; prefix key.  Meet failure if cloning to the sibling C-x C-v C-x.
         ((dei--key-seq-has-non-prefix-in-prefix sibling-keydesc)
          (warn "Sibling key %s is unbindable" sibling-keydesc))

         ;; No problem, proceed.
         ((null sibling-command)
          (setq action (list (kbd sibling-keydesc)
                             this-command
                             keymap
                             "Clone to overwrite unbound sibling       "))))
        (when (and action
                   (not (member action dei--remap-record)))
          (push action dei--remap-actions)
          (push action dei--remap-record))))))

;; TODO: operate multiple times if multiple keymaps found by
;; help--binding-locus, or multiple keys found by where-is-internal
(defun dei--homogenize-binding (key-or-cmd)
  "Duplicate THIS-KEYDESC binding to or from its \"sibling\".
Actually just populates `dei--remap-actions', which you can peep
on during debugging with \\[dei--preview-remap-actions].  To carry
out the remaps, call `dei--execute-remap-actions'.

Assumes that the command to be bound is not itself a keymap,
because those will be implied by binding their children anyway.
You can use `dei--get-relevant-bindings' to filter out keymaps,
and run this function on the results."
  (let* ((this-command
          (if (functionp key-or-cmd)
              key-or-cmd
            (key-binding (kbd key-or-cmd))))
         (this-keydesc
          (if (stringp key-or-cmd)
              key-or-cmd
            ;; NOTE: only takes first key found
            (dei--normalize (key-description
                          (where-is-internal key-or-cmd nil t)))))
         (steps (dei--key-seq-split this-keydesc)))
    ;; REVIEW: is this-command ever... a keymap? I think not, bc we filter them
    ;; out in dei--mass-remap.
    (when (and this-command
               this-keydesc
               (/= 1 (length steps)) ;; nothing to homogenize if length 1
               (dei--chord-match (car steps)) ;; only proceed if seq starts w a chord
               (functionp this-command))
      (let* (;; NOTE: Only takes first keymap found.  Hope it's the same map
             ;; as (key-binding ...)'s origin.
             (in-map (help--binding-locus (kbd this-keydesc) nil))
             ;; NOTE: we are assuming there exist no "bastard sequences",
             ;; they've been filtered out by `dei--unbind-illegal-keys'. In
             ;; addition, if it's an unchorded seq we're not here.  So we only
             ;; have chord-once and perma-chord.
             (this-is-permachord-p (dei--key-is-permachord this-keydesc))
             (permachord-keydesc (if this-is-permachord-p
                                     this-keydesc
                                   (dei--ensure-permachord this-keydesc)))
             (permachord-command (key-binding (kbd permachord-keydesc)))
             (chordonce-keydesc (if this-is-permachord-p
                                    (dei--ensure-chordonce this-keydesc)
                                  this-keydesc))
             (chordonce-command (key-binding (kbd chordonce-keydesc)))
             (sibling-keydesc (if this-is-permachord-p
                                  chordonce-keydesc
                                permachord-keydesc))
             (sibling-command (if this-is-permachord-p
                                  chordonce-command
                                permachord-command))
             (winners (->> dei-homogenizing-winners
                               (-remove #'cdr) ;; drop items with a keymap
                               (-map #'car)))
             (action nil))

        (cond
         ;; Simple case: This key or the sibling key has already been dealt
         ;; with for this keymap by `dei--homogenize-binding-in-keymap'.  Then we
         ;; just no-op.  This approach is simpler than naively populating
         ;; `dei--remap-actions' and teasing out conflicts afterwards, but
         ;; requires we do things in order.
         ((or (when-let ((found (assoc (kbd this-keydesc)
                                       dei--remap-record)))
                (equal (nth 2 found) in-map))
              (when-let ((found (assoc (kbd sibling-keydesc)
                                       dei--remap-record)))
                (equal (nth 2 found) in-map))))
         ;; Complex case #1: both keys have a command, which do we choose?
         ;; Eeny meny miny moe...? No, let's start by checking if one of
         ;; them is a specified winner, then fall back on a rule.
         ((functionp sibling-command)
          (cond ((< 1 (length (-non-nil
                               (list (member sibling-keydesc winners)
                                     (member sibling-command winners)
                                     (member this-keydesc winners)
                                     (member this-command winners)))))
                 ;; Leave it on the user to fix this mess.
                 (warn "Found a contradiction in dei-homogenizing-winners."))
                ((or (member sibling-keydesc winners)
                     (member sibling-command winners))
                 (setq action
                       (list (kbd this-keydesc)
                             sibling-command
                             in-map
                             "Clone winner sibling to overwrite this key")))
                ((or (member this-keydesc winners)
                     (member this-command winners))
                 (setq action
                       (list (kbd sibling-keydesc)
                             this-command
                             in-map
                             "Clone this winner to overwrite sibling key")))
                ;; Neither key and neither command is rigged to win, so fall
                ;; back on some simple rule.
                ;;
                ;; NOTE that here is the only place where this user option
                ;; comes into play!  You'd think it'd affect more code, but
                ;; this monster defun would be necessary even without the
                ;; user option.
                (dei-permachord-wins-homogenizing
                 (setq action
                       (list (kbd chordonce-keydesc)
                             permachord-command
                             in-map
                             "Clone perma-chord to chord-once           ")))
                (t
                 (setq action
                       (list (kbd permachord-keydesc)
                             chordonce-command
                             in-map
                             "Clone chord-once to perma-chord           ")))))
         ;; Complex case #2: turns out the other is a keymap, despite our
         ;; efforts!  We ended up here due to this type of situation: there
         ;; exists a key C-x v x and others under the C-x v prefix, and there
         ;; exists a key C-x C-v which is just a command. If we try to just
         ;; clone the latter to C-x v, we may succeed, because `dei--define' has
         ;; code for that.  But if we then clone C-x v x to C-x C-v
         ;; C-x... BAM!
         ((or (keymapp sibling-command)
              (and (symbolp sibling-command)
                   (boundp sibling-command)
                   (keymapp (dei--as-raw-keymap sibling-command))))
          (warn "Sibling key binds to a keymap: %s" sibling-keydesc))
         ;; Case 3
         ((dei--key-seq-has-non-prefix-in-prefix sibling-keydesc)
          (warn "Sibling key %s is unbindable" sibling-keydesc))
         ;; Simple case: only one of the two is bound, so just duplicate.  We
         ;; don't need to do it both directions b/c this invocation is
         ;; operating on this-keydesc which is the one known to have a
         ;; command.
         ((null sibling-command)
          (setq action
                (list (kbd sibling-keydesc)
                      this-command
                      in-map
                      "Clone to overwrite unbound sibling        "))))
        (when action
          (push action dei--remap-actions))))))

(defun dei--execute-remap-actions (actions)
  "Carry out remaps specified by ACTIONS.
See other uses for the format."
  (dolist (action actions)
    (seq-let (event cmd map _) action
      (dei--define (if (keymapp map)
                    map
                  (symbol-value map))
                event
                cmd)))
  (setq dei--remap-actions nil))

(defun dei--preview-remap-actions ()
  "For convenience while debugging."
  (interactive)
  (with-current-buffer (dei--debug-buffer)
    (goto-char (point-min))
    (newline)
    (insert "Remap actions planned as of " (current-time-string))
    (newline 2)
    (let ((sorted-actions (seq-sort-by (lambda (x)
                                         (symbol-name (nth 2 x)))
                                       #'string-lessp
                                       dei--remap-actions)))
      (dolist (item sorted-actions)
        (seq-let (event cmd map hint) item
          (insert hint
                  " Bind  " (key-description event)
                  "\tto  " (if (symbolp cmd)
                               (symbol-name cmd)
                             cmd)
                  (if (symbolp map)
                      (concat "\t\t  (" (symbol-name map) ")")
                    "")))
        (newline)))
    (goto-char (point-min)))
  (display-buffer (dei--debug-buffer))
  (when-let ((window (get-buffer-window (dei--debug-buffer))))
    (with-selected-window window
      (recenter 0)
      (read-only-mode))))

(defun dei--wipe-remap-actions ()
  "For convenience while debugging."
  (interactive)
  (setq dei--remap-actions nil)
  (message "remap actions wiped"))

(defun dei--keymap-children-keys (keymap)
  "Return a list of key descriptions of keys in KEYMAP.
Check for yourself:
  (setq foo (dei--keymap-children-keys ctl-x-map))  \\[eval-last-sexp]
  \\[describe-variable] foo RET

Note that these strings omit whatever prefix key led up to KEYMAP."
  (let ((lst nil))
    (map-keymap (lambda (ev def)
                  (if (keymapp def)
                      (setq lst
                            (append lst
                                    (--map (concat (key-description (vector ev)) " " it)
                                           (dei--keymap-children-keys def))))
                    (push (key-description (vector ev)) lst)))
                keymap)
    lst))

(defun dei--define (map event new-def)
  "Like `define-key'."
  ;; for debug
  (dei--echo (list "Will call `define-key' with args:"
                   "MAP"
                   event
                   new-def))
  (progn
    (cl-assert (keymapp map))
    (let ((old-def (lookup-key map event)))
      ;; NOTE: Here be a destructive action.
      ;;
      ;; If this key was previously bound to a keymap, we want to unbind
      ;; them all first.  If we don't, we can't rebind the key with define-key.
      ;; Note that you may have general-auto-unbind-keys on, which advises define-key.
      (when (keymapp old-def)
        (let ((children-w-full-keydesc
               (--map (concat (key-description event) " " it)
                      (dei--keymap-children-keys old-def))))
          (dolist (child children-w-full-keydesc)
            (define-key map (kbd child) nil)))))
      (define-key map event new-def)))

;; (dei--define global-map (kbd "M-s") nil)

;; (setq dei-debug "*Deianira debug*")
  ;; (dei--homogenize-binding "C-x d")



;; (help--binding-locus (kbd "C-x n w") nil)
;; (help--binding-locus (kbd "C-c") nil)

;; TODO: do this in keymaps found by help--binding-locus for the capitals
;; found in dei--current-bindings.
(defun dei-bind-all-shiftsyms-to-insert ()
  "Bind all capital letters and shift symbols to self-insert."
  (dolist (leaf dei--all-shifted-symbols-list)
    (global-set-key (kbd leaf) #'self-insert-command)))


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
;; (advice-add #'selectrum-read :before #'dei--slay)
;; (advice-remove #'selectrum-read #'dei--slay)

(defun dei--slay-if-minibuffer (&rest args)
  "Slay any hydra if the minibuffer is active."
  (when (or (minibufferp)
            (string-match-p "magit:" (buffer-name))) ;; doesnt work i think
    (dei--slay))
  args)

(defun dei-reset ()
  "Re-generate hydras from scratch."
  (interactive)
  (setq dei--last-filtered-bindings nil)
  (dei--get-relevant-bindings)
  (dei--mass-remap)
  ;; (dei--unbind-illegal-keys)
  (dei--get-relevant-bindings)
  (dei--target-stems)
  (dei--specify-dire-hydras)
  (cl-loop for x in dei--hydra-blueprints
           do (progn
                (dei--call-defhydra (cadr x) (cddr x))
                (push (car x) dei--live-stems)))
  (setq dei--last-filtered-bindings dei--current-filtered-bindings))

(defvar dei--old-hydra-cell-format nil
  "Backup for `hydra-cell-format'.")

(defvar dei--old-hydra-base-map-C-u nil
  "Backup for key-binding of \"C-u\" in `hydra-base-map'.")

;;;###autoload
(define-minor-mode deianira-mode
  "Bind root hydras.
In addition, configure window change hooks and certain hydra.el
settings."
  :global t
  :lighter " Δ"
  :group 'deianira
  :keymap `((,(kbd "<katakana>") . dei-C/body)
            (,(kbd "<muhenkan>") . dei-M/body)
            (,(kbd "<henkan>") . dei-s/body)
            (,(kbd "<f24>") . dei-H/body)
            (,(kbd "<f31>") . dei-A/body)
            ;; (,(kbd "<f2>") . dei-<f2>/body)
            ;; (,(kbd "<f10>") . dei-<f10>/body)
            ;; in case of sticky keys
            (,(kbd "C-<katakana>") . dei-C/body)
            (,(kbd "M-<muhenkan>") . dei-M/body)
            (,(kbd "s-<henkan>") . dei-s/body)
            (,(kbd "H-<f24>") . dei-H/body)
            (,(kbd "A-<f31>") . dei-A/body))

  (require 'hydra)
  ;; (setq dei--old-hydra-cell-format hydra-cell-format)
  ;; (setq dei--old-hydra-base-map-C-u (lookup-key hydra-base-map (kbd "C-u")))
  (if deianira-mode
      (progn
        ;; Does not work for the consult-* functions with selectrum-mode.
        ;; It's ok because they ignore the hydra and let you type, but why?
        (add-hook 'window-buffer-change-functions #'dei--slay-if-minibuffer)
        (setq dei--old-hydra-cell-format hydra-cell-format)
        (setq dei--old-hydra-base-map-C-u (lookup-key hydra-base-map (kbd "C-u")))
        (setq hydra-cell-format "% -20s %% -11`%s")
        (define-key hydra-base-map (kbd "C-u") nil)
        (when (null dei--live-stems)
          (dei-reset)))

    (setq hydra-cell-format (or dei--old-hydra-cell-format "% -20s %% -8`%s"))
    (define-key hydra-base-map (kbd "C-u") dei--old-hydra-base-map-C-u)
    (remove-hook 'window-buffer-change-functions #'dei--slay-if-minibuffer)))

(provide 'deianira)
;;; deianira.el ends here
