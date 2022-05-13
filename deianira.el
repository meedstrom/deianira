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
  :type 'number
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
  "Keys that kill the hydra."
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
  "Commands that kill the hydra.
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
  (->> keydesc
       (s-split (rx space))
       (-map #'dei--normalize-trim-segment)
       (-map #'dei--normalize-get-atoms)
       (-map #'dei--normalize-wrap-leaf-maybe)
       (-map #'dei--normalize-build-segments)
       (s-join " ")))

(defun dei--not-a-dangling-stem (keydesc)
  "Check that KEYDESC is not a dangling stem.
I.e. it's something you'd pass to `kbd'. If true, return KEYDESC
unmodified, else return nil."
  (declare (pure t) (side-effect-free t))
  (when (or (string-match-p (rx "--" eol) keydesc)
            (string-match-p (rx " -" eol) keydesc)
            (string-match-p (rx (not (or "-" " ")) eol) keydesc))
    keydesc))

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


;;;; Keyboard scanning

;; Think this function is hairy and unnecessary?  We have it because the C
;; function `describe-buffer-bindings' seems the only way to get this information
;; efficiently.  Inspired by `which-key--get-current-bindings'.  Thanks!
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
         '(cond ((string= "C-" stem) '("<katakana>" "C-<katakana>"))
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

;; Tests
;; (dei--specify-extra-heads "M-s ")
;; (dei--specify-extra-heads "M-s M-")
;; (dei--specify-extra-heads "M-")
;; (dei--specify-extra-heads "M-" t)
;; (setq foo (dei--specify-dire-hydra "M-s "))
;; (dei--specify-invisible-heads "C-")
;; (append nil (-map #'car (dei--specify-extra-heads "C-")))

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

(defvar dei--last-thread (cc:thread 0 nil))

(defun dei-generate-hydras-async ()
  "Regenerate hydras to match the local map."
  (interactive)
  ;; Not sure how fast it cancels.
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
while you experiment with this setting.)  For this reason, the setting 

This variable sets the default approach, but you can override
specific cases in `dei-flatten-winners', and if only one is bound
but not the other, it will duplicate since there is no contest."
  :type 'boolean
  :group 'deianira)

;; TODO: i'd prefer a format for each item being either a string or command
;; KEY-OR-COMMAND, or the cell (KEYMAP . KEY-OR-COMMAND).  Keymap coming first
;; means it's easier to get an overview in initfiles.
(defvar dei-homogenizing-winners '(("C-c C-c")
                                 ("C-x a")
                                 ("C-x g")
                                 ("C-x b")
                                 ("C-c C-c" . org-mode-map))
  "Alist of keys that always win.
See `dei-permachord-wins-homogenizing' for explanation.

Each item has the format (KEY-OR-COMMAND . KEYMAP).

KEY-OR-COMMAND can be either a `kbd'-compatible key description
or a symbol which is assumed to refer to a command.
In the event that you add e.g. both (\"C-x C-f\") and
(set-fill-column), normally a binding of C-x f, the first item
wins, so C-x f will be bound to find-file regardless.

If KEYMAP is nil, apply the winner in whichever keymap it is
found in. Otherwise it should be a major or minor mode map. It
will likely have no effect if it is a prefix command such as
Control-X-prefix or kmacro-keymap.")

(defvar dei--homowinners-in-keymaps nil)
(defvar dei--homowinners nil)
(defvar dei--remap-actions nil)
(defvar dei--remap-actions-view nil)

;; dev settings
(general-auto-unbind-keys 'undo) ;; we shouldn't rely on general
(remove-hook 'after-save-hook 'my-compile-and-drop)


(defconst dei--ignore-keys
  (regexp-opt '("mouse" "remap" "scroll-bar" "select" "switch" "help" "state"
                "which-key" "corner" "divider" "edge" "header" "mode-line"
                "tab" "vertical-line" "frame" "open" "menu" "kp-" "iso-"
                "wheel-")))


(defun dei--where-is (command &optional keymap)
  (->> (where-is-internal command (when keymap (list keymap)))
       (-map #'key-description)
       (--remove (string-match-p dei--ignore-keys it))
       (-map #'dei--normalize)))

;; Weird function, but named after string-match...
(defun dei--chord-match (keydesc)
  (cl-assert (dei--key-seq-steps=1 keydesc))
  (string-match (rx bol nonl "-") keydesc))



;; to debug, eval each in turn, btw after-scan-bindings-hook won't run
;; (dei--get-relevant-bindings)
;; (dei--unbind-illegal-keys)
;; (dei--get-relevant-bindings)
;; (dei--mass-remap)
;;
;;
;; BUG: we clone C-x v (find-alternate-file) to C-x C-v, but we are also trying
;; to clone C-x v x to C-x C-v C-x.
;; How to respond?
;; Case 1: C-x v is a winner, and none of the nested keys are. C-x v wins.
;; Case 2: Some of the nested keys are winners, and C-x v isn't. The keymap wins.
;; Case 3: Neither are winners. Follow the permachord-wins-flattening setting.

;; The basic problem is that we don't even include "keymaps as commands" in the
;; loop (too many issues), so the fn has no idea about a conflict. It doesn't
;; help to include them anyway, we need to figure out some check out on a per
;; key basis.

;; I guess the check is that if the sibling-keydesc is unbound BUT has
;; children (unexpected!), i.e. sibling-keydesc is a successful search pattern
;; for some of the others in current-bindings ...
;; Wait. We can just see that sibling-keydesc is bound to a keymap.

(defun dei--mass-remap ()
  (setq dei--remap-actions nil)
  (setq dei--homowinners-in-keymaps (-filter #'cdr dei-homogenizing-winners))
  (setq dei--homowinners (-map #'car (-remove #'cdr dei-homogenizing-winners)))
  ;; FIXME: What to do if we bind e.g. C-x C-k to a single command, overriding
  ;; the keymap, and then the loop tries to operate on C-x C-k C-e?  Sort by
  ;; length first, and that's not possible. But check on each iteration whether
  ;; there are children, and skip if so.

  ;; FIXME: What to do if we attempt to copy C-x C-k C-e to C-x k e, but it
  ;; turns out that C-x k is bound to something?  Q: Why use current-bindings
  ;; over current-filtered-bindings?
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
    (when-let (winner (assoc (car x) dei--homowinners-in-keymaps))
      (dei--homogenize-binding (car winner) (cdr winner)))
    (when-let (winner (assoc (intern (cdr x)) dei--homowinners-in-keymaps))
      (dei--homogenize-binding (car winner) (cdr winner)))
    (dei--homogenize-binding (car x)))
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

;; TODO: split into separate function for handling specified keymap case, to
;; make it simpler. Probably the two functions will wind up sharing code, and
;; this guides us to what to refactor out.  No more (if keymap ...) clauses.
;;
;; TODO: operate multiple times if multiple keymaps found by
;; help--binding-locus or multiple keys by where-is-internal
;; FIXME: what happens if user specifies a command in a keymap as a winner?
(defun dei--homogenize-binding (key-or-cmd &optional keymap)
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
            (if keymap
                (lookup-key (symbol-value keymap) (kbd key-or-cmd))
              (key-binding (kbd key-or-cmd)))))
         (this-keydesc
          (if (stringp key-or-cmd)
              key-or-cmd
            (if keymap
                (dei--where-is key-or-cmd (symbol-value keymap))
              ;; NOTE: only takes first key found
              (dei--normalize (key-description
                            (where-is-internal key-or-cmd nil t))))))
         (steps (dei--key-seq-split this-keydesc)))
    ;; REVIEW: is this-command ever... a keymap? I think not, bc we filter them
    ;; out in dei--mass-remap.
    (when (and this-command
               this-keydesc
               (/= 1 (length steps)) ;; nothing to homogenize if length 1
               (dei--chord-match (car steps)) ;; only proceed if seq starts w a chord
               (functionp this-command))
      (let* ((root-modifier (match-string 0 (car steps)))
             (most-steps (butlast steps)) ;; often one step
             ;; (last-step (-last-item steps))
             (first-step (car steps))
             (butfirst-steps (cdr steps))
             (second-step (cadr steps))
             (leaf (dei--get-leaf this-keydesc)) ;; can be same as last-step
             (in-map (or keymap
                         ;; hopefully same map as (key-binding)'s origin
                         (help--binding-locus (kbd this-keydesc) nil)))
             ;; "Let second step determine this, cuz I think it will let
             ;; dei-homogenizing-winners work more reliably." wat?
             ;;
             ;; NOTE: we are assuming there exist no "bastard sequences",
             ;; they've been filtered out by `dei--unbind-illegal-keys'. In
             ;; addition, if it's an unchorded seq we're not here.  So we only
             ;; have chord-once and perma-chord.
             (this-is-permachord-p (not (null (dei--chord-match second-step))))
             (permachord-keydesc
              (if this-is-permachord-p
                  this-keydesc
                (s-join " " (cons first-step
                                  (--map (if (dei--chord-match it)
                                             (progn (warn "Bastard sequence found")
                                                    it)
                                           (concat root-modifier it))
                                         butfirst-steps)))))
             (permachord-command
              (if keymap
                  (lookup-key (symbol-value keymap) (kbd permachord-keydesc))
                (key-binding (kbd permachord-keydesc))))
             (chordonce-keydesc (if this-is-permachord-p
                                    (s-join " " (-snoc most-steps leaf))
                                  this-keydesc))
             (chordonce-command
              (if keymap
                  (lookup-key (symbol-value keymap) (kbd chordonce-keydesc))
                (key-binding (kbd chordonce-keydesc))))
             (sibling-keydesc (if this-is-permachord-p
                                  chordonce-keydesc
                                permachord-keydesc))
             (sibling-command (if this-is-permachord-p
                                  chordonce-command
                                permachord-command))
             ;; TODO: name this better
             (winner-map (cdr (assoc this-keydesc
                                     dei--homowinners-in-keymaps))))

        ;; If the user specified a special rule for this map+key (see
        ;; `dei-homogenizing-winners'), then we'll do a special thing.  That
        ;; doesn't all happen within this function, please see
        ;; `dei--mass-remap'.  Anyway, then we skip the rest of this body.
        ;; TODO: move that functionality into this function
        (unless (equal (dei--as-raw-keymap winner-map)
                       (dei--as-raw-keymap in-map)) ;; wat?
          (when (and (null keymap) ;; not sure it's needed
                     (functionp sibling-command))
            ;; Complex case #1: both keys have a command, which do we choose?
            ;; Eeny meny miny moe...? no, let's start by checking if one of
            ;; them is a specified winner.
            (cond ((> 1 (length (-non-nil
                                 (list (member sibling-keydesc dei--homowinners)
                                       (member sibling-command dei--homowinners)
                                       (member this-keydesc dei--homowinners)
                                       (member this-command dei--homowinners)))))
                   ;; Leave it on the user to fix this type of mess.
                   (warn "Found a contradiction in dei-homogenizing-winners."))
                  ((or (member sibling-keydesc dei--homowinners)
                       (member sibling-command dei--homowinners))
                   (push (list (kbd this-keydesc)
                               sibling-command
                               in-map
                               "Clone winner sibling to overwrite this key")
                         dei--remap-actions))
                  ((or (member this-keydesc dei--homowinners)
                       (member this-command dei--homowinners))
                   (push (list (kbd sibling-keydesc)
                               this-command
                               in-map
                               "Clone this winner to overwrite sibling key")
                         dei--remap-actions))
                  ;; Neither key and neither command is rigged to win, so fall
                  ;; back on some simple rule.
                  ;;
                  ;; NOTE that here is the only place where this user option
                  ;; comes into play!  You'd think it'd affect more code, but
                  ;; this monster defun would be necessary even without the
                  ;; user option.
                  (dei-permachord-wins-homogenizing
                   (push (list (kbd chordonce-keydesc)
                               permachord-command
                               in-map
                               "Clone perma-chord to chord-once           ")
                         dei--remap-actions))
                  (t
                   (push (list (kbd permachord-keydesc)
                               chordonce-command
                               in-map
                               "Clone chord-once to perma-chord           ")
                         dei--remap-actions))))
          ;; Complex case #2: turns out the other is a keymap, despite our
          ;; efforts!  We ended up here due to this type of situation: there
          ;; exists a key C-x v x and others under the C-x v prefix, and there
          ;; exists a key C-x C-v which is just a command. If we try to just
          ;; clone the latter to C-x v, we may succeed, because `dei--define' has
          ;; code for that.  But if we then clone C-x v x to C-x C-v
          ;; C-x... BAM!
          ;; FIXME: we could try to be smarter, maybe by double-checking the
          ;; dei--remap-actions for conflicts before executing.
          (when (keymapp sibling-command)
            (warn "Sibling-keydesc binds to a keymap: %s" sibling-keydesc)
            )
          ;; Simple case: only one of the two is bound, so just duplicate.  We
          ;; don't need to do it both directions b/c this invocation is
          ;; operating on this-keydesc which is the one known to have a
          ;; command.
          (when (null sibling-command)
            (push (list (kbd sibling-keydesc)
                        this-command
                        in-map
                        "Clone to overwrite unbound sibling       ")
                  dei--remap-actions)))
        (when keymap
          (push (list (kbd sibling-keydesc)
                      (lookup-key (symbol-value keymap) (kbd this-keydesc))
                      keymap
                      "Clone winner to overwrite sibling, in map ")
                dei--remap-actions))))))

;; (dei--homogenize-binding "C-c C-c" 'org-mode-map)

(defun dei--execute-remap-actions (actions)
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
    (dolist (item dei--remap-actions)
      (seq-let (event cmd map hint) item
        (insert hint
                " Bind " (key-description event)
                "\tto " (if (symbolp cmd)
                         (symbol-name cmd)
                       cmd)
                (if (symbolp map)
                    (concat "\tin ")(symbol-name map)
                    "")))
      (newline))
    (goto-char (point-min)))
  (display-buffer (dei--debug-buffer))
  (when-let ((window (get-buffer-window (dei--debug-buffer))))
    (with-selected-window window
      (recenter 0))))

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
