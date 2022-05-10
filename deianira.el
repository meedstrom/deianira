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
;; Package-Requires: ((emacs "28.1") (hydra "0.15.0") (deferred "0.5.0") (concurrent "0.5.0") (named-timer) (dash) (s))

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
  "Keys to show in hydra hint.  Length should be divisible by `dei-columns'."
  :type 'string)

(defcustom dei-all-shifted-symbols "~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?"
  "Characters that imply Shift being pressed; the default reflects an US keyboard."
  :type 'string)

(defcustom dei-colwidth-override nil
  "Character width of hydra hint. If nil, determine from frame."
  :type 'number)

(defcustom dei-columns 10
  "Amount of columns to display in the hydra hint."
  :type 'number)

;; TODO: use it
(defcustom dei-pseudo-quitter-keys
  '("C-c c")
  "Keys that send you to the root hydra.
Unused for now."
  :type '(repeat string))

;; TODO: use it
(defcustom dei-pseudo-quitter-commands
  '(set-mark-command
    rectangle-mark-mode)
  "Commands that send you to the root hydra.
Unused for now."
  :type '(repeat symbol))

(defcustom dei-quitter-keys
  '("<menu>"
    "C-g")
  "Keys that kill the hydra."
  :type '(repeat string))

(defcustom dei-quitter-commands
  '(keyboard-quit
    minibuffer-keyboard-quit
    keyboard-escape-quit
    secretary-keyboard-quit
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
  :type '(repeat symbol))

(defcustom dei-extra-heads
  '(("<print>" dei-universal-argument nil :exit t)
    ("-" dei-negative-argument nil :exit t)
    ("<f5>" hydra-repeat nil))
  "Heads to add to every hydra."
  :type '(repeat sexp))


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
    (string-match-p dei--modifier-regexp-safe keydesc first-modifier-pos)))

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

;; Roughly the inverse of dei--dub-from-key (but that one does more than squash)
(defun dei--from-squashed-to-proper-keydesc (squashed-key)
  "Unsquash a squashed key description back into kbd-compatible.
SQUASHED-KEY may look like \"Cxp\", \"sxp\", \"<f12>a<RET>\".
Does not fully handle the TAB case."
  (declare (pure t) (side-effect-free t))
  (let* ((foo "C<f12>x<insert>")
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
                                     "frame"))))
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
  (cond ((member (concat stem leaf) dei--keys-that-are-hydras)
         (dei--corresponding-hydra stem leaf))
        (t
         `(call-interactively (key-binding (kbd ,(concat stem leaf)))))))

(defun dei--head-arg-hint (stem leaf)
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
  (when (or (member (concat stem leaf) dei--keys-that-are-hydras)
            (member (concat stem leaf) dei-quitter-keys)
            (member (key-binding (kbd (concat stem leaf))) dei-quitter-commands))
    '(:exit t)))

(defun dei--head (stem leaf)
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
  (let ((leaf-list (-difference dei--hydra-keys-list
                                verboten-leafs)))
    (cl-loop for leaf in leaf-list
             collect `(,leaf
                       ,(dei--head-arg-cmd stem leaf)
                       ,(dei--head-arg-hint stem leaf)
                       ,@(dei--head-arg-exit stem leaf)))))

(defun dei--specify-invisible-heads (stem &optional verboten-leafs)
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
    (-remove (lambda (head) (member (car head) verboten-leafs)) x)))

(defun dei--convert-head-for-nonum (head)
  "If HEAD is a keyboard prefix command, fix it for nonum hydra.
Basically, change `dei-universal-argument' to
`hydra--universal-argument' and drop any :exit keyword."
  (cond ((eq (cadr head) 'dei-universal-argument)
         (list (car head) 'hydra--universal-argument))
        ((eq (cadr head) 'dei-negative-argument)
         (list (car head) 'hydra--negative-argument))
         (t
          head)))

(defun dei--specify-extra-heads (stem &optional nonum-p)
  (let ((self-poppers '(cond ((string= "C-" stem) '("<katakana>" "C-<katakana>"))
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
  "Create a hydra named after STEM with LIST-OF-HEADS."
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

;; TODO: rename
;; TODO: unbind bastard sequences
(defun dei--filter-illegal-keys (cell)
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
  (let* ((illegal-keys (->> dei--current-bindings
                            (-filter #'dei--filter-illegal-keys)
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
     ;; have relocated it (which is prolly smart). TODO: save in external variable for perf
     (member keydesc
             (-map #'dei--parent-key
                   (-remove #'dei--key-has-more-than-one-modifier
                            (dei--where-is #'insert-char)))))))

;; (dei--key-has-more-than-one-modifier "s-i")
;;      (dei--contains-upcase keydesc)
;;      (dei--key-contains-multi-chords "s-i")
;;      (dei--key-seq-mixes-modifiers "s-i")
;; (dei--contains-upcase "s-i")
;; (dei--contains-upcase "s-i")

(defun dei--get-relevant-bindings ()
  (->> (dei--current-bindings
        nil
        (rx (or
             (regexp (eval-when-compile
                       (regexp-opt
                        '("ESC" ;; critical to filter out, I think
                          "TAB" "DEL" "backspace" ;; one day, I will let unbind-illegal-keys handle these
                          "<key-chord>" "<compose-last-chars>" "Scroll_Lock"
                          "-margin>" "-fringe>" "iso-leftab" "iso-lefttab"
                          "drag-n-drop" "wheel"
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
       (-remove #'dei--filter-illegal-keys)
       (-remove #'dei--filter-for-hydra-making)
       (setq dei--current-filtered-bindings)))
;; (dei--get-relevant-bindings)

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

;; (add-hook 'prog-mode-hook #'dei--mass-remap)
;; (add-hook 'text-mode-hook)
;; (add-hook 'special-mode-hook)

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
            #'dei--get-relevant-bindings
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

(defvar dei-permachord-wins-flattening t
  "Non-nil if permachord should win as opposed to chord-once.
This means that the behavior of C-x C-f will be kept and
duplicated to C-x f, so they both do what the former always did
If nil, both will instead do what C-x f always did.  This
variable sets the default approach, but you can override specific
cases in `dei-flatten-winners', and if only one is bound but not
the other, it will duplicate since there is no contest.")

;; TODO: i'd prefer a format for each item being either a string or command
;; KEY-OR-COMMAND, or the list (KEYMAP KEY-OR-COMMAND).  Keymap coming first
;; means it's easier to get an overview in initfiles.
(defvar dei-flattening-winners '(("C-c C-c")
                                 ("C-x a")
                                 ("C-x g")
                                 ("C-x b")
                                 ("C-c C-c" . org-mode-map))
  "Alist of keys that always win.
See `dei-permachord-wins-flattening' for explanation.

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

(defvar dei--flatwinners-in-keymaps nil)
(defvar dei--flatwinners nil)

(defun dei--mass-remap ()
  (setq dei--flatwinners-in-keymaps (-filter #'cdr dei-flattening-winners))
  (setq dei--flatwinners (-map #'car (-remove #'cdr dei-flattening-winners)))
  (dolist (x dei--flatwinners-in-keymaps)
    (dei--flatten-binding (car x) (cdr x)))
  ;; FIXME: What to do if we bind e.g. C-x C-k to a single command, overriding the keymap, and then the loop tries to operate on C-x C-k C-e?
  ;; Sort by length first, and use a while loop rather than dolist, removing child keys from a list if necessary.
  ;; FIXME: What to do if we attempt to copy C-x C-k C-e to C-x k e, but it turns out that C-x k is bound to something?
  ;; Q: Why use current-bindings over current-filtered-bindings?
  (dolist (x (-remove (lambda (y)
                        (or (string= "Prefix Command" (cdr y))
                            (null (intern (cdr y)))
                            (keymapp (intern (cdr y)))))
                      dei--current-bindings))
    (dei--flatten-binding (car x)))
  ;; Solution
  ;; using current-filtered-bindings for now for testing
  (let ((keys (nreverse (-map #'car dei--current-bindings))))
    (while (not (null keys))
      ;; maybe flatten-binding should check for keymap and if so then return all children for us to remove from the iteration list
      (when-let ((any-defunct (dei--flatten-binding (car keys))))
        (dolist (x (cdr any-defunct))
          (define-key (car any-defunct) (kbd x) nil) ;; not strictly necessary?
          (setq keys (remove x keys))))
      (setq keys (remove (car keys) keys)))
  ;; When the root is unchorded, like <f1> or similar, just unchord all children.
  ;; (dei--unbind-illegal-keys)
  ))
;; (dei--get-relevant-bindings)
;; (dei--mass-remap)

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

(defun dei--chord-match (keydesc)
  (declare (pure t) (side-effect-free t))
  (cl-assert (dei--key-seq-steps=1 keydesc))
  (string-match (rx bol nonl "-") keydesc))

;; TODO: Ok, this is a big function. Before doing the other TODOs, write some
;;       tests? NOTE: see wip.el.
;;
;; TODO: Catch cases like C-c p a so they will become C-c C-p C-a.
;;       Don't bother to make C-c p C-a.
;;
;; TODO: Unbind bastard sequences like C-c C-e l o
;;
;; TODO: Make it easier to test/debug. What if it returns only a list/cons cell
;;       of what to do without doing it (as if dry run)? I don't want an error
;;       regarding a single key to break the package for all the other keys
;;       that should work fine. Build a list of actions to take, then execute
;;       them while collecting errors into a simple message.
(defun dei--flatten-binding (keydesc &optional keymap)
  "Duplicate KEYDESC binding to or from its sibling.
Assumes that the command to be bound is not itself a keymap,
because those will be implied by binding their children anyway.
You can use `dei--get-relevant-bindings' to filter out keymaps,
and run this function on the results."
  (let ((command (key-binding (kbd keydesc)))
        (steps (dei--key-seq-split keydesc)))
    (and (commandp command)
         (/= 1 (length steps))
         (when (dei--chord-match (car steps))
           (let* ((root-modifier (match-string 0 (car steps)))
                  (most-steps (butlast steps)) ;; often one step
                  (last-step (-last-item steps))
                  (first-step (car steps))
                  (butfirst-steps (cdr steps))
                  (second-step (cadr steps))
                  (leaf (dei--get-leaf keydesc)) ;; may be same as last-step
                  (map (or keymap
                           ;; hopefully same map as (key-binding)'s origin
                           (symbol-value
                            (help--binding-locus (kbd keydesc) nil))))
                  ;; Let second step determine this, cuz I think it will let
                  ;; flattening-winners work more reliably.
                  (this-is-permachord-p (not (null (dei--chord-match second-step))))
                  (permachord-keydesc* (if this-is-permachord-p
                                         keydesc
                                       (s-join " " (-snoc most-steps
                                                          (concat root-modifier
                                                                  leaf)))))
                  (permachord-keydesc (if this-is-permachord-p
                                       keydesc
                                     (s-join " " (cons first-step
                                                       (--map (unless (dei--chord-match it)
                                                                (concat root-modifier it))
                                                              butfirst-steps)))))
                  (permachord-command (key-binding (kbd permachord-keydesc)))
                  (chordonce-keydesc (if this-is-permachord-p
                                         (s-join " " (-snoc most-steps leaf))
                                       keydesc))
                  (chordonce-command (key-binding (kbd chordonce-keydesc)))
                  (sibling-keydesc (if this-is-permachord-p
                                       chordonce-keydesc
                                     permachord-keydesc))
                  (sibling-command (key-binding (kbd sibling-keydesc)))
                  ;; TODO: name this better
                  (winner-map (cdr (assoc keydesc dei--flatwinners-in-keymaps))))
             ;; why don't we want to execute in this case?
             (prog1
                 (unless (equal winner-map map)
                   (if (commandp sibling-command)
                       (cond ((member sibling-keydesc dei--flatwinners)
                              (dei--define map (kbd keydesc) sibling-command))
                             ((member keydesc dei--flatwinners)
                              (dei--define map (kbd sibling-keydesc) command))
                             (dei-permachord-wins-flattening
                              (dei--define map (kbd chordonce-keydesc) permachord-command))
                             (t
                              (dei--define map (kbd permachord-keydesc) chordonce-command)))
                     ;; If one of the two is not bound, just duplicate the other.  We
                     ;; don't need to do it both directions since this whole function will
                     ;; probably be on the other keydesc later if it hasn't already.
                     (dei--define map (kbd sibling-keydesc) command)))
               (when keymap (dei--define keymap
                                         (kbd sibling-keydesc)
                                         (lookup-key keymap (kbd keydesc)))))
             )))))

(defun dei--keymap-children-keys (keymap)
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
;; (setq foo (dei--keymap-children-keys ctl-x-map))

(defun dei--define (map key new-def)
  ;; for debug
  (dei--echo (list "Will call `define-key' with args:"
                   'map ;; too damn big expression
                   key
                   new-def))
  (prog1 (let ((old-def (key-binding key)))
           (when (keymapp old-def)
             ;; return all children, for caller to unbind
             (cons old-def
                   (--map (concat (key-description key) " " it)
                          (dei--keymap-children-keys old-def)))))
    (define-key map key new-def)))
;; (setq dei-debug "*Deianira debug*")
;; (dei--flatten-binding "C-x d")



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
  (not (null hydra-curr-map)))

(defun dei--slay (&rest args)
  (when (dei--hydra-active-p)
    (setq hydra-deactivate t)
    (call-interactively #'hydra-keyboard-quit))
  args)
;; (advice-add #'selectrum-read :before #'dei--slay)
;; (advice-remove #'selectrum-read #'dei--slay)

(defun dei--slay-if-minibuffer (&rest args)
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

(defvar dei--old-hydra-cell-format nil)

(defvar dei--old-hydra-base-map-C-u nil)

;;;###autoload
(define-minor-mode deianira-mode
  "Bind root hydras."
  :global t
  :lighter " Δ"
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
