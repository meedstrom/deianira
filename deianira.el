;;; deianira.el --- Modifier-free pseudo-modal input -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021 Martin Edström

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
;; Package-Requires: ((emacs "27.1") (hydra "0.15.0") (deferred "0.5.0") (dash) (s))

;;; Commentary:

;;; Code:

;; builtin dependencies
(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'help)

;; external dependencies
(require 'deferred)
(require 'dash)
(require 's)
(require 'hydra)
(require 'named-timer)


;;;; User settings

(defcustom dei-hydra-keys "1234567890qwertyuiopasdfghjkl;zxcvbnm,./"
  "Keys to show in hydra hint. Length should be divisible by `dei-columns'."
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
    execute-extended-command
    abort-recursive-edit
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

(defconst dei--modifier-regexp
  (regexp-opt '("A-" "C-" "H-" "M-" "S-" "s-")))

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

(defconst dei-all-shifted-symbols
  (split-string
   "~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?"
   "" t))

(defun dei--hydra-keys-list-reeval ()
  (split-string dei-hydra-keys "" t))

(defun dei--calc-colwidth ()
  (or dei-colwidth-override
      (let ((optimal (- (round (frame-parameter nil 'width) 10) 4)))
        (max optimal 8))))

(defun dei--filler-recalc ()
  (make-string dei--colwidth (string-to-char " ")))

(defvar dei--hydra-keys-list (dei--hydra-keys-list-reeval))
(defvar dei--colwidth (dei--calc-colwidth))
(defvar dei--filler (dei--filler-recalc))


;;;; Handlers for key descriptions
;; If it's not a pure function, it probably doesn't belong in this section.

(defun dei--key-contains-ctl (keydesc)
  (declare (pure t) (side-effect-free t))
  (string-match-p "C-" keydesc))

(defun dei--key-contains-multi-chords (keydesc)
  (declare (pure t) (side-effect-free t))
  ;; Assume keydesc was already normalized.
  (string-match-p (rx (= 2 (regexp dei--modifier-regexp)))
                  keydesc))

(defun dei--key-has-more-than-one-modifier (keydesc)
  (declare (pure t) (side-effect-free t))
  (when (> (length keydesc) 3)
    (string-match-p dei--modifier-regexp (substring keydesc 2))))

;; could probs be programmed better
;; does not catch capitals FWIW
(defun dei--key-seq-mixes-modifiers (keydesc)
  (declare (pure t) (side-effect-free t))
  ;; we use our own regexp instead of dei--modifier-regexp so it is always
  ;; synced with how we define "mods" here
  (if-let* ((mods '("A-" "C-" "H-" "M-" "S-" "s-"))
            (regexp (eval-when-compile (regexp-opt '("A-" "C-" "H-" "M-" "S-" "s-"))))
            (case-fold-search nil)
            (first-match-pos (string-match-p regexp keydesc))
            (caught-mod (substring keydesc first-match-pos (+ 2 first-match-pos)))
            (now-verboten (-difference mods (list caught-mod))))
      (string-match-p (eval `(rx (or ,@now-verboten)) t) keydesc)))

(defun dei--get-leaf (keydesc)
  (declare (pure t) (side-effect-free t))
  (->> keydesc
       (s-split (rx space))
       (-last-item)
       (dei--normalize-trim-segment)
       (dei--normalize-get-atoms)
       (dei--normalize-wrap-leaf-maybe)
       (-last-item)))

(defun dei--normalize-trim-segment (x)
  (declare (pure t) (side-effect-free t))
  (if (and (string-match-p (rx "<") x)
           (string-match-p (rx ">" eol) x))
      (replace-regexp-in-string (rx (any "<>")) "" x)
    x))

(defun dei--normalize-get-atoms (x)
  (declare (pure t) (side-effect-free t))
  (if (string-match-p (rx "-" eol) x)
      (append (split-string x "-" t) (list "-"))
    (split-string x "-")))

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
  (declare (pure t) (side-effect-free t))
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
those don't exist.  We also assume you have special handling for
the root modifier stems like C- and M- and never call this
function on them."
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
(defun dei--squashed-to-proper-keydesc (squashed-key)
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
;; efficiently.  Inspired by `which-key--get-current-bindings'. Thanks!
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
        (that-buffer (current-buffer)) ;; since we go to a temp buffer
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
                                         "Input decoding map translations")))))
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

(defun dei--is-bound (stem leaf)
  (not (dei--is-unbound stem leaf)))

;; REVIEW: This may be slow
(defun dei--sort-like (index alist)
  "Sort ALIST by `car' element according to elements in INDEX.
That is, using INDEX as a master reference, sort ALIST to follow
it as well as possible, appending unknown members to the end.

Example: With INDEX being '(\"b\" \"c\" \"d\" \"a\"),
an example ALIST transformation may look like this:

'((\"a\" nil)           '((\"b\" nil)
  (\"b\" nil)      ⟶      (\"c\" nil)
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
         (name (symbol-name sym)))
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
            (cl-loop for leaf in (append dei-all-shifted-symbols
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
  (let ((self-poppers (cond ((string= "C-" stem) '("<f35>" "C-<f35>"))
                            ((string= "M-" stem) '("<f34>" "M-<f34>"))
                            ((string= "s-" stem) '("<f33>" "s-<f33>"))
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

(defun dei--combined-filter (cell)
  "Filter for rejecting keys as irrelevant to work on.
This function exists because there is a need to go into a list
and look at the `car's, and so we may as well run each filter
while we're at it."
  (declare (pure t) (side-effect-free t))
  (let ((keydesc (car cell))
        (case-fold-search nil))
    (or
     ;; (dei--key-contains-ctl keydesc)
     ;; (s-contains-p "backspace" keydesc)
     ;; (s-contains-p "DEL" keydesc)
     (s-contains-p "S-" keydesc)
     (dei--key-has-more-than-one-modifier keydesc)
     (dei--contains-upcase keydesc)
     (dei--key-contains-multi-chords keydesc)
     (dei--key-seq-mixes-modifiers keydesc))))

;; (dei--key-has-more-than-one-modifier "s-i")
;;      (dei--contains-upcase keydesc)
;;      (dei--key-contains-multi-chords "s-i")
;;      (dei--key-seq-mixes-modifiers "s-i")
;; (dei--contains-upcase "s-i")
;; (dei--contains-upcase "s-i")

(defun dei--contains-upcase (keydesc)
  (let* ((steps (dei--key-seq-split keydesc))
         (steps-without-modifier (mapcar #'dei--get-leaf steps)))
    (--any-p (member it dei-all-shifted-symbols) steps-without-modifier)))

;; TODO: think about API. I think I want to be able to just specify each
;; exception in a single list, even for different stems
;; wip
(defun dei--auto-remap-buffer-bindings ()
  (let ((diff)))
  (dolist (leaf dei--all-keys-on-keyboard)
    (when-let ((map (help--binding-locus (kbd (concat "C-c C-" leaf)) nil)))
      ;; Haven't decided which of these to run. First is probably cleaner.
      (dei-restem map leaf "C-c " "C-c C-"))))
      ;; (local-set-key (kbd (concat "C-c " leaf)) (key-binding (kbd (concat "C-c C-" leaf))))

;; (add-hook 'prog-mode-hook #'dei--auto-remap-buffer-bindings)
;; (add-hook 'text-mode-hook)
;; (add-hook 'special-mode-hook)

(defun dei--get-relevant-bindings ()
  (->> (dei--current-bindings
        nil ;; (rx bol (regexp dei--modifier-regexp)) ;; NOTE: filtering for modifier regexp gets rid of function keys
        (rx (or "ESC" ;; critical
                "TAB" "DEL" "backspace"
                ;; "C-"
                "<f1>" (seq bol "C-h") ;; user should fix
                ;; TODO: make hydras, but don't unbind violators
                (literal doom-leader-alt-key) ;; fulla Shift
                (literal doom-localleader-alt-key)
                "<key-chord>" "<compose-last-chars>"
                "-margin>" "-fringe>")))
       (-map (lambda (x)
               (cons (dei--normalize (car x))
                     (cdr x))))
       (setq dei--current-bindings)
       ;; Remove submaps, we infer their existence later by cutting the leafs
       ;; off every actual key via (-uniq (-map #'dei--drop-leaf KEYS)).
       (-remove (lambda (x)
                  (or (string= "Prefix Command" (cdr x))
                      (null (intern (cdr x)))
                      (keymapp (intern (cdr x))))))
       (-remove #'dei--combined-filter)
       (setq dei--current-filtered-bindings)))
;; (dei--get-relevant-bindings)

;; TODO: make the function just use dei--current-bindings instead of input
;; (add-hook 'dei--after-scan-bindings-hook #'dei--unbind-illegal-keys)

(defun dei--unbind-illegal-keys ()
  (let* ((illegal (seq-filter #'dei--combined-filter dei--current-bindings))
         (illegal-keys (seq-map #'car illegal))
         (sorted (seq-sort-by #'dei--key-seq-steps-length #'> illegal-keys)))
    (seq-do (lambda (x)
              (let ((map (help--binding-locus (kbd x) nil)))
                (unless (null map) ;; there's a bug that leaves some keys in `describe-bindings' but not in the apparently active map, and they get a null map. Check dei--current-bindings, C-M-q and C-M-@ are in there
                  (define-key (eval map t) (kbd x) nil))))
            sorted)))

;; (dei--normalize "<key-chord>")
;; (dei--normalize "<compose-last-chars>")

;; NOTE: Visualize a Venn diagram. The defunct and the new are the last and
;;      current minus their intersection (cases where the key's definition
;;      didn't change), which should never be relevant to look at.
(defun dei--set-variables ()
  (setq dei--defunct-bindings (-difference dei--last-filtered-bindings
                                           dei--current-filtered-bindings))
  (setq dei--new-or-changed-bindings (-difference dei--current-filtered-bindings
                                                  dei--last-filtered-bindings))
  (setq dei--defunct-stems
        (->> dei--defunct-bindings
             (-map #'car)
             (-map #'dei--drop-leaf)
             (-uniq)))
  (setq dei--new-or-changed-stems
        (->> dei--new-or-changed-bindings
             (-map #'car)
             ;; Sort to avail the most relevant hydras to the user soonest.
             ;; NOTE: only matters if we run defhydra from an async loop
             (seq-sort-by #'dei--key-seq-steps-length #'<)
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

(defun dei-reset ()
  "Re-generate hydras from scratch."
  (interactive)
  (setq dei--last-filtered-bindings nil)
  (dei--get-relevant-bindings)
  (dei--unbind-illegal-keys)
  (dei--get-relevant-bindings)
  (dei--set-variables)
  (dei--specify-dire-hydras)
  (cl-loop for x in dei--hydra-blueprints
         do (progn (dei--call-defhydra (cadr x) (cddr x))
                   (push (car x) dei--live-stems)))
  (setq dei--last-filtered-bindings dei--current-filtered-bindings))

(defun dei-generate-hydras-async ()
  "Regenerate hydras to match the local map."
  (interactive)
  (deferred:$
    (deferred:next
      #'dei--get-relevant-bindings)

    (deferred:nextc it
      (lambda ()
        (run-hooks 'dei--after-scan-bindings-hook)))

    (deferred:nextc it
      #'dei--set-variables)

    ;; Re-cache settings in case of changed frame parameters or user options
    (deferred:nextc it
      (lambda ()
        (setq dei--colwidth (dei--calc-colwidth))
        (setq dei--filler (dei--filler-recalc))
        (setq dei--hydra-keys-list (dei--hydra-keys-list-reeval))))

    (deferred:nextc it
      #'dei--specify-dire-hydras)

    (deferred:nextc it
      (lambda ()
        ;; REVIEW: is this condition reliable?
        (unless (dei--hydra-active-p)
          (cl-loop
           for x in dei--hydra-blueprints
           do (progn (dei--call-defhydra (cadr x) (cddr x))
                     (push (car x) dei--live-stems)))
          (setq dei--last-filtered-bindings dei--current-filtered-bindings))))

    (deferred:nextc it
      (lambda ()
        (run-hooks 'dei--after-rebuild-hydra-hook)))

    (deferred:error it
      #'warn)))


;;;; Bonus functions for X11

;; "We've not found a keyboard with more than 35 function keys total."
;; -- /usr/include/X11/keysymdef.h
;; i.e. F35 is the highest F-key defined in upstream keysymdef.h.
(defvar dei-xmodmap-rules
  '(;; ensure that the keysyms exist (this is idempotent)
    "keycode any = F35"
    "keycode any = F34"
    "keycode any = F33"
    "keycode any = F32"
    "keycode any = F31"))
    ;; clear lshift. weeeeird!
    ;;"remove shift = Shift_L"
    ;;"keysym Shift_L = F30 F30 F30 F30"

;; NOTE: User may have to modify this, due to differences in keyboards.
(defvar dei-xcape-rules
  '(
    "Alt_L=F34"
    "Alt_R=F34"
    ;; "Meta_L=F34"
    ;; "Meta_R=F34"
    "Control_L=F35"
    "Control_R=F35"
    ;; "Hyper_L=F32"
    ;; "Hyper_R=F32"
    "Super_L=F33"
    "Super_R=F33"))

(defun dei-xmodmap-reload (&optional output-buffer)
  "(Re-)apply the `dei-xmodmap-rules'."
  (interactive)
  (let* ((shell-command-dont-erase-buffer t)
         (rules (string-join dei-xmodmap-rules "' -e '"))
         (cmd (concat "xmodmap -e '" rules "'")))
    (when (executable-find "xmodmap")
      (start-process-shell-command cmd
                                   (or output-buffer (dei--debug-buffer) "*Messages*")
                                   cmd))))

(defvar dei-xcape-process)

(defvar dei-xcape-log-cleaner)

(defun dei-clean-xcape-log ()
  (when (get-buffer "*xcape*")
    (with-current-buffer "*xcape*"
      (delete-region (point-min) (point-max)))))

(defun dei-xcape-reload ()
  "(Re-)start the xcape process."
  (interactive)
  (let ((shell-command-dont-erase-buffer t)
        (rules (string-join dei-xcape-rules ";")))
    (when (executable-find "xcape")
      (and (boundp 'dei-xcape-process)
           (process-live-p dei-xcape-process)
           (kill-process dei-xcape-process))
      (setq dei-xcape-process
            (start-process "xcape" "*xcape*"
                           "nice" "-20" "xcape" "-d" "-e" rules))
      (named-timer-run 'dei-xcape-log-cleaner 300 300 #'dei-clean-xcape-log))))

(defun dei-xkbset-enable-sticky-keys ()
  (interactive)
  (when (executable-find "xkbset")
    (start-process "xkbset" (dei--debug-buffer)
                   "xkbset" "sticky" "-twokey" "-latchlock")
    (start-process "xkbset" (dei--debug-buffer)
                   "xkbset" "exp" "=sticky")))


;;;; Bonus functions for mass remaps

(defvar dei-rechord-wins-flattening t
  "Non-nil if reused-chord should win as opposed to chord-once.
This means that the behavior of C-x C-f will be kept and
duplicated to C-x f, so they both do what the former always did
If nil, both will instead do what C-x f always did.  This
variable sets the default approach, but you can override specific
cases in `dei-flatten-winners', and if only one is bound but not
the other then it will duplicate since there is no contest.")

(defvar dei-flattening-winners '(("C-c C-c")
                                 ("C-x a")
                                 ("C-c C-c" . org-mode-map))
  "Alist of keys that always win.
See `dei-rechord-wins-flattening' for explanation.

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

(defvar dei--flatwinners-w-keymaps nil)
(defvar dei--flatwinners-universal nil)

(defun dei--mass-remap ()
  (setq dei--flatwinners-w-keymaps (-filter #'cdr dei-flattening-winners))
  (setq dei--flatwinners (-map #'car (-remove #'cdr dei-flattening-winners)))
  (dolist (x (-remove (lambda (x)
                        (or (string= "Prefix Command" (cdr x))
                            (null (intern (cdr x)))
                            (keymapp (intern (cdr x)))))
                      dei--current-bindings))
    (dei--flatten-binding (car x)))
  ;; When the root is unchorded, like <f1> or similar, just unchord all children.
  ;; It's really unimportant though.
  ;; (dei--unbind-illegal-keys)
  (dolist (x dei--flatwinners-w-keymaps)
    (dei--flatten-binding (car x) (cdr x))))

(defconst dei--ignore-keys
  (regexp-opt '("mouse" "remap" "scroll-bar" "select" "switch" "help" "state"
                "which-key" "corner" "divider" "edge" "header" "mode-line"
                "tab" "vertical-line" "frame" "open" "menu" "kp-")))

(defun dei--where-is (command &optional keymap)
  (->> (where-is-internal command (when keymap (list keymap)))
       (-map #'key-description)
       (--remove (string-match-p dei--ignore-keys it))
       (-map #'dei--normalize)))

(defalias 'dei--chord-match #'dei--is-chord)
(defun dei--is-chord (keydesc)
  (declare (pure t) (side-effect-free t))
  (cl-assert (dei--key-seq-steps=1 keydesc))
  (string-match (rx bol nonl "-") keydesc))

;; TODO: Catch cases like C-c p a so they will become C-c C-p C-a.
;; TODO: Make it easier to test/debug
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
                  (leaf (dei--get-leaf last-step)) ;; may be same as last-step
                  (map (or keymap
                           ;; hopefully same map as (key-binding)'s origin
                           (symbol-value
                            (help--binding-locus (kbd keydesc) nil))))
                  (this-is-rechord-p (not (null (dei--chord-match last-step))))
                  (rechorded-keydesc (if this-is-rechord-p
                                         keydesc
                                       (s-join " " (-snoc most-steps
                                                          (concat root-modifier
                                                                  leaf)))))
                  
                  (rechorded-command (key-binding (kbd rechorded-keydesc)))
                  (chordonce-keydesc (if this-is-rechord-p
                                         (s-join " " (-snoc most-steps leaf))
                                       keydesc))
                  (chordonce-command (key-binding (kbd chordonce-keydesc)))
                  (sibling-keydesc (if this-is-rechord-p
                                       chordonce-keydesc
                                     rechorded-keydesc))
                  (sibling-command (key-binding (kbd sibling-keydesc)))
                  ;; TODO: name this better
                  (winner-map (cdr (assoc keydesc dei--flatwinners-w-keymaps))))
             ;; why don't we want to execute in this case?
             (unless (equal winner-map map)
               (if (commandp sibling-command)
                   (cond ((member sibling-keydesc dei--flatwinners)
                          (dei--define map (kbd keydesc) sibling-command))
                         ((member keydesc dei--flatwinners)
                          (dei--define map (kbd sibling-keydesc) command))
                         (dei-rechord-wins-flattening
                          (dei--define map (kbd chordonce-keydesc) rechorded-command))
                         (t
                          (dei--define map (kbd rechorded-keydesc) chordonce-command)))
                 ;; If one of the two is not bound, just duplicate the other.  We
                 ;; don't need to do it both directions since this whole function will
                 ;; probably be on the other keydesc later if it hasn't already.
                 (dei--define map (kbd sibling-keydesc) command)))
             (when keymap (dei--define keymap
                                       (kbd sibling-keydesc)
                                       (lookup-key keymap (kbd keydesc)))))))))

;; Just a debug helper
(defun dei--define (map key def)
  (dei--echo (list "Will call `define-key' with args:" 'map key def))
  (define-key map key def))
;(setq dei-debug "*Deianira debug*")
;(dei--flatten-binding "C-x d")


(defun dei--flatten-binding* (keydesc &optional keymap)
  "Duplicate KEYDESC binding to or from its sibling.
Assumes that the command to be bound is not itself a keymap,
because those will be implied by binding their children anyway.
You can use `dei--get-relevant-bindings' to filter out keymaps,
and run this function on the results."
  (let ((command (key-binding (kbd keydesc)))
        (steps (dei--key-seq-split keydesc)))
    (and (or (commandp command))
         (/= 1 (length steps))
         (when (dei--is-chord (car steps))
           (let* ((root-modifier (match-string 0 (car steps))) ;; from is-chord
                  (most-steps (butlast steps))
                  (last-step (-last-item steps))
                  (leaf (dei--get-leaf keydesc))
                  (sibling-keydesc
                   (if (dei--is-chord last-step)
                       (s-join " " (-snoc most-steps leaf))
                     (s-join " " (-snoc most-steps
                                        (concat root-modifier leaf)))))
                  (sibling-command (key-binding (kbd sibling-keydesc)))
                  (map (or keymap (symbol-value
                                   (help--binding-locus (kbd keydesc) nil)))))
             (if (commandp sibling-command)
                 (progn
                   ;; TODO: check dei--flattening-winners
                   (if dei-rechord-wins-flattening
                       (if (dei--is-chord last-step)
                           (define-key map (kbd sibling-keydesc) command)
                         (define-key map (kbd keydesc) sibling-command))
                     (if (dei--is-chord last-step)
                         (define-key map (kbd keydesc) sibling-command)
                       (define-key map (kbd sibling-keydesc) command))))
               (define-key map (kbd sibling-keydesc) command)))))))

;; Prior art from `which-key-show-minor-mode-keymap', thanks!
(defun dei--current-minor-mode-maps ()
  (mapcar #'car
          (cl-remove-if-not
           (lambda (entry)
             (and (symbol-value (car entry))
                  (not (equal (cdr entry) (make-sparse-keymap)))))
           minor-mode-map-alist)))

;; Prior art from `which-key-show-major-mode', thanks!
(defun dei--current-major-mode-map ()
  (let ((map-sym (intern (format "%s-map" major-mode))))
    (if (and (boundp map-sym)
             (keymapp (symbol-value map-sym)))
        map-sym
      nil)))

;; Generalized flatten-ctl-x
(defun dei-restem-all-leaves (here lower-stem upper-stem)
  "Duplicate bindings on UPPER-STEM to also exist on LOWER-STEM.
Where they conflict, LOWER-STEM is overridden. The naming is
inspired by overlayfs, which do a similar thing with filesystem
mounts. HERE refers to the keymap such as global-map.  Typical
use: (dei-restem-all-leaves global-map \"C-x \" \"C-x C-\")"
  (dolist (leaf (dei--hydra-keys-list-reeval))
    (dei-restem here leaf lower-stem upper-stem)))

(defun dei-restem (here leaf new-stem reference-stem)
  "Keeping LEAF, change stem."
  (let ((ref-cmd (lookup-key here (kbd (concat reference-stem leaf)))))
    (when (dei--of-interest-p ref-cmd)
      (define-key here (kbd (concat new-stem leaf))
        ref-cmd))))

(defun dei-new-leaf (here stem new-leaf reference-leaf)
  (define-key here (kbd (concat stem new-leaf))
    (lookup-key here (kbd (concat stem reference-leaf)))))

;; TODO: make this work
(defmacro dei-backup-keymap-1 (keymap)
  "Backup KEYMAP under the name dei-backup-KEYMAP, unless it's
already been done."
  `(when-let ((name (ignore-errors (symbol-name ',keymap))) ;; guard clause
              (backup (intern (concat "dei-backup-" name))))
     (unless (and (boundp backup)
                  (not (eq nil backup)))
       ;; Maybe you should use `copy-keymap' here
       (set backup ,keymap))))

;; TODO: make it not fail for unnamed maps
;; TODO: backup unnamed maps too
(defmacro dei-backup-keymap (keymap)
  "Backup KEYMAP under the name dei-backup-KEYMAP, unless it's
already been done."
  `(let ((backup (intern (concat "dei-backup-" (symbol-name ',keymap)))))
     (unless (and (boundp backup)
                  (not (eq nil backup)))
       ;; Maybe you should use `copy-keymap' here
       (set backup ,keymap))))

(defmacro dei-restore-keymap (keymap)
  `(let ((backup (intern (concat "dei-backup-" (symbol-name ',keymap)))))
     (when (and (boundp backup)
                (not (eq nil backup)))
       (setq ,keymap backup))))

;; (help--binding-locus (kbd "C-x n w") nil)
;;(help--binding-locus (kbd "C-c") nil)

;; TODO: do this in keymaps found by help--binding-locus for the capitals
;; found in dei--current-bindings.
(defun dei-bind-all-shiftsyms-to-insert ()
  "Bind all capital letters and shift symbols to self-insert."
  (dolist (leaf dei-all-shifted-symbols)
    (global-set-key (kbd leaf) #'self-insert-command)))

;; (defun dei-super-from-ctl ()
;;   (map-keymap (lambda (ev def)
;;                 (let* ((case-fold-search nil)
;;                        (key (key-description (list ev)))
;;                        (newkey (replace-regexp-in-string
;;                                 (rx word-start "C" word-end) "s" key t)))
;;                   (and (dei--of-interest-p def)
;;                        (not (equal key newkey))
;;                        (define-key global-map (kbd newkey) def))))
;;               global-map))

;; TODO: Do this continuously over time
;; TODO: Do this not only on global-map
(defun dei-super-from-ctl (map)
  (map-keymap (lambda (ev def)
                (let* ((case-fold-search nil)
                       (key (key-description (list ev)))
                       (newkey (replace-regexp-in-string
                                (rx word-start "C" word-end) "s" key t)))
                  (when (and (dei--of-interest-p def)
                             (not (equal key newkey))) ;; Don't proceed for those keys that didn't contain C- in the first place, e.g. M-f.
                    (define-key map (kbd newkey) def)))
                (when (keymapp def)
                  (dei-super-from-ctl def))) ;; recurse
              map)
  ;; is this the thing that makes C-g need two presses sometimes?
  (define-key key-translation-map (kbd "s-g") (kbd "C-g")))



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

;;;###autoload
(define-minor-mode deianira-mode
  "Bind root hydras."
  nil
  " Δ"
  `(
    (,(kbd "<f35>") . dei-C/body)
    (,(kbd "<f34>") . dei-M/body)
    (,(kbd "<f33>") . dei-s/body)
    (,(kbd "<f32>") . dei-H/body)
    (,(kbd "<f31>") . dei-A/body)
    (,(kbd "<f2>") . dei-<f2>/body)
    (,(kbd "<f10>") . dei-<f10>/body)
    ;; in case of sticky keys
    (,(kbd "C-<f35>") . dei-C/body)
    (,(kbd "M-<f34>") . dei-M/body)
    (,(kbd "s-<f33>") . dei-s/body)
    (,(kbd "H-<f32>") . dei-H/body)
    (,(kbd "A-<f31>") . dei-A/body))
  :global t
  (if deianira-mode
      (progn
        ;; Does not work for the consult-* functions with selectrum-mode.
        ;; It's ok because they ignore the hydra and let you type, but why?
        (add-hook 'window-buffer-change-functions #'dei--slay-if-minibuffer)

        (define-key hydra-base-map (kbd "C-u") nil)
        (when (null dei--live-stems)
          (dei-reset)))
    (define-key hydra-base-map (kbd "C-u") #'hydra--universal-argument)
    (remove-hook 'window-buffer-change-functions #'dei--slay-if-minibuffer)))

(provide 'deianira)
;;; deianira.el ends here
