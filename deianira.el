;;; deianira.el --- Hydra-ize every key sequence -*- lexical-binding: t -*-

;; (read-symbol-shorthands . '("dei-" . "deianira-"))

;; Copyright (C) 2018-2023 Martin Edström

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

;; Author:  <meedstrom91@gmail.com>
;; Created: 2018-08-03
;; Version: 0.2.6-snapshot
;; Keywords: abbrev convenience
;; Homepage: https://github.com/meedstrom/deianira
;; Package-Requires: ((emacs "28") (asyncloop "0.4.5") (massmapper "0.1.3-snapshot") (compat "29.1.4.3") (hydra "0.15.0") (named-timer "0.1") (dash "2.19.1"))

;;; Commentary:

;; See the README.org.  You may find it by visiting:
;;    https://github.com/meedstrom/deianira
;;
;; or by evalling (but your package manager probably left out the README):
;;    (find-file (file-name-directory (find-library-name "deianira")))
;;
;; or with Straight:
;;    M-x straight-visit-package RET deianira RET

;;; Code:

(defgroup deianira nil
  "Hydra everywhere."
  ;; :link '(info-link "(deianira)")
  :group 'keyboard)

;; builtin dependencies
(require 'subr-x)
(require 'cl-lib)
(require 'deianira-obsoletes)

;; external dependencies
(require 'dash)
(require 'hydra)
(require 'compat)
(require 'asyncloop) ;; was part of this package
(require 'massmapper-lib) ;; was part of this package

;; muffle the compiler
(declare-function #'dei-A/body "deianira" nil t)
(declare-function #'dei-C/body "deianira" nil t)
(declare-function #'dei-H/body "deianira" nil t)
(declare-function #'dei-M/body "deianira" nil t)
(declare-function #'dei-s/body "deianira" nil t)


;;;; User settings

(defcustom dei-hydra-keys
  "1234567890qwertyuiopasdfghjkl;zxcvbnm,./"
  "Keys to show in hydra hint\; default reflects an US keyboard.
Length should be divisible by `dei-columns'.  In other words, if
you have 10 columns this can be 30 or 40 characters, if you
want 11 columns this can be 33 or 44 characters, and so on.

The order matters\; it determines the order in which the keys are
displayed.

Unfortunately I have no advice if you want to show more oddly
located (on QWERTY) keys such as ` or =, but these would waste
columns on an already cramped grid."
  :type 'string
  :group 'deianira)

(defcustom dei-all-shifted-symbols
  "~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?"
  "Characters that imply Shift pressed\; default reflects an US keyboard.
You can add or remove any characters, in any order.  You can even
merge two or more regional layouts' sets of shift-symbols,
because inclusion in this list mostly means that Deianira will do
less with them.

Note also that even if you usually use a non-US layout, you do
not need to modify this variable if you never bind non-US keys to
commands anyway."
  :type 'string
  :group 'deianira)

(defcustom dei-columns 10
  "Amount of columns to display in the hydra hint.
When customizing, please also customize `dei-hydra-keys'."
  :type 'number
  :group 'deianira)

(defcustom dei-debug nil
  "Whether to enable debugging checks."
  :type 'boolean
  :group 'deianira)

(defcustom dei-quasiquitter-keys
  '("C-c c"
    "C-x l" ;; for testing
    )
  "Keys that send you to the root hydra.
Note that if you use Massmapper (see manual), the hydras are
generated afterwards, consulting this list then.  So it is safe
to only refer to e.g. \"C-c c\" even if it's going to be a clone
of \"C-c C-c\".  In fact, only \"C-c c\" will have an effect,
probably."
  :type '(repeat key)
  :group 'deianira
  :set (lambda (var new)
         (set-default var (--map (massmapper--normalize
                                  (key-description (key-parse it))) new))))

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
         (set-default var (--map (massmapper--normalize
                                  (key-description (key-parse it))) new))))

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
         (set-default var (--map (massmapper--normalize
                                  (key-description (key-parse it))) new))))

(defcustom dei-quitter-keys
  '()
  "Key sequences guaranteed to slay the hydra.
Note that if you use Massmapper (see manual), the hydras are
generated afterwards, consulting this list then.  So it is safe
to only refer to e.g. \"C-c c\" even if it's going to be a clone
of \"C-c C-c\".  In fact, only \"C-c c\" will have an effect,
probably."
  :type '(repeat key)
  :group 'deianira
  :set (lambda (var new)
         (set-default var (--map (massmapper--normalize
                                  (key-description (key-parse it))) new))))

(defcustom dei-quitter-commands
  '(keyboard-quit
    keyboard-escape-quit
    minibuffer-keyboard-quit
    abort-recursive-edit
    dei--call-and-return-to-root
    dei--call-and-return-to-parent
    isearch-forward
    isearch-forward-regexp
    isearch-backward
    isearch-backward-regexp
    query-replace
    query-replace-regexp
    doom/escape
    doom/restart
    doom/restart-and-restore
    magit-status
    dired
    dired-jump
    re-builder
    ;; ffap-other-frame
    ;; make-frame-command
    ;; other-frame
    kill-emacs
    save-buffers-kill-emacs
    save-buffers-kill-terminal
    +lookup/online
    +doom-dashboard/open
    org-noter
    org-agenda
    org-roam-capture
    org-capture)
  "Commands guaranteed to slay the hydra.
Note that you usually don't need to add commands that focus the
minibuffer, as Deianira tries to slay the hydra automatically
when that happens."
  :type '(repeat symbol)
  :group 'deianira)

(defcustom dei-extra-heads
  '()
  "Heads to add to every hydra.  See `defhydra' for the format."
  :type '(repeat sexp)
  :group 'deianira)

(defcustom dei-ignore
  nil
  "Regexp for key sequences to avoid making hydras for.
To ignore all C- sequences, write C-."
  :group 'deianira
  :type 'string)

;; FIXME: broken in the Custom interface
(defcustom dei-invisible-leafs
  (append
   (split-string
    "`-=[]\\'"
    "" t)
   (split-string
    (concat
     " <left> <right> <up> <down> SPC"
     " <print> <insert> <next> <prior> <home> <end> <menu>"
     " TAB <tab> <iso-lefttab> <backtab>")))
  "Keys that should not behave as foreign keys.
By default, typing a key not in `dei-hydra-keys' nor this list
will result in calling that key's normal binding, as if there was
no active hydra (in Hydra jargon, it behaves as a foreign key).

Inclusion in this list means that key will be prepended with a
prefix, even though you can't see the key in the hint.

Example:

If TAB is not a member of this list, typing the three keystrokes
<Ctrl> x TAB calls the binding of TAB.

If TAB is a member of this list, it's taken as a leaf grafted
onto the hydra's corresponding stem.  So typing the three
keystrokes <Ctrl> x TAB calls the binding of C-x TAB."
  :type '(repeat key)
  :group 'deianira
  :set (lambda (var new)
         (set-default var (--map (massmapper--normalize
                                  (key-description (key-parse it))) new))))


;;;; Background facts

(defun dei--all-shifted-symbols-list-recalc ()
  "Make new value for `dei--all-shifted-symbols-list'."
  (split-string dei-all-shifted-symbols "" t))

(defvar dei--all-shifted-symbols-list (dei--all-shifted-symbols-list-recalc)
  "Cache variable, not to be modified directly.
Customize `dei-all-shifted-symbols' instead.")

(defun dei--hydra-keys-list-recalc ()
  "Make new value for `dei--hydra-keys-list'."
  (split-string dei-hydra-keys "" t))

(defvar dei--hydra-keys-list (dei--hydra-keys-list-recalc)
  "Cache variable, not to be modified directly.
Customize `dei-hydra-keys' instead.")

(defun dei--colwidth-recalc (width)
  "Recalculate `dei--colwidth' based on frame width WIDTH."
  ;; Minus four because of the legend (there's space reserved around a,b,c in
  ;; "a: COMMAND b: COMMAND c: COMMAND ...".
  (max 1 (- (floor width dei-columns) 4)))

(defvar dei--colwidth (dei--colwidth-recalc (frame-width))
  "Cache variable, not to be modified directly.")

(defun dei--filler-recalc (colwidth)
  "Recalculate `dei--filler' based on COLWIDTH.
See `dei--colwidth-recalc'."
  (make-string colwidth (string-to-char " ")))

(defvar dei--filler (dei--filler-recalc dei--colwidth)
  "Cache variable, not to be modified directly.")


;;;; Library of random stuff

(defun dei--hydra-active-p ()
  "Return t if a hydra is active and awaiting input."
  (not (null hydra-curr-map)))

(defun dei--slay (&rest args)
  "Slay active hydra and return ARGS."
  (when (dei--hydra-active-p)
    (setq hydra-deactivate t)
    (call-interactively #'hydra-keyboard-quit))
  args)

(defun dei--dub-hydra-from-key (keydesc)
  "Example: if KEYDESC is \"C-x C-a\", return \"dei-Cxa\"."
  (declare (pure t) (side-effect-free t))
  (if (string-empty-p keydesc)
      nil
    (let* ((steps (split-string keydesc " "))
           (first-is-long (> (length (car steps)) 1)))
      (and dei-debug
           first-is-long
           ;; Never try to represent chords beyond the first.  This restriction
           ;; allows compact notation with dashes and spaces stripped.  Else
           ;; there would be ambiguity due to confusing chords and keys for
           ;; each other (even banning upcase keys leaves s-).
           (cl-assert (not (string-match-p massmapper--modifier-safe-re
                                           (substring keydesc 2)))))
      (concat "dei-"
              ;; root modifier
              (and first-is-long
                   (string-match-p massmapper--modifier-safe-re
                                   (substring keydesc 0 2))
                   (substring keydesc 0 1))
              ;; the rest
              (string-join
               (cl-loop
                for step in steps
                as leaf = (massmapper--get-leaf step)
                collect
                ;; TODO: Before transitioning to this, ensure <tab> and TAB
                ;; don't generate diff hydras
                ;; (cond (;; Eliminate ambiguity by downcasing capitals
                ;;        (and (= 1 (length leaf))
                ;;             (seq-contains-p dei-all-shifted-symbols
                ;;                             (string-to-char leaf)))
                ;;        (concat "S" (downcase leaf)))
                ;;       ;; Make function keys compact (and stay unambiguous
                ;;       ;; by upcasing them instead)
                ;;       ((string-match-p "<.*?>" leaf)
                ;;        (upcase (string-replace "-" "" (substring 1 -1 leaf))))
                ;;       (t
                ;;        leaf))
                ;; wrap RET SPC DEL etc in <>, to distinguish from
                ;; (extremely unusual) all-caps seqs such as T A B
                (if (member leaf '("NUL" "RET" "TAB" "LFD" "ESC" "SPC" "DEL"))
                    (concat "<" leaf ">")
                  leaf)))))))

(defun dei--dub-hydra-from-stem (stem)
  "Example: if STEM is \"C-x C-\", return \"dei-Cx\"."
  (declare (pure t) (side-effect-free t))
  (if (string-empty-p stem)
      nil
    (if (= 2 (length stem))
        (concat "dei-" (substring stem 0 1)) ;; stem is just C- or M- etc
      (dei--dub-hydra-from-key (string-join (butlast (split-string stem " "))
                                            " ")))))

(defun dei--dub-hydra-from-key-or-stem (keydesc-or-stem)
  (if (key-valid-p keydesc-or-stem)
      (dei--dub-hydra-from-key keydesc-or-stem)
    (dei--dub-hydra-from-stem keydesc-or-stem)))

;; This is used by `dei--head-arg-cmd'
(defun dei--corresponding-hydra (keydesc-or-stem &optional leaf)
  "Return the hydra body that corresponds to a key.
If only one argument is given, KEYDESC-OR-STEM, it should be a
valid key description.  If supplying LEAF, then KEYDESC-OR-STEM
should be a dangling stem, as they will be concatenated to make a
valid key description."
  (let ((keydesc (concat keydesc-or-stem leaf)))
    (if (string-empty-p keydesc)
        nil
      (intern (concat
               (dei--dub-hydra-from-key-or-stem keydesc)
               "/body")))))

;; REVIEW: write test for it with a key-simulator
(defun dei-universal-argument (arg)
  "Enter a nonum hydra and activate the universal argument."
  (interactive "p")
  (prefix-command-preserve-state)
  (call-interactively
   (intern (concat
            (substring (symbol-name hydra-curr-body-fn) 0 -5) ;; chop "/body"
            "-nonum/body")))
  (hydra--universal-argument arg))

(defun dei-negative-argument (arg)
  "Enter a nonum hydra and activate the negative argument."
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
  "For use in a hydra: call the key binding of KEYDESC.
Then return to the root hydra.

Nice in some cases, like C-c C-c for which it's often desirable
to end up in the Control root hydra rather than exit altogether.
Say you want to call it for each item in a list of Org headings,
and `next-line' is bound to the standard C-n, then you want to be
able to type nccnccnccncc."
  (interactive)
  (call-interactively (key-binding (key-parse keydesc)))
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
  "For use in a hydra: call the key binding of KEYDESC.
Then return to the parent hydra."
  (interactive)
  (call-interactively (key-binding (key-parse keydesc)))
  (if-let ((parent (dei--corresponding-hydra
                    (massmapper--parent-stem
                     (massmapper--drop-leaf keydesc)))))
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

(defvar dei--interrupts-counter 0
  "How many times the hydra maker was interrupted recently.")

(defun dei--interrupts-decrement ()
  "Decrement `dei--interrupts-counter' unless already zero."
  (unless (zerop dei--interrupts-counter)
    (cl-decf dei--interrupts-counter)))

(defun dei--on-which-keys (command &optional keymap)
  "Find to which keys COMMAND is bound.
Optional argument KEYMAP means look only in that keymap."
  (->> (where-is-internal command (when keymap (list keymap)))
       (-map #'key-description)
       (--remove (string-match-p dei--ignore-regexp-merged it))))

(defvar dei--give-up nil)

;;;###autoload
(define-minor-mode deianira-mode
  "Set up hooks to forge hydras, and in the darkness bind them."
  :global t
  :lighter " dei"
  :group 'deianira
  :keymap (make-sparse-keymap)
  (if deianira-mode
      (progn
        (setq dei--interrupts-counter 0)
        (setq dei--ctr-decay-timer
              (run-with-timer 60 60 #'dei--interrupts-decrement))
        (setq dei--old-hydra-cell-format hydra-cell-format)
        (setq dei--old-hydra-C-u (keymap-lookup hydra-base-map "C-u"))
        (setq hydra-cell-format "% -20s %% -11`%s")
        (setq dei--give-up nil)
        (keymap-unset hydra-base-map "C-u" t)
        ;; REVIEW: I may prefer to just instruct the user to set this stuff
        ;; themselves, so they get familiar with the variables
        (cl-loop for key in (dei--on-which-keys #'hydra--universal-argument hydra-base-map)
                 do
                 (cl-pushnew `( ,key dei-universal-argument nil :exit t) dei-extra-heads)
                 (setq dei-invisible-leafs (remove key dei-invisible-leafs)))
        (cl-loop for key in (dei--on-which-keys #'hydra--negative-argument hydra-base-map)
                 do
                 (cl-pushnew `( ,key dei-negative-argument nil :exit t) dei-extra-heads)
                 (setq dei-invisible-leafs (remove key dei-invisible-leafs)))
        ;; REVIEW: Still necessary?
        (cl-loop for key in (dei--on-which-keys #'hydra-repeat hydra-base-map)
                 do (cl-pushnew `( ,key hydra-repeat nil) dei-extra-heads))

        (add-hook 'before-make-frame-hook #'dei--slay)
        (add-hook 'minibuffer-setup-hook #'dei--slay)
        (advice-add #'completing-read :before #'dei--slay)
        (advice-add #'read-key :before #'dei--slay)
        (advice-add #'read-char :before #'dei--slay)
        (advice-add #'read-event :before #'dei--slay)
        (advice-add #'ido-read-internal :before #'dei--slay)
        (advice-add #'ivy-read :before #'dei--slay)
        (advice-add #'helm :before #'dei--slay)
        (add-hook 'window-buffer-change-functions #'dei-make-hydras-maybe 56)
        (add-hook 'window-selection-change-functions #'dei-make-hydras-maybe)
        ;; Unfortunately this is triggered every time hydra calls a command.
        ;; Maybe upstream would consider it a bug:
        ;; (add-hook 'after-change-major-mode-hook #'dei-make-hydras-maybe)
        ;; With auto-save-visited-mode, this watcher is triggered every 5
        ;; seconds.  No big deal but clutters the log buffer:
        ;; (add-variable-watcher 'local-minor-modes #'dei-make-hydras-maybe)
        (when (and dei-warn-hydra-is-helpful
                   (not hydra-is-helpful))
          (message "hydra-is-helpful is nil!  If intended, disable `%S'"
                   'dei-warn-hydra-is-helpful))
        (when (or (bound-and-true-p dei-keymap-found-hook)
                  (bound-and-true-p dei-homogenizing-winners))
          (deianira-mode 0)
          (user-error "%s%s%s"
                      "Deianira has split into two packages,"
                      " you'll want to add the massmapper package:"
                      " https://github.com/meedstrom/massmapper"))
        (when (null massmapper-homogenizing-winners)
          (deianira-mode 0)
          (user-error "Disabling Deianira, please customize massmapper-homogenizing-winners")))
    (when dei--loop (asyncloop-cancel dei--loop))
    (when (timerp dei--ctr-decay-timer) (cancel-timer dei--ctr-decay-timer))
    (setq hydra-cell-format (or dei--old-hydra-cell-format "% -20s %% -8`%s"))
    (keymap-set hydra-base-map "C-u" dei--old-hydra-C-u)
    (remove-hook 'window-buffer-change-functions #'dei-make-hydras-maybe)
    (remove-hook 'window-selection-change-functions #'dei-make-hydras-maybe)
    (remove-hook 'before-make-frame-hook #'dei--slay)
    (remove-hook 'minibuffer-setup-hook #'dei--slay)
    (advice-remove #'completing-read #'dei--slay)
    (advice-remove #'read-key #'dei--slay)
    (advice-remove #'read-event #'dei--slay)
    (advice-remove #'read-char #'dei--slay)
    (advice-remove #'ido-read-internal #'dei--slay)
    (advice-remove #'ivy-read #'dei--slay)
    (advice-remove #'helm #'dei--slay)))

(defcustom dei-warn-hydra-is-helpful t
  "Whether to warn that `hydra-is-helpful' is nil.")

(defvar dei--ctr-decay-timer nil)
(defvar dei--loop nil)

(defconst dei--ersatz-keys-alist
  '((dei-ersatz-alt . dei-A/body)
    (dei-ersatz-control . dei-C/body)
    (dei-ersatz-hyper . dei-H/body)
    (dei-ersatz-meta . dei-M/body)
    (dei-ersatz-super . dei-s/body))
  "Table associating the ersatz keys with root hydras.
Never expected to change.  Customize `dei-ersatz-alt' & co
instead.")

(defvar dei--hidden-obarray (obarray-make)
  "Place to store the huge variable named \"flocks\".
Reason not to store it in the global obarray as simply just
another variable \"dei--flocks\" is that when the user reads the
source code and happens to place point on \"dei--flocks\", eldoc
chokes Emacs for minutes.")

(defun dei--set-ersatz-key (sym newkey)
  "Bind SYM to NEWKEY, and help other code cope with the change."
  ;; Reset all hydras because value gets hardcoded by `dei--specify-extra-heads'
  (obarray-remove dei--hidden-obarray "flocks")
  ;; Unbind last key in case it was different
  (when (boundp sym)
    (keymap-unset deianira-mode-map (symbol-value sym) t))
  ;; Bind new key
  (keymap-set deianira-mode-map newkey (alist-get sym dei--ersatz-keys-alist))
  (set-default sym newkey))

;; FIXME: these options are broken in the Custom interface
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


;;;; Hydra blueprinting

(defvar dei--hydrable-prefixes-with-ancestors nil
  "List of keys that may turn into hydras.")

;; Head arguments

(defun dei--head-arg-cmd (stem leaf)
  "See `dei--head'."
  (let ((key (concat stem leaf)))
    (cond
     ;; Sub-hydra
     ((member key dei--hydrable-prefixes-with-ancestors)
      (dei--corresponding-hydra key))
     ;; Quasi-quitter, meaning call key and return to root hydra
     ;; Only works if root hydra is a modifier, not if it's <f1> or other keys!
     ((and (massmapper--key-starts-with-modifier key)
           (or (member (key-binding (key-parse key)) dei-quasiquitter-commands)
               (member key dei-quasiquitter-keys)))
      `(lambda ()
         (interactive)
         (call-interactively (key-binding ,(key-parse key)))
         ;; the root hydra /body
         (,(dei--corresponding-hydra (substring key 0 2)))))
     ;; Regular key
     (t
      `(call-interactively (key-binding ,(key-parse key)))))))

(defun dei--head-arg-cmd-leads-to-subhydra-p (stem leaf)
  (let ((binding (dei--head-arg-cmd stem leaf)))
    (when (symbolp binding)
      (string-suffix-p "/body" (symbol-name binding)))))

(defun dei--head-arg-hint (stem leaf)
  "See `dei--head'."
  (let* ((sym (if (member (concat stem leaf)
                          dei--hydrable-prefixes-with-ancestors)
                  (dei--corresponding-hydra stem leaf)
                (key-binding (key-parse (concat stem leaf)))))
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
    (when (or (member (key-binding (key-parse key)) dei-quasiquitter-commands)
              (member (key-binding (key-parse key)) dei-quitter-commands)
              (member key dei-quasiquitter-keys)
              (member key dei-quitter-keys)
              (member key dei--hydrable-prefixes-with-ancestors)
              ;; Extra safety measure which could be upstreamed to Hydra
              (dei--head-arg-cmd-leads-to-subhydra-p stem leaf))
      '(:exit t))))

;; Different types of full heads

(defun dei--head (stem leaf)
  "Return a hydra head specification (a list), see `defhydra'.
Strings STEM and LEAF concatenate to form a key description
satisfying `key-valid-p' (Emacs 29 function), and string LEAF is
a single unchorded key, typically one character but can also be a
named function key such as <return>."
  `( ,leaf
     ,(dei--head-arg-cmd stem leaf)
     ,(dei--head-arg-hint stem leaf)
     ,@(dei--head-arg-exit stem leaf)))

(defun dei--head-invisible (stem leaf)
  `( ,leaf
     ,(dei--head-arg-cmd stem leaf)
     ,@(dei--head-arg-exit stem leaf)))

(defun dei--head-invisible-self-inserting-stemless (_stem leaf)
  `( ,leaf
     self-insert-command
     :exit t))

(defun dei--head-invisible-exiting-stemless (_stem leaf)
  `( ,leaf
     ,(dei--head-arg-cmd "" leaf)
     :exit t))

(defun dei--head-invisible-stemless (_stem leaf)
  `( ,leaf
     ,(dei--head-arg-cmd "" leaf)))

;; Lists of full heads

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
          `("<backspace>" ,(dei--corresponding-hydra
                            (massmapper--parent-stem stem)) :exit t))
       ,@(when self-poppers
           (cl-loop for key in self-poppers
                    collect `(,key nil :exit t)))
       ,@extras))))

;; Grand merged list of heads

(defun dei--specify-hydra (stem name &optional nonum-p)
  "Return a list of hydra heads, with NAME as first element.
You can pass the output to `dei--try-birth-hydra'.

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

;; NOTE: Further improving performance is not a priority, but here's where the
;; package spends over 50% of CPU time, sometimes 90%.  I guess defhydra was
;; not meant to be used in a performance-intensive way, especially when quoted.
;; Approaches:
;;
;; 1. Dodge the need to `eval' a quoted macro, so the byte-compiler can help.
;;    I don't really see a way without rewriting defhydra as a defun that suits
;;    our purposes (even if it would be limited for other purposes).  I doubt
;;    I'm capable of doing so without abo-abo's advice since the perf is not an
;;    itch for me anymore.
;;
;; 2. Reduce workload by giving up on the self-inserting quitters (principally
;;    capital keys) since there are many of those heads, or giving up on nonum
;;    hydras.
;;
;; 3. C-s cream on top RET
;;
;; 4. It seems that every time this function runs, it gets slower, in a way
;;    that doesn't have to do with point #3. On emacs start, it takes 0.01s,
;;    but after many flocks, it takes 0.30s per invocation!  (And because of
;;    this, the stopgap measure described in point #2 would compound to a huge
;;    difference)
(defun dei--try-birth-hydra (recipe)
  "Birth a hydra out of RECIPE formed by `dei--specify-hydra'.
You can view that function as a dry-run and this one as the step
that finally wets it, altering Emacs state in a way the user can
see."
  (let ((name (car recipe))
        (list-of-heads (cdr recipe)))
    (if (eq hydra-curr-body-fn (intern name))
        ;; REVIEW: An old safety measure, not sure it's needed
        (error "This hydra active, skipped redefining: %s" name)
      (eval `(defhydra ,(intern name)
               (:columns ,dei-columns
                :exit nil
                :hint nil
                :foreign-keys run)
               ,name
               ,@list-of-heads)
            t))))


;;;; Keymap scanner

;; Since (rx (or ...)) calls the slow `regexp-opt', cache the regexp.
(defconst dei--shift-chord-regexp (rx (or bol "-" " ") "S-")
  "Match explicit \"S-\" chords in key descriptions.
Do not match capital letters.")

;; (will probably export to massmapper.el)
(defun dei--key-is-illegal (keydesc)
  "Non-nil if KEYDESC would be unbound in a purist scheme.
This means a scheme of homogenizing, no shift, no multi-chords,
and no mixed modifiers."
  (let ((case-fold-search nil))
    (or
     (string-match-p dei--shift-chord-regexp keydesc)
     (massmapper--key-contains dei--all-shifted-symbols-list keydesc)
     (massmapper--key-contains-multi-chord keydesc)
     (massmapper--key-mixes-modifiers keydesc)
     ;; Drop bastard sequences.
     (and (massmapper--key-has-more-than-one-chord keydesc)
          (not (massmapper--permachord-p keydesc)))
     ;; Drop e.g. <f1> C-f.
     (and (not (massmapper--key-starts-with-modifier keydesc))
          (string-match-p massmapper--modifier-safe-re keydesc)))))

(defconst dei--ignore-regexp-merged
  (regexp-opt
   (append '("DEL" "backspace")
           ;; declutter a bit (unlikely someone wants)
           '("help" "kp-" "iso-" "wheel" "menu" "redo" "undo" "again" "XF86")
           massmapper--ignore-keys-irrelevant
           massmapper--ignore-keys-control-chars)))

;; REVIEW: Test <ctl> x 8 with Deianira active.
(defvar dei--unnest-avoid-prefixes
  (cons "C-x 8"
        (mapcar #'key-description
                (where-is-internal #'iso-transl-ctl-x-8-map)))
  "List of prefixes to avoid looking up.")

(defun dei--unnest-and-filter-current-bindings (&optional raw)
  "List the current bindings appropriate for hydra-making.
The result is an alist of \(KEYDESC . COMMAND\), where each
KEYDESC is normalized to satisfy `key-valid-p'.  COMMAND is
expected to be either a command or a lambda or closure.

Perhaps obvious but: this is not the bindings from any single
keymap such as `global-map', but from the composite of all
currently active keymaps, many of which occlude parts of
`global-map'.

Optional argument RAW may be useful for debugging, by returning a
less filtered and not normalized list."
  ;; You know that the same key can be found several times in multiple keymaps.
  ;; Fortunately we can know which binding is correct for the current buffer,
  ;; as `current-active-maps' seems to return maps in the order in which Emacs
  ;; selects them.  So what comes earlier in the list is what would in fact be
  ;; used.  The rest is history.
  (cl-loop
   with merged = nil
   with case-fold-search = nil
   for raw-map in (current-active-maps)
   ;; as cleaned = (member map (bound-and-true-p dei--cleaned-maps))
   append (cl-loop
           with bindings = nil
           for vec being the key-seqs of raw-map
           using (key-bindings cmd)
           as key = (key-description vec)
           as norm = (ignore-errors (massmapper--normalize key))
           if raw collect (cons key cmd) into bindings
           else
           when cmd
           ;; Skip nonstandard menu-bar/tool-bar/tab-bar mess
           unless (string-search "-bar>" key)
           unless (and (listp cmd) (eq 'menu-item (car cmd)))
           ;; Sometimes (key-description vec) evalutes to a key called
           ;; "C-x (..*", a key called "ESC 3..9", etc.  Even stranger,
           ;; VEC for every one of them is just [switch-frame].  And
           ;; all are bound to self-insert-command.  I don't get it.
           ;; But it's ok to filter out self-insert-command since I
           ;; deem it really rare people bind it in nonstandard places.
           unless (string-search ".." key)
           ;; (Late error-check because some of those we filtered out
           ;; would've tripped an error)
           if (null norm)
           do (error "key couldn't be normalized: %s" key)
           else
           unless (assoc norm bindings)
           unless (assoc norm merged)
           unless (string-match-p dei--ignore-regexp-merged norm)
           unless (massmapper--key-has-more-than-one-chord norm)
           ;; unless (unless cleaned
           ;;         (dei--key-is-illegal norm))
           unless (dei--key-is-illegal norm)
           ;; unless (-any-p #'dei--not-on-keyboard (split-string norm " "))
           unless (cl-loop for prefix in dei--unnest-avoid-prefixes
                           when (string-prefix-p prefix norm)
                           return t)
           ;; Check for ancestors of this key sequence...  the things I
           ;; put up with...
           unless (cl-loop
                   with n = (length (split-string norm " " t))
                   with parents = (-iterate #'massmapper--parent-key
                                            norm n)
                   for parent in parents
                   when (assoc parent merged)
                   return t)
           ;; Oh!  Better check for descendants of this key sequence
           ;; too.
           unless (cl-loop
                   for (superseder . _) in merged
                   when (string-prefix-p norm superseder)
                   return t)
           collect (cons norm cmd) into bindings
           finally return bindings)
   into merged
   finally return merged))

(defun dei--connection-exists (parent-stem child-stem)
  (when (and parent-stem
             child-stem)
    (let* ((parent-hydra-name (dei--dub-hydra-from-stem parent-stem))
           (child-hydra-name (dei--dub-hydra-from-stem child-stem)))
      (and (fboundp (intern (concat parent-hydra-name "/body")))
           (cl-find-if
            (lambda (head)
              (eq (nth 1 head)
                  (intern (concat child-hydra-name "/body"))))
            (symbol-value (intern (concat parent-hydra-name "/heads"))))))))
;; (dei--connection-exists "C-c " "C-c p ")


;;;; Async worker
;; Because it would cause noticable lag to do everything at once, we slice up
;; the work into pieces and run them only while the user is not operating
;; Emacs.  That's not true async, but it makes no difference to the user and
;; it's easier to debug than true async (especially when every invocation
;; mutates Emacs state).  Library I made for this need:
;; https://github.com/meedstrom/asyncloop

(defvar dei--hydra-recipes nil "List of arglists to pass to defhydra.")
(defvar dei--buffer-under-analysis nil "The buffer to make hydras for.")
(defvar dei--current-hash 0 "The keymap composite in buffer under analysis.")
(defvar dei--current-width (frame-width) "Width of frame under analysis.")
(defvar dei--current-bindings nil "The bindings in buffer under analysis.")
(defvar dei--last-bindings nil "Bindings in last fully analyzed buffer.")
(defvar dei--changed-stems nil "Stems needing new hydra since last analysis.")

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

(cl-defstruct (dei--flock
               (:constructor dei--flock-define)
               (:copier nil))
  hash
  width
  funs
  vars
  bindings)

(defun dei--first-flock-by-hash (hash flocks)
  "Return a flock with :hash HASH, among FLOCKS."
  (cl-loop for flock in flocks
           when (eq hash (dei--flock-hash flock))
           return flock))

(defun dei--flock-by-hash-and-width (hash width flocks)
  "Return flock with HASH and WIDTH from FLOCKS."
  (cl-loop for flock in flocks
           when (and (eq hash (dei--flock-hash flock))
                     (eq width (dei--flock-width flock)))
           return flock))

;; TODO: figure out if it's necessary to respecify heads due to changed
;; dei--filler and if so how to get the effect here (theory: we don't need
;; filler to be more than one char, I think it was for gracefully degrading
;; from a weird issue) (theory: we see no effect when starting with a narrow
;; frame and rehint for a big frame, but the other way around could get messy)
(defun dei--rehint-flock (flock width)
  "Copy FLOCK and update hints to match frame width WIDTH."
  (cl-letf* ((dei--colwidth (dei--colwidth-recalc width))
             (dei--filler (dei--filler-recalc dei--colwidth))
             (new (copy-sequence flock)))
    (setf (dei--flock-width new) width)
    (setf (dei--flock-vars new)
          (cl-loop
           for pair in (dei--flock-vars flock)
           when (string-suffix-p "/hint" (symbol-name (car pair)))
           collect
           (let ((basename (substring (symbol-name (car pair)) 0 -5)))
             (cons (car pair)
                   ;; Note that we eval to get a static string, see also
                   ;; `dei--step-6-birth-hydra'.  It's ok b/c there is
                   ;; nothing dynamic in the input, so the result is
                   ;; deterministic.
                   (eval (hydra--format
                          ;; These arguments are identical to what `defhydra'
                          ;; passes to `hydra--format'. The different result
                          ;; is due to let-binding `dei--colwidth'.
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
;;

(defun dei--abort-if-buffer-killed (loop buf)
  "Cancel hydre-building LOOP if buffer killed.
Otherwise, return nil."
  (unless (buffer-live-p buf)
    (asyncloop-log loop
      "Cancelling because buffer killed: %s" buf)
    (asyncloop-cancel loop)
    t))

(defun dei--step-1-find-premade (loop)
  (let* ((buffer (current-buffer))
         (flocks (symbol-value (obarray-get dei--hidden-obarray "flocks")))
         (hash (abs (sxhash (current-active-maps))))
         (width (frame-width))
         ;; If we already made hydras for this keymap composite, retrieve them
         (some-flock (dei--first-flock-by-hash hash flocks)))
    (setq dei--buffer-under-analysis buffer)
    (setq dei--current-hash hash)
    (setq dei--current-width width)
    (if some-flock
        (let* ((past (dei--flock-by-hash-and-width hash width flocks))
               (existed (if past
                            t
                          (setq past (dei--rehint-flock some-flock width))
                          nil))
               (bindings (dei--flock-bindings past))
               (funs (dei--flock-funs past))
               (vars (dei--flock-vars past)))
          ;; Reuse cached values.  This code came from an eureka, so beautiful.
          (cl-loop for (sym . val) in funs do (fset sym val))
          (cl-loop for (sym . val) in vars do (set sym val))
          (setq dei--last-bindings bindings)
          (if existed
              (progn
                ;; Do not proceed to next steps
                (asyncloop-cancel loop 'quietly)
                (format "Summoned flock %d" hash))
            ;; After conversion, skip to step 7 to register them
            (setf (asyncloop-remainder loop) (list t #'dei--step-7-register))
            (format "Converted to %d chars wide: %d" width hash)))
      (format "Requisition new flock for buffer %s" buffer))))

;; REVIEW: Maybe write it more readable somehow
(defun dei--step-2-validate-user-settings (loop)
  "Signal an error if any two user settings overlap.
That is, if a given key description is found more than once.

This prevents a situation where a hydra defines two heads on
the same key."
  (unless (dei--abort-if-buffer-killed loop dei--buffer-under-analysis)
    (unless (--all-p (equal (symbol-value (car it)) (cdr it))
                     dei--last-settings-alist)
      (unless (= 0 (mod (length dei-hydra-keys) dei-columns))
        (user-error "`dei-hydra-keys' not divisible by `dei-columns'"))
      ;; Record the newest setting values, so we can skip the relatively
      ;; expensive calculations next time if nothing changed.
      (setq dei--last-settings-alist
            (cl-loop for cell in dei--last-settings-alist
                     collect (cons (car cell) (symbol-value (car cell)))))
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
             do (let ((overlap (-intersection (cdr var)
                                              (cdr remaining-var))))
                  (when overlap
                    (error (asyncloop-log loop
                             "Found %s in both %s and %s"
                             overlap (car var) (car remaining-var))))))))))
    "OK"))

;; v2
(defun dei--connection-exists (parent child)
  "Check if PARENT hydra exists and binds CHILD hydra."
  (when (and parent child)
    (let* ((parent-hydra-name (dei--dub-hydra-from-key parent))
           (child-hydra-name (dei--dub-hydra-from-key child)))
      (and (fboundp (intern (concat parent-hydra-name "/body")))
           (cl-find-if
            (lambda (head)
              (eq (nth 1 head)
                  (intern (concat child-hydra-name "/body"))))
            (symbol-value (intern (concat parent-hydra-name "/heads"))))))))

;; what to do if child hydra stops existing?, we still need to redefine the ancestor then
;;
;; the trick is that if child hydra stops existing, that should be visible as a
;; difference in new-bindings.  (not necessarily if it's just deeply nested
;; keymap that disappeared...)
;;
;; but then what we want to check is not that a connection exists, but that it
;; unexists.
;;
;; may once again have to split the defunct bindings from the new.
(defun ancestors-to-connect (key)
  (let (ancestors
        (run t))
    (while run
      (let ((parent (massmapper--parent-key key)))
        (if (dei--connection-exists parent key)
            ;; also push the parent of the parent in case
            (setq run nil)
          (push parent ancestors)
          ))

      )
    ancestors)
  )

(defun dei--new-bindings ()
  (let* ((new-bindings (append (-difference dei--current-bindings
                                            dei--last-bindings)
                               (-difference dei--last-bindings
                                            dei--current-bindings)))
         (new-with-ancestors
          (cl-loop
           for (key . _) in new-bindings
           as n = (length (split-string key " "))
           append (-iterate #'massmapper--parent-key key n)))
         )

    (-union dei--hydrable-prefixes
            (cl-loop
             for prefix in new-with-ancestors
             unless (dei--connection-exists parent-stem stem)
             collect parent-stem))
    ))

(defun dei--step-3-model-the-world (loop)
  "Calculate facts."
  (unless (dei--abort-if-buffer-killed loop dei--buffer-under-analysis)
    (with-current-buffer dei--buffer-under-analysis
      ;; Cache settings
      (setq dei--all-shifted-symbols-list (dei--all-shifted-symbols-list-recalc)
            dei--hydra-keys-list (dei--hydra-keys-list-recalc)
            dei--colwidth (dei--colwidth-recalc dei--current-width)
            dei--filler (dei--filler-recalc dei--colwidth))

      (condition-case err
          (setq dei--current-bindings
                (dei--unnest-and-filter-current-bindings))
        ((error debug)
         (asyncloop-log loop "Problem filtering bindings: %s" (cdr err))
         (signal (car err) (cdr err))))

      ;; Since `dei--unnest-and-filter-current-bindings' returns full keydescs
      ;; only, infer prefixes by cutting the last key off each sequence.
      (setq dei--hydrable-prefixes
            (-uniq
             (cl-loop
              for key in (-uniq (-keep #'massmapper--parent-key
                                       (-map #'car dei--current-bindings)))
              collect key)))

      ;; Add every possible ancestor (count C-c and C-c c as well as C-c c p).
      ;; This variable will be looked up by the hydra-head makers, which I'm
      ;; 80% sure don't need this much info, but it doesn't hurt.  It will also
      ;; be used shortly below.
      (setq dei--hydrable-prefixes-with-ancestors
            (-uniq
             (cl-loop
              for key in dei--hydrable-prefixes
              as n = (length (split-string key " "))
              append (-iterate #'massmapper--parent-key key n))))

      ;; Note that this algo typically gives identical or near-identical
      ;; results to just collecting all parent stems of
      ;; `dei--hydrable-prefixes-with-ancestors'.  However, this code better
      ;; expresses what we actually need, so I prefer to keep it like this.
      ;; It does let us skip making a hydra or two sometimes.
      (setq dei--hydrable-stems
            (-uniq
             (-union
              (-keep #'massmapper--prefix-to-stem dei--hydrable-prefixes)
              ;; Check ancestors in case we have to create those hydras too
              (cl-loop
               for prefix in dei--hydrable-prefixes-with-ancestors
               as stem = (massmapper--prefix-to-stem prefix)
               as parent-stem = (massmapper--parent-stem stem)
               unless (dei--connection-exists parent-stem stem)
               collect parent-stem))))

      ;; Figure out which stems have changed, so we can exploit the previous
      ;; flock's work and skip running defhydra for results we know will be
      ;; identical.  This has a TREMENDOUS performance boost each time we
      ;; encounter a new keymap combo, cutting 30 seconds of computation down
      ;; to 0-2 seconds.

      ;; TODO: have "bindings" include subkeymaps too just to be sure we really
      ;; update.  In emacs-lisp-mode, I was getting a Cxn stem that comes from
      ;; org-mode, because I had no equivalent prefix in emacs-lisp-mode.
      ;; Although that issue should not happen if C-x n is bound to a command
      ;; -- the issue there was it was bound to an empty keymap, now fixed.
      ;; Still, it casts light on how this algo really works, and I think I
      ;; need a flowchart before I remove this comment, just to ensure that all
      ;; edge cases will be taken care of.

      (setq dei--changed-stems
            (cl-loop
             with case-fold-search = nil
             ;; Visualize a Venn diagram, with two circles for the LAST and
             ;; CURRENT bindings. The defunct bindings are somewhere in LAST,
             ;; the new bindings somewhere in CURRENT, but neither are in the
             ;; circles' intersection, which should never be relevant to update
             ;; as they represent cases where the key's definition is the same
             ;; as in the last keymap combo.
             with new-bindings = (append (-difference dei--current-bindings
                                                      dei--last-bindings)
                                         (-difference dei--last-bindings
                                                      dei--current-bindings))
             with new-stems = '()
             ;; TODO: I have a feeling that the whole -connection-exists stuff
             ;; earlier becomes moot here--ancestors don't get rebuilt?
             ;; Reimplement -connection-exists to use keydescs, and use it in
             ;; calculating new-bindings.  That may also take care of the other
             ;; todo regarding subkeymaps.
             for stem in dei--hydrable-stems
             as it = (cl-loop
                      for (key . _) in new-bindings
                      when (equal stem (massmapper--drop-leaf key))
                      unless (and dei-ignore (string-match-p dei-ignore key))
                      return stem)
             when it collect it))


      ;; Clear the workbench in case of a previous half-done iteration.
      (setq dei--hydra-recipes nil)

      (if dei--changed-stems
          (format "%d changed stems: %S"
                  (length dei--changed-stems)
                  dei--changed-stems)
        (setf (asyncloop-remainder loop) (list t #'dei--step-7-register))
        "No practical keymap diffs"))))

(defun dei--step-4-write-recipe (loop)
  "Write recipe for the first item of `dei--changed-stems'.
Specifically, pop a stem off the list `dei--changed-stems',
transmute it into a recipe, and push that onto the list
`dei--hydra-recipes'.

What is meant by \"recipe\" is a list of arguments that we
precompute as much as possible, that we'll later pass to
`defhydra' in `dei--step-6-birth-hydra'.

Repeatedly add another invocation of this function to the front
of asyncloop LOOP, so it will run again until
`dei--changed-stems' empty."
  (unless (dei--abort-if-buffer-killed loop dei--buffer-under-analysis)
    (with-current-buffer dei--buffer-under-analysis
      (if (null dei--changed-stems)
          "All recipes written"
        (let* ((stem (car dei--changed-stems))
               (name (dei--dub-hydra-from-stem stem))
               (r1 (dei--specify-hydra stem (concat name "-nonum") t))
               (r2 (dei--specify-hydra stem name)))
          ;; Side-effects start here
          (push r1 dei--hydra-recipes)
          (push r2 dei--hydra-recipes)
          (push t (asyncloop-remainder loop))
          (pop dei--changed-stems))))))

(defun dei--step-5-sort-recipes (loop)
  "Sort to avail most relevant hydras soonest, so the important
hydras like C- and C-x are among the first to come into
existence."
  (setq dei--hydra-recipes
        (cl-sort dei--hydra-recipes #'<
                 :key (lambda (recipe)
                        (length (car recipe)))))
  "Sorted by shortest first")

(defun dei--step-6-birth-hydra (loop)
  "Pass a recipe to `defhydra', wetting what has been a dry-run.
Each invocation pops one off `dei--hydra-recipes'.

Repeatedly add another invocation of this function to the front
of asyncloop LOOP, so it will run again until
`dei--hydra-recipes' empty."
  (unless (dei--abort-if-buffer-killed loop dei--buffer-under-analysis)
    (if (null dei--hydra-recipes)
        "All hydras born"
      ;; Side-effects start here
      (dei--try-birth-hydra (car dei--hydra-recipes))
      (push t (asyncloop-remainder loop))
      (car (pop dei--hydra-recipes)))))

(defun dei--step-7-register (loop)
  "Record the hydras made under this keymap combination."
  (let ((flocks (symbol-value (obarray-get dei--hidden-obarray "flocks")))
        funs
        vars)
    (when (dei--flock-by-hash-and-width
           dei--current-hash dei--current-width flocks)
      (error "Hash already recorded: %s" dei--current-hash))
    ;; All symbols generated by `defhydra' contain a slash in the name, so just
    ;; collect all such names from the global obarray and call that the current
    ;; flock.
    ;;
    ;; Note that this will also catch syms not used by the current flock, which
    ;; is slightly inefficient as this extra "cream on top" per flock builds up
    ;; the more flocks we make.  I expect it won't add up to even one doubling
    ;; of work, so it's fine.  The ideal alternative is patching `defhydra' to
    ;; list for us what symbols it set.
    (mapatoms
     (lambda (sym)
       (when (string-match-p "dei-.*?/" (symbol-name sym))
         (when (fboundp sym)
           (push (cons sym (symbol-function sym)) funs))
         (when (boundp sym)
           (push (cons sym (symbol-value sym)) vars)))))
    (push (dei--flock-define
           :hash dei--current-hash
           :width dei--current-width
           :funs funs
           :vars vars
           :bindings dei--current-bindings)
          flocks)
    ;; Side-effects start here
    (set (obarray-put dei--hidden-obarray "flocks") flocks)
    (setq dei--last-bindings dei--current-bindings)
    (format "Flock #%d born: %d" (length flocks) dei--current-hash)))

(defun dei--actions-on-interrupt (loop)
  "Stop trying to make hydras if excessive interrupts recently."
  (if (<= dei--interrupts-counter 4)
      (cl-incf dei--interrupts-counter)
    (setq dei--give-up t)
    (asyncloop-cancel loop)
    (warn (asyncloop-log loop
            "5 interrupts last 3 min, stopped trying to make hydras!  %s"
            "To resume, turn `deianira-mode' off and on."))))

(defun dei-make-hydras-maybe (&rest _)
  "Maybe make hydras for the current keymap combo."
  (unless (or dei--give-up
              ;; REVIEW: works fine with commenting these out?
              ;;(and (equal dei--buffer-under-analysis (current-buffer))
              ;;     (equal dei--current-width (frame-width)))
              (equal dei--current-hash (abs (sxhash (current-active-maps)))))
    (setq dei--loop
          (asyncloop-run
            (list #'dei--step-1-find-premade
                  #'dei--step-2-validate-user-settings
                  #'dei--step-3-model-the-world
                  #'dei--step-4-write-recipe
                  #'dei--step-5-sort-recipes
                  #'dei--step-6-birth-hydra
                  #'dei--step-7-register)
            :log-buffer-name "*deianira*"
            :immediate-break-on-user-activity t
            :on-interrupt-discovered #'dei--actions-on-interrupt))))


;;;; Debug toolkit

(defun dei-reset ()
  (interactive)
  (asyncloop-reset-all)
  (setq dei--last-bindings nil)
  (obarray-remove dei--hidden-obarray "flocks")
  (message "Note there's a known bug that slows down the hydra maker each reset"))

(provide 'deianira)
;;; deianira.el ends here
