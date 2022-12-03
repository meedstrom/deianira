;;; deianira-mass-remap.el --- Systematically rebind keys -*- lexical-binding: t -*-

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

;; Will probably be spun out as a standalone package called mass-keybind or
;; something.

;;; Code:

(require 'deianira-lib)
(require 'dash)
(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'help-fns)) ;; help-fns-find-keymap-name
(eval-when-compile (require 'seq)) ;; seq-let
(eval-when-compile (require 'subr-x)) ;; if-let, when-let, string-join


;;; Lib

(defun dei--is-chordonce (keydesc)
  "Non-nil if KEYDESC can be described as chordonce."
  (declare (pure t) (side-effect-free t))
  (let ((steps (split-string keydesc " ")))
    (and (string-match-p dei--modifier-regexp-safe (car steps))
         (equal (cdr steps) (-map #'dei--get-leaf (cdr steps))))))

(defun dei--is-permachord (keydesc)
  "Non-nil if KEYDESC can be described as permachord."
  (declare (pure t) (side-effect-free t))
  (when (> (length keydesc) 2)
    (let ((first-2-chars (substring keydesc 0 2)))
      (when (string-suffix-p "-" first-2-chars)
        (cl-loop for step in (split-string keydesc " ")
                 if (not (string-search first-2-chars step))
                 return nil
                 else finally return t)))))

;; Heh, when I made this function by axing where the code originally was
;; written, I found my code never did what I wanted.  Now fixed.  Score one for
;; small testable functions.
(defun dei--ensure-chordonce (keydesc)
  "Strip chords from most of key sequence KEYDESC.
Leave alone the first step of the key sequence."
  (declare (pure t) (side-effect-free t))
  (let ((steps (split-string keydesc " ")))
    (string-join (cons (car steps)
                       (-map #'dei--get-leaf (cdr steps)))
                 " ")))

(defun dei--ensure-permachord (keydesc)
  "Return KEYDESC as perma-chord.
If it's already that, return it unmodified."
  (declare (pure t) (side-effect-free t))
  (when (> (length keydesc) 2)
    (let* ((first-2-chars (substring keydesc 0 2))
           (root-modifier (when (string-suffix-p "-" first-2-chars)
                            first-2-chars)))
      (if root-modifier
          (string-join (cl-loop
                        for step in (split-string keydesc " ")
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


;;; Basics

(defcustom dei-mass-remap-debug-level 1
  "Verbosity of debug."
  :type 'integer
  :group 'deianira)

(defvar dei--remap-record nil
  "Record of work done.")

(defvar dei--remap-actions nil
  "List of actions to pass to `define-key'.")

(defun dei--pretty-print-def (def)
  "Return a string that tries to say what DEF refers to.
DEF should be a key definition such as that returned by
`lookup-key'; most of the time, it's a command, and then this
function simply returns its name."
  (cond ((symbolp def)
         (symbol-name def))
        ((keymapp def)
         (if-let ((named (help-fns-find-keymap-name def)))
             (symbol-name named)
           "(some sub-keymap)"))
        ((functionp def)
         "Anonymous lambda")
        ((listp def)
         "Likely a menu item")
        ((stringp def)
         def)
        (t
         (error "Unable to identify: %s" def))))

(define-derived-mode dei-list-remaps-mode tabulated-list-mode
  "Remaps List"
  nil
  (setq tabulated-list-format
        [("Keymap" 20 t)
         ("Reason" 45 t)
         ("Key bound" 15 t)
         ("New command" 20 t)
         ("Old command" 20 t)])
  (add-hook 'tabulated-list-revert-hook #'dei-list-remaps nil t)
  (tabulated-list-init-header))

(defun dei-list-remaps ()
  "List the key-bindings made so far by deianira-mass-remap."
  (interactive)
  (let ((buf (get-buffer-create "*Deianira remaps*")))
    (with-current-buffer buf
      (setq tabulated-list-entries nil)
      (dei-list-remaps-mode)
      (cl-loop
       for item in dei--remap-record
       do (seq-let (keydesc cmd map hint old) item
            (let ((cmd-string (dei--pretty-print-def cmd))
                  (old-string (if old
                                  (concat "(was " (dei--pretty-print-def old) ")")
                                "")))
              (push (list (sxhash item) (vector (symbol-name map)
                                                hint
                                                keydesc
                                                cmd-string
                                                old-string))
                    tabulated-list-entries))))
      (tabulated-list-print)
      (display-buffer buf))))

(defvar dei--remap-revert-list nil
  "Experimental, see `dei-remap-revert'.")

(defun dei-remap-actions-execute (actions)
  "Carry out remaps specified by ACTIONS."
  (let ((emacs29 (version<= "29" emacs-version)))
    (while actions
      (let ((action (pop actions)))
        (if (member action dei--remap-record)
            (when (> dei-mass-remap-debug-level 1)
              (message "Mass-keybind already took this action: %S" action))
          (push action dei--remap-record)
          (seq-let (keydesc cmd map _ old-def) action
            (let* (;;(old-def (lookup-key-ignore-too-long raw-map (kbd keydesc)))
                   (raw-map (dei--raw-keymap map)))
              (when-let* ((conflict-prefix (dei--key-seq-has-non-prefix-in-prefix raw-map keydesc))
                          (conflict-def (lookup-key-ignore-too-long raw-map (kbd conflict-prefix))))
                (unless (keymapp conflict-def)
                  ;; If the prefix is bound and it's not to a keymap, unbind the
                  ;; prefix so we'll be allowed to bind our key. (prevent error
                  ;; "Key sequence starts with non-prefix key")
                  (apply #'define-key raw-map (kbd conflict-prefix) nil emacs29)))
              (push (list map keydesc old-def) dei--remap-revert-list)
              (define-key raw-map (kbd keydesc) cmd))))))))

(defun dei-remap-revert ()
  "Experimental command to undo all remaps made.
It's recommended to just restart Emacs, but this might work too."
  (interactive)
  (cl-loop for x in dei--remap-revert-list
           do (seq-let (map keydesc old-def) x
                (define-key (dei--raw-keymap map) keydesc old-def))))

(defcustom dei-keymap-found-hook nil
  "Run after adding one or more keymaps to `dei--known-keymaps'.
See `dei-record-keymap-maybe', which triggers this hook.

You may be interested in hooking some of these functions:

- `dei-homogenize-all-keymaps'
- `dei-define-alt-like-meta-everywhere'
- `dei-define-super-like-ctl-everywhere'
- `dei-define-super-like-ctlmeta-everywhere'"
  :type 'hook
  :group 'deianira)

(defvar dei--known-keymaps nil
  "List of named keymaps seen active.
This typically gets populated (by `dei-record-keymap-maybe') with
just mode maps, rarely (never?) those used as transient maps and never
so-called prefix commands like `Control-X-prefix', nor the category
of sub-keymaps like `ctl-x-map' or `help-map.'")

(defvar dei--known-keymap-composites nil
  "List of unique keymap composites seen active.
These are identified as their hashes; each and every one was a
product of (sxhash (current-active-maps)), called in different
places and times.")

(defun dei-record-keymap-maybe (&optional _)
  "If Emacs has seen new keymaps, record them in a variable.
This simply checks the output of `current-active-maps' and adds
to `dei--known-keymaps' anything not already added.  Every time
we find one or more new keymaps, trigger `dei-keymap-found-hook'.

Suitable to hook on `window-buffer-change-functions' like:

\(add-hook 'window-buffer-change-functions #'dei-record-keymap-maybe)"
  (let* ((maps (current-active-maps))
         (composite-hash (abs (sxhash maps))))
    ;; Make sure we only iterate the expensive `help-fns-find-keymap-name' once
    ;; for this keymap composite.
    (unless (member composite-hash dei--known-keymap-composites)
      (push composite-hash dei--known-keymap-composites)
      (let* ((named-maps (-uniq (-keep #'help-fns-find-keymap-name maps)))
             (new-maps (-difference named-maps dei--known-keymaps)))
        (when new-maps
          (setq dei--known-keymaps (append new-maps dei--known-keymaps))
          (run-hooks 'dei-keymap-found-hook))))))


;;; Cleaning
;; Unused for now.  No point to purging ugly key bindings except that it
;; declutters which-key popups. It can also make Deianira a bit faster, as it
;; puts less workload on `dei--unnest-keymap-for-hydra-to-eat'.

(defvar dei--cleaned-maps nil)

;;  (defvar dei--clean-actions nil)

;; (defun dei-unbind-illegal-keys ()
;;   "Push keys to unbind onto `dei--clean-actions'."
;;   (cl-loop
;;    for map in (-difference dei--known-keymaps dei--cleaned-maps)
;;    with doom = (and (bound-and-true-p doom-leader-alt-key)
;;                     (bound-and-true-p doom-localleader-alt-key))
;;    do (push
;;        (cons map
;;              (cl-sort
;;               (cl-loop
;;                for x being the key-seqs of (dei--raw-keymap map)
;;                as key = (key-description x)
;;                when (and
;;                      (not (string-match-p dei--ignore-keys-regexp key))
;;                      (or (dei--key-is-illegal key)
;;                          ;; I don't want to touch these, I want to see what
;;                          ;; Doom does with them.
;;                          (when doom
;;                            (or (string-prefix-p doom-localleader-alt-key key)
;;                                (string-prefix-p doom-leader-alt-key key)))))
;;                collect key)
;;               #'> :key #'dei--key-seq-steps-length))
;;        dei--clean-actions)))


;;; Reflecting one stem in another

;; REVIEW: Consider whether to use 'for x in (dei--unnest-keymap map)'
(defun dei--how-define-a-like-b-in-keymap (recipient-mod donor-mod map)
  "Return actions needed to clone one set of keys to another set.
Inside keymap MAP, take all keys and key sequences that contain
DONOR-MOD \(a substring such as \"C-\"\), replace the substring
wherever it occurs in favor of RECIPIENT-MOD \(a substring such
as \"H-\"\), and assign them to the same commands."
  (cl-loop
   with actions = nil
   with reason = (concat "Define " recipient-mod " like " donor-mod)
   with raw-map = (dei--raw-keymap map)
   for x being the key-seqs of raw-map using (key-bindings cmd)
   as key = (key-description x)
   when (and (string-search donor-mod key)
             (not (string-search recipient-mod key))
             (or (dei--is-chordonce key)
                 (dei--is-permachord key)))
   do (let ((recipient (string-replace donor-mod recipient-mod key)))
        (if (lookup-key-ignore-too-long raw-map (kbd recipient))
            (message "User bound key, leaving it alone: %s in %S" recipient map)
          (push (list recipient cmd map reason nil) actions)))
   finally return actions))

(defvar dei--super-reflected-keymaps nil
  "List of keymaps where Super has been mass-bound.")

(defvar dei--alt-reflected-keymaps nil
  "List of keymaps where Alt has been mass-bound.")

(defun dei-define-super-like-ctl-everywhere ()
  "Duplicate all Control bindings to exist also on Super.
Appropriate on `dei-keymap-found-hook'."
  (cl-loop
   for map in (-difference dei--known-keymaps dei--super-reflected-keymaps)
   as actions = (dei--how-define-a-like-b-in-keymap "s-" "C-" map)
   when actions do
   (dei-remap-actions-execute actions)
   (message "Copied keys from Control to Super in %S: %d" map (length actions))
   do (push map dei--super-reflected-keymaps)))

(defun dei-define-super-like-ctlmeta-everywhere ()
  "Duplicate all Control-Meta bindings to exist also on Super.
Appropriate on `dei-keymap-found-hook'."
  (cl-loop
   for map in (-difference dei--known-keymaps dei--super-reflected-keymaps)
   as actions = (dei--how-define-a-like-b-in-keymap "s-" "C-M-" map)
   when actions do
   (dei-remap-actions-execute actions)
   (message "Copied keys from Ctl-Meta to Super in %S: %d" map (length actions))
   do (push map dei--super-reflected-keymaps)))

(defun dei-define-alt-like-meta-everywhere ()
  "Duplicate all Meta bindings to exist also on Alt.
Appropriate on `dei-keymap-found-hook'.

Useful in certain environments, such as inside UserLand on an
Android tablet with a Mac keyboard, where the Option key emits A-
instead of M-.

May also interest people looking to break with ESC behaving like
a sticky Meta.  ESC isn't a sticky Alt, so you could safely
re-bind it.  The benefit is unclear though: you'd be able to use
ESC as another function key in the TTY, but you'd have to set up
the console to emit Alt codes instead of Meta to benefit, and a
console capable of such would also be capable of simply emitting
F13 in place of ESC, which seems easier."
  (cl-loop
   for map in (-difference dei--known-keymaps dei--super-reflected-keymaps)
   as actions = (dei--how-define-a-like-b-in-keymap "A-" "M-" map)
   when actions do
   (dei-remap-actions-execute actions)
   (message "Copied keys from Meta to Alt in %S: %d" map (length actions))
   do (push map dei--super-reflected-keymaps)))

;; Experimental
;; FIXME: Find a way to let user hold off on binding C-i/C-m until after
;;        keymap-found-hook has triggered this function on the given keymap.
;; TODO: Also take care of C-M-m, C-H-m, C-s-m, C-S-m, C-H-M-S-s-m.
(defun dei--protect-ret-and-tab (map)
  "In MAP, look for control character representations of C-m and
C-i, and duplicate their bindings to the function keys <return>
and <tab>.  This permits you to bind C-m and C-i to other
commands under GUI Emacs without clobbering the Return and Tab
keys' behavior."
  (cl-loop for key being the key-seqs of map
           with retkeys
           with tabkeys
           if (or (string-search "C-m" key)
                  (string-search "RET" key))
           collect key into retkeys
           else if (or (string-search "C-i" key)
                       (string-search "TAB" key))
           collect key into tabkeys
           finally do
           (progn
             (cl-loop for retkey in retkeys
                      do (define-key map
                           (kbd (string-replace
                                 "C-m" "<return>" (string-replace
                                                   "RET" "<return>" retkey)))
                           (lookup-key map key)))
             (cl-loop for tabkey in tabkeys
                      do (define-key map
                           (kbd (string-replace
                                 "C-i" "<tab>" (string-replace
                                                "TAB" "<tab>" tabkey)))
                           (lookup-key map key))))))


;;; Homogenizing

(defcustom dei-homogenizing-winners '()
  "Alist of keys that always win the homogenizing battle.
Normally, the behavior of a chord-once sequence such as C-x k e
will be kept as is, and cloned to the perma-chord sequence
C-x C-k C-e, overwriting any binding there, so they both do what
the former always did.

If this list contains the member (\"C-x C-k C-e\"), the opposite
will happen in that particular case, overwriting C-x k e.

Each item in the list has the format (KEY-OR-COMMAND . KEYMAP).
See the package readme for how a full list may look.

KEY-OR-COMMAND can be either a `kbd'-compatible key description
or a symbol assumed to refer to a command.  In the event that you
add e.g. both (\"C-x C-f\") and (set-fill-column -- normally a
binding of C-x f -- to the list, then the first item wins.
If (\"C-x C-f\") was first, then C-x f will be bound to find-file.

If KEYMAP is nil, make KEY-OR-COMMAND win in whichever keymap
where it is found.  If non-nil, KEYMAP should be a major or minor
mode map.  It will likely have no effect if it is a so-called
named prefix command such as Control-X-prefix or
kmacro-keymap (you can find these with `describe-function',
whereas you can't find org-mode-map, as that's a proper mode
map)."
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
For example: C-x C-v is bound to a simple command by default, so if
you attempt to bind C-x C-v C-x to a command, you get an error.
So here we check for that.

If KEYDESC is for example C-x C-v C-x, return non-nil if either
C-x or C-x C-v are bound to a command.  If both of them are bound
to either nothing or a prefix map, it's okay, so return nil.

Does not additionally check that KEYDESC is not itself a prefix
map with children bound: that's another thing that can make KEYDESC
unbindable.

KEYMAP is the keymap in which to look."
  (let ((steps (split-string keydesc " "))
        (ret nil))
    (when (> (length steps) 1)
      ;; TODO: don't use dotimes, but some sort of "until" pattern.  This
      ;; function is very un-Lispy right now.  You can tell, because it takes
      ;; time to understand wtf it's doing.
      (dotimes (i (- (length steps) 1))
        (let ((subseq (string-join (-take (1+ i) steps) " ")))
          (when (lookup-key-ignore-too-long keymap (kbd subseq)))
            (push subseq ret)))
      (car ret))))
;; (dei--key-seq-has-non-prefix-in-prefix global-map "C-x C-v C-x")

(defun dei--nightmare-p (keydesc)
  "Non-nil if homogenizing KEYDESC can cause bugs.
This has to do with e.g. C-x \[ being duplicated to C-x C-\[,
which is the same as C-x ESC, which is the same as
C-x M-anything.  You do this, then Magit tries to bind C-x M-g
and complains it's not a prefix key and you can't even load
Magit.  That's a mild issue.  A more silent bug is C-x i becoming
C-x C-i which means it overrides your C-x TAB binding, something
you're blissfully unaware of until you try to use C-x TAB.

The root problem is anachronistic Unix control character
behavior, which Emacs has chosen not to deprecate, for the sake
of functioning inside basic terminal emulators, TTYs and ssh
connections.  We have a clean solution in
`dei-define-super-like-ctl-everywhere' and never typing another
control character in your life.  Alternatively, we have an
untested partial solution in `dei--protect-ret-and-tab'.

If you don't apply the solution, it pays to know this: always
bind the function key <tab> instead of the control character TAB,
<return> instead of RET, <escape> instead of ESC, <linefeed>
instead of LFD, <backspace> instead of DEL, and <delete> instead
of BS.  GUI Emacs always looks up the function key if bound, and
only falls back to the control character if the function key is
unbound.  The function keys may not work on the terminal/TTY, but
neither do Super, Hyper or many other niceties, and I recommend
just using chemacs to run a barebone Emacs for the odd time
you're on the TTY.

While it is possible to rescue C-i and C-m from the cold dead
hands of Unix, you cannot ever use C-\[ as anything other than an
ESC shorthand.  As for C-g, the problem isn't Unix, only a
hardcoded default in Emacs, and I deem it's not fully rescuable
without a patchset that lets you decide via a command-line flag
or Xresource, which key should act as keyboard-quit instead of
C-g.

To sum up, we return non-nil if the key sequence starts with
Control and involves any of \[, m, i, or g anywhere in the
sequence.  So even the sequence C-h g is a nightmare key:
homogenizing it means binding C-h C-g to the same, creating a
situation when C-g is not available to do `keyboard-quit'."
  (declare (pure t) (side-effect-free t))
  (or (and (string-prefix-p "C-" keydesc)
           (string-match-p (rx (any "[" "m" "i" "g")) keydesc))
      ;; this bit is unnecessary because `dei--ignore-keys-regexp' filters them out
      ;; (string-match-p (eval-when-compile (regexp-opt "ESC" "TAB" "RET")) keydesc)
      ))

;; NOTE: must return either nil or a list
(defun dei--how-homogenize-key-in-keymap (this-key keymap)
  "In KEYMAP, homogenize THIS-KEY.
See `dei-homogenizing-winners' for explanation.

Actually just pushes an action onto `dei--remap-actions', which
you can preview with \\[dei-remap-actions-preview] and execute
with \\[dei-remap-actions-execute]."
  (unless (stringp this-key)
    (error "String not passed: %s" this-key))
  (and
   (not (dei--key-seq-steps=1 this-key)) ;; nothing to homogenize if length 1
   (not (dei--nightmare-p this-key))
   (dei--key-starts-with-modifier this-key)
   (let* ((raw-map (dei--raw-keymap keymap))
          (this-cmd (lookup-key-ignore-too-long raw-map (kbd this-key))))
      ;; REVIEW: what do if this-cmd is another keymap?
     (when (functionp this-cmd)
        (let* (;; NOTE: we assume we get passed no "bastard sequences",
               ;; so we don't bother to ensure the alternative is a chordonce.
               (this-is-permachord (dei--key-seq-is-permachord this-key))
               (permachord-key
                (if this-is-permachord
                    this-key
                  (dei--ensure-permachord this-key)))
               (permachord-cmd
                (if this-is-permachord
                    this-cmd
                  (lookup-key-ignore-too-long raw-map (kbd permachord-key))))
               (chordonce-key
                (if this-is-permachord
                    (dei--ensure-chordonce this-key)
                  this-key))
               (chordonce-cmd
                (if this-is-permachord
                    (lookup-key-ignore-too-long raw-map (kbd chordonce-key))
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
               ;; Fortunately (equal widget-global-map global-map) returns t, so
               ;; user can refer to global-map in `dei-homogenizing-winners'...
               (winners-for-this-keymap (->> dei-homogenizing-winners
                                             (-filter #'cdr)
                                             (--filter (equal keymap (cdr it)))
                                             (-map #'car))))

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
                                     (assoc this-key dei--remap-record))))
            nil)

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
                   (warn "Found a contradiction in dei-homogenizing-winners.")
                   nil)

                  ((or (member sibling-keydesc winners-for-this-keymap)
                       (member sibling-cmd winners-for-this-keymap))
                   (list this-key
                         sibling-cmd
                         keymap
                         "Homogenize: winning sibling overwrites this"
                         this-cmd))

                  ((or (member this-key winners-for-this-keymap)
                       (member this-cmd winners-for-this-keymap))
                   (list sibling-keydesc
                         this-cmd
                         keymap
                         "Homogenize: preset winner overwrites sibling"
                         sibling-cmd))

                  ((< 1 (length
                         (-non-nil
                          (list (member sibling-keydesc winners)
                                (member sibling-cmd winners)
                                (member this-key winners)
                                (member this-cmd winners)))))
                   ;; Leave it on the user to fix this mess.
                   (warn "Found a contradiction in dei-homogenizing-winners.")
                   nil)

                  ((or (member sibling-keydesc winners)
                       (member sibling-cmd winners))
                   (list this-key
                         sibling-cmd
                         keymap
                         "Homogenize: winning sibling overwrites this"
                         this-cmd))

                  ((or (member this-key winners)
                       (member this-cmd winners))
                   (list sibling-keydesc
                         this-cmd
                         keymap
                         "Homogenize: preset winner overwrites sibling"
                         sibling-cmd))

                  ;; Neither key and neither command is rigged to win, so take
                  ;; the default action.  (Back when we had the boolean
                  ;; dei-permachord-wins-homogenizing, this was the only place
                  ;; it'd apply)
                  (t
                   (list permachord-key
                         chordonce-cmd
                         keymap
                         "Homogenize: chord-once overwrites perma-chord"
                         permachord-cmd))))

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
            (list sibling-keydesc
                  this-cmd
                  keymap
                  "Homogenize: clone to unbound sibling"
                  sibling-cmd))
           ;; Default.  This will also, via dei-remap-actions-execute, unbind the
           ;; key seqs that would've blocked us from proceeding.
           (t
            (list permachord-key
                  chordonce-cmd
                  keymap
                  "Homogenize: chord-once overwrites perma-chord"
                  permachord-cmd))))))))

(defun dei--how-homogenize-keymap (map)
  "Homogenize most of keymap MAP."
  (cl-loop
   with cleaned = (member map dei--cleaned-maps)
   for key in (dei--unnest-keymap map nil `(,(unless cleaned
                                               #'dei--key-is-illegal)))
   as action = (dei--how-homogenize-key-in-keymap key map)
   when action collect action))

(defvar dei--homogenized-keymaps nil)

(defun dei-homogenize-all-keymaps ()
  "Homogenize the keymaps newly seen since last call."
  (cl-loop
   for map in (-difference dei--known-keymaps dei--homogenized-keymaps)
   as actions = (dei--how-homogenize-keymap map)
   as overwritten = (cl-loop for action in actions
                             when (string-search "overwrite" (nth 3 action))
                             count action)
   do
   (when actions
     (dei-remap-actions-execute actions)
     (message "Homogenized %S: %d new keys, %d overwrites"
              map
              (- (length actions) overwritten)
              overwritten))
   (push map dei--homogenized-keymaps)))

(provide 'deianira-mass-remap)

;;; deianira-mass-remap.el ends here
