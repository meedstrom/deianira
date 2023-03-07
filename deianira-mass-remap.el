;;; deianira-mass-remap.el --- Systematically rebind keys -*- lexical-binding: t -*-

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

;;; Commentary:

;; Will probably be spun out as a standalone package called mass-keybind or
;; something.

;; Setup goes like this:

;; (require 'deianira-mass-remap)
;; (add-hook 'window-buffer-change-functions #'dei-record-keymap-maybe -70)
;; (add-hook 'dei-keymap-found-hook #'dei-define-super-like-ctl-everywhere)
;; (add-hook 'dei-keymap-found-hook #'dei-homogenize-all-keymaps))

;;; Code:

(require 'deianira-lib)
(require 'dash)
(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'help-fns)) ;; help-fns-find-keymap-name
(eval-when-compile (require 'map)) ;; map-let
(eval-when-compile (require 'subr-x)) ;; string-join


;;; Lib

(defun dei--key-seq-has-non-prefix-in-prefix (keymap keydesc)
  "Is any prefix of KEYDESC bound to a command?
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

(defun dei--root-modifier-chunk (keydesc)
  "Return the first modifier chunk.
For example, if KEYDESC is C-M-x C-S-RET, return the substring
\"C-M-\".  If there are no no modifiers on the first key, return
nil."
  (declare (pure t) (side-effect-free t))
  (let* ((first-item (car (split-string keydesc " ")))
         (last-mod-pos (save-match-data
                         (string-match (rx bol (* (regexp dei--modifier-regexp)))
                                       first-item)
                         (match-end 0))))
    (when (not (zerop last-mod-pos))
      (substring first-item 0 last-mod-pos))))
;; (dei--root-modifier-chunk "C-M-x C-S-RET")
;; (dei--root-modifier-chunk "x C-S-RET")

(defun dei--permachord-p (keydesc)
  "Non-nil if KEYDESC can be described as permachord.
Be permissive towards multi-chords: simply determine modifiers
from the first step and see if they're present on every step.
Also return nil if there's an additional modifier anywhere.

Assumes KEYDESC is a sequence, not a single key."
  (declare (pure t) (side-effect-free t))
  (when (> (length keydesc) 2)
    (let ((rootmod (dei--root-modifier-chunk keydesc)))
      (when rootmod 
        (cl-loop
         for step in (split-string keydesc " ")
         if (or (not (string-prefix-p rootmod step))
                (string-match-p dei--modifier-regexp-safe
                                (substring step (length rootmod))))
         return nil
         else finally return t)))))

(defun dei--ensure-permachord (keydesc)
  "Return KEYDESC as perma-chord.
If it's already that, return it unmodified.

The key sequence KEYDESC must not contain any modifiers that are
not part of the first key in the sequence.  If it satisfies
`dei--is-chordonce' or `dei--permachord-p', or
dissatisfies `dei--key-mixes-modifiers', there'll be no problem."
  (declare (pure t) (side-effect-free t))
  (let ((rootmod (dei--root-modifier-chunk keydesc)))
    (if rootmod
        (string-join
         (cl-loop for step in (split-string keydesc " ")
                  if (string-prefix-p rootmod step)
                  collect step
                  and do (when (string-match-p dei--modifier-regexp-safe
                                               (substring step (length rootmod)))
                           (error "Key contains other modifiers: %s" keydesc))
                  else collect (concat rootmod step)
                  and do (when (string-match-p dei--modifier-regexp-safe step)
                           (error "Key contains other modifiers: %s" keydesc)))
         " ")
      (warn "dei--ensure-permachord probably shouldn't be called on: %s"
            keydesc)
      keydesc)))

(defun dei--ensure-chordonce (keydesc)
  "Strip chords from most of key sequence KEYDESC.
Leave alone the first step of the key sequence.  Technically,
since we don't check the first step, the resulting sequence could
be entirely unchorded."
  (declare (pure t) (side-effect-free t))
  (let ((steps (split-string keydesc " ")))
    (string-join (cons (car steps)
                       (-map #'dei--get-leaf (cdr steps)))
                 " ")))


;;; Basics

(defcustom dei-mass-remap-debug-level 0
  "Verbosity of debug.  0, 1, or 2."
  :type 'integer
  :group 'deianira)

(defvar dei--remap-record nil
  "Record of work done.")

(defvar dei--remap-actions nil
  "List of actions to pass to `define-key'.")

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

(defun dei--pretty-print-def (def)
  "Return a string that tries to say what DEF refers to.
DEF should be a key definition such as that returned by
`lookup-key'; most of the time, it's a command, and then this
function simply returns its name."
  (cond ((symbolp def)
         (symbol-name def))
        ((keymapp def)
         (let ((named (help-fns-find-keymap-name def)))
           (if named
               (symbol-name named)
             "(some sub-keymap)")))
        ((functionp def)
         "Anonymous lambda")
        ((listp def)
         "Likely a menu item")
        ((stringp def)
         def)
        (t
         (error "Unable to identify: %s" def))))

(defun dei-list-remaps ()
  "List the key-bindings made so far by deianira-mass-remap."
  (interactive)
  (let ((buf (get-buffer-create "*Deianira remaps*")))
    (with-current-buffer buf
      (setq tabulated-list-entries nil)
      (dei-list-remaps-mode)
      (cl-loop
       for item in dei--remap-record
       do (map-let ((:keydesc keydesc) (:cmd cmd) (:map map) (:reason reason) (:olddef oldcmd)) item
            (let ((cmd-string (dei--pretty-print-def cmd))
                  (oldcmd-string
                   (if oldcmd
                       (concat "(was " (dei--pretty-print-def oldcmd) ")")
                     "")))
              (push (list (sxhash item)
                          (vector (symbol-name map)
                                  reason
                                  keydesc
                                  cmd-string
                                  oldcmd-string))
                    tabulated-list-entries))))
      (tabulated-list-print)
      (display-buffer buf))))

;; TODO: also copy any associated repeat-map...
(defun dei-remap-actions-execute (actions)
  "Carry out remaps specified by ACTIONS."
  (let ((emacs29 (version<= "29" emacs-version)))
    (while actions
      (let ((action (pop actions)))
        (if (member action dei--remap-record)
            (when (> dei-mass-remap-debug-level 1)
              (message "Mass-remap already took this action: %S" action))
          (push action dei--remap-record)
          (map-let ((:keydesc keydesc) (:cmd cmd) (:map map)) action
            (let* ((raw-map (dei--raw-keymap map)))
              (when-let* ((conflict-prefix (dei--key-seq-has-non-prefix-in-prefix
                                            raw-map keydesc))
                          (conflict-def (lookup-key-ignore-too-long
                                         raw-map (kbd conflict-prefix))))
                (unless (keymapp conflict-def)
                  ;; If the prefix is bound and it's not to a keymap, unbind the
                  ;; prefix so we'll be allowed to bind our key. (prevent error
                  ;; "Key sequence starts with non-prefix key")
                  (apply #'define-key raw-map (kbd conflict-prefix) nil emacs29)))
              (define-key raw-map (kbd keydesc) cmd))))))))

(defun dei-remap-revert ()
  "Experimental command to undo all remaps made.
It's recommended to just restart Emacs, but this might work.

It won't restore everything: it's likely a few prefix keys
were unbound and will stay unbound."
  (interactive)
  (cl-loop
   for item in dei--remap-record
   ;; Alternative expression
   ;; do (cl-destructuring-bind (&key keydesc map olddef &allow-other-keys) item
   ;;      (define-key (dei--raw-keymap map) keydesc olddef))
   do (map-let ((:keydesc keydesc) (:map map) (:olddef olddef)) item
        (define-key (dei--raw-keymap map) keydesc olddef))
   finally do
   (setq dei--remap-record nil)
   (setq dei--homogenized-keymaps nil)
   (setq dei--super-reflected-keymaps nil)
   (setq dei--alt-reflected-keymaps nil)))

(defcustom dei-keymap-found-hook nil
  "Run after adding one or more keymaps to `dei--known-keymaps'.
See `dei-record-keymap-maybe', which triggers this hook.

You may be interested in hooking some of these functions:

- `dei-homogenize-all-keymaps'
- `dei-define-super-like-ctl'
- `dei-define-super-like-ctlmeta'
- `dei-define-super-like-meta'
- `dei-define-alt-like-ctl'
- `dei-define-alt-like-ctlmeta'
- `dei-define-alt-like-meta'
- `dei-define-hyper-like-ctl'
- `dei-define-hyper-like-ctlmeta'
- `dei-define-hyper-like-meta'
- `dei-define-metasuper-like-ctlmeta'"
  :type 'hook
  :group 'deianira)

(defvar dei--known-keymaps '(global-map)
  "List of named keymaps seen active.
This typically gets populated (by `dei-record-keymap-maybe') with
just mode maps, rarely (never?) those used as transient maps and never
so-called prefix commands like `Control-X-prefix', nor the category
of sub-keymaps like `ctl-x-map' or `help-map.'")

(defvar dei--known-keymap-composites nil
  "List of unique keymap composites seen active.
These are identified by their hashes; each one was a
product of (sxhash (current-active-maps)), called in different
places and times.")

(defun dei-record-keymap-maybe (&optional _)
  "If Emacs has seen new keymaps, record them in a variable.
This simply checks the output of `current-active-maps' and adds
to `dei--known-keymaps' anything not already added.  Every time
we find one or more new keymaps, trigger `dei-keymap-found-hook'.

Suitable to hook on `window-buffer-change-functions' like this:

\(add-hook 'window-buffer-change-functions #'dei-record-keymap-maybe)"
  (let* ((maps (current-active-maps))
         (composite-hash (abs (sxhash maps))))
    ;; Make sure we only iterate the expensive `help-fns-find-keymap-name' once
    ;; for this keymap composite.
    (unless (member composite-hash dei--known-keymap-composites)
      (push composite-hash dei--known-keymap-composites)
      (let* ((named-maps (-uniq (-keep #'help-fns-find-keymap-name maps)))
             (new-maps (-difference named-maps dei--known-keymaps)))
        (setq new-maps (remove 'context-menu-mode-map new-maps))
        ;; For whatever reason, (help-fns-find-keymap-name global-map) returns
        ;; `widget-global-map'.  Try it!  Fortunately,
        ;; (equal widget-global-map global-map) returns t, so it doesn't matter
        ;; for most purposes, as edits to the raw value of one also affects the
        ;; raw value of the other (keymap variables are weird that way).
        ;; However, where we want to identify the keymap by e.g. membership in a
        ;; list or with `equal', I expect it to take much more CPU time to
        ;; compare their raw values instead of simply their symbols.  The user
        ;; is likely to set options that refer to `global-map'.  Therefore,
        ;; ensure we use that name everywhere by prepopulating
        ;; `dei--known-keymaps' and removing `widget-global-map' in this step.
        ;; As a bonus, user won't need to see and be puzzled by
        ;; `widget-global-map' in M-x dei-list-remaps.  These shenanigans
        ;; wouldn't be necessary if keymap values contained their symbol names
        ;; (and we could eliminate the function `help-fns-find-keymap-name'),
        ;; which could be a suggestion for upstream.
        (setq new-maps (remove 'widget-global-map new-maps))

        ;; Rare situation, and maybe it's only iedit that has this hack, but the
        ;; default value of `iedit-occurrence-keymap' is the symbol
        ;; `iedit-occurrence-keymap-default', and we should work strictly with
        ;; the latter.  I assume we should do likewise all similar cases, so
        ;; just recursively evaluate all such "indirect variables".  Especially
        ;; in that example, it's appropriate since the former is a buffer-local
        ;; variable that sometimes actually evaluates to a (buffer-local) keymap
        ;; instead of a symbol, which is why it's picked up by
        ;; `current-active-maps' even though some of the time it isn't even a
        ;; keymap.  Were Elisp statically typed, this wouldn't ever happen.
        (setq new-maps
              (cl-loop for map in new-maps
                       collect (cl-loop
                                until (keymapp (default-value map))
                                do (setf map (default-value map))
                                finally return map)))
        ;; After the above de-hack, we must re-check.
        (setq new-maps (-difference new-maps dei--known-keymaps))

        (when new-maps
          (setq dei--known-keymaps (append new-maps dei--known-keymaps))
          (run-hooks 'dei-keymap-found-hook))))))


;;; Cleaning
;; Unused for now.  No point to purging ugly key bindings except that it
;; declutters which-key popups and can make Deianira build hydras a bit
;; faster.

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

(defun dei--how-define-a-like-b-in-keymap (recipient-mod donor-mod map)
  "Return actions needed to clone one set of keys to another set.
Inside keymap MAP, take all keys and key sequences that contain
DONOR-MOD \(a substring such as \"C-\"\), replace the substring
wherever it occurs in favor of RECIPIENT-MOD \(a substring such
as \"H-\"\), and assign them to the same commands.

Key sequences that already contain RECIPIENT-MOD are ignored,
even if they also contain DONOR-MOD."
  (cl-loop
   with actions = nil
   with case-fold-search = nil
   with reason = (concat "Define " recipient-mod " like " donor-mod)
   with raw-map = (dei--raw-keymap map)
   for vec being the key-seqs of raw-map using (key-bindings cmd)
   as key = (key-description vec)
   when (and cmd
             (string-search donor-mod key)
             (not (string-search recipient-mod key)))
   do (let ((recipient (string-replace donor-mod recipient-mod key)))
        (if (lookup-key-ignore-too-long raw-map (kbd recipient))
            (and (> dei-mass-remap-debug-level 0)
                 (message "User bound key, leaving it alone: %s in %S" recipient map))
          (push (list :keydesc recipient
                      :cmd cmd
                      :map map
                      :reason reason
                      :olddef nil) actions)))
   finally return actions))

(defvar dei--reflected-maps-per-mod nil)

(defun dei--define-a-like-b-everywhere (recipient-mod donor-mod)
  "Copy all bindings starting with DONOR-MOD to RECIPIENT-MOD."
  (cl-loop
   for map in (-difference dei--known-keymaps
                           (alist-get recipient-mod dei--reflected-maps-per-mod))
   as start = (current-time)
   as actions = (dei--how-define-a-like-b-in-keymap recipient-mod donor-mod map)
   when actions do
   (dei-remap-actions-execute actions)
   (when (> dei-mass-remap-debug-level 0)
     (message
      "(In %.3fs) Copied keys from %s to %s in %S: %d"
      (float-time (time-since start))
      donor-mod
      recipient-mod
      map
      (length actions)))
   do (push map (alist-get recipient-mod dei--reflected-maps-per-mod))))

(defun dei-define-super-like-ctl ()
  "Duplicate all Control bindings to exist also on Super."
  (dei--define-a-like-b-everywhere "s-" "C-"))

(defun dei-define-super-like-ctlmeta ()
  "Duplicate all Control-Meta bindings to exist also on Super."
  (dei--define-a-like-b-everywhere "s-" "C-M-"))

(defun dei-define-super-like-meta ()
  "Duplicate all Meta bindings to exist also on Super."
  (dei--define-a-like-b-everywhere "s-" "M-"))

(defun dei-define-hyper-like-ctl ()
  "Duplicate all Control bindings to exist also on Hyper."
  (dei--define-a-like-b-everywhere "H-" "C-"))

(defun dei-define-hyper-like-ctlmeta ()
  "Duplicate all Control-Meta bindings to exist also on Hyper."
  (dei--define-a-like-b-everywhere "H-" "C-M-"))

(defun dei-define-hyper-like-meta ()
  "Duplicate all Meta bindings to exist also on Hyper."
  (dei--define-a-like-b-everywhere "H-" "M-"))

(defun dei-define-alt-like-ctl ()
  "Duplicate all Control bindings to exist also on Alt."
  (dei--define-a-like-b-everywhere "A-" "C-"))

(defun dei-define-alt-like-ctlmeta ()
  "Duplicate all Control-Meta bindings to exist also on Alt."
  (dei--define-a-like-b-everywhere "A-" "C-M-"))

(defun dei-define-alt-like-meta ()
  "Duplicate all Meta bindings to exist also on Alt."
  (dei--define-a-like-b-everywhere "A-" "M-"))

(defun dei-define-metasuper-like-ctlmeta ()
  "Experimental!
Duplicate all Control-Meta bindings to exist also on Meta-Super."
  (dei--define-a-like-b-everywhere "M-s-" "C-M-"))


;;; Lightweight alternative to Super as Ctl: sanitize some control chars

;; Experimental, completely untested!

(defvar dei--tabret-protected-keymaps nil)

(defvar dei--ret-and-tab-bindings nil
  "Alist of bindings to bind after running `dei--protect-ret-and-tab'.
The alist should follow this structure:

((KEYMAP . ((KEY . COMMAND)
            (KEY . COMMAND)
            ...))
 (KEYMAP . ((KEY . COMMAND)
            (KEY . COMMAND)
            ...))
 ...)

After `dei--protect-ret-and-tab' has operated on a given KEYMAP,
it will apply the bindings in the associated sublist, which is
hopefully self-explanatory.")

;; TODO: Also take care of C-M-m, C-H-m, C-s-m, C-S-m, C-H-M-S-s-m...
(defun dei--protect-ret-and-tab ()
  "Experimental.
In keymap MAP, look for bound control character representations
of C-m and C-i, and duplicate their bindings to the function keys
<return> and <tab>.  This permits you to bind C-m and C-i to
other commands under GUI Emacs without clobbering the Return and
Tab keys' behavior.  Although you have to defer binding them by
specifying `dei--ret-and-tab-bindings'."
  (cl-loop
   for map in (-difference dei--tabret-protected-keymaps
                           dei--known-keymaps)
   do
   (let ((raw-map (dei--raw-keymap map)))
     (cl-loop for vec being the key-seqs of raw-map
              as key = (key-description vec)
              with retkeys = nil
              with tabkeys = nil
              if (or (string-search "C-m" key)
                     (string-search "RET" key))
              collect key into retkeys
              else if (or (string-search "C-i" key)
                          (string-search "TAB" key))
              collect key into tabkeys
              finally do
              (cl-loop for retkey in retkeys
                       do (define-key raw-map
                            (kbd (string-replace
                                  "C-m" "<return>" (string-replace
                                                    "RET" "<return>" retkey)))
                            (lookup-key raw-map vec)))
              (cl-loop for tabkey in tabkeys
                       do (define-key raw-map
                            (kbd (string-replace
                                  "C-i" "<tab>" (string-replace
                                                 "TAB" "<tab>" tabkey)))
                            (lookup-key raw-map vec))))
     (when (eq map 'widget-global-map)
       (setq map 'global-map))
     (cl-loop for x in (alist-get map dei--ret-and-tab-bindings)
              do (define-key raw-map (kbd (car x)) (cdr x))))))


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

(defun dei--nightmare-p (keydesc)
  "Non-nil if homogenizing KEYDESC can cause bugs.
This has to do with e.g. C-x \[ being duplicated to C-x C-\[,
which is the same as C-x ESC, which is the same as
C-x M-anything.  You do this, then Magit tries to bind C-x M-g
and complains it's not a prefix key and you can't even load
Magit.  That's a mild issue.  A more silent bug is C-x i becoming
C-x C-i which means it overrides your C-x TAB binding, something
you're blissfully unaware of until you try to use C-x TAB and
have no idea why it isn't bound to what you thought.

The root problem is anachronistic Unix control character
behavior, which Emacs has chosen not to deprecate, for the sake
of functioning inside basic terminal emulators, TTYs and ssh
connections.  We have a clean solution in
`dei-define-super-like-ctl' and never pressing the Control key
again in our lives.  Alternatively, we have an untested partial
solution in `dei--protect-ret-and-tab'.

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

While it is possible to rescue C-i and C-m from the mummified
hands of Unix, you cannot ever use C-\[ as anything other than an
ESC shorthand.  As for C-g, the problem is only a hardcoded
default in Emacs, and I deem it's not fully fixable without a
patchset that lets you decide via \(say) a command-line flag
which key should act as keyboard-quitter instead of C-g.

To sum up, we return non-nil if the key sequence starts with
Control and involves any of \[, m, i, or g anywhere in the
sequence.  So we even count the sequence C-h g as a nightmare
key: homogenizing it means binding C-h C-g to the same, creating
a situation when C-g is not available to do
`abort-recursive-edit'."
  (declare (pure t) (side-effect-free t))
  (or (and (string-prefix-p "C-" keydesc)
           (string-match-p (rx (any "[" "m" "i" "g")) keydesc))
      (string-match-p (eval-when-compile (regexp-opt '("ESC" "TAB" "RET")))
                      keydesc)))

(defcustom dei-all-upcase-letters
  (-map #'char-to-string
        (string-to-list "AÄÂÀÁBCÇDEËÊÈÉFGHIÏÎÌÍJKLMNOÖÔÒÓPQRSTUÜÛÙÚVWXYZØÆÅÞẞ"))
  "List of capital letters, non-exhaustive.
Keys involving one of these will be ignored by
`dei--how-homogenize-key-in-keymap' because Emacs treats
Control-chords as case-insensitive.

In principle, we could permit capitals for other chords than
Control -- file an issue if this interests you."
  :group 'deianira
  :type '(repeat string))

(defconst dei--homogenize-ignore-regexp
  "Regexp matching keys to skip homogenizing.
These include keys such as <help> which simply clutter up the
output of \\[dei-list-remaps]."
  (regexp-opt dei--ignore-keys-irrelevant))

;; NOTE: must return either nil or a list
(defun dei--how-homogenize-key-in-keymap (this-key keymap)
  "Return the action to homogenize THIS-KEY in KEYMAP.
See `dei-homogenizing-winners' for explanation."
  (unless (stringp this-key)
    (error "Expected `kbd'-compatible string: %s" this-key))
  (let* ((raw-map (dei--raw-keymap keymap))
         (this-cmd (lookup-key-ignore-too-long raw-map (kbd this-key)))
         (case-fold-search nil)) ;; hopefully boosts performance
    (when (> dei-mass-remap-debug-level 1)
      (when (and this-cmd (not (functionp this-cmd)) (symbolp this-cmd))
        (message "Found non-function symbol binding: %s" this-cmd))
      (when (keymapp this-cmd)
        (message "Found keymap at: %s in %S" this-key keymap)))
    (and
     this-cmd
     ;; Use symbolp, not functionp, b/c of not-yet-defined commands, e.g. when
     ;; initfiles bind an eshell command globally but eshell hasn't loaded.
     (symbolp this-cmd)
     (not (dei--key-seq-steps=1 this-key)) ;; nothing to homogenize if length 1
     ;; There's no sense to homogenizing e.g. <f1> C-f because you'd
     ;; never do C-<f1> to start the sequence.  As a bonus, this assumption
     ;; simplifies functions like `dei--ensure-permachord'.  Although you could
     ;; make the case that we'd want to homogenize all subsequences after <f1>,
     ;; i.e. ensure <f1> C-f is duplicated to <f1> f, but that's so rare
     ;; we can just offload the work on to the user if it's important to them.
     (dei--key-starts-with-modifier this-key)
     (not (dei--nightmare-p this-key))
     ;; HACK: ignore capital letters because binding C-x C-K also binds
     ;; C-x C-k (think there's a setting to control case sensitivity since Emacs
     ;; 27 or so -- we need to check its value), messing up the expected binding
     ;; there.  The specific example of C-x C-K comes from a Doom Emacs binding
     ;; of C-x K, and the error is caused by the fact that unlike
     ;; other modifiers, control-bindings are always case insensitive! See
     ;; (info "(emacs)Modifier Keys")
     ;; TL;DR: fix pending: If Control, ignore shiftsyms.  For other modifiers,
     ;; check case sensitivity first.
     (not (dei--key-contains dei-all-upcase-letters this-key))
     ;; Drop bastard sequences
     (not (and (dei--key-has-more-than-one-chord this-key)
               (not (dei--permachord-p this-key))))
     (let* (;; NOTE: we filtered out "bastard sequences",
            ;; so we don't bother to ensure the alternative is a chordonce.
            (this-is-permachord (dei--permachord-p this-key))
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
            ;; Here we'd have run into issue fixed at `dei-record-keymap-maybe'.
            (winners-for-this-keymap (->> dei-homogenizing-winners
                                          (-select #'cdr)
                                          (--select (equal keymap (cdr it)))
                                          (-map #'car))))

       (cond
        ;; Simple case: This key or the sibling key has already been dealt
        ;; with.  Then we just no-op.
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
                (list :keydesc this-key
                      :cmd sibling-cmd
                      :map keymap
                      :reason "Homogenize: winning sibling overwrites this"
                      :olddef this-cmd))

               ((or (member this-key winners-for-this-keymap)
                    (member this-cmd winners-for-this-keymap))
                (list :keydesc sibling-keydesc
                      :cmd this-cmd
                      :map keymap
                      :reason "Homogenize: preset winner overwrites sibling"
                      :olddef sibling-cmd))

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
                (list :keydesc this-key
                      :cmd sibling-cmd
                      :map keymap
                      :reason "Homogenize: winning sibling overwrites this"
                      :olddef this-cmd))

               ((or (member this-key winners)
                    (member this-cmd winners))
                (list :keydesc sibling-keydesc
                      :cmd this-cmd
                      :map keymap
                      :reason "Homogenize: preset winner overwrites sibling"
                      :olddef sibling-cmd))

               ;; Neither key and neither command is rigged to win, so take
               ;; the default action.  (Back when we had the boolean
               ;; dei-permachord-wins-homogenizing, this was the only place
               ;; it'd apply)
               (t
                (list :keydesc permachord-key
                      :cmd chordonce-cmd
                      :map keymap
                      :reason "Homogenize: chord-once overwrites perma-chord"
                      :olddef permachord-cmd))))

        ;; Simple case: only one of the two is bound, so just duplicate.  We
        ;; don't need to do it both directions, b/c this invocation is
        ;; operating on this-key which must be the one known to have a
        ;; command.
        ((null sibling-cmd)
         (list :keydesc sibling-keydesc
               :cmd this-cmd
               :map keymap
               :reason "Homogenize: clone to unbound sibling"
               :olddef sibling-cmd))

        ;; Default.
        (t
         (list :keydesc permachord-key
               :cmd chordonce-cmd
               :map keymap
               :reason "Homogenize: chord-once overwrites perma-chord"
               :olddef permachord-cmd)))))))

(defvar dei--homogenized-keymaps nil)

(defun dei--how-homogenize-keymap (map)
  "Homogenize most of keymap MAP."
  (cl-loop
   for vec being the key-seqs of (dei--raw-keymap map)
   as key = (key-description vec)
   ;; REVIEW: consider pre-filtering the keymap with nonessential filters
   ;; like these--we used them before, a legacy from the hydra maker:
   ;; (not (member this-cmd '(self-insert-command
   ;;                         ignore
   ;;                         ignore-event
   ;;                         company-ignore)))
   ;; (not (string-match-p dei--shift-regexp this-key))
   ;; (not (dei--key-contains dei--all-shifted-symbols-list this-key))
   ;; (not (dei--key-contains-multi-chord this-key))
   ;; (not (dei--key-mixes-modifiers this-key))
   as action =
   (unless (string-match-p dei--homogenize-ignore-regexp key)
     (dei--how-homogenize-key-in-keymap key map))
   when action collect action))

(defun dei-homogenize-all-keymaps ()
  "Homogenize the keymaps newly seen since last call."
  (cl-loop
   for map in (-difference dei--known-keymaps dei--homogenized-keymaps)
   as start = (current-time)
   as actions = (dei--how-homogenize-keymap map)
   as overwritten = (cl-loop
                     for action in actions
                     when (string-search "overwrite" (plist-get action :reason))
                     count action)
   when actions do
   (dei-remap-actions-execute actions)
   (when (> dei-mass-remap-debug-level 0)
     (message "(In %.3fs) Homogenized %S: %d new bindings and %d overwrites"
              (float-time (time-since start))
              map
              (- (length actions) overwritten)
              overwritten))
   do (push map dei--homogenized-keymaps)))
;; (dei-homogenize-all-keymaps)

(provide 'deianira-mass-remap)

;;; deianira-mass-remap.el ends here
