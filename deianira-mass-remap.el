(require 'deianira-lib)

(defcustom dei-keymap-found-hook nil
  "Run after adding one or more keymaps to `dei--known-keymaps'.
See `dei-record-keymap-maybe', which runs this hook.

You may be interested in adding some of these functions:

- `dei-homogenize-all-keymaps'

and one, but obviously ONLY ONE, of:

- `dei-define-super-like-ctl-everywhere'
- `dei-define-super-like-ctlmeta-everywhere'"
  :type 'hook
  :group 'deianira)

(defvar dei--known-keymaps '(global-map)
  "List of named keymaps seen while `deianira-mode' was active.")

(defvar dei--known-keymap-composites nil)

(defun dei-record-keymap-maybe (&optional _)
  "If Emacs has seen new keymaps, record them in a variable.
This simply checks the output of `current-active-maps' and adds
to `dei--known-keymaps' anything not already added.  Also trigger
`keymap-found-hook' every time a new keymap is found.

Suitable on `window-buffer-change-functions'."
  (require 'help-fns)
  (let* ((maps (current-active-maps))
         (combn-hash (sxhash maps)))
    ;; Make sure we only iterate the expensive `help-fns-find-keymap-name' once
    ;; for this keymap combination.
    (unless (member combn-hash dei--known-keymap-composites)
      (push combn-hash dei--known-keymap-composites)
      (let* ((named-maps (-uniq (-keep #'help-fns-find-keymap-name maps)))
             (new-maps (-difference named-maps dei--known-keymaps)))
        (setq new-maps (remove 'widget-global-map new-maps))
        (when new-maps
          (setq dei--known-keymaps (append new-maps dei--known-keymaps))
          (run-hooks 'dei-keymap-found-hook))))))

;;; Reflecting one stem in another

(defvar dei--reflect-actions nil
  "List of actions to pass to `define-key'.")

(defvar dei--ctl-reflected-keymaps nil
  "List of keymaps worked on by `dei-define-super-like-ctl-everywhere'.")

(defvar dei--ctlmeta-reflected-keymaps nil
  "List of keymaps worked on by `dei-define-super-like-ctlmeta-everywhere'.")

(defun dei--define-super-like-ctl-in-keymap (map &optional preserve)
  (let ((raw-map (dei--raw-keymap map)))
    (map-keymap
     (lambda (event ctl-definition)
       (let ((key (key-description (vector event))))
         (when (string-search "C-" key)
           (unless (string-search "s-" key)
             (let ((ctl-key key)
                   (superkey (string-replace "C-" "s-" key)))
               (if (lookup-key raw-map (kbd superkey))
                   (message "User bound key, leaving it alone: %s" superkey)
                 (if (keymapp ctl-definition)
                     (let ((new-submap (make-sparse-keymap)))
                       (set-keymap-parent new-submap ctl-definition)
                       (dei--define-super-like-ctl-in-keymap new-submap) ;; Recurse!
                       (push (list map superkey new-submap) dei--reflect-actions))
                   (push (list map superkey ctl-definition) dei--reflect-actions)))
               ;; Eliminates many C- bindings... unbinds C-g, for example.  But does not clean up exwm-mode-map!
               (unless preserve
                 (push (list map ctl-key nil) dei--reflect-actions)))))))
     raw-map)))

(defun dei--define-super-like-ctlmeta-in-keymap (map &optional preserve)
  (let ((raw-map (dei--raw-keymap map)))
    (map-keymap
     (lambda (event cm-def)
       (let ((key (key-description (vector event))))
         (when (string-search "C-M-" key)
           (unless (string-search "s-" key)
             (let ((cm-key key)
                   (superkey (string-replace "C-M-" "s-" key)))
               (if (lookup-key raw-map (kbd superkey))
                   (message "User bound key, leaving it alone: %s" superkey)
                 (if (keymapp cm-def)
                     (let ((new-submap (make-sparse-keymap)))
                       (set-keymap-parent new-submap cm-def)
                       (dei--define-super-like-ctl-in-keymap new-submap) ;; Recurse!
                       (push (list map superkey new-submap) dei--reflect-actions))
                   (push (list map superkey cm-def) dei--reflect-actions)))
               (unless preserve
                 (push (list map cm-key nil) dei--reflect-actions)))))))
     raw-map)))

;; TODO: A minimal alternative.  React to every package that binds C-m or RET
;; (synonyms for the same event), and copy that binding to <return>.  That way,
;; the Return key works as expected even if user then changes C-m to something
;; else.  Ditto for TAB/C-i, copied to <tab>.  However, user has to hold off on
;; binding C-m until on keymap-found-hook, after the function that does this.
(defun dei--protect-ret-and-tab (map)
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
           (cl-loop for retkey in retkeys
                    do
                    (define-key map (kbd (string-replace
                                          "C-m" "<return>" (string-replace
                                                            "RET" "<return>" retkey)))
                      (lookup-key map key)))
           ))

(defun dei-define-super-like-ctl-everywhere ()
  (cl-loop for map in (-difference dei--known-keymaps
                                   dei--ctl-reflected-keymaps)
           do (progn
                (dei--define-super-like-ctl-in-keymap map t)
                (push map dei--ctl-reflected-keymaps))
           finally (dei--reflect-actions-execute)))

(defun dei-define-super-like-ctlmeta-everywhere ()
  (cl-loop for map in (-difference dei--known-keymaps
                                   dei--ctlmeta-reflected-keymaps)
           do (progn
                (dei--define-super-like-ctlmeta-in-keymap map t)
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
                            t)))
           finally (setq dei--reflect-actions nil)))

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
For example: C-x C-v is bound to a simple command by default, so
if you attempt to bind C-x C-v C-x to a command, you get an
error.  So here we check for that.  If KEYDESC is C-x C-v C-x, we
check if either C-x or C-x C-v are bound to anything.

Does not additionally check that KEYDESC is not itself a prefix
map with children bound: that's another thing that can make KEYDESC
unbindable.

KEYMAP is the keymap in which to look."
  (let ((steps (dei--key-seq-split keydesc))
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

(defvar dei--homogenized-keymaps nil)

(defvar dei--remap-record nil
  "Record of work done.")

(defvar dei--remap-actions nil
  "List of actions to pass to `define-key'.")

(defun dei--nightmare-p (keydesc)
  "Non-nil if homogenizing KEYDESC would cause bugs.
This has to do with e.g. C-x \[ becoming C-x C-\[, which is the
same as C-x ESC, which is the same as C-x M-anything.  You do
this, then Magit tries to bind C-x M-g and complains it's not a
prefix key and you can't even load Magit.  That's mild issue.  A
more silent bug is C-x i becoming C-x C-i which goes ahead and
overrides your C-x TAB binding without signaling anything even to
the developer.

The problem is anachronistic Unix control character behavior,
which Emacs has chosen not to deprecate, for the sake of
functioning inside basic terminal emulators.  We have a clean
solution in `dei-define-super-like-ctl-everywhere' and never
typing another control character.

If you don't apply the solution, it pays to know this: always
bind <tab> instead of TAB, <return> instead of RET, and <escape>
instead of ESC.  GUI Emacs always looks up these if bound, and
only falls back to the control character if these function keys
are unbound.  They do not work on the terminal/TTY, but neither
does Super or many other niceties.  This will still not let you
use C-\[ as anything other than an ESC shorthand, but you'll be
able to bind C-m and C-i without clobbering the Tab or Return
keys.  You may still encounter issues with packages either
binding RET/TAB directly, ruining your C-m/C-i, and such packages
are typically not thinking to bind <return>/<tab>, so you have to
add them yourself.

As an additional nicety, we avoid touching key sequences
involving Control and \"g\" so as to prevent clobbering the
meaning of \"C-g\".  Otherwise, homogenizing C-h g binds C-h C-g
to the same, creating a situation when C-g is not available for
`keyboard-quit'."
  (declare (pure t) (side-effect-free t))
  (or (and (string-prefix-p "C-" keydesc)
           (string-match-p (rx (any "[" "m" "i" "g")) keydesc))
      ;; this bit is unnecessary because `dei--ignore-keys-regexp' filters them out
      ;; (string-match-p (eval-when-compile (regexp-opt "ESC" "TAB" "RET")) keydesc)
      ))

;; TODO: Maybe just return the action, and let the caller push onto
;; external variables if they wish.
(defun dei--homogenize-key-in-keymap (this-key keymap)
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
   ;; (not (equal keymap 'widget-global-map))
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
                  ;; Neither key and neither command is rigged to win, so take
                  ;; the default action.  (Back when we had the boolean
                  ;; dei-permachord-wins-homogenizing, this was the only place
                  ;; it'd apply)
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
;; TODO: return how many overridden and how many new bindings
(defun dei--homogenize-keymap (map)
  "Homogenize most of keymap MAP."
  (message "Keys (re)bound: %s"
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
            (let ((cmd-string (if (symbolp cmd)
                                  (symbol-name cmd)
                                (if (keymapp cmd)
                                    (if-let ((named (help-fns-find-keymap-name cmd)))
                                        (symbol-name named)
                                      "(some sub-keymap)")
                                  cmd)))
                  (old-string (if (symbolp old)
                                  (symbol-name old)
                                (if (keymapp old)
                                    (if-let ((named (help-fns-find-keymap-name old)))
                                        (symbol-name named)
                                      "(some sub-keymap)")
                                  old))))
              (insert
               (if (symbolp map)
                   (concat "(" (symbol-name map) ")\t\t ")
                 "")
               hint
               " Bind  " keydesc
               "\tto  " cmd-string
               "\t ... was " old-string)))
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

(defvar dei--remap-revert-list nil)

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
        (push (list map keydesc old-def) dei--remap-revert-list)
        (define-key raw-keymap (kbd keydesc) cmd)))))

;; Experimental
(defun dei-remap-revert ()
  (interactive)
  (cl-loop for x in dei--remap-revert-list
           do (seq-let (map keydesc old-def) x
                (define-key (dei--raw-keymap map) keydesc old-def))))

(provide 'deianira-mass-remap)
