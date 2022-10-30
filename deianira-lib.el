(require 'dash)
(require 'hydra)

(defun dei--raw-keymap (map)
  "If MAP is a keymap, return it, and if a symbol, evaluate it."
  (if (keymapp map)
      map
    (if (symbolp map)
        (symbol-value map)
      (error "Not a keymap or keymap name: %s" map))))

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

(defun dei--hydra-active-p ()
  "Return t if a hydra is active and awaiting input."
  (not (null hydra-curr-map)))

(defun dei--slay (&rest args)
  "Slay active hydra and return ARGS."
  (when (dei--hydra-active-p)
    (setq hydra-deactivate t)
    (call-interactively #'hydra-keyboard-quit))
  args)


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

(provide 'deianira-lib)
