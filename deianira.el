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
;; Package-Requires: ((emacs "26.1") (hydra "0.15.0") (deferred) (dash) (s))

;;; Commentary:

;;; Code:

;; builtin dependencies
(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'ert)
(require 'help)

;; external dependencies
(require 'deferred)
(require 'dash)
(require 's)
(require 'hydra)


;;; Background facts

(defconst dei--modifier-regexp
  (regexp-opt '("A-" "C-" "H-" "M-" "S-" "s-")))

(defconst dei-all-keys-on-keyboard-except-shifted-symbols
  (append
   (split-string
    "`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./"
    "" t)
   (split-string
    "<left> <right> <up> <down> <SPC> <RET> <backspace> <delete>
     TAB <f1> <f2> <f3> <f4> <f5> <f6> <f7> <f8> <f9> <f10> <f11>
     <f12> <print> <insert> <next> <prior> <home> <end>")
   ;; untested
   ;; (split-string
   ;; "DEL <return> <f13> <f14> <f15> <f16> <f17> <f18> <f19> <f20>
   ;; <iso-lefttab> <XF86MonBrightnessUp>") ;; and so on...
   )
  "All keys, except where a held-down Shift is implied.")

(defconst dei--all-shifted-symbols
  (split-string
   "~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?"
   "" t))

(defconst dei-all-keys-on-keyboard
  (append
   dei-all-keys-on-keyboard-except-shifted-symbols
   dei--all-shifted-symbols))

(defun dei--hydra-keys-in-a-list ()
  (split-string dei--hydra-keys "" t))

(defun dei--hydra-keys-nonum ()
    (replace-regexp-in-string (rx num) "" dei--hydra-keys))

(defun dei--hydra-keys-list-nonum ()
  (split-string (replace-regexp-in-string (rx num) "" dei--hydra-keys) "" t))

;; "List of keys like C-a, C-e, M-f, M-g but not C-M-f or M-%."
;; note: it's not quite all of them since it uses dei--hydra-keys
(defun dei--all-duo-chords ()
  (let (chords)
    (mapc (lambda (char)
            (push (concat "C-" (string char)) chords)
            (push (concat "M-" (string char)) chords)
            (push (concat "s-" (string char)) chords))
          dei--hydra-keys)
    chords))

(defvar dei--hydra-keys "1234567890qwertyuiopasdfghjkl;zxcvbnm,./")
(defvar dei--hydra-keys-list (dei--hydra-keys-in-a-list))
(defvar dei--hydra-keys-nonum (dei--hydra-keys-nonum))
(defvar dei--hydra-keys-list-nonum (dei--hydra-keys-list-nonum))
(defvar dei--all-duo-chords (dei--all-duo-chords))


;;;; X11

;; "We've not found a keyboard with more than 35 function keys total."
;; -- /usr/include/X11/keysymdef.h
;; i.e. F35 is the highest F-key defined in upstream keysymdef.h.
;; Neat: I would have thought we need to check xmodmap -pke in case we've been
;; starting emacs many times, but xmodmap will only apply the following rules
;; if they don't already have a location.
(defvar dei-xmodmap-rules
  '(;; necessary for xcape to send them
    "keycode any = F35"
    "keycode any = F34"
    "keycode any = F33"
    "keycode any = F32"
    "keycode any = F31"))

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
            (start-process "xcape" "*xcape*" "nice" "-20" "xcape" "-d" "-e" rules))
      (run-with-named-timer 'dei-xcape-log-cleaner 300 300 #'dei-clean-xcape-log))))

(defun dei-xkbset-enable-sticky-keys ()
  (interactive)
  (when (executable-find "xkbset")
    (start-process "xkbset" (dei--debug-buffer) "xkbset" "sticky" "-twokey" "-latchlock")
    (start-process "xkbset" (dei--debug-buffer) "xkbset" "exp" "=sticky")))


;;; Enforce tidy


;; Generalized flatten-ctl-x
(defun dei-restem-all-leaves (here lower-stem upper-stem)
  "Duplicate bindings on UPPER-STEM to also exist on LOWER-STEM.
Where they conflict, LOWER-STEM is overridden. The naming is
inspired by overlayfs, which do a similar thing with filesystem
mounts. HERE refers to the keymap such as global-map.  Typical
use: (dei-restem-all-leaves global-map \"C-x \" \"C-x C-\")"
  (dolist (leaf (dei--hydra-keys-in-a-list))
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
  (define-key key-translation-map (kbd "s-g") (kbd "C-g"))
  )


;;; Keyboard scanning

;; Think this function is hairy and unnecessary? It's this way because the C
;; function `describe-buffer-bindings' is the only way to get this information
;; efficiently. Inspired by which-key--get-current-bindings. Thanks!
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



;;; User settings

(defun dei--colwidth ()
  (or dei-colwidth-override
      (let ((optimal (- (round (frame-parameter nil 'width) 10) 4)))
        (max optimal 8))))

(defvar dei-colwidth-override nil
  "An integer for the width of hydra hints. If nil, figure it out
from the frame width.")

(defvar dei--colwidth (dei--colwidth)
  "Width of hydra hint.
You should prefer to call the function `dei--colwidth' in case of
frame or font changes.")

(defvar dei-quitters '(;;"C-q"
                       "C-s" "C-g" "s-s" "s-g" "C-u" "M-x" "M-u" "s-u" "C-2"
                       "C-c c"))

(defvar dei-noquitters '("C-2 t" "C-2 n" "C-2 p" "C-x ;" "C-x x" "C-x q"
                           "C-x h" "C-x u" "C-x i" "C-x p" "C-x l" "C-x 1"
                           "C-x 2" "C-x 3" "C-x 0" "C-c c"
                           "C-c C-c" ;;testing
                           ))

(defvar dei-exit-by-default nil)


;;; Keydesc handling
;; If it's not a pure function, it probably doesn't belong here.

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

(defun dei--of-interest-p (cmd)
  "Return t if CMD is worth carrying over to another key.
It does not fail if CMD is a keymap, check that separately."
  (declare (pure t) (side-effect-free t))
  (not (member cmd '(self-insert-command
                     nil
                     ignore
                     ignore-event
                     company-ignore))))

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

(defun dei--valid-keydesc (keydesc)
  "Check that KEYDESC is not a dangling stem.
I.e. it's something you'd pass to `kbd'. If true, return KEYDESC
unmodified, else return nil."
  (declare (pure t) (side-effect-free t))
  (when (or (string-match-p (rx "--" eol) keydesc)
            (string-match-p (rx " -" eol) keydesc)
            (string-match-p (rx (not (any "- ")) eol) keydesc))
    keydesc))

(defun dei-dub-from-key (keydesc)
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

(defun dei--corresponding-hydra (stem leaf)
  (declare (pure t) (side-effect-free t))
  (intern (concat
           (dei-dub-from-key (dei--normalize (concat stem leaf)))
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

;; IDK if it fails well
(defun dei--stem-to-keydesc (stem)
  (declare (pure t) (side-effect-free t))
  (let ((keydesc (substring stem 0 -1)))
    (when (dei--valid-keydesc keydesc)
      keydesc)))

;; Assumption: Pre-normalized keydesc
(defun dei--get-stem (keydesc)
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (concat (regexp-quote (dei--get-leaf keydesc)) "$")
                            ""
                            keydesc))

(defun dei--corresponding-hydra-from-stem (stem)
  (declare (pure t) (side-effect-free t))
  (intern (concat (dei-dub-from-key stem) "/body")))

;; Could probably be more clearly programmed
;; Maybe toatlly uncessary
(defun dei--parent-stem2 (keydesc)
  "Drop leaf of KEYDESC, return a valid keydesc.
Assume that there are no modifiers beyond the root. If there are, IDK."
  (declare (pure t) (side-effect-free t))
  (let* ((stem (dei--get-stem keydesc))
         (stem-trimmed (substring stem 0 -1)))
    (if (dei--valid-keydesc stem-trimmed)
        (if (s-ends-with-p "-" stem-trimmed)
            stem-trimmed ;; probably C-, but can be C-c C-.
          (if (s-matches-p dei--modifier-regexp stem)
              stem))
      (if (s-matches-p dei--modifier-regexp keydesc)
          nil ;; just exit
        (warn "deianira: Code should not have landed here")))))

(defun dei--parent-stem (stem)
  (declare (pure t) (side-effect-free t))
  (dei--parent-stem2 (dei--stem-to-keydesc stem)))

(defun dei--parent-hydra (stem)
  (declare (pure t) (side-effect-free t))
  (if (dei--key-seq-steps=1 stem)
      nil ;; exit
    (dei--corresponding-hydra-from-stem (dei--parent-stem stem))))


;;; Library

(defvar dei-debug nil
  "A buffer name or nil.")

(defun dei-echo (x)
  (when dei-debug
    (print x (dei--debug-buffer))))

(defun dei--debug-buffer ()
  (when dei-debug
    (let ((buf (get-buffer-create dei-debug)))
      (with-current-buffer buf
        (setq-local truncate-lines t)
        buf))))

;; TODO: Difference from dei--corresponding-hydra?
;; REVIEW: Can this be tested? Need Buttercup?
(defun dei--subhydra-or-nil (stem leaf)
  "Return the hydra body in question."
  (if-let ((x (cdr (assoc (concat stem leaf) dei--live-hydras))))
      (intern (concat x "/body"))))

;; REVIEW: Write test for it with a key-simulator
(defun dei-universal-arg (arg)
  (interactive "P")
  (prefix-command-preserve-state)
  (call-interactively
   (intern (concat
            (substring (symbol-name hydra-curr-body-fn) 0 -5) ;; chop "/body"
            "-nonum/body")))
  (hydra--universal-argument arg))

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
         (dei-control/body))
        ((string-match-p "^M-" keydesc)
         (dei-meta/body))
        ((string-match-p "^s-" keydesc)
         (dei-super/body))))

;; TODO: delete
(defun dei-cc-cc ()
  (interactive)
  ;; (call-interactively (key-binding (kbd "C-c c"))) ;; if flattened
  (call-interactively (key-binding (kbd "C-c C-c")))
  (dei-control/body))

(defun dei-cmd (stem leaf)
  (key-binding (kbd (concat stem leaf))))

(defun dei--is-unbound (stem leaf)
  (null (dei-cmd stem leaf)))

(defun dei--is-bound (stem leaf)
  (not (dei--is-unbound stem leaf)))


;;; Hydra specifier

(defun dei-head-cmd (stem leaf)
  (cond ((not (dei--of-interest-p (dei-cmd stem leaf)))
         leaf) ;; make a blank spot
        ((keymapp (dei-cmd stem leaf))
         (dei--corresponding-hydra stem leaf))
        ((string= (concat stem leaf) "C-c c")
         #'dei-cc-cc)
        ((string= (concat stem leaf) "C-g") ;; TODO: don't check key, check if the binding is keyboard-quit.
         #'keyboard-quit)
        ((eq #'universal-argument (dei-cmd stem leaf))
         #'dei-universal-arg)
        (t `(call-interactively (key-binding (kbd ,(concat stem leaf)))))))

(defun dei-head-hint (stem leaf)
  (let* ((sym (or (dei--subhydra-or-nil stem leaf)
                  ;; (dei-is-known-prefix stem leaf)
                  (key-binding (kbd (concat stem leaf)))))
         (name (if (symbolp sym) (symbol-name sym) " ")))
    (if (string= name "nil")
        " "
      (if (> (length name) dei--colwidth)
          (substring name 0 dei--colwidth)
        name))))

(defun dei-head-key (stem leaf)
  "If the given hotkey is bound to a command, return LEAF,
otherwise return a space character. This can be used to
make a visible blank spot in a hydra for hotkeys that are unbound."
  (if (dei--of-interest-p (dei-cmd stem leaf))
      leaf
    " "))

(defun dei-head-exit (stem leaf &optional exit-almost-never-p)
  (cond ((dei--subhydra-or-nil stem leaf) '(:exit t)) ;; important
        ;; ((eq #'universal-argument (dei-cmd stem leaf)) '(:exit nil))
        ((member (concat stem leaf) dei-quitters) '(:exit t))
        ((member (concat stem leaf) dei-noquitters) '(:exit nil))
        ((dei--is-unbound stem leaf) '(:exit t))
        (exit-almost-never-p '(:exit nil))
        (t '()))) ;; defer to hydra's default behavior

(defun dei-head (stem leaf)
  "Return a \"head\" specification, in other words a list in the
form (KEY COMMAND HINT EXIT) as needed in `defhydra'."
  `( ,(dei-head-key stem leaf) ,(dei-head-cmd stem leaf) ,(dei-head-hint stem leaf)
     ,@(dei-head-exit stem leaf)))

(defun dei-head-invisible (stem leaf)
  `( ,(dei-head-key stem leaf) ,(dei-head-cmd stem leaf) nil
     ,@(dei-head-exit stem leaf 'almost-never)))

(defun dei-head-invisible-self-inserting (_stem leaf)
  `( ,leaf self-insert-command nil :exit t))

(defun dei--specify-visible-heads (stem &optional keys)
  (cl-loop for leaf in (or keys dei--hydra-keys-list)
           collect (dei-head stem leaf)))

(defun dei--specify-invisible-heads (stem)
  (append (cl-loop for leaf in '("<left>" "<right>" "<up>" "<down>"
                                 "=" "\\" "'" "`"
                                 "<f5>")  ;; each new key generates quite a lot of heads...
                   collect (dei-head-invisible stem leaf))
          (cl-loop for chord in dei--all-duo-chords
                   collect (dei-head-invisible "" chord))
          (cl-loop for leaf in dei--all-shifted-symbols
                   collect (dei-head-invisible-self-inserting stem leaf))))

(defun dei--specify-extra-heads (stem)
  (declare (pure t) (side-effect-free t))
  (let ((pop-key (cond ((string= "C-" stem) "<f35>")
                       ((string= "M-" stem) "<f34>")
                       ((string= "s-" stem) "<f33>"))))
    (-non-nil (list `("<backspace>" ,(dei--parent-hydra stem)
                      nil :exit t)
                    (when pop-key `(,pop-key nil nil :exit t))))))

;(dei--get-parent "C-x a ")
;(dei--specify-extra-heads "C-x a")
;(dei--corresponding-hydra (dei--parent-stem "C-x a ") "")

(defun dei--call-defhydra (name heads)
  "Create a hydra named NAME with HEADS.
Tip: This is a thin wrapper around `defhydra', the magic happens
when it's called by `dei-generate-hydras-async'."
  (eval `(defhydra ,(intern name)
           (nil nil :columns 10 :exit ,dei-exit-by-default
                ;; :body-pre (dei-generate-hydras-async)
                ;; :body-post (dei-generate-hydras-async)
                )
           ,name
           ,@heads)
        t))


;;; Async worker

(defvar dei--after-scan-bindings-hook nil
  "Things to do after updating `dei--current-bindings'.
Use this to unbind new keys that may have appeared due to a
buffer change or major mode change. You should remove those
records from dei--current-bindings when you do that as we will not
rescan.")

(defvar dei--after-rebuild-hydra-hook nil
  "Hook run after updating hydras to match the local map.")

;; Persistent variables helpful for debugging
(defvar dei--current-bindings nil)
(defvar dei--last-filtered-bindings nil)
(defvar dei--new-or-rebound-keys nil)
(defvar dei--changed-keymaps nil)
(defvar dei--hydra-blueprints nil)
(defvar dei--live-hydras nil)
(defvar dei--defunct-bindings nil)
(defvar dei--defunct-hydras nil)

(defun dei--combined-filter (cell)
  "Filter for rejecting keys as irrelevant to work on.
This function exists because there is a need to go into a list
and look at the `car's, and so we may as well run each filter
while we're at it."
  (declare (pure t) (side-effect-free t))
  (let ((keydesc (car cell)))
    (or ;;(dei--key-contains-ctl keydesc)
     ;; (s-contains-p "backspace" keydesc)
     ;; (s-contains-p "DEL" keydesc)
     ;; (s-contains-p "SPC" keydesc)
     ;; (dei--key-has-more-than-one-modifier keydesc)
     ;; (dei--contains-upcase keydesc)
     ;; (dei--key-contains-multi-chords keydesc)
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
    (--any-p (member it dei--all-shifted-symbols) steps-without-modifier)))

(defun dei--get-relevant-bindings ()
  (->> (dei--current-bindings (rx bol (regexp dei--modifier-regexp))
                              ;; (rx (or "ESC" "C-"))
                              (rx (or "ESC")))
       (-map (lambda (x)
               (cons (if (dei--valid-keydesc (car x))
                         (dei--normalize (car x))
                       (car x))
                     (cdr x))))
       ;; (dei--unbind-illegal-keys)
       (-remove (lambda (x) (or (string= "Prefix Command" (cdr x))
                           (null (intern (cdr x)))
                           (keymapp (intern (cdr x)))))) ;;
       (setq dei--current-bindings) ;; should end up the same as filtered bindings once you run the function twice
       (-remove #'dei--combined-filter)
       (setq dei--current-filtered-bindings)))

(defun dei--unbind-illegal-keys (input)
  (let* ((illegal (seq-filter #'dei--combined-filter input))
         (illegal-keys (seq-map #'car illegal))
         (sorted (seq-sort-by #'dei--key-seq-steps-length #'> illegal-keys)))
    (seq-do (lambda (x)
              (let ((map (help--binding-locus (kbd x) nil)))
                (unless (null map) ;; there's a bug that leaves some keys in `describe-bindings' but not in the apparently active map, and they get a null map. Check dei--current-bindings, C-M-q and C-M-@ are in there
                  (define-key (eval map t) (kbd x) nil))))
            sorted))
  input)

;; NOTE: Visualize a Venn diagram. The defunct and the new are the last and
;;      current minus their intersection (cases where the key's definition
;;      didn't change), which should never be relevant to look at.
(defun dei--set-variables ()
  (when dei-debug (message "Updating variables"))
  (setq dei--defunct-bindings (-difference dei--last-filtered-bindings
                                           dei--current-filtered-bindings))
  (setq dei--new-or-rebound-keys
        (-map #'car (-difference dei--current-filtered-bindings
                                 dei--last-filtered-bindings)))
  ;; Mark hydras for oblivion or redefinition
  (setq dei--defunct-hydras (-intersection dei--live-hydras
                                           (-map #'car dei--defunct-bindings)))
  ;; Unlist said marked hydras so they will be reborn. (Actually, we don't ever
  ;; check this variable, it's now debugging only).
  (setq dei--live-hydras
        (seq-remove (lambda (x) (member x dei--defunct-hydras))
                    dei--live-hydras)))

(defun dei--specify-dire-hydra-pair (stem)
  (list
   (cons (dei-dub-from-key stem)
         (append (dei--specify-visible-heads stem)
                 (dei--specify-invisible-heads stem)
                 (dei--specify-extra-heads stem)))
   ;; For each hydra, a nonum hydra for when universal-arg is active, so the digit arguments work.
   ;; (cons (concat (dei-dub-from-key stem) "-nonum")
   ;;          (append (dei--specify-visible-heads
   ;;                   stem dei--hydra-keys-list-nonum)
   ;;                  (dei--specify-invisible-heads stem)
   ;;                  (dei--specify-extra-heads stem)))
   ))

;; NOTE: See tests in the manual tests file
(defun dei--specify-dire-hydras ()
  (setq dei--hydra-blueprints
        (cl-loop for key in dei--new-or-changed-stems
                 append (dei--specify-dire-hydra-pair key))))

;; The magic spell that runs the entire damn codebase
(defun dei-generate-hydras-async ()
  "Regenerate hydras to match the local map."
  (deferred:$
    (deferred:next
      #'dei--get-relevant-bindings)

    (deferred:nextc it
      (lambda ()
        (run-hooks 'dei--after-scan-bindings-hook)))

    (deferred:nextc it
      #'dei--set-variables)

    (deferred:nextc it
      (lambda ()
        (setq dei--new-or-changed-stems
              (-uniq (mapcar #'dei--get-stem
                             ;; Sort by length to avail the most relevant hydras to the user soonest.
                             (seq-sort-by #'dei--key-seq-steps-length #'< dei--new-or-rebound-keys))))))

    (deferred:nextc it
      #'dei--specify-dire-hydras)

    ;; Re-cache settings in case of changed frame parameters or user options
    (deferred:nextc it
      (lambda ()
        (setq dei--colwidth (dei--colwidth))
        (setq dei--hydra-keys-list (dei--hydra-keys-in-a-list))
        (setq dei--hydra-keys-nonum (dei--hydra-keys-nonum))
        (setq dei--hydra-keys-list-nonum (dei--hydra-keys-list-nonum))
        (setq dei--all-duo-chords (dei--all-duo-chords))))

    (deferred:nextc it
      (lambda ()
        (cl-loop for x in dei--hydra-blueprints
                 do (push (dei--call-defhydra (car x) (cdr x))
                          dei--live-hydras))
        (setq dei--last-filtered-bindings dei--current-filtered-bindings)))

    (deferred:nextc it
      (lambda ()
        (run-hooks 'dei--after-rebuild-hydra-hook)))

    (deferred:nextc it
      #'dei--fix-which-key)

    (deferred:error it
      #'warn)))

;; when I want the package to be more modular, this should sit on a hook
;; NOTE: keymap-based replacements are more performant, see README:
;; https://github.com/justbur/emacs-which-key
;; not that I care
(defun dei--fix-which-key ()
  "Hide keys with repeated modifiers like C-x C-f from which-key.
We do this because it's bound to the same command as C-x f in our
paradigm, so all these keys just crowd the display.

Destructive; overwrites `which-key-replacement-alist' (but does
not change the value as saved in `custom-file') ignoring any
rules already present, because they may not work without enabling
`which-key-allow-multiple-replacements'. Customizations of
which-key are mostly superfluous in our paradigm, so we opt not
to for performance."
  (if (bound-and-true-p dei--current-bindings)
      (->> dei--current-bindings
           (-map #'car)
           (-remove #'dei--key-seq-steps=1)
           (-filter #'dei--key-has-more-than-one-modifier)
           (--map (s-replace "\\" "\\\\" it))
           (--map (cons (cons it nil) t))
           (setq which-key-replacement-alist))
    (message "deianira: Variable dei--current-bindings unexpectedly empty.")))


;;;; Main

;;;###autoload
(define-minor-mode deianira-mode
  "Bind root hydras."
  nil
  " ESM"
  `((,(kbd "<f35>") . dei-C/body)
    (,(kbd "<f34>") . dei-M/body)
    (,(kbd "<f33>") . dei-s/body))
  :global t
  (unless t
    (when deianira-mode

      (add-hook 'window-buffer-change-functions #'dei-generate-hydras-async)

      ;; TEST CODE

      ;; TODO: Make it faster. Ideally run a deferred loop.
      (dei-generate-hydras-async)

      (dei--get-relevant-bindings)
      (dei--get-relevant-bindings)
      (dei--set-variables)
      (setq dei--new-or-changed-stems
            (-uniq (mapcar #'dei--get-stem
                           ;; Sort by length to avail the most relevant hydras to the user soonest.
                           (seq-sort-by #'dei--key-seq-steps-length #'< dei--new-or-rebound-keys))))
      (dei--specify-dire-hydras)

      ;; the unnamed lambda that builds it all
      (cl-loop for x in dei--hydra-blueprints
               do (push (dei--call-defhydra (car x) (cdr x))
                        dei--live-hydras))
      (setq dei--last-filtered-bindings dei--current-filtered-bindings)

      )))

(provide 'deianira)
;;; deianira.el ends here
