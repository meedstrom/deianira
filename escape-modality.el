;;; escape-modality.el --- Modifier-free pseudo-modal input -*- lexical-binding: t; -*-

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
;; Homepage: https://github.com/meedstrom/escape-modality
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


;;;; X11

;; "We've not found a keyboard with more than 35 function keys total."
;; -- /usr/include/X11/keysymdef.h
;; i.e. F35 is the highest F-key defined in upstream keysymdef.h.
;; Neat: I would have thought we need to check xmodmap -pke in case we've been
;; starting emacs many times, but xmodmap will only apply the following rules
;; if they don't already have a location.
(defvar esm-xmodmap-rules
  '(;; necessary for xcape to send them
    "keycode any = F35"
    "keycode any = F34"
    "keycode any = F33"
    "keycode any = F32"
    "keycode any = F31"))

(defvar esm-xcape-rules
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

(defun esm-xmodmap-reload (&optional output-buffer)
  "(Re-)apply the `esm-xmodmap-rules'."
  (interactive)
  (let* ((shell-command-dont-erase-buffer t)
         (rules (string-join esm-xmodmap-rules "' -e '"))
         (cmd (concat "xmodmap -e '" rules "'")))
    (when (executable-find "xmodmap")
      (start-process-shell-command cmd
                                   (or output-buffer (esm--debug-buffer) "*Messages*")
                                   cmd))))

(defvar esm-xcape-process)

(defvar esm-xcape-log-cleaner)

(defun esm-clean-xcape-log ()
  (when (get-buffer "*xcape*")
    (with-current-buffer "*xcape*"
      (delete-region (point-min) (point-max)))))

(defun esm-xcape-reload ()
  "(Re-)start the xcape process."
  (interactive)
  (let ((shell-command-dont-erase-buffer t)
        (rules (string-join esm-xcape-rules ";")))
    (when (executable-find "xcape")
      (and (boundp 'esm-xcape-process)
           (process-live-p esm-xcape-process)
           (kill-process esm-xcape-process))
      (setq esm-xcape-process
            (start-process "xcape" "*xcape*" "nice" "-20" "xcape" "-d" "-e" rules))
      (setq esm-xcape-log-cleaner (run-with-timer 300 300 #'esm-clean-xcape-log)))))

(defun esm-xkbset-enable-sticky-keys ()
  (interactive)
  (when (executable-find "xkbset")
    (start-process "xkbset" (esm--debug-buffer) "xkbset" "sticky" "-twokey")
    (start-process "xkbset" (esm--debug-buffer) "xkbset" "exp" "=sticky")))


;;; Enforce tidy

(defun esm-super-translate-to-ctl-meta ()
  (dolist (key esm-all-keys-on-keyboard)
    (define-key key-translation-map
      (kbd (concat "s-" key)) (kbd (concat "C-" key)))))

;; Generalized flatten-ctl-x
(defun esm-restem-all-leaves (here lower-stem upper-stem)
  "Duplicate bindings on UPPER-STEM to also exist on LOWER-STEM.
Where they conflict, LOWER-STEM is overridden. The naming is
inspired by overlayfs, which do a similar thing with filesystem
mounts. HERE refers to the keymap such as global-map.  Typical
use: (esm-restem-all-leaves global-map \"C-x \" \"C-x C-\")"
  (dolist (leaf (esm--hydra-keys-in-a-list))
    (esm-restem here leaf lower-stem upper-stem)))

(defun esm-restem (here leaf new-stem reference-stem)
  "Keeping LEAF, change stem."
  (let ((ref-cmd (lookup-key here (kbd (concat reference-stem leaf)))))
    (when (esm--of-interest-p ref-cmd)
      (define-key here (kbd (concat new-stem leaf))
        ref-cmd))))

(defun esm-new-leaf (here stem new-leaf reference-leaf)
  (define-key here (kbd (concat stem new-leaf))
    (lookup-key here (kbd (concat stem reference-leaf)))))

;; TODO: make this work
(defmacro esm-backup-keymap-1 (keymap)
  "Backup KEYMAP under the name esm-backup-KEYMAP, unless it's
already been done."
  `(when-let ((name (ignore-errors (symbol-name ',keymap))) ;; guard clause
              (backup (intern (concat "esm-backup-" name))))
     (unless (and (boundp backup)
                  (not (eq nil backup)))
       ;; Maybe you should use `copy-keymap' here
       (set backup ,keymap))))

;; TODO: make it not fail for unnamed maps
;; TODO: backup unnamed maps too
(defmacro esm-backup-keymap (keymap)
  "Backup KEYMAP under the name esm-backup-KEYMAP, unless it's
already been done."
  `(let ((backup (intern (concat "esm-backup-" (symbol-name ',keymap)))))
     (unless (and (boundp backup)
                  (not (eq nil backup)))
       ;; Maybe you should use `copy-keymap' here
       (set backup ,keymap))))

(defmacro esm-restore-keymap (keymap)
  `(let ((backup (intern (concat "esm-backup-" (symbol-name ',keymap)))))
     (when (and (boundp backup)
                (not (eq nil backup)))
       (setq ,keymap backup))))

;; TODO: don't just operate on global map
(defun esm-kill-shift ()
  "Unbind."
  (dolist (x esm-all-keys-on-keyboard-except-shifted-symbols)
    (global-unset-key (kbd (concat "S-" x)))
    (global-unset-key (kbd (concat "C-S-" x)))
    (global-unset-key (kbd (concat "M-S-" x)))
    (global-unset-key (kbd (concat "C-M-S-" x))))
  (dolist (x esm-all-shifted-symbols)
    (global-unset-key (kbd (concat "C-" x)))
    (global-unset-key (kbd (concat "M-" x)))
    (global-unset-key (kbd (concat "C-M-" x)))
    (global-unset-key (kbd (concat "C-x C- " x)))
    (global-unset-key (kbd (concat "C-x " x)))))

(defun esm-bind-all-shiftsyms-to-insert ()
  "Bind all capital letters and shift symbols to self-insert."
  (dolist (leaf esm-all-shifted-symbols)
    (global-set-key (kbd leaf) #'self-insert-command)))

;; (defun esm-super-from-ctl ()
;;   (map-keymap (lambda (ev def)
;;                 (let* ((case-fold-search nil)
;;                        (key (key-description (list ev)))
;;                        (newkey (replace-regexp-in-string
;;                                 (rx word-start "C" word-end) "s" key t)))
;;                   (and (esm--of-interest-p def)
;;                        (not (equal key newkey))
;;                        (define-key global-map (kbd newkey) def))))
;;               global-map))

;; TODO: Do this continuously over time
;; TODO: Do this not only on global-map
(defun esm-super-from-ctl (map)
  (map-keymap (lambda (ev def)
                (let* ((case-fold-search nil)
                       (key (key-description (list ev)))
                       (newkey (replace-regexp-in-string
                                (rx word-start "C" word-end) "s" key t)))
                  (when (and (esm--of-interest-p def)
                             (not (equal key newkey))) ;; Don't proceed for those keys that didn't contain C- in the first place, e.g. M-f.
                    (define-key map (kbd newkey) def)))
                (when (keymapp def)
                  (esm-super-from-ctl def))) ;; recurse
              map)
  ;; is this the thing that makes C-g need two presses sometimes?
  (define-key key-translation-map (kbd "s-g") (kbd "C-g"))
  )


;;; Keyboard scanning

;; Think this function is hairy and unnecessary? It's this way because the C
;; function `describe-buffer-bindings' is the only way to get this information.
;;
;; Inspired by which-key--get-current-bindings. Thanks!
(defun esm--current-bindings (&optional keep flush)
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
      (while (re-search-forward (rx "<" (group (regexp esm--modifier-regexp)))
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



;;; Background facts

(defconst esm--modifier-regexp
  (regexp-opt '("A-" "C-" "H-" "M-" "S-" "s-")))

(defconst esm-all-keys-on-keyboard-except-shifted-symbols
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

(defconst esm--all-shifted-symbols
  (split-string
   "~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?"
   "" t))

(defconst esm-all-keys-on-keyboard
  (append
   esm-all-keys-on-keyboard-except-shifted-symbols
   esm--all-shifted-symbols))

(defun esm--hydra-keys-in-a-list ()
  (split-string esm--hydra-keys "" t))

(defun esm--hydra-keys-nonum ()
    (replace-regexp-in-string (rx num) "" esm--hydra-keys))

(defun esm--hydra-keys-list-nonum ()
  (split-string (replace-regexp-in-string (rx num) "" esm--hydra-keys) "" t))

;; "List of keys like C-a, C-e, M-f, M-g but not C-M-f or M-%."
;; note: it's not quite all of them since it uses esm--hydra-keys
(defun esm--all-duo-chords ()
  (let (chords)
    (mapc (lambda (char)
            (push (concat "C-" (string char)) chords)
            (push (concat "M-" (string char)) chords)
            (push (concat "s-" (string char)) chords))
          esm--hydra-keys)
    chords))

(defvar esm--hydra-keys "1234567890qwertyuiopasdfghjkl;zxcvbnm,./")
(defvar esm--hydra-keys-list (esm--hydra-keys-in-a-list))
(defvar esm--hydra-keys-nonum (esm--hydra-keys-nonum))
(defvar esm--hydra-keys-list-nonum (esm--hydra-keys-list-nonum))
(defvar esm--all-duo-chords (esm--all-duo-chords))


;;; User settings

(defun esm--colwidth ()
  (or esm-colwidth-override
      (let ((optimal (- (round (frame-parameter nil 'width) 10) 4)))
        (max optimal 8))))

(defvar esm-colwidth-override nil
  "An integer for the width of hydra hints. If nil, figure it out
from the frame width.")

(defvar esm--colwidth (esm--colwidth)
  "Width of hydra hint.
You should prefer to call the function `esm--colwidth' in case of
frame or font changes.")

(defvar esm-quitters '(;;"C-q"
                       "C-s" "C-g" "s-s" "s-g" "C-u" "M-x" "M-u" "s-u" "C-2"
                       "C-c c"))

(defvar esm-noquitters '("C-2 t" "C-2 n" "C-2 p" "C-x ;" "C-x x" "C-x q"
                           "C-x h" "C-x u" "C-x i" "C-x p" "C-x l" "C-x 1"
                           "C-x 2" "C-x 3" "C-x 0" "C-c c"
                           "C-c C-c" ;;testing
                           ))

(defvar esm-exit-by-default nil)


;;; Keydesc handling
;; If it's not a pure function, it probably doesn't belong here.

(defun esm--key-contains-ctl (keydesc)
  (declare (pure t) (side-effect-free t))
  (string-match-p "C-" keydesc))

(defun esm--key-contains-multi-chords (keydesc)
  (declare (pure t) (side-effect-free t))
  ;; Assume keydesc was already normalized.
  (string-match-p (rx (= 2 (regexp esm--modifier-regexp)))
                  keydesc))

(defun esm--key-has-more-than-one-modifier (keydesc)
  (declare (pure t) (side-effect-free t))
  (when (> (length keydesc) 3)
    (string-match-p esm--modifier-regexp (substring keydesc 2))))

;; could probs be programmed better
;; does not catch capitals FWIW
(defun esm--key-seq-mixes-modifiers (keydesc)
  (declare (pure t) (side-effect-free t))
  ;; we use our own regexp instead of esm--modifier-regexp so it is always
  ;; synced with how we define "mods" here
  (if-let* ((mods '("A-" "C-" "H-" "M-" "S-" "s-"))
            (regexp (eval-when-compile (regexp-opt '("A-" "C-" "H-" "M-" "S-" "s-"))))
            (first-match-pos (string-match-p regexp keydesc))
            (caught-mod (substring keydesc first-match-pos (+ 2 first-match-pos)))
            (now-verboten (-difference mods (list caught-mod))))
      (string-match-p (eval `(rx (or ,@now-verboten)) t) keydesc)))

(defun esm--of-interest-p (cmd)
  "Return t if CMD is worth carrying over to another key.
It does not fail if CMD is a keymap, check that separately."
  (declare (pure t) (side-effect-free t))
  (not (member cmd '(self-insert-command
                     nil
                     ignore
                     ignore-event
                     company-ignore))))

(defun esm--get-leaf (keydesc)
  (declare (pure t) (side-effect-free t))
  (->> keydesc
       (s-split (rx space))
       (-last-item)
       (esm--normalize-trim-segment)
       (esm--normalize-get-atoms)
       (esm--normalize-wrap-leaf-maybe)
       (-last-item)))

(defun esm--normalize-trim-segment (x)
  (declare (pure t) (side-effect-free t))
  (if (and (string-match-p (rx "<") x)
           (string-match-p (rx ">" eol) x))
      (replace-regexp-in-string (rx (any "<>")) "" x)
    x))

(defun esm--normalize-get-atoms (x)
  (declare (pure t) (side-effect-free t))
  (if (string-match-p (rx "-" eol) x)
      (append (split-string x "-" t) (list "-"))
    (split-string x "-")))

(defun esm--normalize-wrap-leaf-maybe (x)
  (declare (pure t) (side-effect-free t))
  (let* ((leaf (car (last x)))
         (corrected-leaf (if (string= "TAB" leaf)
                             leaf
                           (if (< 1 (length leaf))
                               (concat "<" leaf ">")
                             leaf))))
    (-snoc (butlast x) corrected-leaf)))

(defun esm--normalize-build-segments (x)
  (declare (pure t) (side-effect-free t))
  (string-join x "-"))

(defun esm--normalize (keydesc)
  (declare (pure t) (side-effect-free t))
  (->> keydesc
       (s-split (rx space))
       (-map #'esm--normalize-trim-segment)
       (-map #'esm--normalize-get-atoms)
       (-map #'esm--normalize-wrap-leaf-maybe)
       (-map #'esm--normalize-build-segments)
       (s-join " ")))

(defun esm--valid-keydesc (keydesc)
  "Check that KEYDESC is not a dangling stem.
I.e. it's something you'd pass to `kbd'. If true, return KEYDESC
unmodified, else return nil."
  (declare (pure t) (side-effect-free t))
  (when (or (string-match-p (rx "--" eol) keydesc)
            (string-match-p (rx " -" eol) keydesc)
            (string-match-p (rx (not (any "- ")) eol) keydesc))
    keydesc))

(defun esm-dub-from-key (keydesc)
  "Example: If KEY is the string \"C-x a\", return \"esm-Cxa\"."
  (declare (pure t) (side-effect-free t))
  ;; (if (member keydesc '("C-" "M-" "s-" "H-" "A-"))
  ;;     (concat "esm-" (cond ((string= keydesc "C-") "control")
  ;;                          ((string= keydesc "M-") "meta")
  ;;                          ((string= keydesc "s-") "super")
  ;;                          ((string= keydesc "H-") "hyper")
  ;;                          ((string= keydesc "A-") "alt")))
  ;; else
  (let ((squashed (string-join (split-string keydesc (rx (any " -"))))))
    (if (string-match (rx "-" eol) keydesc)
        (if (= 2 (length keydesc))
            (concat "esm-" squashed) ;; C-, M-, s-
          (concat "esm-" squashed "-")) ;; leaf is -
      (concat "esm-" squashed))))

(defun esm--key-seq-steps=1 (keydesc)
  (declare (pure t) (side-effect-free t))
  (not (string-match-p " " keydesc)))

(defun esm--key-seq-split (keydesc)
  (declare (pure t) (side-effect-free t))
  (split-string keydesc " "))

(defun esm--corresponding-hydra (stem leaf)
  (declare (pure t) (side-effect-free t))
  (intern (concat
           (esm-dub-from-key (esm--normalize (concat stem leaf)))
           "/body")))

(defun esm--are-keymaps (keys)
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
      (seq-sort-by #'esm--key-seq-steps-length #'< all))))

;; tests
;; (esm--are-keymaps '("C-x a" "C-M-s-f" "M-c C-o" "M-g"))

(defun esm--key-seq-steps-length (keydesc)
  (declare (pure t) (side-effect-free t))
  (length (esm--key-seq-split keydesc)))



;;; Library

(defvar esm-debug nil
  "A buffer name or nil.")

(defun esm-echo (x)
  (when esm-debug
    (print x (esm--debug-buffer))))

(defun esm--debug-buffer ()
  (when esm-debug
    (let ((buf (get-buffer-create esm-debug)))
      (with-current-buffer buf
        (setq-local truncate-lines t)
        buf))))

;; TODO: Difference from esm--corresponding-hydra?
;; REVIEW: Can this be tested? Need Buttercup?
(defun esm--subhydra-or-nil (stem leaf)
  "Return the hydra body in question."
  (if-let ((x (cdr (assoc (concat stem leaf) esm--live-hydras))))
      (intern (concat x "/body"))))

;; REVIEW: Write test for it with a key-simulator
(defun esm-universal-arg (arg)
  (interactive "P")
  (prefix-command-preserve-state)
  (call-interactively
   (intern (concat
            (substring (symbol-name hydra-curr-body-fn) 0 -5) ;; chop "/body"
            "-nonum/body")))
  (hydra--universal-argument arg))

;; unused
(defun esm--call-and-return-to-root (keydesc)
  "Nice in some cases, like C-c C-c for which it's often
desirable to end up in the Control root hydra rather than exit
altogether. Say you want to call it for each item in a list of
Org headings, and next-line is bound to the standard C-n, then
you want to be able to type nccnccnccncc."
  (interactive)
  (call-interactively (key-binding (kbd keydesc)))
  (cond ((string-match-p "^C-" keydesc)
         (esm-control/body))
        ((string-match-p "^M-" keydesc)
         (esm-meta/body))
        ((string-match-p "^s-" keydesc)
         (esm-super/body))))

;; TODO: delete
(defun esm-cc-cc ()
  (interactive)
  ;; (call-interactively (key-binding (kbd "C-c c"))) ;; if flattened
  (call-interactively (key-binding (kbd "C-c C-c")))
  (esm-control/body))

(defun esm-cmd (stem leaf)
  (key-binding (kbd (concat stem leaf))))

(defun esm--is-unbound (stem leaf)
  (null (esm-cmd stem leaf)))

(defun esm--is-bound (stem leaf)
  (not (esm--is-unbound stem leaf)))


;;; Hydra specifier

(defun esm-head-cmd (stem leaf)
  (cond ((not (esm--of-interest-p (esm-cmd stem leaf)))
         leaf) ;; make a blank spot
        ((keymapp (esm-cmd stem leaf))
         (esm--corresponding-hydra stem leaf))
        ((string= (concat stem leaf) "C-c c")
         #'esm-cc-cc)
        ((string= (concat stem leaf) "C-g") ;; TODO: don't check key, check if the binding is keyboard-quit.
         #'keyboard-quit)
        ((eq #'universal-argument (esm-cmd stem leaf))
         #'esm-universal-arg)
        (t `(call-interactively (key-binding (kbd ,(concat stem leaf)))))))

(defun esm-head-hint (stem leaf)
  (let* ((sym (or (esm--subhydra-or-nil stem leaf)
                  ;; (esm-is-known-prefix stem leaf)
                  (key-binding (kbd (concat stem leaf)))))
         (name (if (symbolp sym) (symbol-name sym) " ")))
    (if (string= name "nil")
        " "
      (if (> (length name) esm--colwidth)
          (substring name 0 esm--colwidth)
        name))))

(defun esm-head-key (stem leaf)
  "If the given hotkey is bound to a command, return LEAF,
otherwise return a space character. This can be used to
make a visible blank spot in a hydra for hotkeys that are unbound."
  (if (esm--of-interest-p (esm-cmd stem leaf))
      leaf
    " "))

(defun esm-head-exit (stem leaf &optional exit-almost-never-p)
  (cond ((esm--subhydra-or-nil stem leaf) '(:exit t)) ;; important
        ;; ((eq #'universal-argument (esm-cmd stem leaf)) '(:exit nil))
        ((member (concat stem leaf) esm-quitters) '(:exit t))
        ((member (concat stem leaf) esm-noquitters) '(:exit nil))
        ((esm--is-unbound stem leaf) '(:exit t))
        (exit-almost-never-p '(:exit nil))
        (t '()))) ;; defer to hydra's default behavior

(defun esm-head (stem leaf)
  "Return a \"head\" specification, in other words a list in the
form (KEY COMMAND HINT EXIT) as needed in `defhydra'."
  `( ,(esm-head-key stem leaf) ,(esm-head-cmd stem leaf) ,(esm-head-hint stem leaf)
     ,@(esm-head-exit stem leaf)))

(defun esm-head-invisible (stem leaf)
  `( ,(esm-head-key stem leaf) ,(esm-head-cmd stem leaf) nil
     ,@(esm-head-exit stem leaf 'almost-never)))

(defun esm-head-invisible-self-inserting (_stem leaf)
  `( ,leaf self-insert-command nil :exit t))

(defun esm--specify-visible-heads (stem &optional keys)
  (cl-loop for leaf in (or keys esm--hydra-keys-list)
           collect (esm-head stem leaf)))

(defun esm--specify-invisible-heads (stem)
  (append (cl-loop for leaf in '("<left>" "<right>" "<up>" "<down>"
                                 "<SPC>" "=" "\\" "'" "`"
                                 "<f1>" "<f2>" "<f3>" "<f4>" "<f5>"
                                 "<f6>" "<f7>" "<f8>" "<f9>" "<f10>"
                                 "<f11>" "<f12>")
                   collect (esm-head-invisible stem leaf))
          (cl-loop for chord in esm--all-duo-chords
                   collect (esm-head-invisible "" chord))
          (cl-loop for leaf in esm--all-shifted-symbols
                   collect (esm-head-invisible-self-inserting stem leaf))))

(defun esm--specify-extra-heads (stem)
  (declare (pure t) (side-effect-free t))
  (let ((pop-key (cond ((string= "C-" stem) "<f35>")
                       ((string= "M-" stem) "<f34>")
                       ((string= "s-" stem) "<f33>"))))
    (-non-nil (list `("<backspace>" ,(esm--parent-hydra stem)
                      nil :exit t)
                    (when pop-key `(,pop-key nil nil :exit t))))))

;; Assumption: Pre-normalized keydesc
(defun esm--get-stem (keydesc)
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (concat (regexp-quote (esm--get-leaf keydesc)) "$")
                            ""
                            keydesc))

;; IDK if it fails well
(defun esm--stem-to-keydesc (stem)
  (let ((keydesc (substring stem 0 -1)))
    (when (esm--valid-keydesc keydesc)
      keydesc)))

(defun esm--corresponding-hydra-from-stem (stem)
  (declare (pure t) (side-effect-free t))
  (intern (concat (esm-dub-from-key stem) "/body")))

;; Could probably be more clearly programmed
;; Maybe toatlly uncessary
(defun esm--parent-stem2 (keydesc)
  "Drop leaf of KEYDESC, return a valid keydesc.
Assume that there are no modifiers beyond the root. If there are, IDK."
  (declare (pure t) (side-effect-free t))
  (let* ((stem (esm--get-stem keydesc))
         (stem-trimmed (substring stem 0 -1)))
    (if (esm--valid-keydesc stem-trimmed)
        (if (s-ends-with-p "-" stem-trimmed)
            stem-trimmed ;; probably C-, but can be C-c C-.
          (if (s-matches-p esm--modifier-regexp stem)
              stem))
      (if (s-matches-p esm--modifier-regexp keydesc)
          nil ;; just exit
        (warn "escape-modality: Code should not have landed here")))))

(defun esm--parent-stem (stem)
  (esm--parent-stem2 (esm--stem-to-keydesc stem)))

(defun esm--parent-hydra (stem)
  (if (esm--key-seq-steps=1 stem)
      nil ;; exit
    (esm--corresponding-hydra-from-stem (esm--parent-stem stem))))

;(esm--get-parent "C-x a ")
;(esm--specify-extra-heads "C-x a")
;(esm--corresponding-hydra (esm--parent-stem "C-x a ") "")

(defun esm--call-defhydra (name heads)
  "Create a hydra named NAME with HEADS.
Tip: This is a thin wrapper around `defhydra', the magic happens
when it's called by `esm-generate-hydras-async'."
  (eval `(defhydra ,(intern name)
           (nil nil :columns 10 :exit ,esm-exit-by-default
                :body-pre (esm-generate-hydras-async)
                ;; :body-post (esm-generate-hydras-async)
                )
           ,name
           ,@heads)
        t))


;;; Async worker

(defvar esm--after-scan-bindings-hook nil
  "Things to do after updating `esm--current-bindings'.
Use this to unbind new keys that may have appeared due to a
buffer change or major mode change. You should remove those
records from esm--current-bindings when you do that as we will not
rescan.")

(defvar esm--after-rebuild-hydra-hook nil
  "Hook run after updating hydras to match the local map.")

;; Persistent variables helpful for debugging
(defvar esm--current-bindings nil)
(defvar esm--last-filtered-bindings nil)
(defvar esm--new-or-rebound-keys nil)
(defvar esm--changed-keymaps nil)
(defvar esm--hydra-blueprints nil)
(defvar esm--live-hydras nil)
(defvar esm--defunct-bindings nil)
(defvar esm--defunct-hydras nil)

(defun esm--combined-filter (cell)
  "Filter for rejecting keys as irrelevant to work on.
This function exists because there is a need to go into a list
and look at the `car's, and so we may as well run each filter
while we're at it."
  (declare (pure t) (side-effect-free t))
  (let ((keydesc (car cell)))
    (or ;;(esm--key-contains-ctl keydesc)
     (esm--key-has-more-than-one-modifier keydesc)
     (esm--contains-upcase keydesc)
     (esm--key-contains-multi-chords keydesc)
     (esm--key-seq-mixes-modifiers keydesc))))

(defun esm--contains-upcase (keydesc)
  (let* ((steps (esm--key-seq-split keydesc))
         (steps-without-modifier (mapcar #'esm--get-leaf steps)))
    (--any-p (member it esm--all-shifted-symbols) steps-without-modifier)))

(defun esm--get-relevant-bindings ()
  (->> (esm--current-bindings (rx bol (regexp esm--modifier-regexp))
                              ;; (rx (or "ESC" "C-"))
                              (rx (or "ESC")))
       (-map (lambda (x)
               (cons (if (esm--valid-keydesc (car x))
                         (esm--normalize (car x))
                       (car x))
                     (cdr x))))
       (esm--unbind-illegal-keys)
       (-remove (lambda (x) (or (string= "Prefix Command" (cdr x))
                           (null (intern (cdr x)))
                           (keymapp (intern (cdr x)))))) ;;
       (setq esm--current-bindings) ;; should end up the same as filtered bindings once you run the function twice
       (-remove #'esm--combined-filter)
       (setq esm--current-filtered-bindings)))

;; (current-local-map)
;; (local-set-key (kbd "C-M-q") nil)
;; (define-key (current-local-map) (kbd "C-M-q") nil)
;; (esm--get-relevant-bindings)
;; (->> '("C-M-a f V M <right>"
;;        "C-M-a f v m <right>"
;;        "C-M-a f M")
;;      (-map #'esm--key-seq-split)
;;      (--filter (--filter (member it esm--all-shifted-symbols) it)))

(defun esm--unbind-illegal-keys (input)
  (let* ((illegal (seq-filter #'esm--combined-filter input))
         (illegal-keys (seq-map #'car illegal))
         (sorted (seq-sort-by #'esm--key-seq-steps-length #'> illegal-keys)))
    (seq-do (lambda (x)
              (let ((map (help--binding-locus (kbd x) nil)))
                (unless (null map) ;; there's a bug that leaves some keys in `describe-bindings' but not in the apparently active map, and they get a null map. Check esm--current-bindings, C-M-q and C-M-@ are in there
                  (define-key (eval map t) (kbd x) nil))))
            sorted))
  input)

;; NOTE: Visualize a Venn diagram. The defunct and the new are the last and
;;      current minus their intersection (cases where the key's definition
;;      didn't change), which should never be relevant to look at.
(defun esm--set-variables ()
  (when esm-debug (message "Updating variables"))
  (setq esm--defunct-bindings (-difference esm--last-filtered-bindings
                                           esm--current-filtered-bindings))
  (setq esm--new-or-rebound-keys
        (-map #'car (-difference esm--current-filtered-bindings
                                 esm--last-filtered-bindings)))
  ;; Mark hydras for oblivion or redefinition
  (setq esm--defunct-hydras (-intersection esm--live-hydras
                                           (-map #'car esm--defunct-bindings)))
  ;; Unlist said marked hydras so they will be reborn. (Actually, we don't ever
  ;; check this variable, it's now debugging only).
  (setq esm--live-hydras
        (seq-remove (lambda (x) (member x esm--defunct-hydras))
                    esm--live-hydras)))

(defun esm--specify-dire-hydra-pair (stem)
  (list
   (cons (esm-dub-from-key stem)
         (append (esm--specify-visible-heads stem)
                 (esm--specify-invisible-heads stem)
                 (esm--specify-extra-heads stem)))
   ;; For each hydra, a nonum hydra for when universal-arg is active, so the digit arguments work.
   (cons (concat (esm-dub-from-key stem) "-nonum")
         (append (esm--specify-visible-heads
                  stem esm--hydra-keys-list-nonum)
                 (esm--specify-invisible-heads stem)
                 (esm--specify-extra-heads stem)))))

;; NOTE: See tests in the manual tests file
(defun esm--specify-dire-hydras ()
  (setq esm--hydra-blueprints
        (cl-loop for key in esm--new-or-changed-stems
                 append (esm--specify-dire-hydra-pair key))))

;; The magic spell that runs the entire damn codebase
(defun esm-generate-hydras-async ()
  "Regenerate hydras to match the local map."
  (deferred:$
    (deferred:next
      #'esm--get-relevant-bindings)

    (deferred:nextc it
      (lambda ()
        (run-hooks 'esm--after-scan-bindings-hook)))

    (deferred:nextc it
      #'esm--set-variables)

    (deferred:nextc it
      (lambda ()
        (setq esm--new-or-changed-stems
              (-uniq (mapcar #'esm--get-stem
                             ;; Sort by length to avail the most relevant hydras to the user soonest.
                             (seq-sort-by #'esm--key-seq-steps-length #'< esm--new-or-rebound-keys))))))

    (deferred:nextc it
      #'esm--specify-dire-hydras)

    ;; Re-cache settings in case of changed frame parameters or user options
    (deferred:nextc it
      (lambda ()
        (setq esm--colwidth (esm--colwidth))
        (setq esm--hydra-keys-list (esm--hydra-keys-in-a-list))
        (setq esm--hydra-keys-nonum (esm--hydra-keys-nonum))
        (setq esm--hydra-keys-list-nonum (esm--hydra-keys-list-nonum))
        (setq esm--all-duo-chords (esm--all-duo-chords))))

    (deferred:nextc it
      (lambda ()
        (cl-loop for x in esm--hydra-blueprints
                 do (push (esm--call-defhydra (car x) (cdr x))
                          esm--live-hydras))
        (setq esm--last-filtered-bindings esm--current-filtered-bindings)))

    (deferred:nextc it
      (lambda ()
        (run-hooks 'esm--after-rebuild-hydra-hook)))

    (deferred:nextc it
      #'esm--fix-which-key)

    (deferred:error it
      #'warn)))

;; when I want the package to be more modular, this should sit on a hook
;; NOTE: keymap-based replacements are more performant, see README:
;; https://github.com/justbur/emacs-which-key
;; not that I care
(defun esm--fix-which-key ()
  "Hide keys with repeated modifiers like C-x C-f from which-key.
We do this because it's bound to the same command as C-x f in our
paradigm, so all these keys just crowd the display.

Destructive; overwrites `which-key-replacement-alist' (but does
not change the value as saved in `custom-file') ignoring any
rules already present, because they may not work without enabling
`which-key-allow-multiple-replacements'. Customizations of
which-key are mostly superfluous in our paradigm, so we opt not
to for performance."
  (if (bound-and-true-p esm--current-bindings)
      (->> esm--current-bindings
           (-map #'car)
           (-remove #'esm--key-seq-steps=1)
           (-filter #'esm--key-has-more-than-one-modifier)
           (--map (s-replace "\\" "\\\\" it))
           (--map (cons (cons it nil) t))
           (setq which-key-replacement-alist))
    (message "escape-modality: Variable esm--current-bindings unexpectedly empty.")))


;;;; Main

;;;###autoload
(define-minor-mode escape-modality-mode
  "Bind root hydras."
  nil
  " ESM"
  `((,(kbd "<f35>") . esm-C/body)
    (,(kbd "<f34>") . esm-M/body)
    (,(kbd "<f33>") . esm-s/body))
  :global t
  ;; TODO: Faster init. Ideally run a deferred loop, and disable
  ;; esm-generate-hydras-async until after the loop finishes.
  ;;
  ;; Seed initial hydras.
  (unless t
    (when escape-modality-mode

      (esm--get-relevant-bindings)
      (esm--set-variables)
      (setq esm--new-or-changed-stems
            (-uniq (mapcar #'esm--get-stem
                           ;; Sort by length to avail the most relevant hydras to the user soonest.
                           (seq-sort-by #'esm--key-seq-steps-length #'< esm--new-or-rebound-keys))))
      (esm--specify-dire-hydras)

      ;; the unnamed lambda that builds it all
      (cl-loop for x in esm--hydra-blueprints
               do (push (esm--call-defhydra (car x) (cdr x))
                        esm--live-hydras))
      (setq esm--last-filtered-bindings esm--current-filtered-bindings)

      )))

(provide 'escape-modality)
;;; escape-modality.el ends here
