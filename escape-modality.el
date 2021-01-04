;;; escape-modality.el --- Modifier-free pseudo-modal human-Emacs interface -*- lexical-binding: t; -*-

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

;; external dependencies
(require 'deferred)
(require 'dash)
(require 's)
(require 'alert)


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
            (start-process "xcape" "*xcape*" "xcape" "-d" "-e" rules))
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

(defun esm-flatten-keymap (keymap)
  ;; (esm-backup-keymap keymap)
  (map-keymap
   (lambda (ev def)
     (when (keymapp def)
       (esm-flatten-keymap def)) ;; recurse
     (when (and (commandp def)
                (esm--of-interest-p def))
       (let* ((keydesc (key-description (list ev)))
              (leaf (esm-get-leaf keydesc)))
         (when (not (equal keydesc leaf))
           (define-key keymap ev (lookup-key keymap (kbd leaf)))))))
   keymap))

;; Generalized flatten-ctl-x
(defun esm-restem-all-leaves (here lower-stem upper-stem)
  "Duplicate bindings on UPPER-STEM to also exist on LOWER-STEM.
Where they conflict, LOWER-STEM is overridden, hence the
name (inspired by overlayfs). HERE refers to the keymap such as
global-map.  Typical use: (esm-restem-all-leaves global-map \"C-x
\" \"C-x C-\")"
  (dolist (leaf (esm--hydra-keys-in-a-list))
    (esm-restem here leaf lower-stem upper-stem)))

(defun esm-restem (here leaf new-stem reference-stem)
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

;; Inspired by which-key--get-current-bindings. Thanks!
(defun esm--current-bindings (&optional keep flush)
  "Get the list of all currently active bindings.
This ignores those masked by other keymaps, returning only the
binding in the winning keymap.

Optional argument KEEP is a regexp describing keys to keep. Can
be left at nil to keep everything.

Optional argument FLUSH is a regexp describing keys to discard
after the above filter has been applied (even if KEEP was nil)."
  (let ((result '())
        (buffer (current-buffer))
        (ignore-keys (eval-when-compile
                       (regexp-opt '("mouse-" "remap" "scroll-bar" "select-"
                                     "switch-" "help" "-state" "which-key-"
                                     "-corner" "-divider" "-edge" "header-"
                                     "mode-line" "tab-" "vertical-line" "frame"
                                     ))))
        (ignore-bindings (eval-when-compile
                           (regexp-opt '("self-insert-command" "ignore"
                                         "ignore-event" "company-ignore"))))
        (ignore-sections (eval-when-compile
                           (regexp-opt '("Key translations"
                                         "Function key map translations"
                                         "Input decoding map translations")))))
    (with-temp-buffer
      (setq-local indent-tabs-mode t)
      (describe-buffer-bindings buffer)
      (goto-char (point-min))
      (flush-lines ignore-bindings)
      (flush-lines (rx (regexp ignore-sections)
                       (* (not ""))))
      (flush-lines (rx "---"))
      (flush-lines (rx bol "key" (* nonl) "binding"))
      (flush-lines (rx bol (* nonl) ":" eol))
      (flush-lines (rx ""))
      (flush-lines (rx " .. "))
      (while (re-search-forward (rx "\n\t") nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward (rx "<" (group (or "A-" "C-" "H-" "M-" "S-" "s-")))
                                nil t)
        (replace-match "\\1<")
        (goto-char (point-min)))
      (flush-lines (rx (regexp ignore-keys) (* nonl) "	"))
      (when keep (keep-lines keep))
      (when flush (flush-lines flush))
      (while (re-search-forward (rx (group (+? nonl)) (+ "	") (group (+ nonl)))
                                nil t)
        (push (cons (match-string 1) (match-string 2))
              result)))
    ;; Hydraize shallower prefixes before longer (implying nested) ones
    ;; (seq-sort-by (lambda (x) (string-width (car x))) #'< result)
    result
    ))

(defvar esm--after-scan-bindings-hook)

(defvar esm--last-filtered-bindings nil)

(defvar esm--last-bindings nil)

(defvar esm--new-or-rebound-keys)

(ert-deftest test-esm--current-bindings ()
  (let ((foo (esm--current-bindings)))
    (should (-none-p #'null (-map #'esm--valid-keydesc (-map #'car foo))))
    ;; (should (not (null (seq-find (lambda (x) (esm--subhydra-or-nil (car ()))) foo))))
    ))


;;; Background facts

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
frame or font changes, rather than consult this variable.")

(defvar esm-quitters '(;;"C-q"
                       "C-s" "C-g" "s-s" "s-g" "C-u" "M-x" "M-u" "s-u" "C-2"
                       "C-c c"))

(defvar esm-noquitters '("C-2 t" "C-2 n" "C-2 p" "C-x ;" "C-x x" "C-x q"
                           "C-x h" "C-x u" "C-x i" "C-x p" "C-x l" "C-x 1"
                           "C-x 2" "C-x 3" "C-x 0" "C-c c"
                           "C-c C-c" ;;testing
                           ))

(defvar esm-exit-by-default t)


;;; Keydesc handling

(defun esm--key-contains-ctl (keydesc)
  (declare (pure t) (side-effect-free t))
  (string-match-p "C-" keydesc))

(defconst esm--modifier-regexp
  (regexp-opt '("A-" "C-" "H-" "M-" "S-" "s-")))

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
(defun esm--key-seq-mixes-modifiers (keydesc)
  (if-let* ((k keydesc)
            (mods '("A-" "C-" "H-" "M-" "S-" "s-"))
            (first-match-pos (string-match-p esm--modifier-regexp k))
            (caught-mod (substring k first-match-pos (+ 2 first-match-pos)))
            (now-verboten (-difference mods (list caught-mod))))
      (string-match-p (eval `(rx (or ,@now-verboten)) t) k)
      ;; (string-match-p (regexp-opt now-verboten) k)
      ))

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
  (if (member keydesc '("C-" "M-" "s-" "H-" "A-"))
      (concat "esm-" (cond ((string= keydesc "C-") "control")
                           ((string= keydesc "M-") "meta")
                           ((string= keydesc "s-") "super")
                           ((string= keydesc "H-") "hyper")
                           ((string= keydesc "A-") "alt")))
    ;; else
    (let ((squashed (string-join (split-string keydesc (rx (any " -"))))))
      (if (string-match (rx "-" eol) keydesc)
          (concat "esm-" squashed "-")
        (concat "esm-" squashed)))))

;; Performant alternative to the old method (= 1 (esm--key-seq-steps x)).
(defun esm--key-seq-steps=1 (keydesc)
  (declare (pure t) (side-effect-free t))
  (not (string-match-p " " keydesc)))

(defun esm--corresponding-hydra (stem leaf)
  (declare (pure t) (side-effect-free t))
  (intern (concat
           (esm-dub-from-key (esm--normalize (concat stem leaf)))
           "/body")))

(defun esm--get-parent (stem &optional _leaf)
  "Get the parent of the hydra specified by STEM.
E.g. when STEM is \"C-x a \" then return the value of
(esm--corresponding-hydra \"C-\" \"x\")."
  (declare (pure t) (side-effect-free t))
  ;; In the case where STEM is such as "C-x " or "C-x a ", trim the space.
  (if-let ((keydesc (esm--valid-keydesc (substring stem 0 -1))))
      (if (esm--key-seq-steps=1 keydesc)
          ;; STEM must be such as "C-x ", so the parent is a root hydra.
          (cond ((string-match-p "^C-." stem) #'esm-control/body)
                ((string-match-p "^M-." stem) #'esm-meta/body)
                ((string-match-p "^s-." stem) #'esm-super/body))
        ;; STEM must be such as "C-x a ".
        ;; FIXME: go up one level, i.e .return esm-Cx/body, not esm-Cxa/body.
        (esm--corresponding-hydra stem ""))
    nil))

(ert-deftest keydesc-handling-1 ()
  (should (esm--key-seq-mixes-modifiers "C-h M-o"))
  (should-not (esm--key-seq-mixes-modifiers "C-h C-o"))
  (should-not (esm--key-seq-mixes-modifiers "C-h o"))
  (should (esm--key-contains-multi-chords "C-M-f"))
  (should-not (esm--key-contains-multi-chords "C-f M-f"))
  ;; (should (esm--key-contains-multi-chords "C-<M-return>")) ;; fail ok b/c we assume normalized input
  (should (esm--key-has-more-than-one-modifier "C-x C-h"))
  (should-not (esm--key-has-more-than-one-modifier "C-x H"))
    (should (equal (esm--get-parent "C-x ") #'esm-control/body))
  (should (equal (esm--get-parent "M-x ") #'esm-meta/body))
  (should (equal (esm--get-parent "s-x ") #'esm-super/body))
  (should (equal (esm--get-parent "s-x a") nil))
  (should (equal (esm--get-parent "s-x a ") #'esm-sx/body))
  ;; (should (equal (esm--get-parent "s-x <print>") nil)) ;; probably not a major problem
  (should (equal (esm--get-parent "s-x <print> ") #'esm-sx/body)))

(ert-deftest esm--normalize-components ()
  (should (equal (esm--normalize-trim-segment "<next>") "next"))
  (should (equal (esm--normalize-trim-segment "<C-next>") "C-next"))
  (should (equal (esm--normalize-trim-segment "<") "<"))
  (should (equal (esm--normalize-trim-segment "C-<") "C-<"))
  (should (equal (esm--normalize-trim-segment "C->") "C->"))
  (should (equal (esm--normalize-trim-segment ">") ">"))
  (should (equal (esm--normalize-get-atoms "C--") '("C" "-")))
  (should (equal (esm--normalize-get-atoms "C-f") '("C" "f")))
  (should (equal (esm--normalize-get-atoms "C-M-f") '("C" "M" "f")))
  (should (equal (esm--normalize-wrap-leaf-maybe '("C" "M" "next")) '("C" "M" "<next>")))
  (should (equal (esm--normalize-wrap-leaf-maybe '("C" "M" ">")) '("C" "M" ">")))
  (should-not (equal (esm--normalize-wrap-leaf-maybe '("C" "M" "f")) '("C" "M" "<f>")))
  (should (equal (esm--normalize-build-segments '("C" "M" "-")) "C-M--")))

(ert-deftest keydesc-handling ()
  (let ((problematic-key-descriptions
         '(;; raw          normalized       squashed           leaf    1step?
           ("C-x 8 RET"    "C-x 8 <RET>"    "esm-Cx8<RET>"     "<RET>"  nil  )
           ("<f2> 8 RET"   "<f2> 8 <RET>"   "esm-<f2>8<RET>"   "<RET>"  nil  )
           ("<f2> f r"     "<f2> f r"       "esm-<f2>fr"       "r"      nil  )
           ("<f2> <f2>"    "<f2> <f2>"      "esm-<f2><f2>"     "<f2>"   nil  )
           ("ESC <C-down>" "<ESC> C-<down>" "esm-<ESC>C<down>" "<down>" nil  )
           ("C-x RET C-\\" "C-x <RET> C-\\" "esm-Cx<RET>C\\"   "\\"     nil  )
           ("TAB"          "TAB"            "esm-TAB"          "TAB"      t  )
           ("A-T A-B"      "A-T A-B"        "esm-ATAB"         "B"      nil  )
           ("A-T A B"      "A-T A B"        "esm-ATAB"         "B"      nil  )
           ("A-TAB"        "A-TAB"          "esm-ATAB"         "TAB"      t  )
           ("C-<M-return>" "C-M-<return>"   "esm-CM<return>"   "<return>" t  )
           ("<C-M-return>" "C-M-<return>"   "esm-CM<return>"   "<return>" t  )
           ;; ("C-- - -"      "C-- - -"         "esm-C---"       "-"       nil  )
           ;; TODO: Because see  (kbd "TAB")  (kbd "<TAB>")
           ;; ("<TAB>"        "<TAB>"          "esm-<TAB>"        "<TAB>" t  )
           ;; ("s-S-M-H-C-A-<return>" "A-C-H-M-S-s-<return>" "esm-ACHMSs<return>" "<return>" t)
           )))
    (dolist (case problematic-key-descriptions)
      (seq-let (raw normalized squashed leaf 1step?) case
        (should (string= normalized (esm--normalize raw)))
        (should (string= squashed (esm-dub-from-key normalized)))
        (should (string= leaf (esm--get-leaf normalized)))
        (should (eq 1step? (esm--key-seq-steps=1 normalized)))))))


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


;;; Hydra maker

(defun esm-head-cmd (stem leaf)
  (cond ((not (esm--of-interest-p (esm-cmd stem leaf)))
         leaf) ;; make a blank spot
        ((keymapp (esm-cmd stem leaf))
         (esm--corresponding-hydra stem leaf))
        ((string= (concat stem leaf) "C-c c")
         #'esm-cc-cc)
        ((string= (concat stem leaf) "C-g") ;; TODO: don't check key, check if the binding is keyboard-quit.
         #'keyboard-quit)
        ((eq 'universal-argument (esm-cmd stem leaf))
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
        ((member (concat stem leaf) esm-quitters) '(:exit t))
        ((member (concat stem leaf) esm-noquitters) '(:exit nil))
        ((esm--is-unbound stem leaf) '(:exit t))
        (exit-almost-never-p '(:exit nil))
        (t '()))) ;; defer to hydra's default behavior

(defun esm-head (stem leaf)
  "Return a \"head\" specification, in other words a list in the
form (KEY COMMAND HINT EXIT) as needed in `defhydra'. "
  `( ,(esm-head-key stem leaf) ,(esm-head-cmd stem leaf) ,(esm-head-hint stem leaf)
     ,@(esm-head-exit stem leaf)))

(defun esm-head-invisible (stem leaf)
  `( ,(esm-head-key stem leaf) ,(esm-head-cmd stem leaf) nil
     ,@(esm-head-exit stem leaf 'almost-never)))

(defun esm-head-invisible-self-inserting (_stem leaf)
  `( ,leaf self-insert-command nil :exit t))

;; TODO: Update docstrings in later macros about the type of KEYS
(defun esm--specify-visible-heads (stem &optional keys)
  (cl-loop for leaf in (or keys esm--hydra-keys-list)
           collect (esm-head stem leaf)))

(defun esm--specify-invisible-heads* (stem)
  (append (cl-loop for leaf in '("<left>" "<right>" "<up>" "<down>"
                                 "<SPC>" "=" "\\" "'" "`")
                   collect (esm-head-invisible stem leaf))
          (cl-loop for chord in esm--all-duo-chords
                   collect (esm-head-invisible "" chord))))

(defun esm--specify-invisible-heads (stem)
  (append (cl-loop for leaf in '("<left>" "<right>" "<up>" "<down>"
                                 "<SPC>" "=" "\\" "'" "`")
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
    (-non-nil (list `("<backspace>" ,(esm--get-parent stem) nil :exit t)
                    (when pop-key `(,pop-key nil nil :exit t))))))

;; The big red button
(defun esm--define-dire-hydra (name heads)
  "Create a hydra named NAME with HEADS."
  (eval `(defhydra ,(intern name)
           (nil nil :columns 10 :exit ,esm-exit-by-default
                :body-pre (esm-generate-hydras-async)
                :body-post (esm-generate-hydras-async))
           ,name
           ,@heads)
        t))

(ert-deftest generate-heads ()
  (should (equal (esm-head "C-" "f")
                 '("f" (call-interactively (key-binding (kbd "C-f"))) "forward-char")))
  (should (equal (esm-head "C-x " "f")
                 '("f" (call-interactively (key-binding (kbd "C-x f"))) "set-fill-column")))
  (should (equal (esm-head-invisible "C-" "f")
                 '("f" (call-interactively (key-binding (kbd "C-f"))) nil :exit nil)))
  (should (equal (esm-head-hint "C-" "f")
                 (symbol-name (key-binding (kbd "C-f")))))
  (should (equal (esm-head-hint "C-" "<f12>")
                 " "))
  ;; weird case, behaves different interactively than when called by ert
  ;; (should (equal (esm-head-hint "C-" "x") "Control-X-prefi"))
  )

(ert-deftest head-specifiers ()
  (should (equal (esm--specify-extra-heads "C-x ")
                 '(("<backspace>" esm-control/body nil :exit t))))
  (should (equal (esm--specify-extra-heads "C-")
                 '(("<backspace>" nil nil :exit t) ("<f35>" nil nil :exit t)))))


;;; DEPRECATED The monster stage

(defmacro esm-define-many-headed-hydra
    (name stem &optional doctitle pop-key parent exit keys pop-key-2)
  "Make a hydra with many heads.
This is exhaustive; it checks almost every leaf on STEM, but doesn't recurse into subkeymaps.

Optional argument EXIT controls whether the hydra's heads exit by
default (overridden case-by-case by `esm-quitters' or `esm-noquitters').
Optional argument KEYS is a string specifying which keys to
display in the hydra hint (see `hydra'), defaulting to the value of
`esm--hydra-keys'."
  (declare (indent defun))
  `(defhydra ,name (nil nil :columns 10 :exit ,exit
                        :body-pre (esm-generate-hydras-async)
                        ;; :body-pre (esm-colorize-cursor)
                        ;; (setq exwm-input-line-mode-passthrough t)
                        ;; :post (esm-decolorize-cursor)
                        ;; (setq exwm-input-line-mode-passthrough nil)
                        ;; :foreign-keys run
                        )
     ,@(if doctitle (list doctitle) '())
     ,@(mapcar (lambda (leaf) (esm-head stem (string leaf)))
               (or keys esm--hydra-keys))
     ,@(mapcar (lambda (leaf) (esm-head-invisible stem leaf))
               '("<left>" "<right>" "<up>" "<down>" "<SPC>" "=" "\\" "'" "`"))
     ,@(mapcar (lambda (chord) (esm-head-invisible "" chord))
               esm--all-duo-chords)
     ,@(let (extras)
         (mapc (lambda (candidate) (if (car candidate) (push (cdr candidate) extras)))
               ;; List of extra heads to go in the hydra.
               ;; For example ("C-f" . ("C-f" nil nil :exit t))
               `((,pop-key . (,pop-key nil nil :exit t))
                 (,pop-key-2 . (,pop-key-2 nil nil :exit t))
                 (,parent . ("<backspace>" ,parent nil :exit t)))) extras)))


(defun esm-define-prefix-hydra (key)
  (let* ((x            (esm-dub-from-key (esm--normalize key)))
         (title        (intern x))
         (nonum-title  (intern (concat x "-nonum")))
         (nonum-keymap (intern (concat x "-nonum/keymap")))
         (body         (intern (concat x "/body")))
         (keymap       (intern (concat x "/keymap")))
         (stem         (concat key " "))
         (parent-key (substring key 0 -2)) ;; doesn't fail with <RET>, strange!
         (parent-symname (cdr (assoc parent-key esm--live-hydras)))
         (parent-body (cond ;; ((string= parent-key "C") #'esm-control/body)
                       ((string= parent-key "M") #'esm-meta/body)
                       ((string= parent-key "s") #'esm-super/body)
                       ;; problem: may not exist yet
                       ((assoc parent-key esm--live-hydras)
                        (intern (concat parent-symname "/body"))))))
    (eval `(progn
             (esm-define-many-headed-hydra ,title       ,stem ,key nil ,parent-body t)
             (esm-define-many-headed-hydra ,nonum-title ,stem ,(concat "\n\n" key) nil ,body t ,esm--hydra-keys-nonum)
             ;; (define-key ,keymap "u" #'esm-universal-arg)
             (define-key ,nonum-keymap "u" #'hydra--universal-argument)
             (dotimes (i 10)
               (let ((key (kbd (int-to-string i))))
                 (when (eq (lookup-key ,keymap key) 'hydra--digit-argument)
                   (define-key ,keymap key nil))))
             (cons ,key ,x)
             ))))

;; (esm-dub-from-key (esm--normalize "C-a"))
;; (macroexpand (esm-define-many-headed-hydra Chydr "C-" "C-" "<f35>" ))
;; (macroexpand (esm-define-stem-hydra "C-"))
;; (general-swap-key nil '(global local) "<menu>" "C-g")
;; (general-def "C-g" #'keyboard-quit)

(defun esm-define-stem-hydra (key)
  "For now just used to create root hydras, but could in theory
be used to create a prefix hydra for non-traditional meanings of
\"prefix key\" -- for example, a hydra for all keys starting with
`C-x C-'."
  (let* ((x            (esm-dub-from-key key))
         (title        (intern x))
         (nonum-title  (intern (concat x "-nonum")))
         (nonum-keymap (intern (concat x "-nonum/keymap")))
         (body         (intern (concat x "/body")))
         (keymap       (intern (concat x "/keymap")))
         (stem         key))
    (eval `(progn
             (esm-define-many-headed-hydra ,title       ,stem ,key)
             (esm-define-many-headed-hydra ,nonum-title ,stem ,(concat "\n\n" key) nil ,body nil ,esm--hydra-keys-nonum)
             ;; (define-key ,keymap "u" #'esm-universal-arg)
             (define-key ,nonum-keymap "u" #'hydra--universal-argument)
             (dotimes (i 10)
               (let ((key (kbd (int-to-string i))))
                 (when (eq (lookup-key ,keymap key) 'hydra--digit-argument)
                   (define-key ,keymap key nil))))
             (cons ,key ,x)
             ))))


;;; Async worker

(defvar esm--live-hydras nil)

(defvar esm--requested-hydras nil)

(defun esm--combined-filter (cell)
  (declare (pure t) (side-effect-free t))
  (let ((keydesc (car cell)))
    (not (or ;;(esm--key-contains-ctl keydesc)
             (esm--key-contains-multi-chords keydesc)
             (esm--key-seq-mixes-modifiers keydesc)))))

(defun esm--set-last-bindings-stage-1 (current-bindings)
  (alert "Filtering bindings" :severity 'trivial)
  (-remove #'esm--combined-filter current-bindings))

;; (upcasing helps my clarity in this case)
(defun esm--set-last-bindings-stage-2 (CURRENT-FILTERED-BINDINGS)
  (alert "Updating new-or-rebound-keys" :severity 'trivial)
  (prog1 (-difference esm--last-filtered-bindings
                      CURRENT-FILTERED-BINDINGS)
    (setq esm--new-or-rebound-keys
          (-map #'car (-difference CURRENT-FILTERED-BINDINGS
                                   esm--last-filtered-bindings)))
    (setq esm--last-filtered-bindings
          CURRENT-FILTERED-BINDINGS)))

(defun esm--set-last-bindings-stage-3 (defunct-bindings)
  (alert "Check for defunct hydras (slated for oblivion or redefinition)."
         :severity 'trivial)
  (-intersection (-map #'car esm--live-hydras)
                 (-map #'car defunct-bindings)))

(defun esm--set-last-bindings-stage-4 (defunct-hydras)
  (alert "Unlist defunct hydras" :severity 'trivial)
  (setq esm--live-hydras
        (seq-remove (lambda (x) (member (car x) defunct-hydras))
                    esm--live-hydras)))

;; when I want the package to be more modular, this should sit on a hook
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
  (if (bound-and-true-p esm--last-bindings)
      (->> esm--last-bindings
           (-map #'car)
           (-remove #'esm--key-seq-steps=1)
           (-filter #'esm--key-has-more-than-one-modifier)
           (--map (s-replace "\\" "\\\\" it))
           (--map (cons (cons it nil) t))
           (setq which-key-replacement-alist))
    (alert "escape-modality: A variable is unexpectedly empty."
           :severity 'moderate)))

;; NOTE: See tests in the manual tests file
(defun esm--specify-hydras (keys-to-hydraize)
  "Specify the set of hydras now needing to be (re-)defined.
Return an alist where the car is the name of the hydra as a
string and the remainder are head specifications.  Also set
`esm--requested-hydras' for your inspection.

Argument KEYS-TO-HYDRAIZE is a list of key descriptions such as
those in `esm--new-or-rebound-keys'."
  (setq esm--requested-hydras
        (append
         (cl-loop for key in keys-to-hydraize
                  collect
                  (let ((stem (concat key " ")))
                    (cons (esm-dub-from-key key)
                          (append (esm--specify-visible-heads stem)
                                  (esm--specify-invisible-heads stem)
                                  (esm--specify-extra-heads stem)))))
         (cl-loop for key in keys-to-hydraize
                  collect
                  (let ((stem (concat key " ")))
                    (cons (concat (esm-dub-from-key key) "-nonum")
                          (append (esm--specify-visible-heads
                                   stem esm--hydra-keys-list-nonum)
                                  (esm--specify-invisible-heads stem)
                                  (esm--specify-extra-heads stem))))))))

;; The magic spell that runs on every hydra invocation and buffer change
(defun esm-generate-hydras-async ()
  "Regenerate hydras to match the local map."
  (deferred:$
    (deferred:next
      (lambda ()
        (alert "Getting bindings" :severity 'trivial)
        (setq esm--last-bindings
              (esm--current-bindings (rx bol (regexp esm--modifier-regexp))
                                     ;; (rx (or "ESC" "C-"))
                                     (rx (or "ESC"))))))

    (deferred:nextc it
      #'esm--set-last-bindings-stage-1)

    (deferred:nextc it
      #'esm--set-last-bindings-stage-2)

    (deferred:nextc it
      #'esm--set-last-bindings-stage-3)

    (deferred:nextc it
      #'esm--set-last-bindings-stage-4)

    (deferred:nextc it
      #'esm--fix-which-key)

    ;; Re-cache settings in case of changed circumstances or user options
    (deferred:nextc it
      (lambda ()
        (setq esm--hydra-keys-nonum (esm--hydra-keys-nonum))
        (setq esm--colwidth (esm--colwidth))
        (setq esm--hydra-keys-list (esm--hydra-keys-in-a-list))
        (setq esm--hydra-keys-list-nonum (esm--hydra-keys-list-nonum))
        (setq esm--all-duo-chords (esm--all-duo-chords))))

    (deferred:nextc it
      (lambda ()
        (--filter (keymapp (key-binding (kbd it))) ;; note: not the same `it'
                  esm--new-or-rebound-keys)))

    (deferred:nextc it
      #'esm--specify-hydras)

    (deferred:nextc it
      (lambda (hydra-specs)
        (cl-loop for x in hydra-specs
                 do (push (esm--define-dire-hydra (car x) (cdr x))
                          esm--live-hydras))))

    (deferred:nextc it
      (lambda ()
        (run-hooks 'esm--after-scan-bindings-hook)))

    (deferred:error it
      #'warn)))


;;;; Main

;;;###autoload
(define-minor-mode escape-modality-mode
  "Bind root hydras. "
  nil
  " ESM"
  `((,(kbd "<f35>") . esm-control/body)
    (,(kbd "<f34>") . esm-meta/body)
    (,(kbd "<f33>") . esm-super/body))
  :global t)

(defun escape-modality ()
  "Turn on the whole paradigm described in Info node `'. "
  (interactive "P")
  ;; (call-interactively #'massmap-tidy-mode)
  (call-interactively #'escape-modality-mode)
  ;; (call-interactively #'deianira-global-mode)
  )

;; not used for much, delete?
(defvar esm-assume-no-multi-chords nil
  "Assume there are no key seqs starting with a multi-chord.
In other words, bindings like C-M-f don't exist. A non-nil
setting makes some code run a little faster.")

(defun esm-mirror-root-maps ()
  (dolist (x esm--last-bindings)
    ;; Each x has the form ("C-x a" . "move-beginning-of-line").
    (when (esm--key-seq-steps=1 (car x))
      (if-let* ((key (car x))
                (cmd-name (cdr x))
                (root (cond ((string-match-p (rx line-start "C-") key)
                             (cons "C-" esm-root-control-map))
                            ((string-match-p (rx line-start "M-") key)
                             (cons "M-" esm-root-meta-map))
                            ((string-match-p (rx line-start "s-") key)
                             (cons "s-" esm-root-super-map))
                            (t nil)))
                (key-to-bind (if esm-assume-no-multi-chords
                                 (substring key 2)
                               (replace-regexp-in-string (car root) "" key))))
          (define-key (cdr root)
            (kbd key-to-bind)
            (if (equal cmd-name "Prefix Command")
                (key-binding (kbd key))
              (intern cmd-name)))))))

;; (add-hook 'esm--after-scan-bindings-hook #'esm-mirror-root-maps)
;; they come in a specific order, see (key-description (kbd "C-S-M-s-H-A-5"))
(define-prefix-command 'esm-root-alt-map)
(define-prefix-command 'esm-root-control-map)
(define-prefix-command 'esm-root-hyper-map)
(define-prefix-command 'esm-root-meta-map)
(define-prefix-command 'esm-root-super-map)

;; TODO: put this in the mode command
(eval '(esm-define-stem-hydra "M-"))
(eval '(esm-define-stem-hydra "s-"))
(eval '(esm-define-stem-hydra "C-"))

(provide 'escape-modality)
;;; escape-modality.el ends here
