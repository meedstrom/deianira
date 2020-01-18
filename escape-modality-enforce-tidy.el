;; escape-modality-enforce-tidy.el -- Cleaning up all keymaps
;; Copyright (C) 2019 Martin Erik Edström

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

;; This file can probably be spun out as a separate package.
(require 'escape-modality-common)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instructive prior art: which-key--get-keymap-bindings
;; (map-keymap 'readkey1 global-map)
(defun readkey1 (ev def)
  (print (key-description (list ev)) (get-buffer-create "*scratch*"))
  (print def (get-buffer-create "*scratch*")))

;; (map-keymap 'readkey2 global-map)
(defun readkey2 (ev def)
  (when (esm-of-interest def)
    (print (key-description (list ev)))
    (print def)))

;;(substitute-command-keys "\\{org-mode-map}")

;; map-keymap passes events to a function, and those events look like (list
;; 67108918), and it turns out you can append instead of concat:
;; (key-description (append (kbd "C-x") (list 67108918)))

;; So we are trying to flatten the ctl-x-map. What does that mean? It's not
;; replacing one modifier with another, it's not deleting one modifier, it's
;; restemming keys with structure C-x ... to C-x C-... so we need the somewhat
;; simplistic verb of 'restem' for now. But what if we could generalize to
;; detect the structure of a key string so we can flatten everything,
;; including M-g o and M-g M-o.
;;
;; The structure is ... take a string x which we'll say contains "M". Then (rx
;; word-start x not-wordchar graphic) should match a step that
;; contains M- in a key sequence. Make a list of these, say, and find those
;; where it occurs again.
;; Or do it all in one regexp:... wip
;; (rx line-start (* (not space)) "M" eow
;;     (* (seq
;;         (group (* nonl)) bow "M" eow))
;;     )

;; (map-keymap (lambda (ev def)
;;               (let* ((case-fold-search nil)
;;                      (key (key-description (list ev)))
;;                      (newkey (replace-regexp-in-string
;;                               (rx line-start (* (not space)) "C" eow
;;                                   (+ (seq
;;                                       (group (* nonl)) bow "C" eow)))
;;                               "\\1Y"
;;                               key)))
;;                 (and (esm-of-interest def)
;;                      (print key)
;;                      (print newkey))))
;;             'Control-X-prefix)

;; A far neater way of going about it. See, control-x-prefix
;; isn't really included when you scan global-map, nor any other subkeymap.
;; Well, they are, M-a is bound to ((a horrific (list) of (lists))), but you
;; can decline to operate on those and instead recurse into them. That is,
(defun esm-flatten-keymap (keymap)
  (map-keymap (lambda (ev def)
                (when (keymapp def)
                  (esm-flatten-keymap def))
                (and (commandp def)
                     (esm-of-interest def)
                     ;; do things
                     )))
  keymap)
;; Ok, now realize that the "events" at each level do not involve sequences. So
;; the hairiest you'd see is "C-M-S-k". A given modifier only appears once.
;; The simplest you'd see is just "k". So, if a leaf occurs twice in a map,
;; simply overlay one of them on all the others.


(defun esm-flatten-keymap-2 (keymap)
  (map-keymap
   (lambda (ev def)
     (when (keymapp def)
       (esm-flatten-keymap-2 def)) ;; recurse
     (when (and (commandp def)
                (esm-of-interest def))
       (let* ((key (key-description (list ev)))
              (leaf (esm-get-leaf key)))
         (when (not (equal key leaf))
           (define-key keymap ev (lookup-key keymap (kbd leaf))))))))
  keymap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defvar esm-C-x-C-key-overwrites-C-x-key t)

(defun esm-flatten-ctl-x ()
  (if esm-C-x-C-key-overwrites-C-x-key
      (dolist (x (esm-get-hydra-keys))
        (unless   (eq nil (key-binding (kbd (concat "C-x C-" x))))
          (global-set-key (kbd (concat "C-x " x))
                          (key-binding (kbd (concat "C-x C-" x))))))
    (dolist (x (esm-get-hydra-keys))
      (unless   (eq nil (key-binding (kbd (concat "C-x " x))))
        (global-set-key (kbd (concat "C-x C-" x))
                        (key-binding (kbd (concat "C-x " x)))))))
                                        ;(print "Flattened ctl-x-map." (esm-debug-buffer))
  )

;; Generalized flatten-ctl-x
(defun esm-overlay (here lower-stem upper-stem)
  "Duplicate bindings on UPPER-STEM so they also exist on LOWER-STEM,
overwriting the latter for those leaves where both are bound."
  (dolist (leaf (esm-get-hydra-keys))
    (let ((upper-cmd (lookup-key here (kbd (concat upper-stem leaf)))))
      (when (esm-of-interest upper-cmd)
        (define-key here (kbd (concat lower-stem leaf))
          upper-cmd)))))

;; More smartly written
(defun esm-restem-all-leaves (here lower-stem upper-stem)
  "Duplicate bindings on UPPER-STEM so they also exist on LOWER-STEM,
overwriting the latter for those leaves where both are bound."
  (dolist (leaf (esm-get-hydra-keys))
    (esm-restem* here leaf lower-stem upper-stem)))

(defun esm-restem* (here leaf new-stem reference-stem)
  (let ((ref-cmd (lookup-key here (kbd (concat reference-stem leaf)))))
    (when (esm-of-interest ref-cmd)
      (define-key here (kbd (concat new-stem leaf))
        ref-cmd))))

(defun esm-restem (reference-stem leaf new-stem)
  (global-set-key (kbd (concat new-stem leaf))
                  (global-key-binding (kbd (concat reference-stem leaf)))))

;;  rename to esm-new-leaf? esm-copy-leaf?
(defun esm-releaf (here stem new-leaf reference-leaf)
  (define-key here (kbd (concat stem new-leaf))
    (lookup-key here (kbd (concat stem reference-leaf)))))


(defun esm-copy-key (prefix new-key reference-key)
  (global-set-key (kbd (concat prefix new-key))
                  (global-key-binding (kbd (concat prefix reference-key)))))

;; Allows cmd to be a keymap, this is intended
(defun esm-of-interest (cmd)
  (and (not (eq cmd nil))
       (not (eq cmd 'self-insert-command))))

;; TODO: make a fn that works for unnamed prefix maps
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

;; experimental
(defmacro esm-with (map &rest body)
  "Run BODY with the following exposed variables.

Variable 'qmap' refers to MAP.
Variable 'qbackup' refers to the backup of MAP.

Note that they are quoted, so to refer to the value of backup (the
full keymap spec rather than its name), run (eval backup).

The variable 'qmap' exists for convenience, for when you call
this as a top-level form. The convenience is to not have to enter
the full keymap name, such as follows.

(esm-with org-clock-mode-line-map
  (do stuff with qmap))

However, you'll probably take the keymap name from a function
parameter, such as follows.

(defun do-something (map)
  (esm-with map
    ...))

in which case you can just continue to say map inside the
function body instead of qmap, as it is defined. The difference
is that qmap is quoted, whereas map will evaluate to the full
keymap spec."
  `(let ((qbackup (intern (concat "esm-backup-" (symbol-name ',map))))
         (qmap ',map))
     ,@body))

(defun esm-flatten-keymap (keymap)
  "An example of KEYMAP is org-mode-map. Only acts on stuff under C-c for now."
  (dolist (key (esm-get-hydra-keys))
    (define-key keymap (kbd (concat "C-c " key))
      (lookup-key keymap (kbd (concat "C-c C-" key))))))

(defun esm-super-translate-to-ctl-meta ()
  (dolist (key esm-all-keys-on-keyboard)
    (define-key key-translation-map
      (kbd (concat "s-" key)) (kbd (concat "C-M-" key)))))

;; unused
(defun esm-super-map-from-ctl-meta ()
  (dolist (key esm-all-keys-on-keyboard)
    (global-set-key (kbd (concat "s-" key))
                    (global-key-binding (kbd (concat "C-M-" key))))
    (eval-after-load 'smartparens
      `(let ((cmd (lookup-key smartparens-mode-map (kbd (concat "C-M-" ,key)))))
         (define-key smartparens-mode-map (kbd (concat "s-" ,key))
           (if (symbolp cmd) cmd nil))))

    ;; Alternatively
    ;; (ignore-errors
    ;;   (define-key smartparens-mode-map (kbd (concat "s-" (string key)))
    ;;     (lookup-key smartparens-mode-map (kbd (concat "C-M-" (string key))))))
    )

  ;; Also bind C-M-left and other special keys for smartparens.
  ;; There is potential here for a macro.
  (map-keymap
   (lambda (φ1 φ2)
     (let* ((naive (key-description (vector φ1)))
            (keystring (if (or (string-match "^C-M-<" naive)
                               (string-match "^C-<M-" naive))
                           (concat "<C-M-" (substring naive 5))
                         naive)))
       ;; Needed to bind alphanumeric C-M-keys. Not needed for left, right,
       ;; delete etc.
       ;; Similar needed for any subkeymaps, if you have them.
       ;; (if (eq φ1 meta-prefix-char)
       ;;     (let ((metakeys (cdr (lookup-key smartparens-mode-map
       ;;                                      (vector meta-prefix-char)))))
       ;;       (mapc
       ;;        (lambda (conscell)
       ;;          (define-key smartparens-mode-map
       ;;            (kbd (concat "s-" (key-description (vector (car conscell)))))))
       ;;        metakeys)))
       (if (string-match "^C-M-" keystring)
           (define-key smartparens-mode-map
             (kbd (concat "s-" (substring keystring 4)))
             φ2)
         (if (string-match "^<C-M-" keystring)
             (define-key smartparens-mode-map
               (kbd (concat "<s-" (substring keystring 5)))
               φ2)))))
   smartparens-mode-map))

(defmacro esm-flatten-shift-map ()
  "Make it not matter whether or not Shift is pressed."
  `(progn ,@(cl-mapcar
             (lambda (loser winner)
               `(progn (esm-copy-key "C-" ,loser ,winner)
                       (esm-copy-key "M-" ,loser ,winner)
                       (esm-copy-key "C-x " ,loser ,winner)
                       (esm-copy-key "C-x C-" ,loser ,winner)
                       (esm-copy-key "C-M-" ,loser ,winner)))
             (split-string "~!@#$%^&*()_+{}:<>?\"|" "" t) ;; loser: default binds overwritten
             (split-string "`1234567890-=[];,./'\\" "" t) ;; winner: its commands stay
             )))

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






(defun esm-super-from-ctl-2 ()
  (map-keymap (lambda (ev def)
                (let* ((case-fold-search nil)
                       (key (key-description (list ev)))
                       (newkey (replace-regexp-in-string
                                (rx word-start "C" word-end) "s" key t)))
                  (and (esm-of-interest def)
                       (not (equal key newkey))
                       (define-key global-map (kbd newkey) def))))
              global-map))


;; even better: map-keymap (key-description ) regexp replace
(defvar esm-stem-equivalences
  '(("C-"      "M-"      "s-")
    ("C-x "    "M-x "    "s-x ")
    ("C-x C-"  "M-x M-"  "s-x s-")

    ;; better
    ("C-" "C-x " "C-x C-")
    ("M-" "M-x " "M-x M-")
    ("s-" "s-x " "s-x s-")))

;; unused
(defun esm-super-from-ctl* ()
  (let ((reference (nth 1 esm-stem-equivalences))
        (new       (nth 3 esm-stem-equivalences)))
    (dolist (i (length reference))
      (dolist (leaf esm-all-keys-on-keyboard)
        (esm-restem (nth i reference) leaf (nth i new))))))


(defun esm-super-from-ctl ()
  (dolist (x esm-all-keys-on-keyboard)
    (esm-restem "C-"     x "s-")
    (esm-restem "C-x "   x "s-x ")
    (esm-restem "C-x C-" x "s-x s-")
    (esm-restem "C-h "   x "s-h ")
    (esm-restem "C-h C-" x "s-h s-")
    )
  (define-key key-translation-map (kbd "s-g") (kbd "C-g")))

(defun esm-hyper-alt-maps-from-ctl-meta-maps ()
  "Populate the right half of the keyboard with Hyper and Alt
hotkeys, unbinding the Control and Meta hotkeys.

WARNING: Destructive, run only once."

  ;; Due to the way this loop works, most combination hotkeys like C-M-k get
  ;; lost, and good riddance.
  (dolist (letter (string-to-list "yuiophjklnm[];',./\\"))
    (global-set-key (kbd (concat "H-" (string letter)))
                    (global-key-binding (kbd (concat "C-" (string letter)))))
    (global-set-key (kbd (concat "A-" (string letter)))
                    (global-key-binding (kbd (concat "M-" (string letter)))))

    (unless (or (eq letter (string-to-char "m")) ;; do not unbind RET
                (eq letter (string-to-char "i")) ;; do not unbind TAB
                (eq letter (string-to-char "h")) ;; do not unbind bksp
                (eq letter (string-to-char "["))) ;; do not unbind ESC
      (global-unset-key (kbd (concat "C-" (string letter)))))

    (global-unset-key (kbd (concat "M-" (string letter)))))

  ;; because binding to the command C-m etc is bound to isn't the same thing
  (define-key key-translation-map (kbd "H-m") (kbd "<RET>"))
  (define-key key-translation-map (kbd "H-i") (kbd "TAB"))
  (define-key key-translation-map (kbd "H-[") (kbd "<ESC>"))
  (define-key key-translation-map (kbd "H-h") (kbd "<DEL>"))

  ;; Change right Ctl and Meta into Hyper and Alt
  (dolist (var esm-xmodmap-rules)
    (when (or (string-match "keycode 108" var)
              (string-match "keycode 105" var))
      (delq var esm-xmodmap-rules)))
  (push "keycode 108 = Hyper_R" esm-xmodmap-rules)
  (push "keycode 105 = Alt_R" esm-xmodmap-rules)
  (esm-xmodmap-reload))

(provide 'escape-modality-enforce-tidy)
