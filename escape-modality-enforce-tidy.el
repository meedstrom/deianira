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

(defun esm-copy-key (prefix new-key reference-key)
  (global-set-key (kbd (concat prefix new-key))
                  (global-key-binding (kbd (concat prefix reference-key)))))

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

(defun esm-restem (reference-stem leaf new-stem)
  (global-set-key (kbd (concat new-stem leaf))
                  (global-key-binding (kbd (concat reference-stem leaf)))))

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
