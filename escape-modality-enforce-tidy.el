;; escape-modality-enforce-tidy.el  -*- lexical-binding: t; -*-
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

;;;; EDUCATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instructive prior art: which-key--get-keymap-bindings
;; Also see results of evaluating
;; (map-keymap 'readkey1 global-map)
;; (map-keymap 'readkey2 global-map)

(defun readkey1 (ev def)
  (print (key-description (list ev)) (get-buffer-create "*scratch*"))
  (print def (get-buffer-create "*scratch*")))

(defun readkey2 (ev def)
  (when (esm-of-interest def)
    (print (key-description (list ev)))
    (print def)))

;; Unfortunately C source, so you can't learn from them:
;; (substitute-command-keys "\\{org-mode-map}")
;; (accessible-keymaps global-map)

;; Other cool functions
;; (keyboard-translate)

;; map-keymap passes events to a function, and those events look like
;; (list 67108918)
;; and it turns out you can append events instead of concatting strings:
;; (key-description (append (kbd "C-x") (list 67108918)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun esm-super-translate-to-ctl-meta ()
  (dolist (key (esm-all-keys-on-keyboard))
    (define-key key-translation-map
      (kbd (concat "s-" key)) (kbd (concat "C-" key)))))

(defun esm-flatten-keymap (keymap)
  ;; (esm-backup-keymap keymap)
  (map-keymap
   (lambda (ev def)
     (when (keymapp def)
       (esm-flatten-keymap def)) ;; recurse
     (when (and (commandp def)
                (esm-of-interest def))
       (let* ((keydesc (key-description (list ev)))
              (leaf (esm-get-leaf keydesc)))
         (when (not (equal keydesc leaf))
           (define-key keymap ev (lookup-key keymap (kbd leaf)))))))
   keymap))

;; Generalized flatten-ctl-x
(defun esm-restem-all-leaves (here lower-stem upper-stem)
  "Duplicate bindings on UPPER-STEM so they also exist on LOWER-STEM,
overwriting the latter for those leaves where both are bound."
  (dolist (leaf (esm-hydra-keys-in-a-list))
    (esm-restem here leaf lower-stem upper-stem)))

(defun esm-restem (here leaf new-stem reference-stem)
  (let ((ref-cmd (lookup-key here (kbd (concat reference-stem leaf)))))
    (when (esm-of-interest ref-cmd)
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

;; (defun esm-super-from-ctl ()
;;   (map-keymap (lambda (ev def)
;;                 (let* ((case-fold-search nil)
;;                        (key (key-description (list ev)))
;;                        (newkey (replace-regexp-in-string
;;                                 (rx word-start "C" word-end) "s" key t)))
;;                   (and (esm-of-interest def)
;;                        (not (equal key newkey))
;;                        (define-key global-map (kbd newkey) def))))
;;               global-map))

(defun esm-super-from-ctl (map)
  (map-keymap (lambda (ev def)
                (let* ((case-fold-search nil)
                       (key (key-description (list ev)))
                       (newkey (replace-regexp-in-string
                                (rx word-start "C" word-end) "s" key t)))
                  (and (esm-of-interest def)
                       (not (equal key newkey))
                       (define-key map (kbd newkey) def)))
                (when (keymapp def)
                  (esm-super-from-ctl def)))
              map))


(provide 'escape-modality-enforce-tidy)
