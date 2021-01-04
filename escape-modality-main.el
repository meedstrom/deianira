;; escape-modality-main.el -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Martin Edström

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

(require 'escape-modality-deianira)
(eval-when-compile (require 'subr-x))

(define-minor-mode escape-modality-mode
  "Instruct Deianira-mode to create root hydras. "
  nil
  " ESM"
  `(;;(,(kbd "<f35>") . esm-control/body)
    (,(kbd "<f34>") . esm-meta/body)
    (,(kbd "<f33>") . esm-super/body)))

;;;###autoload
(define-globalized-minor-mode escape-modality-global-mode
  escape-modality-mode
  escape-modality-mode)

(defun escape-modality ()
  "Turn on the whole paradigm described in `(info \"(elisp)\")'. "
  (interactive "P")
  (prefix-command-preserve-state)
  ;; (call-interactively #'massmap-tidy-mode)
  (call-interactively #'escape-modality-global-mode)
  (call-interactively #'deianira-global-mode)
  )

;; performant alternative to the old method (= 1 (esm--key-seq-steps x))
(defun esm--key-seq-steps=1 (keydesc)
  (not (string-match " " keydesc)))

(defvar esm-assume-no-multi-chords nil
  "Assumption: There exists no keys or key sequences, in any
keymap, that has a multi-chord as the first key. In other words,
bindings like C-M-f don't exist.

A non-nil setting makes some code run faster, but may cause
errors if the assumption is false.")

(defun esm-mirror-root-maps ()
  (dolist (x esm-last-bindings-for-external-use)
    ;; each x has the form ("C-x a" . "move-beginning-of-line")
    (when (esm--key-seq-steps=1 (car x))
      (if-let ((key (car x))
                ;; root should be a cons cell of the form ("C-" . control-map)
                (root (cond ((string-match (rx line-start "C-") key)
                             (cons "C-" esm-root-control-map))
                            ((string-match (rx line-start "M-") key)
                             (cons "M-" esm-root-meta-map))
                            ((string-match (rx line-start "s-") key)
                             (cons "s-" esm-root-super-map))
                            (t nil)))
                (key-to-bind (if esm-assume-no-multi-chords
                                 (substring key 2)
                               (replace-regexp-in-string (car root) "" key))))
          (define-key (cdr root)
            (kbd key-to-bind)
            (if (equal (cdr x) "Prefix Command")
                (key-binding (kbd (car x)))
              (intern (cdr x))))))))

;; (esm-generate-hydras-async)
;; (keymapp 'esm-root-super-command)
;; (esm-mirror-root-maps)

;; (add-hook 'esm-after-scan-bindings-hook #'esm-mirror-root-maps)
;; they come in a specific order, see (key-description (kbd "C-S-M-s-H-A-5"))
(define-prefix-command 'esm-root-alt-map)
(define-prefix-command 'esm-root-control-map)
(define-prefix-command 'esm-root-hyper-map)
(define-prefix-command 'esm-root-meta-map)
(define-prefix-command 'esm-root-super-map)

(eval '(esm-define-stem-hydra "M-"))
(eval '(esm-define-stem-hydra "s-"))

(provide 'escape-modality-main)

;;; escape-modality-main.el ends here
