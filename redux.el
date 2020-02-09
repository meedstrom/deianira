;; Assumptions:
;; - Control-Meta bindings don't exist
;; - Shift doesn't exist
;; - Keymaps are "flattened"

(require 'hercules)
(require 'which-key)
(require 'general)
(require 'escape-modality-common)
(require 'escape-modality-x11)

(defun esm-filter (stem binding-list)
  (seq-filter (lambda (x) (string-match (rx bol (literal stem) anychar eol) (car x)))
              binding-list))

(defun esm-bind-into (cell keymap)
  (when (commandp (intern (cdr cell)))
    (define-key keymap (kbd (car cell)) (intern (cdr cell)))))

(let ((binding-list (which-key--get-current-bindings)))
  (setq esm-bindings-control (esm-filter "C-" binding-list))
  (setq esm-bindings-meta    (esm-filter "M-" binding-list))
  (setq esm-bindings-super   (esm-filter "s-" binding-list)))

(setq esm-basemap-control (make-sparse-keymap))
(setq esm-basemap-meta    (make-sparse-keymap))
(setq esm-basemap-super   (make-sparse-keymap))

(mapc (lambda (cell) (esm-bind-into esm-basemap-control cell))
      esm-bindings-control)
(mapc (lambda (cell) (esm-bind-into esm-basemap-meta    cell))
      esm-bindings-meta)
(mapc (lambda (cell) (esm-bind-into esm-basemap-super   cell))
      esm-bindings-super)



(mapc (lambda (cell)
        (when (commandp (intern (cdr cell)))
          (define-key esm-basemap-control (kbd (car cell)) (intern (cdr cell)))))
      esm-bindings-control)

;; Pure which-key solution (no persistence, sucks)
(general-def "<f9>" esm-control-map)



(hercules-def
 :toggle-funs #'esm-mode-control
 :keymap 'esm-basemap-control)

(general-def "<f9>" #'esm-mode-control)


(esm-xmodmap-reload)
(esm-xcape-reload)

;;(describe-keymap my-control-map);; from help-fns+
;;(which-key-show-keymap 'my-control-map)
