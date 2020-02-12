;; Assumptions:
;; - Control-Meta bindings don't exist
;; - Shift doesn't exist
;; - Keymaps are "flattened"

(require 'hercules)
(require 'which-key)
(require 'escape-modality-common)
(require 'escape-modality-x11)

(defun esm-filter (stem binding-list)
  (seq-filter (lambda (x) (string-match (rx bol (literal stem) anychar eol) (car x)))
              binding-list))


(defun esm-bind-into (keymap cell)
  "CELL is a cons cell like (\"C-a\" . \"move-beginning-of-line\")."
  (let ((def (if (string= "Prefix Command" (cdr (cell)))
                  (key-binding (kbd (car cell)))
                (intern (cdr cell)))))
    (if (commandp def)
        ;; Bind only the leaf -- e.g. take C-a and rebind it on a.
        (define-key keymap (kbd (esm-get-leaf (car cell))) def)
      (if (keymapp def)
          (when (keymapp def)
            (hercules-def
             :keymap def
             :show-funs (intern (car cell)))
            (define-key keymap (car cell) (intern (car cell)))
            (mapc (lambda (cell)
                    (esm-bind-into def cell))
                  def)

            )
          ;; Here we have a keymap variable. It should be herculized, of
          ;; course, but we also want to recurse into it to herculize nested
          ;; keymaps...
          ;; Maybe the solution is to simply bind def like above, and make it a
          ;; Hercules separately. But that is prima facie not doable unless
          ;; hercules-def is smart enough to enable something like
          ;; (hercules-def :toggle-funs (key-binding "M-g") ...).
          ;; Probably necessary to give a name to every subkeymap, herculize
          ;; them and then bind them.
        ))))

(map-keymap
 )


(def)

(setq esm-basemap-control (make-sparse-keymap))
(setq esm-basemap-meta    (make-sparse-keymap))
(setq esm-basemap-super   (make-sparse-keymap))

(let ((binding-list (which-key--get-current-bindings)))
  (setq esm-bindings-control (esm-filter "C-" binding-list))
  (setq esm-bindings-meta    (esm-filter "M-" binding-list))
  (setq esm-bindings-super   (esm-filter "s-" binding-list)))

(mapc (lambda (cell) (esm-bind-into esm-basemap-control cell))
      esm-bindings-control)
(mapc (lambda (cell) (esm-bind-into esm-basemap-meta    cell))
      esm-bindings-meta)
(mapc (lambda (cell) (esm-bind-into esm-basemap-super   cell))
      esm-bindings-super)

(hercules-def
 :show-funs #'esm-mode-control
 :keymap 'esm-basemap-control)

(general-def "<f9>" #'esm-mode-control)

(esm-xmodmap-reload)
(esm-xcape-reload)

;;(describe-keymap my-control-map);; from help-fns+
;;(which-key-show-keymap 'my-control-map)

(lookup-key (current-global-map) (kbd "M-g"))

(with-temp-buffer
  (describe-buffer-bindings (current-buffer)))

(setq foo (current-active-maps))


;; Ok, if the command is "Prefix Command", we will loop through
;; (current-active-maps) using seq-find, looking for the key in question to get
;; its definition (a keymap value instead of "Prefix Command"). The
;; (current-active-maps) should be sorted with global map coming first.


;; Ok, if the command is "Prefix Command", we will simply do (key-binding key)
