;; escape-modality.el
;; Copyright (C) 2019 Martin Edström

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

;; warnings: odd behavior if a head that calls another hydra is not set to exit

(require 'hydra)
(require 'escape-modality-common)
(require 'escape-modality-x11)
(require 'escape-modality-enforce-tidy)
;; (require 'keymap-utils)
(eval-when-compile (require 'subr-x)) ;; string-join
(autoload #'which-key--get-current-bindings "which-key")

;;;;;;;;;;;;;;;
;;; Micro level

(define-minor-mode deianira-mode
  "Bind hydras everywhere there used to be a prefix key.

Also bind hydras on Control, Meta etc if `esm-xcape-rules' area
enabled and functional. "
  nil
  " Δ"
  `((,(kbd "<f35>") . esm-control/body)
    (,(kbd "<f34>") . esm-meta/body)
    (,(kbd "<f33>") . esm-super/body)))

(define-globalized-minor-mode deianira-global-mode deianira-mode deianira-mode)

;; TODO: adapt to changing frame sizes
(defvar esm-colwidth
  (let ((optimal (- (round (frame-parameter nil 'width) 10) 4)))
    (max optimal 8)))

(defun esm-colwidth ()
  (setq esm-colwidth
        (let ((optimal (- (round (frame-parameter nil 'width) 10) 4)))
          (max optimal 8))))

(defvar esm-quitters '(;;"C-q"
                       "C-s" "C-g" "s-s" "s-g" "C-u" "M-x" "M-u" "s-u" "C-2"
                       "C-c c"))

(defvar esm-noquitters '("C-2 t" "C-2 n" "C-2 p" "C-x ;" "C-x x" "C-x q"
                           "C-x h" "C-x u" "C-x i" "C-x p" "C-x l" "C-x 1"
                           "C-x 2" "C-x 3" "C-x 0" "C-c c"
                           "C-c C-c" ;;testing
                           ))

;; Inspired by which-key--get-current-bindings. Thanks!
(defun esm-current-bound-keys (&optional keep flush)
  "Optional argument KEEP is a regexp describing keys to keep (meaning discard
keys that don't match). If nil, keep all.

Optional argument FLUSH is a regexp describing keys to discard after the above
filter has been applied."
  (let ((keys '())
        (ignore-keys (eval-when-compile
                       (regexp-opt '("mouse-" "remap" "scroll-bar" "select-"
                                     "switch-" "help" "-state" "which-key-"
                                     "-corner" "-divider" "-edge" "header-"
                                     "mode-line" "tab-" "vertical-line"
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
      (describe-buffer-bindings (current-buffer))
      (goto-char (point-min))
      (flush-lines ignore-bindings)
      (flush-lines (rx (regexp ignore-sections)
                       (* (not ""))))
      (flush-lines (rx "---"))
      (flush-lines (rx bol "key" (* nonl) "binding"))
      (flush-lines (rx bol (* nonl) ":" eol))
      (flush-lines (rx ""))
      (flush-lines (rx " .. "))
      (while (re-search-forward (rx "	" (* nonl))
                                nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward (rx "<" (group (or "C-" "M-" "s-" "H-" "A-")))
                                nil t)
        (replace-match "\\1<")
        (goto-char (point-min)))
      (flush-lines ignore-keys)
      (when keep
        (keep-lines keep))
      (when flush
        (flush-lines flush))
      (while (re-search-forward (rx (+ nonl)) nil t)
        (push (match-string 0) keys)))
    (seq-sort-by #'length #'< keys)))

;; (setq foo (esm-current-bound-keys))

;; TODO: rename more descriptively
(defun esm-is-a-subhydra (stem leaf)
  "Return the hydra body in question."
  (if-let ((x (cdr (assoc (concat stem leaf) esm-live-hydras))))
      (intern (concat x "/body"))))

(defun esm-universal-arg (arg)
  (interactive "P")
  (prefix-command-preserve-state)
  (call-interactively
   (intern (concat
            (substring (symbol-name hydra-curr-body-fn) 0 -5) ;; chop "/body"
            "-nonum/body")))
  (hydra--universal-argument arg))

;; unused
;; inspired by esm-cc-cc
;; but what commands need it?
(defun esm-wrapper (hotkey)
  (call-interactively (key-binding (kbd hotkey)))
  (cond ((string-match "^C-" hotkey)
         (esm-control/body))
        ((string-match "^M-" hotkey)
         (esm-meta/body))
        ((string-match "^s-" hotkey)
         (esm-super/body))))

(defun esm-cc-cc (arg)
  (interactive "P")
  ;; (prefix-command-preserve-state)
  ;; (setq unread-command-events (listify-key-sequence "c"))
  ;; (call-interactively (key-binding (kbd "C-c c"))) ;; if flattened
  (call-interactively (key-binding (kbd "C-c C-c")))
  (esm-control/body))

(defvar esm--original-cursor-color (face-background 'cursor))

(defun esm-colorize-cursor ()
  (let ((curr (symbol-name hydra-curr-body-fn)))
    (cond ((string-match "^esm-C\\|control" curr) (set-cursor-color "green"))
          ((string-match "^esm-M\\|meta" curr) (set-cursor-color "red"))
          ((string-match "^esm-CM\\|super" curr) (set-cursor-color "blue")))))

(defun esm-decolorize-cursor ()
  (set-cursor-color esm--original-cursor-color))

(defun esm-is-known-prefix (stem leaf)
  (car-safe
   (member (key-binding (kbd (concat stem leaf))) esm-known-prefix-commands)))

;; TODO: Stop needing this. I think (keymapp) is reliable. Just see
;; (key-binding (kbd "C-x"))
;; (keymapp (key-binding (kbd "C-x")))
;; (key-binding (kbd "C-x a"))
;; (keymapp (key-binding (kbd "C-x a")))
;; (keymapp 'Control-X-prefix)
(defvar esm-known-prefix-commands '(2C-command
                                    Control-X-prefix
                                    ESC-prefix
                                    ctl-x-4-prefix
                                    ctl-x-5-prefix
                                    facemenu-keymap
                                    help-command
                                    ehelp-command
                                    kmacro-keymap
                                    mode-specific-command-prefix
                                    projectile-command-map
                                    vc-prefix-map))

(defun esm-cmd (stem leaf)
  (key-binding (kbd (concat stem leaf))))

(defun esm-is-unbound (stem leaf)
  (eq nil (esm-cmd stem leaf)))

(defun esm-is-bound (stem leaf)
  (not (esm-is-unbound stem leaf)))

(defun esm-is-unnamed-prefix (stem leaf)
  (not (symbolp (esm-cmd stem leaf))))

(defun esm-valid-keydesc (keydesc)
  "Check that KEYDESC is something you'd pass to `kbd', not a dangling stem."
  (or (string-match (rx "--" eol) keydesc)
      (string-match (rx " -" eol) keydesc)
      (string-match (rx (not (any "- ")) eol) keydesc)))

;; TODO: What about this case: (esm-dub-from-key (esm-normalize "C-- - -"))
(defun esm-dub-from-key (keydesc)
  "Example: If KEY is the string \"C-x a\", return \"esm-Cxa\"."
  (let ((squashed (string-join (split-string keydesc (rx (any " -"))))))
    (if (string-match (rx "-" eol) keydesc)
        (concat "esm-" squashed "-")
      (concat "esm-" squashed))))

;;;;;;;;;;;;;;;;;;;;;
;;; Micro-macro level

;; unused
(defun esm-drop-last-chord-in-seq (keydesc)
  (if (esm-valid-keydesc keydesc)
      (replace-regexp-in-string (rx space (*? (not space)) eol) "" keydesc)))

;; "List of keys like C-a, C-e, M-f, M-g but not C-M-f or M-%."
;; note: it's not quite all of them since it uses esm-hydra-keys
(defvar esm-all-duo-chords
  (let (chords)
    (mapc (lambda (char)
            (push (concat "C-" (string char)) chords)
            (push (concat "M-" (string char)) chords)
            (push (concat "s-" (string char)) chords))
          esm-hydra-keys)
    chords))

(defun esm-corresponding-hydra (stem leaf)
  (intern (concat
           (esm-dub-from-key (esm-normalize (concat stem leaf)))
           "/body")))

(defun esm-head-cmd (stem leaf)
  (cond ((not (esm-of-interest (esm-cmd stem leaf)))
         leaf) ;; make a blank spot
        ((keymapp (esm-cmd stem leaf))
         (esm-corresponding-hydra stem leaf))
        ((string= (concat stem leaf) "C-c c")
         #'esm-cc-cc)
        ((string= (concat stem leaf) "C-g")
         #'keyboard-quit)
        ((string= leaf "u")
         #'esm-universal-arg)
        (t `(call-interactively (key-binding (kbd ,(concat stem leaf)))))))

(defun esm-head-hint (stem leaf)
  (let* ((sym (or (esm-is-a-subhydra stem leaf)
                  (esm-is-known-prefix stem leaf)
                  (key-binding (kbd (concat stem leaf)))))
         (name (if (symbolp sym) (symbol-name sym) " ")))
    (if (string= name "nil")
        " "
      (if (> (length name) (esm-colwidth))
          (substring name 0 esm-colwidth)
        name))))

(defun esm-head-key (stem leaf)
  "If the given hotkey is bound to a command, return LEAF,
otherwise return a space character. This can be used to
make a visibly blank spot in a hydra for hotkeys that are unbound."
  (if (esm-of-interest (esm-cmd stem leaf))
      leaf
    " "))

(defun esm-head-exit (stem leaf &optional exit-almost-never?)
  (cond ((member (concat stem leaf) esm-quitters) '(:exit t))
        ((member (concat stem leaf) esm-noquitters) '(:exit nil))
        ((esm-is-a-subhydra stem leaf) '(:exit t)) ;; important
        ((esm-is-unbound stem leaf) '(:exit t))
        (exit-almost-never? '(:exit nil))
        (t '()))) ;; defer to hydra's default behavior

(defun esm-head (stem leaf)
  "Return a \"head\" specification, in other words a list in the
form (KEY COMMAND HINT EXIT) as desired by `defhydra'. "
  `( ,(esm-head-key stem leaf) ,(esm-head-cmd stem leaf) ,(esm-head-hint stem leaf)
     ,@(esm-head-exit stem leaf)))

(defun esm-head-invisible (stem leaf)
  `( ,(esm-head-key stem leaf) ,(esm-head-cmd stem leaf) nil
     ,@(esm-head-exit stem leaf 'almost-never)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro level: tying it all together.

(defmacro esm-defmode
    (name stem &optional doctitle pop-key parent exit keys pop-key-2)
  "Make a hydra with many heads.

Optional argument EXIT controls whether the hydra's heads exit by
default (overridden on a case-by-case basis by `esm-quitters').
Optional argument KEYS is a string specifying which keys to
display in the hydra hint, defaulting to the value of
`esm-hydra-keys'."

  `(defhydra ,name (nil nil :columns 10 :exit ,exit
                        ;; :body-pre (esm-colorize-cursor)
                        ;; (setq exwm-input-line-mode-passthrough t)
                        ;; :post (esm-decolorize-cursor)
                        ;; (setq exwm-input-line-mode-passthrough nil)
                        ;; :foreign-keys run
                        )
     ,@(if doctitle (list doctitle) '())
     ,@(mapcar (lambda (leaf) (esm-head stem (string leaf)))
               (or keys esm-hydra-keys))
     ,@(mapcar (lambda (leaf) (esm-head-invisible stem leaf))
               '("<left>" "<right>" "<up>" "<down>" "<SPC>" "=" "\\" "'" "`"))
     ,@(mapcar (lambda (chord) (esm-head-invisible "" chord))
               esm-all-duo-chords)
     ,@(let (extras)
         (mapc (lambda (candidate) (if (car candidate) (push (cdr candidate) extras)))
               ;; List of extra heads to go in the hydra.
               ;; For example ("C-f" . ("C-f" nil nil :exit t))
               `((,pop-key . (,pop-key nil nil :exit t))
                 (,pop-key-2 . (,pop-key-2 nil nil :exit t))
                 (,parent . ("<backspace>" ,parent nil :exit t)))) extras)))

(defvar esm-live-hydras nil)

(defun esm-defmode-maybe (key)
  (when (keymapp (key-binding (kbd key)))
    (let* ((x            (esm-dub-from-key (esm-normalize key)))
           (title        (intern x))
           (nonum-title  (intern (concat x "-nonum")))
           (nonum-keymap (intern (concat x "-nonum/keymap")))
           (hint         (intern (concat x "/hint")))
           (heads        (intern (concat x "/heads")))
           (body         (intern (concat x "/body")))
           (keymap       (intern (concat x "/keymap")))
           (stem-for-children (concat key " "))
           (parent-key (substring key 0 -2))
           (parent-symname (cdr (assoc parent-key esm-live-hydras)))
           (parent-body (cond ((string= parent-key "C") #'esm-control/body)
                              ((string= parent-key "M") #'esm-meta/body)
                              ((string= parent-key "s") #'esm-super/body)
                              ;; problem: may not exist yet
                               ((assoc parent-key esm-live-hydras)
                               (intern (concat parent-symname "/body"))))))
      (eval `(progn
               (esm-defmode ,title       ,stem-for-children ,key nil ,parent-body t)
               (esm-defmode ,nonum-title ,stem-for-children ,(concat "\n\n" key) nil ,body t ,esm-hydra-keys-nonum)
               (define-key ,keymap "u" #'esm-universal-arg)
               (define-key ,nonum-keymap "u" #'hydra--universal-argument)
               (dotimes (i 10)
                 (let ((key (kbd (int-to-string i))))
                   (when (eq (lookup-key ,keymap key) 'hydra--digit-argument)
                     (define-key ,keymap key nil))))
               (cons ,key ,x)
               ))
      ;; (set hint `(eval (hydra--format nil '(nil nil :columns 10)
      ;;                                 ,key (esm-update-hints ,heads))))
      )))

(defun esm-generate-hydras ()

  (setq esm-live-hydras nil)
  (setq esm-hydra-keys-nonum (esm-hydra-keys-nonum))
  (dolist (key (esm-current-bound-keys (rx bol (or "s-" "M-"))
                                       (rx (or "C-" "ESC"))))
    (push (esm-defmode-maybe key) esm-live-hydras))
  (setq esm-live-hydras (seq-reverse (seq-remove 'null esm-live-hydras)))

  (esm-defmode esm-control       "C-"  "CONTROL" "<f35>")
  (esm-defmode esm-meta          "M-"  "META"  "<f34>")
  (esm-defmode esm-super         "s-"  "SUPER" "<f33>" nil nil nil "s-<f33>")

  (esm-defmode esm-control-nonum "C-"  "\nCONTROL" "C-<f13>" esm-control/body nil "qwertyuiopasdfghjkl;zxcvbnm,./")
  (esm-defmode esm-meta-nonum    "M-"  "\nMETA"    "M-<f14>" esm-meta/body nil "qwertyuiopasdfghjkl;zxcvbnm,./")
  (esm-defmode esm-super-nonum   "s-"  "\nSUPER"   "s-<f15>" esm-super/body nil "qwertyuiopasdfghjkl;zxcvbnm,./")

  ;; Clean up digit argument bindings
  (dolist (map (list esm-control/keymap
                     esm-meta/keymap
                     esm-super/keymap))
    (dotimes (i 10)
      (let ((key (kbd (int-to-string i))))
        (when (eq (lookup-key map key) 'hydra--digit-argument)
          (define-key map key nil)))))

  (define-key esm-control-nonum/keymap "u" #'hydra--universal-argument)
  (define-key esm-meta-nonum/keymap    "u" #'hydra--universal-argument)
  (define-key esm-super-nonum/keymap   "u" #'hydra--universal-argument)
  )

;; I guess it's not really the whole keyboard
(defun esm-whole-keyboard ()
  (let (biglist)
    (dolist (chord esm-all-duo-chords)
      (dolist (key esm-all-keys-on-keyboard-except-shifted-symbols)
        (push (concat chord " " key) biglist))
      (push chord biglist))
    biglist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change Emacs state. Should go in user init file.

(defun esm-config ()
  ;; Stage 1

  (setq esm-debug "*Escape-modality debug*")
  (global-set-key (kbd "C-x e") #'eval-last-sexp)
  (global-set-key (kbd "C-x f") #'find-file)
  (global-set-key (kbd "C-x c") #'save-buffers-kill-emacs)
  (global-set-key (kbd "C-x x") #'exchange-point-and-mark)
  (global-set-key (kbd "C-x o") #'delete-blank-lines)

  ;; (esm-flatten-ctl-x)

  ;; Stage 2
  ;; (esm-kill-shift)

  ;; Stage 3
  (esm-super-from-ctl global-map)

  ;; Stage 4
  (esm-generate-hydras)

  ;; (add-hook 'buffer-list-update-hook #'esm-generate-hydras)


  )

(provide 'escape-modality)
