;; escape-modality.el  -*- lexical-binding: t; -*-
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
(eval-when-compile (require 'subr-x)) ;; string-join, if-let
(autoload #'which-key--get-current-bindings "which-key")

;;;;;;;;;;;;;;;
;;; Micro level

(define-minor-mode deianira-mode
  "Bind hydras everywhere there used to be a prefix key. "
  nil
  " Δ")

(define-globalized-minor-mode deianira-global-mode
  deianira-mode
  deianira-mode)

(define-minor-mode escape-modality-mode
  "Bind Deianira root hydras on Control, Meta etc if `esm-xcape-rules'
are enabled and functional. Also flatten the keymap; see info. "
  nil
  " ESM"
  `((,(kbd "<f35>") . esm-control/body)
    (,(kbd "<f34>") . esm-meta/body)
    (,(kbd "<f33>") . esm-super/body)))

(define-globalized-minor-mode escape-modality-global-mode
  escape-modality-mode
  escape-modality-mode)

(defun escape-modality (&optional arg)
  "Bind Deianira root hydras on Control, Meta etc if `esm-xcape-rules'
are enabled and functional. Also flatten the keymap; see info. "
  (interactive "P")
  (prefix-command-preserve-state)
  (call-interactively #'escape-modality-global-mode)
  (call-interactively #'deianira-global-mode)
  ;; (call-interactively #'massmap-tidy-mode)
  )

(defun esm--colwidth ()
  (or esm-colwidth-override
      (let ((optimal (- (round (frame-parameter nil 'width) 10) 4)))
        (max optimal 8))))

(defvar esm-colwidth-override nil
  "An integer for the width of hydra hints. If nil, figure it out from the frame width.")

(defvar esm--colwidth
  (esm--colwidth))

(defvar esm-quitters '(;;"C-q"
                       "C-s" "C-g" "s-s" "s-g" "C-u" "M-x" "M-u" "s-u" "C-2"
                       "C-c c"))

(defvar esm-noquitters '("C-2 t" "C-2 n" "C-2 p" "C-x ;" "C-x x" "C-x q"
                           "C-x h" "C-x u" "C-x i" "C-x p" "C-x l" "C-x 1"
                           "C-x 2" "C-x 3" "C-x 0" "C-c c"
                           "C-c C-c" ;;testing
                           ))

;; Inspired by which-key--get-current-bindings. Thanks!
(defun esm-current-bindings (&optional keep flush)
  "Optional argument KEEP is a regexp describing keys to keep. Can be left at
nil to keep everything.

Optional argument FLUSH is a regexp describing keys to discard after the above
filter has been applied."
  (let ((result '())
        (buffer (current-buffer)) ;; because with-temp-buffer changes it
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
      (while (re-search-forward (rx "\n\t")
                                nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward (rx "<" (group (or "C-" "M-" "s-" "H-" "A-")))
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
    (seq-sort-by (lambda (x) (string-width (car x))) #'< result)))

;; (setq foo (esm-current-bindings))

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

(defun esm-valid-keydesc (keydesc)
  "Check that KEYDESC is something you'd pass to `kbd', not a dangling stem."
  (or (string-match (rx "--" eol) keydesc)
      (string-match (rx " -" eol) keydesc)
      (string-match (rx (not (any "- ")) eol) keydesc)))

;; TODO: What about this case: (esm-dub-from-key (esm-normalize "C-- - -"))
(defun esm-dub-from-key (keydesc)
  "Example: If KEY is the string \"C-x a\", return \"esm-Cxa\"."
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
        ((eq 'universal-argument (esm-cmd stem leaf))
         #'esm-universal-arg)
        (t `(call-interactively (key-binding (kbd ,(concat stem leaf)))))))

(defun esm-head-hint (stem leaf)
  (let* ((sym (or (esm-is-a-subhydra stem leaf)
                  (esm-is-known-prefix stem leaf)
                  (key-binding (kbd (concat stem leaf)))))
         (name (if (symbolp sym) (symbol-name sym) " ")))
    (if (string= name "nil")
        " "
      (if (> (length name) (esm--colwidth))
          (substring name 0 esm--colwidth)
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

(defmacro esm-define-many-headed-hydra
    (name stem &optional doctitle pop-key parent exit keys pop-key-2)
  "Make a hydra with many heads. This is exhaustive; it checks almost every
leaf on STEM.

Optional argument EXIT controls whether the hydra's heads exit by
default (overridden case-by-case by `esm-quitters' or `esm-noquitters').
Optional argument KEYS is a string specifying which keys to
display in the hydra hint, defaulting to the value of
`esm-hydra-keys'."

  `(defhydra ,name (nil nil :columns 10 :exit ,exit
                        :body-pre (esm-generate-hydras)
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

(defun esm-associated-title (name)
  (intern name))

(defun esm-associated-nonum-title (name)
  (intern (concat name "-nonum")))

(defun esm-associated-nonum-keymap (name)
  (intern (concat name "-nonum/keymap")))

(defun esm-associated-hint (name)
  (intern (concat name "/hint")))

(defun esm-associated-heads (name)
  (intern (concat name "/heads")))

(defun esm-associated-body (name)
  (intern (concat name "/body")))

(defun esm-associated-keymap (name)
  (intern (concat name "/keymap")))

;; not nearly all of them
(defun esm-all-associated-variables (name)
  (list (esm-associated-keymap name)
        (esm-associated-body name)
        (esm-associated-heads name)
        (esm-associated-hint name)
        (esm-associated-title name)
        (esm-associated-nonum-title name)
        (esm-associated-nonum-keymap name)))

(defun esm-define-prefix-hydra (key)
  (when (keymapp (key-binding (kbd key)))
    (let* ((x            (esm-dub-from-key (esm-normalize key)))
           (title        (intern x))
           (nonum-title  (intern (concat x "-nonum")))
           (nonum-keymap (intern (concat x "-nonum/keymap")))
           (hint         (intern (concat x "/hint")))
           (heads        (intern (concat x "/heads")))
           (body         (intern (concat x "/body")))
           (keymap       (intern (concat x "/keymap")))
           (stem         (concat key " "))
           (parent-key (substring key 0 -2)) ;; doesn't fail with <RET>, strange!
           (parent-symname (cdr (assoc parent-key esm-live-hydras)))
           (parent-body (cond ((string= parent-key "C") #'esm-control/body)
                              ((string= parent-key "M") #'esm-meta/body)
                              ((string= parent-key "s") #'esm-super/body)
                              ;; problem: may not exist yet
                              ((assoc parent-key esm-live-hydras)
                               (intern (concat parent-symname "/body"))))))
      (eval `(progn
               (esm-define-many-headed-hydra ,title       ,stem ,key nil ,parent-body t)
               (esm-define-many-headed-hydra ,nonum-title ,stem ,(concat "\n\n" key) nil ,body t ,esm-hydra-keys-nonum)
               ;; (define-key ,keymap "u" #'esm-universal-arg)
               (define-key ,nonum-keymap "u" #'hydra--universal-argument)
               (dotimes (i 10)
                 (let ((key (kbd (int-to-string i))))
                   (when (eq (lookup-key ,keymap key) 'hydra--digit-argument)
                     (define-key ,keymap key nil))))
               (cons ,key ,x)
               )))))

;; unused
(defun esm-undefine-hydra (hydra-key)
  (seq-map #'makunbound
           (esm-all-associated-variables (esm-dub-from-key hydra-key))))

(defvar esm-last-bindings nil)

(defun esm-generate-hydras ()
  (let* ((curr (esm-current-bindings (rx bol (or "s-" "M-"))
                                     (rx (or "C-" "ESC"))))
         ;; Detect any change, be it bound keys or their commands
         (new     (seq-difference curr esm-last-bindings))
         (defunct (seq-difference esm-last-bindings curr))
         (defunct-hydras (seq-intersection
                          (seq-map #'car defunct)
                          (seq-map #'car esm-live-hydras))))

    ;; Unlist defunct hydras
    (setq esm-live-hydras
          (seq-remove (lambda (x) (member (car x) defunct-hydras))
                      esm-live-hydras))

    ;; Reinit variables in case of change
    (setq esm-hydra-keys-nonum (esm-hydra-keys-nonum))
    (setq esm--colwidth (esm--colwidth))

    ;; TODO: Put some async here. Deferred.el?
    (dolist (key (seq-map #'car new))
      (when (keymapp (key-binding (kbd key)))
        (push (esm-define-prefix-hydra key) esm-live-hydras)))
    (setq esm-last-bindings curr) ;; for next call
    )

  ;; (esm-define-many-headed-hydra esm-control "C-"  "CONTROL" "<f35>" nil nil nil "C-<f35>")
  (esm-define-many-headed-hydra esm-meta    "M-"  "META"    "<f34>" nil nil nil "M-<f34>")
  (esm-define-many-headed-hydra esm-super   "s-"  "SUPER"   "<f33>" nil nil nil "s-<f33>")

  ;; (esm-define-many-headed-hydra esm-control-nonum "C-"  "\nCONTROL" "<f35>" esm-control/body nil "qwertyuiopasdfghjkl;zxcvbnm,./")
  (esm-define-many-headed-hydra esm-meta-nonum    "M-"  "\nMETA"    "<f34>" esm-meta/body  nil "qwertyuiopasdfghjkl;zxcvbnm,./")
  (esm-define-many-headed-hydra esm-super-nonum   "s-"  "\nSUPER"   "<f33>" esm-super/body nil "qwertyuiopasdfghjkl;zxcvbnm,./")

  ;; Clean up digit argument bindings
  (dolist (map (list ;; esm-control/keymap
                     esm-meta/keymap
                     esm-super/keymap))
    (dotimes (i 10)
      (when (eq (lookup-key map (kbd (int-to-string i))) 'hydra--digit-argument)
        (define-key map (kbd (int-to-string i)) nil))))

  ;; (define-key esm-control-nonum/keymap "u" #'hydra--universal-argument)
  (define-key esm-meta-nonum/keymap    "u" #'hydra--universal-argument)
  (define-key esm-super-nonum/keymap   "u" #'hydra--universal-argument)
  t)

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
