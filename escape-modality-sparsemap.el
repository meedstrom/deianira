;; escape-modality-sparsemap.el -- Modifications to the Emacs keymap

(defun esmod-stringify (x)
  "Like `char-to-string', but accepts string input."
  (if (characterp x) (char-to-string x) x))

(defun esmod-copy-key (copier copied key)
  ;; Transform in case the key is coming from a string-to-list operation
  (let ((key (esmod-stringify key)))
    (global-set-key (kbd (concat copier key))
                    (global-key-binding (kbd (concat copied key))))))

(defun esmod-copy-key-1 (prefix copying-key copied-key)
  (let ((copying-key (esmod-stringify copying-key))
        (copied-key (esmod-stringify copied-key)))
    (global-set-key (kbd (concat prefix copying-key))
                    (global-key-binding (kbd (concat prefix copied-key))))))

(defvar esmod-C-x-C-key-overwrites-C-x-key t)

(defun esmod-flatten-ctl-x ()
  ;; Prevent clobbering useful commands. Would take the useful command and bind
  ;; it elsewhere, but Emacsers remember these bindings. They're friendly,
  ;; familiar.
  ;; Should go in user init file.
  (if esmod-C-x-C-key-overwrites-C-x-key
      (progn
        ;; Bind here stuff from ctl-x <KEY> that you want to keep.
        ;; (global-set-key (kbd "C-x C-b") #'switch-to-buffer)
        ;; (global-set-key (kbd "C-x C-k") #'kill-buffer)
        (global-set-key (kbd "C-x C-a") (key-binding (kbd "C-x a"))) ;;abbrev-map
        (global-set-key (kbd "C-x C-h") #'mark-whole-buffer)
        (global-set-key (kbd "C-x C-n") #'narrow-to-region))
    ;; Bind here stuff from ctl-x ctl-<KEY> that you want to keep.
    (global-set-key (kbd "C-x e") #'eval-last-sexp)
    (global-set-key (kbd "C-x f") #'find-file)
    ;;(global-set-key (kbd "C-x s") #'save-buffer)
    (global-set-key (kbd "C-x c") #'save-buffers-kill-emacs)
    (global-set-key (kbd "C-x x") #'exchange-point-and-mark)
    (global-set-key (kbd "C-x o") #'delete-blank-lines))

  ;; - TODO: make this a function that can recover the previous keymap.
  (if esmod-C-x-C-key-overwrites-C-x-key
      (dolist (x (esmod-get-hydra-keys))
        (global-set-key (kbd (concat "C-x " x))
                        (key-binding (kbd (concat "C-x C-" x)))))
    (dolist (x (esmod-get-hydra-keys))
      (global-set-key (kbd (concat "C-x C-" x))
                      (key-binding (kbd (concat "C-x " x)))))))

(defmacro esmod-backup-keymap (keymap)
  "Backup under the name esmod-backup-KEYMAP, unless it already exists."
  `(let ((backup (intern (concat "esmod-backup-" (symbol-name ',keymap)))))
     (unless (and (boundp backup)
                  ((not (eq nil backup))))
       ;; Maybe you should use `copy-keymap' here
       (set backup ,keymap))))

(defun esmod-flatten-keymap (keymap)
  "An example of KEYMAP is org-mode-map. Only acts on stuff under C-c for now."
  (dolist (key (esmod-get-hydra-keys))
    (define-key keymap (kbd (concat "C-c " key))
      (lookup-key keymap (kbd (concat "C-c C-" key))))))

(defmacro esmod-restore-keymap (keymap)
  `(let ((backup (intern (concat "esmod-backup-" (symbol-name ',keymap)))))
     (when (and (boundp backup) ((not (eq nil backup))))
       (setq ,keymap backup))))

(defvar esmod-all-keys-on-keyboard
  (append
   (split-string
    "`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./~!@#$%^&*()_+{}|:\"<>?"
    "" t)
   (split-string
    "<left> <right> <up> <down> <SPC> <RET> <backspace> <delete>
     TAB <f1> <f2> <f3> <f4> <f5> <f6> <f7> <f8> <f9> <f10> <f11>
     <f12> <print> <insert> <next> <prior> <home> <end>"
    "\s\\|\n" t))
  "If you want to customize this to your local layout, try
copying the source.")

(defvar esmod-all-keys-on-keyboard-without-shift
  (seq-difference esmod-all-keys-on-keyboard
                  (split-string "~!@#$%^&*()_+{}|:\"<>?" "" t)))

(defun esmod-super-translate-to-ctl-meta ()
  (dolist (key esmod-all-keys-on-keyboard)
    (define-key key-translation-map
      (kbd (concat "s-" key)) (kbd (concat "C-M-" key)))))

(defun esmod-super-map-from-ctl-meta ()
  (dolist (key (string-to-list
                "`1234567890-=qwertyuiop[]\\asdfghjkl;'zxcvbnm,./~!@#$%^&*()_+{}|:\"<>?"))
    (global-set-key (kbd (concat "s-" (string key)))
                    (global-key-binding (kbd (concat "C-M-" (string key)))))
    (eval-after-load 'smartparens
      `(let ((cmd (lookup-key smartparens-mode-map (kbd (concat "C-M-" (string ,key))))))
         (define-key smartparens-mode-map (kbd (concat "s-" (string ,key)))
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
               φ2))))) smartparens-mode-map))


(defmacro esmod-kill-shift-1 ()
  "Make it not matter whether or not Shift is pressed."
  `(progn ,@(cl-mapcar
             (lambda (loser winner)
               `(progn (esmod-copy-key-1 "C-" ,loser ,winner)
                       (esmod-copy-key-1 "M-" ,loser ,winner)
                       (esmod-copy-key-1 "C-x " ,loser ,winner)
                       (esmod-copy-key-1 "C-x C-" ,loser ,winner)
                       (esmod-copy-key-1 "C-M-" ,loser ,winner)
                       ))
             "~!@#$%^&*()_+{}:<>?\"|" ;; loser: default binds overwritten
             "`1234567890-=[];,./'\\" ;; winner: its commands stay
             )))

(defun esmod-shift-map-from-unshifted ()
  (esmod-kill-shift-1)
  ;; (global-set-key (kbd "C-~") (global-key-binding (kbd "C-`")))
  ;; (global-set-key (kbd "M-~") (global-key-binding (kbd "M-`")))
  ;; (global-set-key (kbd "C-M-~") (global-key-binding (kbd "C-M-`")))
  (setq org-support-shift-select t))

(defun esmod-hyper-alt-maps-from-ctl-meta-maps ()
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
  (dolist (var esmod-xmodmap-rules)
    (when (or (string-match "keycode 108" var)
              (string-match "keycode 105" var))
      (delq var esmod-xmodmap-rules)))
  (push "keycode 108 = Hyper_R" esmod-xmodmap-rules)
  (push "keycode 105 = Alt_R" esmod-xmodmap-rules)
  (esmod-xmodmap-reload))

(provide 'escape-modality-sparsemap)
