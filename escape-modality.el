;; escape-modality.el -- indescribable as yet

;; warnings: very odd behavior if a head that calls another hydra is not set to exit

(require 'hydra)
(require 'escape-modality-sparsemap)
(eval-when-compile (require 'subr-x)) ;; string-join

;;;;;;;;;;;;;;;
;;; Micro level

(defvar esmod-colwidth
  (let ((optimal (- (round (frame-parameter nil 'width) 10) 4)))
    (max optimal 8)))

(defvar esmod-hydra-keys "1234567890qwertyuiopasdfghjkl;zxcvbnm,./")

(defun esmod-hydra-keys () (mapcar #'char-to-string esmod-hydra-keys))

(defvar esmod-quitters '("C-q" "C-s" "C-g" "C-u" "M-x" "M-u" "s-u" "C-2"))

(defvar esmod-noquitters '("C-2 t" "C-2 n" "C-2 p" "C-x ;" "C-x x" "C-x q"
                           "C-x h" "C-x u" "C-x i" "C-x p" "C-x l" "C-x 1"
                           "C-x 2" "C-x 3" "C-x 0"))

(defun esmod-if-prefix-which (lead tail)
  "Return the hydra appropriate, if any. Hardcoded for now."
  (let ((cmd (global-key-binding (kbd (concat lead tail)))))
    (cond ((eq cmd 'Control-X-prefix) #'emo-cx/body)
          ((eq cmd 'mode-specific-command-prefix) #'emo-cc/body)
          ((eq cmd 'ctl-x-4-prefix) #'emo-cx4/body)
          ((eq cmd 'ctl-x-5-prefix) #'emo-cx5/body)
          ((eq cmd 'vc-prefix-map) nil) ;; C-x v
          ((eq cmd 'help-command) #'emo-ch/body)
          ((eq cmd '2C-command) nil) ;; f2
          ((eq cmd 'ESC-prefix) nil)
          ((eq cmd 'projectile-command-map) nil)
          ;; some unnamed prefixes
          ((string= (concat lead tail) "C-x 8") nil)
          ((string= (concat lead tail) "C-x ESC") nil)
          ((string= (concat lead tail) "C-x n") nil)
          ((string= (concat lead tail) "C-x r") nil)
          ((string= (concat lead tail) "C-x a") #'emo-cxa/body)
          ((string= (concat lead tail) "C-x RET") nil)
          ((string= (concat lead tail) "C-x @") nil)
          ((string= (concat lead tail) "C-2") #'emo-c2/body)
          ((string= (concat lead tail) "M-s") #'emo-ms/body)
          ((string= (concat lead tail) "C-M-g") #'emo-sg/body)
          ((string= (concat lead tail) "M-g") #'emo-mg/body)
          ;; ((string= lead "C-") #'emo-control/body)
          ;; ((string= lead "M-") #'emo-meta/body)
          ;; ((string= lead "C-M-") #'emo-super/body)
          )))

(defun esmod-is-a-subhydra (lead tail)
  (if-let ((x (cdr (assoc (concat lead tail) esmod-live-hydras))))
      (intern (concat x "/body"))))

(defun esmod-universal-arg (arg)
  (interactive "P")
  (prefix-command-preserve-state)
  (call-interactively
   (intern (concat (substring (symbol-name hydra-curr-body-fn) 0 -5)
                   "-nonum/body")))
  (hydra--universal-argument arg))

(setq esmod-original-cursor-color (face-background 'cursor))

(defun esmod-colorize-cursor ()
  (let ((curr (symbol-name hydra-curr-body-fn)))
    (cond ((string-match "^emo-C\\|control" curr) (set-cursor-color "green"))
          ((string-match "^emo-M\\|meta" curr) (set-cursor-color "red"))
          ((string-match "^emo-CM\\|super" curr) (set-cursor-color "blue")))))

(defun esmod-decolorize-cursor ()
  (set-cursor-color esmod-original-cursor-color))

;; Could be refactored
(defun esmod-is-prefix-or-unbound (lead tail)
  "Return t if the hotkey described by concatenating LEAD and
TAIL is a prefix command, such as C-x or C-h."
  (string-match
   "prefix\\|nil\\|help-command\\|2C-command"
   (symbol-name (let ((cmd (global-key-binding (kbd (concat lead tail)))))
                  (when (symbolp cmd) cmd)))))

(defun esmod-is-named-prefix (lead tail)
  (car-safe
   (member (global-key-binding (kbd (concat lead tail))) esmod-named-prefixes)))

(defvar esmod-named-prefixes '(Control-X-prefix
                               mode-specific-command-prefix
                               ctl-x-4-prefix
                               ctl-x-5-prefix
                               vc-prefix-map
                               help-command
                               2C-command
                               ESC-prefix
                               projectile-command-map))

;;;;;;;;;;
;;; UNUSED

(defun esmod-is-prefix (lead tail)
  "Return t if the hotkey described by concatenating LEAD and
TAIL is a prefix command, such as C-x or C-h."
  (let* ((cmd (global-key-binding (kbd (concat lead tail))))
         (cmdname (if (symbolp cmd) (symbol-name cmd) "foo1234")))
    (string-match "prefix\\|foo1234\\|help-command\\|2C-command" cmdname)))

(defun esmod-is-unbound (lead tail)
  (and (esmod-is-prefix-or-unbound lead tail)
       (not (esmod-is-prefix lead tail))))

(defun esmod-is-prefix-or-unbound* (lead tail)
  (or (esmod-is-prefix lead tail)
      (not (esmod-is-bound lead tail))))

(defun esmod-is-prefix-or-unbound+ (lead tail)
  (or (not (eq nil (global-key-binding (kbd (concat lead tail)))))
      (not (esmod-is-bound lead tail))
      (esmod-if-prefix-which lead tail)))

(defun esmod-if-prefix-which-hydra (lead tail)
  (if (eq t (esmod-if-prefix-which lead tail))
      nil
    (esmod-if-prefix-which lead tail)))

(defun esmod-is-prefix-or-unbound++ (lead tail)
  (or (esmod-if-prefix-which lead tail)
      (esmod-is-unbound lead tail)))

(defun esmod-is-known-prefix (lead tail)
  (or (assoc (concat lead tail) esmod-prefix-hydras-alist)
      (assoc (global-key-binding (kbd (concat lead tail))) esmod-prefix-hydras-alist)))

(defun esmod-is-prefix (lead tail)
  (or (esmod-is-known-prefix lead tail)
      (esmod-is-unnamed-prefix lead tail)))

(defvar esmod-prefix-hydras-alist '((Control-X-prefix . emo-cx/body)
                                    (mode-specific-command-prefix . emo-cc/body)
                                    (ctl-x-4-prefix . emo-cx4/body)
                                    (ctl-x-5-prefix . emo-cx5/body)
                                    (vc-prefix-map . nil)
                                    (help-command . emo-ch/body)
                                    (2C-command . nil)
                                    (ESC-prefix . nil)
                                    (projectile-command-map . emo-ccp/body)
                                    ("C-x 8" . nil)
                                    ("C-x ESC" . nil)
                                    ("C-x n" . nil)
                                    ("C-x r" . nil)
                                    ("C-x a" . emo-cxa/body)
                                    ("C-x RET" . nil)
                                    ("C-x @" . nil)
                                    ("M-s" . emo-ms/body)
                                    ("M-g" . emo-mg/body)))

;; No idea why these dont work
(defun esmod-if-prefix-which-hydra* (lead tail)
  (let ((cell (assoc (concat lead tail) esmod-prefix-hydras-alist #'esmod-eq)))
    cell))

(defun esmod-eq (obj hotkey)
  (cond ((equal obj (global-key-binding hotkey)) t)
        ((equal obj hotkey) t)))


(defun esmod-is-bound (lead tail)
  (let ((cmd (global-key-binding (kbd (concat lead tail)))))
    (not (eq cmd nil))))

(defun esmod-is-unbound (lead tail)
  (not (esmod-is-bound lead tail)))

(defun esmod-is-unnamed-prefix (lead tail)
  (let ((cmd (global-key-binding (kbd (concat lead tail)))))
    (not (symbolp cmd))))


;;;;;;;;;;;;;;;;;;;;;
;;; Micro-macro level

;; "List of keys like C-a, C-e, M-f, M-g but not C-M-f or M-%."
(setq esmod-all-simple-chords
      (let (chords)
        (mapc (lambda (char)
                (push (concat "C-" (string char)) chords)
                (push (concat "M-" (string char)) chords)
                (push (concat "C-M-" (string char)) chords))
              esmod-hydra-keys)
        chords))

(defun esmod-cmd-conservative (lead tail)
  (if (esmod-is-prefix-or-unbound lead tail)
      tail
    `(call-interactively (key-binding (kbd ,(concat lead tail))))))

(defun esmod-cmd-3 (lead tail)
  (or (when (string= tail "u") #'esmod-universal-arg)
      (esmod-is-a-subhydra lead tail)
      `(call-interactively (key-binding (kbd ,(concat lead tail))))))

(defun esmod-cmd-2 (lead tail)
  (or (when (string= tail "u") #'esmod-universal-arg)
      (esmod-if-prefix-which lead tail)
      `(call-interactively (key-binding (kbd ,(concat lead tail))))))

(defun esmod-hint-3 (lead tail)
  (let* ((sym (or (esmod-is-a-subhydra lead tail)
                  (esmod-is-named-prefix lead tail)
                  (global-key-binding (kbd (concat lead tail)))))
         (name (if (symbolp sym) (symbol-name sym) " ")))
    (if (string= name "nil")
        " "
      (if (> (length name) esmod-colwidth)
          (substring name 0 esmod-colwidth)
        name))))

(defun esmod-hint (lead tail)
  (if (esmod-is-prefix-or-unbound lead tail)
      " "
    (let ((name (symbol-name (global-key-binding (kbd (concat lead tail))))))
      (if (> (length name) esmod-colwidth)
          (substring name 0 esmod-colwidth)
        (if (string-equal name "nil")
            " "
          name)))))

(defun esmod-hint* (lead tail)
  (if (esmod-if-prefix-which lead tail)
      (symbol-name (esmod-if-prefix-which lead tail))
    (if (esmod-is-prefix-or-unbound lead tail)
        " "
      (let ((name (symbol-name (global-key-binding (kbd (concat lead tail))))))
        (if (> (length name) esmod-colwidth)
            (substring name 0 esmod-colwidth)
          (if (string-equal name "nil")
              " "
            name))))))

(defun esmod-key (lead tail)
  "If the given hotkey is bound to a command, return TAIL,
otherwise return a space character. This can be used to
make a visibly blank spot in a hydra for hotkeys that are unbound."
  (if (stringp (esmod-cmd-conservative lead tail)) " " tail))

(defun esmod-exit-almost-never (lead tail)
  (when (or (member (concat lead tail) esmod-quitters)
            (esmod-is-prefix-or-unbound lead tail)) t))

(defun esmod-exit-2 (lead tail)
  (cond ((member (concat lead tail) esmod-quitters) '(:exit t))
        ((member (concat lead tail) esmod-noquitters) '(:exit nil))
        ((esmod-is-prefix-or-unbound lead tail) '(:exit t))
        (t '()))) ;; defer to hydra's default behavior

(defun esmod-head-3 (lead tail)
  `( ,tail ,(esmod-cmd-3 lead tail) ,(esmod-hint-3 lead tail)
           ,@(esmod-exit-2 lead tail)))

(defun esmod-head-regardless-2 (lead tail)
  `( ,tail ,(esmod-cmd-2 lead tail) ,(esmod-hint* lead tail)
           ,@(esmod-exit-2 lead tail)))

(defun esmod-head-invisible (lead tail)
  `( ,(esmod-key lead tail) ,(esmod-cmd-conservative lead tail) nil
     :exit ,(esmod-exit-almost-never lead tail)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro level: tying it all together.

(defmacro esmod-defmode
    (name lead &optional doctitle pop-key parent exit keys)
  "Make a hydra with very many heads.

Optional argument EXIT controls whether the hydra's heads exit by
default (overridden on a case-by-case basis by `esmod-quitters').
Optional argument KEYS is a string specifying which keys to
display in the hydra hint, defaulting to the value of
`esmod-hydra-keys'."

  `(defhydra ,name (nil nil :columns 10 :exit ,exit
                        :body-pre (esmod-colorize-cursor)
                        :post (esmod-decolorize-cursor))
     ,@(if doctitle (list doctitle) '())
     ,@(mapcar (lambda (char) (esmod-head-3 lead (string char)))
               (or keys esmod-hydra-keys))
     ,@(mapcar (lambda (tail) (esmod-head-invisible lead tail))
               '("<left>" "<right>" "<up>" "<down>" "<SPC>" "=" "\\" "'" "`"))
     ,@(mapcar (lambda (chord) (esmod-head-invisible "" chord))
               esmod-all-simple-chords)
     ,@(let (extras)
         (mapc (lambda (candidate) (if (car candidate) (push (cdr candidate) extras)))
               ;; List of extra heads to go in the hydra.
               ;; For example ("C-f" . ("C-f" nil nil :exit t))
               `((,pop-key . (,pop-key nil nil :exit t))
                 (,parent . ("<backspace>" ,parent nil :exit t)))) extras)))


(defun esmod-define-local-hydra (key keymap)
  (when (symbolp keymap) ;; only those with names like org-mode-map
    (let* ((x (concat "emo-" (symbol-name ,keymap) "-"
                      (string-join (split-string key "[- ]"))))
           (title              (intern x))
           (title-nonum        (intern (concat x "-nonum")))
           (title-nonum-keymap (intern (concat x "-nonum/keymap")))
           (hint               (intern (concat x "/hint")))
           (heads              (intern (concat x "/heads")))
           (body               (intern (concat x "/body")))
           )
      (print title)
      (print key)
      (push `(,key . ,body) esmod-live-hydras)
      )))

(defvar esmod-live-hydras '(("C-c" . "emo-Cc")
                            ("C-x" . "emo-Cx")
                            ("C-h" . "emo-Ch")))

;; Will bug w maps bound to -, like C--.
(defun esmod-dub-from-key (key)
  "Return \"emo-Cxa\" if KEY is \"C-x a\"."
  (concat "emo-" (string-join (split-string key "[- ]"))))

(defun esmod-define-global-hydra-maybe (key &optional symname)
  (when (keymapp (global-key-binding (kbd key)))
    (let* ((x (or symname (esmod-dub-from-key key)))
           (title        (intern x))
           (nonum-title  (intern (concat x "-nonum")))
           (nonum-keymap (intern (concat x "-nonum/keymap")))
           (hint         (intern (concat x "/hint")))
           (heads        (intern (concat x "/heads")))
           (body         (intern (concat x "/body")))
           (lead (concat key " "))
           (parent-key (substring key 0 -2)) ;; Doesnt bug with <RET>, strange!
           (parent-symname (cdr (assoc parent-key esmod-live-hydras)))
           (parent-body (cond ((string= parent-key "C") #'emo-control/body)
                              ((string= parent-key "M") #'emo-meta/body)
                              ((assoc parent-key esmod-live-hydras)
                               (intern (concat parent-symname "/body"))))))
      (eval `(progn
               (esmod-defmode ,title       ,lead ,key nil ,parent-body t)
               (esmod-defmode ,nonum-title ,lead ,(concat "\n\n" key) nil ,body t "qwertyuiopasdfghjkl;zxcvbnm,./")
               (define-key ,nonum-keymap "u" #'hydra--universal-argument)))
      (set hint `(eval (hydra--format nil '(nil nil :columns 10)
                                      ,key (esmod-update-hints ,heads)))))))

(defun esmod-scan ()
  "Scan for global prefix keys to populate `esmod-live-hydras'."
  (setq esmod-live-hydras nil)
  (dolist (x esmod-whole-keyboard)
    (when (keymapp (global-key-binding (kbd x)))
      (push `(,x . ,(esmod-dub-from-key x)) esmod-live-hydras))))

(defun esmod-generate-hydras ()
  (dolist (x esmod-live-hydras)
    (esmod-define-global-hydra-maybe (car x) (cdr x))))

(setq esmod-whole-keyboard
      (let (biglist)
        (dolist (chord esmod-all-simple-chords)
          (dolist (key esmod-all-keys-on-keyboard-without-shift)
            (push (concat chord " " key) biglist))
          (push chord biglist))
        biglist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change Emacs state. Should go in user init file.

;; Stage 1
(esmod-flatten-ctl-x)

;; Stage 2
(esmod-shift-map-from-unshifted)

;; Stage 3
;; (esmod-super-map-from-ctl-meta)
(esmod-super-translate-to-ctl-meta)

;; Stage 4

(esmod-scan)
(esmod-generate-hydras)

;; (esmod-defmode emo-cxa       "C-x a "  "CTL-X-A" nil emo-cx/body t)
;; (esmod-defmode emo-cx4       "C-x 4 "  "CTL-X-4" nil emo-cx/body t)
;; (esmod-defmode emo-cx5       "C-x 5 "  "CTL-X-5" nil emo-cx/body t)
;; (esmod-defmode emo-cx          "C-x "  "CTL-X"   nil emo-control/body t)
;; (esmod-defmode emo-c2          "C-2 "  "CTL-2"   nil emo-control/body t)
;; (esmod-defmode emo-cc          "C-c "  "CTL-C"   nil emo-control/body t)
;; (esmod-defmode emo-ch          "C-h "  "CTL-H"   nil emo-control/body t)
(esmod-defmode emo-control       "C-"  "CONTROL" "C-<f13>")
;; (esmod-defmode emo-ms          "M-s "  "META-S"  nil emo-meta/body t)
;; (esmod-defmode emo-mg          "M-g "  "META-G"  nil emo-meta/body t)
(esmod-defmode emo-meta          "M-"  "META"  "M-<f14>")
;; (esmod-defmode emo-sg        "C-M-g "  "S-G"   nil emo-super/body t)
(esmod-defmode emo-super       "C-M-"  "SUPER" "s-<f15>")

(esmod-defmode emo-control-nonum "C-"  "
CONTROL" "C-<f13>" emo-control/body nil "qwertyuiopasdfghjkl;zxcvbnm,./")
(esmod-defmode emo-meta-nonum    "M-"  "
META"  "M-<f14>" emo-meta/body nil "qwertyuiopasdfghjkl;zxcvbnm,./")
(esmod-defmode emo-super-nonum "C-M-"  "
SUPER" "s-<f15>" emo-super/body nil "qwertyuiopasdfghjkl;zxcvbnm,./")

(define-key emo-control-nonum/keymap "u" #'hydra--universal-argument)
(define-key emo-meta-nonum/keymap "u" #'hydra--universal-argument)
(define-key emo-super-nonum/keymap "u" #'hydra--universal-argument)

(with-eval-after-load 'org
  (esmod-backup-and-flatten-keymap org-mode-map))


;; the post at http://oremacs.com/2015/06/30/context-aware-hydra/ finally makes
;; sense. Basically, hydra-foo/hint is a form that gets evalled every time a
;; head is called. Typically this is just a (format) call, which returns a
;; string (what you see in the minibuffer). But you can put other stuff in,
;; making sure you return a string all the same. For example, you can
;; (define-key hydra-foo/keymap "n" 'some-new-function), then return the result
;; of (hydra--format ...).

;; For it to be context aware, the top-level form could be an (if) or (cond) or
;; (let)... Returning different format strings if you like.

;; the main thing is i want is hints reflecting what's on (key-binding)
;; so re-eval that every time

;; Set the hint to what it normally is
;; (setq emo-control-nonum/hint
;;       '(eval (hydra--format nil '(nil nil :columns 10)
;;                             "CONTROL" emo-control-nonum/heads)))

;; Tip: check out the variable `yourhydra/heads' to see what you have to work with.

;; Context-aware hints. Be aware this does not alter the .../heads variable at
;; all. For that, eval something before the hydra--format.
(dolist (x '(("emo-cx" . "CTL-X")
             ("emo-cc" . "CTL-C")
             ("emo-sg" . "SUPER-G")
             ("emo-super" . "SUPER")
             ("emo-control" . "CONTROL")
             ("emo-meta" . "META")
             ))
  (set (intern (concat (car x) "/hint"))
       `(eval (hydra--format nil '(nil nil :columns 10)
                             ,(cdr x) (esmod-update-hints ,(intern (concat (car x) "/heads")))))))

;; It works, but I don't understand how. Why does it not return updated
;; heads with hints, for keys that were previously invisible? For those keys,
;; it returns heads with nil hints as they were, but the funcall does make it all
;; the way into the most nested 'if' call.
(defun esmod-update-hints (heads)
  (mapcar
   (lambda (head)
     (if (and (listp (cadr head)) ;; if it is bound to (call-interactively)
              (stringp (car-safe (cddr head)))) ;; if the head has a hint
         (let* ((hotkey (cadr (cadadr (cadr head))))
                (cmd (key-binding (kbd hotkey)))
                ;; give " " when cmd is an unnamed subkeymap, else the cmd name
                (hint (if (symbolp cmd) (symbol-name cmd) " ")))
           (if (not (eq nil cmd))
               ;; Return a modified head.
               `(,(car head)
                 ,(cadr head)
                 ,(substring hint 0 (min (length hint) esmod-colwidth))
                 ,@(cdddr head)
                 )
             head))
       head))
   heads))

(provide 'escape-modality)
