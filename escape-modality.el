;; escape-modality.el -- indescribable as yet

;; warnings: odd behavior if a head that calls another hydra is not set to exit

(require 'hydra)
(require 'escape-modality-sparsemap)
(require 'escape-modality-x11)
(eval-when-compile 'subr) ;; split-string
(eval-when-compile (require 'subr-x)) ;; string-join

;; Compact ways to get a list
(mapcar #'char-to-string "sdfsdfds")
(split-string "sadfsafas" "" t)
;;;;;;;;;;;;;;;
;;; Micro level

;; TODO: adapt to changing frame sizes
(defvar esm-colwidth
  (let ((optimal (- (round (frame-parameter nil 'width) 10) 4)))
    (max optimal 8)))

(defvar esm-hydra-keys "1234567890qwertyuiopasdfghjkl;zxcvbnm,./")

(defun esm-get-hydra-keys ()
  (split-string esm-hydra-keys "" t))

(defvar esm-quitters '("C-q" "C-s" "C-g" "C-u" "M-x" "M-u" "s-u" "C-2" "C-c c"))

(defvar esm-noquitters '("C-2 t" "C-2 n" "C-2 p" "C-x ;" "C-x x" "C-x q"
                           "C-x h" "C-x u" "C-x i" "C-x p" "C-x l" "C-x 1"
                           "C-x 2" "C-x 3" "C-x 0" "C-c c"))

(defun esm-is-a-subhydra (lead tail)
  "Returns the hydra body in question."
  (if-let ((x (cdr (assoc (concat lead tail) esm-live-hydras))))
      (intern (concat x "/body"))))

(defun esm-universal-arg (arg)
  (interactive "P")
  (prefix-command-preserve-state)
  (call-interactively
   (intern (concat (substring (symbol-name hydra-curr-body-fn) 0 -5)
                   "-nonum/body")))
  (hydra--universal-argument arg))

;; wip; inspired by esm-cc-cc
(defun esm-wrapper (hotkey)
  (call-interactively (key-binding (kbd hotkey)))
  (if (string-match "^C" lead)
      (esm-control/body)))

(defun esm-cc-cc (arg)
  (interactive "P")
  ;; (prefix-command-preserve-state)
  ;; (setq unread-command-events (listify-key-sequence "c"))
  (call-interactively (key-binding (kbd "C-c c")))
  (esm-control/body))

(setq esm-original-cursor-color (face-background 'cursor))

(defun esm-colorize-cursor ()
  (let ((curr (symbol-name hydra-curr-body-fn)))
    (cond ((string-match "^esm-C\\|control" curr) (set-cursor-color "green"))
          ((string-match "^esm-M\\|meta" curr) (set-cursor-color "red"))
          ((string-match "^esm-CM\\|super" curr) (set-cursor-color "blue")))))

(defun esm-decolorize-cursor ()
  (set-cursor-color esm-original-cursor-color))

(defun esm-is-named-prefix (lead tail)
  (car-safe
   (member (global-key-binding (kbd (concat lead tail))) esm-named-prefixes)))

(defvar esm-named-prefixes '(Control-X-prefix
                               mode-specific-command-prefix
                               ctl-x-4-prefix
                               ctl-x-5-prefix
                               vc-prefix-map
                               help-command
                               2C-command
                               ESC-prefix
                               projectile-command-map))

(defun esm-is-bound (lead tail)
  (let ((cmd (global-key-binding (kbd (concat lead tail)))))
    (not (eq cmd nil))))

(defun esm-is-unbound (lead tail)
  (not (esm-is-bound lead tail)))

(defun esm-is-unnamed-prefix (lead tail)
  (let ((cmd (global-key-binding (kbd (concat lead tail)))))
    (not (symbolp cmd))))


(defun esm-is-prefix-or-unbound (lead tail)
  "Return t if the hotkey described by concatenating LEAD and
TAIL is a prefix command, such as C-x or C-h."
  (string-match
   "prefix\\|nil\\|help-command\\|2C-command"
   (symbol-name (let ((cmd (global-key-binding (kbd (concat lead tail)))))
                  (when (symbolp cmd) cmd)))))

(defun esm-is-prefix-or-unbound-4 (lead tail)
  (when (or (esm-is-a-subhydra lead tail)
            (esm-is-unbound lead tail))
    t))

;;;;;;;;;;;;;;;;;;;;;
;;; Micro-macro level

;; "List of keys like C-a, C-e, M-f, M-g but not C-M-f or M-%."
(setq esm-all-simple-chords
      (let (chords)
        (mapc (lambda (char)
                (push (concat "C-" (string char)) chords)
                (push (concat "M-" (string char)) chords)
                (push (concat "C-M-" (string char)) chords))
              esm-hydra-keys)
        chords))

(defun esm-cmd-conservative (lead tail)
  (if (esm-is-prefix-or-unbound lead tail)
      tail
    `(call-interactively (key-binding (kbd ,(concat lead tail))))))

(defun esm-cmd-4 (lead tail)
  (if (esm-is-unbound lead tail) tail
    (or (when (string= tail "u") #'esm-universal-arg)
        (when (and (string= lead "C-c ")
                   (string= tail "c")) #'esm-cc-cc)
        (esm-is-a-subhydra lead tail)
        `(call-interactively (key-binding (kbd ,(concat lead tail)))))))

(defun esm-cmd-3 (lead tail)
  (or (when (string= tail "u") #'esm-universal-arg)
      (esm-is-a-subhydra lead tail)
      `(call-interactively (key-binding (kbd ,(concat lead tail))))))

(defun esm-hint-3 (lead tail)
  (let* ((sym (or (esm-is-a-subhydra lead tail)
                  (esm-is-named-prefix lead tail)
                  (global-key-binding (kbd (concat lead tail)))))
         (name (if (symbolp sym) (symbol-name sym) " ")))
    (if (string= name "nil")
        " "
      (if (> (length name) esm-colwidth)
          (substring name 0 esm-colwidth)
        name))))

(defun esm-key-4 (lead tail)
  "Return either TAIL or a string containing a single space. This
can be used to make a visibly blank spot in a hydra for hotkeys
that are unbound."
  (if (esm-is-unbound lead tail) " " tail))

(defun esm-key (lead tail)
  "If the given hotkey is bound to a command, return TAIL,
otherwise return a space character. This can be used to
make a visibly blank spot in a hydra for hotkeys that are unbound."
  (if (stringp (esm-cmd-conservative lead tail)) " " tail))

(defun esm-exit-almost-never (lead tail)
  (when (or (member (concat lead tail) esm-quitters)
            (esm-is-unbound lead tail))
    t))

(defun esm-exit-4 (lead tail)
  (cond ((member (concat lead tail) esm-quitters) '(:exit t))
        ((member (concat lead tail) esm-noquitters) '(:exit nil))
        ((esm-is-a-subhydra lead tail) '(:exit t)) ;; very important
        ((esm-is-unbound lead tail) '(:exit t))
        (t '()))) ;; defer to hydra's default behavior

(defun esm-exit-2 (lead tail)
  (cond ((member (concat lead tail) esm-quitters) '(:exit t))
        ((member (concat lead tail) esm-noquitters) '(:exit nil))
        ((esm-is-prefix-or-unbound lead tail) '(:exit t))
        (t '()))) ;; defer to hydra's default behavior

(defun esm-head-4 (lead tail)
  `( ,(esm-key-4 lead tail) ,(esm-cmd-4 lead tail) ,(esm-hint-3 lead tail)
     ,@(esm-exit-4 lead tail)))

(defun esm-head-3 (lead tail)
  `( ,tail ,(esm-cmd-3 lead tail) ,(esm-hint-3 lead tail)
           ,@(esm-exit-2 lead tail)))

(defun esm-head-invisible (lead tail)
  `( ,(esm-key lead tail) ,(esm-cmd-conservative lead tail) nil
     :exit ,(esm-exit-almost-never lead tail)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro level: tying it all together.


(defmacro esm-defmode
    (name lead &optional doctitle pop-key parent exit keys)
  "Make a hydra with many heads.

Optional argument EXIT controls whether the hydra's heads exit by
default (overridden on a case-by-case basis by `esm-quitters').
Optional argument KEYS is a string specifying which keys to
display in the hydra hint, defaulting to the value of
`esm-hydra-keys'."

  `(defhydra ,name (nil nil :columns 10 :exit ,exit
                        :body-pre (esm-colorize-cursor)
                        ;; (setq exwm-input-line-mode-passthrough t)
                        :post (esm-decolorize-cursor)
                        ;; (setq exwm-input-line-mode-passthrough nil)
                        ;; :foreign-keys run
                        )
     ,@(if doctitle (list doctitle) '())
     ,@(mapcar (lambda (char) (esm-head-4 lead (string char)))
               (or keys esm-hydra-keys))
     ,@(mapcar (lambda (tail) (esm-head-invisible lead tail))
               '("<left>" "<right>" "<up>" "<down>" "<SPC>" "=" "\\" "'" "`"))
     ;; ,@(mapcar (lambda (chord) (esm-head-invisible "" chord))
     ;; esm-all-simple-chords)
     ,@(let (extras)
         (mapc (lambda (candidate) (if (car candidate) (push (cdr candidate) extras)))
               ;; List of extra heads to go in the hydra.
               ;; For example ("C-f" . ("C-f" nil nil :exit t))
               `((,pop-key . (,pop-key nil nil :exit t))
                 (,parent . ("<backspace>" ,parent nil :exit t)))) extras)))

(defun esm-define-local-hydra (key keymap)
  (when (symbolp keymap) ;; only those with names like org-mode-map
    (let* ((x (concat "esm-" (symbol-name ,keymap) "-"
                      (string-join (split-string key "[- ]"))))
           (title              (intern x))
           (title-nonum        (intern (concat x "-nonum")))
           (title-nonum-keymap (intern (concat x "-nonum/keymap")))
           (hint               (intern (concat x "/hint")))
           (heads              (intern (concat x "/heads")))
           (body               (intern (concat x "/body"))))
      (print title)
      (print key)
      (push `(,key . ,body) esm-live-hydras))))

(defvar esm-live-hydras '(("C-c" . "esm-Cc")
                            ("C-x" . "esm-Cx")
                            ("C-h" . "esm-Ch")))

;; Will bug w maps bound to -, like C--.
(defun esm-dub-from-key (key)
  "Return \"esm-Cxa\" if KEY is \"C-x a\"."
  (concat "esm-" (string-join (split-string key "[- ]"))))

(defun esm-define-global-hydra-maybe (key &optional symname)
  (when (keymapp (global-key-binding (kbd key)))
    (let* ((x (or symname (esm-dub-from-key key)))
           (title        (intern x))
           (nonum-title  (intern (concat x "-nonum")))
           (nonum-keymap (intern (concat x "-nonum/keymap")))
           (hint         (intern (concat x "/hint")))
           (heads        (intern (concat x "/heads")))
           (body         (intern (concat x "/body")))
           (keymap       (intern (concat x "/keymap")))
           (lead (concat key " "))
           (parent-key (substring key 0 -2)) ;; Doesnt bug with <RET>, strange!
           (parent-symname (cdr (assoc parent-key esm-live-hydras)))
           (parent-body (cond ((string= parent-key "C") #'esm-control/body)
                              ((string= parent-key "M") #'esm-meta/body)
                              ((string= parent-key "C-M") #'esm-super/body)
                              ((assoc parent-key esm-live-hydras)
                               (intern (concat parent-symname "/body"))))))
      (eval `(progn
               (esm-defmode ,title       ,lead ,key nil ,parent-body t)
               (esm-defmode ,nonum-title ,lead ,(concat "\n\n" key) nil ,body t "qwertyuiopasdfghjkl;zxcvbnm,./")
               (define-key ,nonum-keymap "u" #'hydra--universal-argument)
               (dotimes (i 10)
                 (define-key ,keymap (int-to-string i) nil))))
      (set hint `(eval (hydra--format nil '(nil nil :columns 10)
                                      ,key (esm-update-hints ,heads)))))))


(defun esm-scan ()
  "Scan for global prefix keys to populate `esm-live-hydras'.
This may take a couple of seconds, so you may want to save the
results in your init file and set the variable directly."
  (setq esm-live-hydras nil)
  (dolist (x esm-whole-keyboard)
    (when (keymapp (global-key-binding (kbd x)))
      (push `(,x . ,(esm-dub-from-key x)) esm-live-hydras))))

(defun esm-generate-hydras ()
  (dolist (x esm-live-hydras)
    (esm-define-global-hydra-maybe (car x) (cdr x))))

(setq esm-whole-keyboard
      (let (biglist)
        (dolist (chord esm-all-simple-chords)
          (dolist (key esm-all-keys-on-keyboard-without-shift)
            (push (concat chord " " key) biglist))
          (push chord biglist))
        biglist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change Emacs state. Should go in user init file.

;; Stage 1
(esm-flatten-ctl-x)

;; Stage 2
(esm-shift-map-from-unshifted)

;; Stage 3
;; (esm-super-map-from-ctl-meta)
(esm-super-translate-to-ctl-meta)

;; Stage 4

(esm-scan)
(esm-generate-hydras)

;; (add-hook 'buffer-list-update-hook #'esm-generate-hydras)

;; (esm-defmode esm-cxa       "C-x a "  "CTL-X-A" nil esm-cx/body t)
;; (esm-defmode esm-cx4       "C-x 4 "  "CTL-X-4" nil esm-cx/body t)
;; (esm-defmode esm-cx5       "C-x 5 "  "CTL-X-5" nil esm-cx/body t)
;; (esm-defmode esm-cx          "C-x "  "CTL-X"   nil esm-control/body t)
;; (esm-defmode esm-c2          "C-2 "  "CTL-2"   nil esm-control/body t)
;; (esm-defmode esm-cc          "C-c "  "CTL-C"   nil esm-control/body t)
;; (esm-defmode esm-ch          "C-h "  "CTL-H"   nil esm-control/body t)
(esm-defmode esm-control       "C-"  "CONTROL" "C-<f13>")
;; (esm-defmode esm-ms          "M-s "  "META-S"  nil esm-meta/body t)
;; (esm-defmode esm-mg          "M-g "  "META-G"  nil esm-meta/body t)
(esm-defmode esm-meta          "M-"  "META"  "M-<f14>")
;; (esm-defmode esm-sg        "C-M-g "  "S-G"   nil esm-super/body t)
(esm-defmode esm-super       "C-M-"  "SUPER" "s-<f15>")

(esm-defmode esm-control-nonum "C-"  "
CONTROL" "C-<f13>" esm-control/body nil "qwertyuiopasdfghjkl;zxcvbnm,./")
(esm-defmode esm-meta-nonum    "M-"  "
META"  "M-<f14>" esm-meta/body nil "qwertyuiopasdfghjkl;zxcvbnm,./")
(esm-defmode esm-super-nonum "C-M-"  "
SUPER" "s-<f15>" esm-super/body nil "qwertyuiopasdfghjkl;zxcvbnm,./")

;; (dotimes (i 10)
;;   (define-key esm-control/keymap (int-to-string i) nil)
;;   (define-key esm-meta/keymap (int-to-string i) nil)
;;   (define-key esm-super/keymap (int-to-string i) nil))

;; (defun esm-unbind-maybe (keymap key)
;;   (when (eq 'hydra--digit-argument (lookup-key keymap (kbd key)))
;;     (define-key keymap (kbd key) nil)))

;; Clean up digit argument bindings
(dolist (map (list esm-control/keymap
                   esm-meta/keymap
                   esm-super/keymap))
  (dotimes (i 10)
    (let ((key (kbd (int-to-string i))))
      (when (eq (lookup-key map key) 'hydra--digit-argument)
        (define-key map key nil)))))

(define-key esm-control-nonum/keymap "u" #'hydra--universal-argument)
(define-key esm-meta-nonum/keymap "u" #'hydra--universal-argument)
(define-key esm-super-nonum/keymap "u" #'hydra--universal-argument)

(with-eval-after-load 'org
  (esm-backup-keymap org-mode-map)
  (esm-flatten-keymap org-mode-map))


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
;; (setq esm-control-nonum/hint
;;       '(eval (hydra--format nil '(nil nil :columns 10)
;;                             "CONTROL" esm-control-nonum/heads)))

;; Tip: check out the variable `yourhydra/heads' to see what you have to work with.

;; Context-aware hints. Be aware this does not alter the .../heads variable at
;; all. For that, eval something before the hydra--format.
(dolist (x '(;; ("esm-cx" . "CTL-X")
             ;; ("esm-cc" . "CTL-C")
             ;; ("esm-sg" . "SUPER-G")
             ("esm-super" . "SUPER")
             ("esm-control" . "CONTROL")
             ("esm-meta" . "META")))
  ;; (set (intern (concat (car x) "/hint"))
  ;; `(eval (progn (set (ca))
  ;; (hydra--format nil '(nil nil :columns 10) ,(cdr x)
  ;; (esm-update-heads ,(intern (concat (car x)
  ;; "/heads")))))))
  (let ((y (intern (concat (car x) "/heads"))))
    (set (intern (concat (car x) "/hint"))
         `(eval (progn (setq ,y (esm-update-heads ,y))
                       (hydra--format nil '(nil nil :columns 10) ,(cdr x) ,y))))))


(defun esm-update-all-keymaps ()
  (dolist (hyd esm-live-hydras)
    (let ((x (cdr hyd))))))

(defun esm-update-keymap (m)

  ;; Figure out the prefix/"lead" for this hydra by iterating until a head is found.
  (let* ((example
          (catch t
            (dotimes (i 30)
              (let ((cmd (cadr (elt heads i))))
                (when (listp cmd) ;; when we find a (call-interactively ...)
                  (throw t (cadr (cadadr cmd))))))))
         (lead (substring example 0 -1)))

    ;; Go thru heads one by one
    (mapcar
     (lambda (head)

       ;; If it's a chord, do not bother updating. Affects <RET> and the like though.
       (if (> (length (car head)) 1)
           head

         ;; Check out the heads not currently bound. If the
         ;; corresponding hotkey has changed, return a new head.
         (let ((cmd (cadr head)))
           (if (and (stringp cmd)
                    (esm-is-bound lead cmd))
               (esm-head-4 lead cmd)

             ;; Otherwise, check if it's a (call-interactively).
             (if (listp cmd)
                 (esm-head-4 lead (car head))

               ;; Otherwise, return head unmodified.
               head)
             head))))
     heads)))

;; How to detect a change in the keymap?
;; https://github.com/abo-abo/hydra/wiki/Conditional-Hydra
;; Instead of re-generating hydra, maybe you'll have luck with altering the /keymap.
(defun esm-update-hydra-maybe (heads)

  ;; Figure out the prefix/"lead" for this hydra by iterating until a head is found.
  (let* ((heads esm-Cx/heads)
         (example
          (catch t
            (dotimes (i 30)
              (let ((cmd (cadr (elt heads i))))
                (when (listp cmd) ;; when we find a (call-interactively ...)
                  (throw t (cadr (cadadr cmd))))))))
         (lead (substring example 0 -1)))

    (if (catch t
          (mapc (lambda (head)
                  (if (> (length (car head)) 1)
                      nil
                    (let ((cmd (cadr head)))
                      (cond ((and (stringp cmd) (esm-is-bound lead cmd))
                             (throw t t))
                            ((and (listp cmd) (esm-is-unbound lead (car head)))
                             (throw t t))))))
                heads))
        (eval '(esm-define-global-hydra-maybe (substring lead 0 -1))))))


;; let's use htis in place of the update-hints thing. Return a list of heads for
;; passing to hydra--format, but also actually set this thing globally.
(defun esm-update-heads (heads)

  ;; Figure out the prefix (aka "lead") for this hydra by iterating until a
  ;; head is found, any head.
  (let* ((example
          (catch t
            (dotimes (i 30)
              (let ((cmd (cadr (elt heads i))))
                (when (listp cmd) ;; when we find a (call-interactively) form
                  (throw t (cadr (cadadr cmd))))))))
         (lead (substring example 0 -1)))

    ;; Go thru heads one by one
    (mapcar
     (lambda (head)

       ;; If it's a chord, do not bother updating. Affects <RET> and the like though.
       (if (> (length (car head)) 1)
           head

         ;; Check out the heads not currently bound. If the
         ;; corresponding hotkey has changed, return a new head.
         (let ((cmd (cadr head)))
           (if (and (stringp cmd)
                    (esm-is-bound lead cmd))
               (esm-head-4 lead cmd)

             ;; Otherwise, check if it's a (call-interactively).
             (if (listp cmd)
                 (esm-head-4 lead (car head))

               ;; Otherwise, return head unmodified.
               head)
             head))))
     heads)))

;; (global-set-key (kbd "C-x 7") 'next-line)

;; It works, but I don't understand how. Why does it not return updated
;; heads with hints, for keys that were previously invisible? For those keys,
;; it returns heads with nil hints as they were, but the funcall does make it all
;; the way into the most nested 'if' call.
(defun esm-update-hints (heads)
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
                 ,(substring hint 0 (min (length hint) esm-colwidth))
                 ,@(cdddr head))
             head))
       head))
   heads))


(provide 'escape-modality)
