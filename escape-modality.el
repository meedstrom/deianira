;; escape-modality.el -- indescribable as yet
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

;; warnings: odd behavior if a head that calls another hydra is not set to exit

(require 'hydra)
(require 'escape-modality-common)
(require 'escape-modality-x11)
(require 'escape-modality-enforce-tidy)
(eval-when-compile (require 'subr-x)) ;; string-join

;;;;;;;;;;;;;;;
;;; Micro level

;; wip. So they can be unbound by simply disabling the mode
(setq esm-mode-map (make-sparse-keymap))
(define-key esm-mode-map (kbd "<f35>") 'esm-control/body)
(define-key esm-mode-map (kbd "<f34>") 'esm-meta/body)
(define-key esm-mode-map (kbd "<f33>") 'esm-super/body)

(define-minor-mode esm-mode
  "Bind hydras on Control, Meta etc if `esm-xcape-rules' are
enabled and functional.

Not to be confused with binding hydras on every prefix key, which
is to be done only if you hate which-key."
  nil
  "ESM"
  'esm-mode-map)


;; TODO: adapt to changing frame sizes
(defvar esm-colwidth
  (let ((optimal (- (round (frame-parameter nil 'width) 10) 4)))
    (max optimal 8)))

(defvar esm-quitters '(;;"C-q"
                       "C-s" "C-g" "s-s" "s-g" "C-u" "M-x" "M-u" "s-u" "C-2" "C-c c"))

(defvar esm-noquitters '("C-2 t" "C-2 n" "C-2 p" "C-x ;" "C-x x" "C-x q"
                           "C-x h" "C-x u" "C-x i" "C-x p" "C-x l" "C-x 1"
                           "C-x 2" "C-x 3" "C-x 0" "C-c c"))

(defun esm-is-a-subhydra (stem leaf)
  "Returns the hydra body in question."
  (if-let ((x (cdr (assoc (concat stem leaf) esm-live-hydras))))
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
  (if (string-match "^C" hotkey)
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

(defun esm-is-named-prefix (stem leaf)
  (car-safe
   (member (global-key-binding (kbd (concat stem leaf))) esm-named-prefixes)))

(defvar esm-named-prefixes '(Control-X-prefix
                             mode-specific-command-prefix
                             ctl-x-4-prefix
                             ctl-x-5-prefix
                             vc-prefix-map
                             help-command
                             2C-command
                             ESC-prefix
                             projectile-command-map))

(defun esm-cmd (stem leaf)
  (global-key-binding (kbd (concat stem leaf))))

(defun esm-is-bound (stem leaf)
  (not (eq nil (esm-cmd stem leaf))))

(defun esm-is-bound* (stem leaf)
  (let ((cmd (global-key-binding (kbd (concat stem leaf)))))
    (not (eq cmd nil))))

(defun esm-is-unbound (stem leaf)
  (not (esm-is-bound stem leaf)))

(defun esm-is-unnamed-prefix (stem leaf)
  (let ((cmd (global-key-binding (kbd (concat stem leaf)))))
    (not (symbolp cmd))))


(defun esm-is-prefix-or-unbound (stem leaf)
  "Return t if the hotkey described by concatenating STEM and
LEAF is a prefix command, such as C-x or C-h."
  (string-match
   "prefix\\|nil\\|help-command\\|2C-command"
   (symbol-name (let ((cmd (global-key-binding (kbd (concat stem leaf)))))
                  (when (symbolp cmd) cmd)))))

(defun esm-is-prefix-or-unbound-4 (stem leaf)
  (when (or (esm-is-a-subhydra stem leaf)
            (esm-is-unbound stem leaf))
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

(defun esm-cmd-conservative (stem leaf)
  (if (esm-is-prefix-or-unbound stem leaf)
      leaf
    `(call-interactively (key-binding (kbd ,(concat stem leaf))))))

(defun esm-cmd-4 (stem leaf)
  (if (esm-is-unbound stem leaf) leaf
    (or (when (string= leaf "u")
          #'esm-universal-arg)
        (when (and (string= stem "C-c ")
                   (string= leaf "c"))
          #'esm-cc-cc)
        (esm-is-a-subhydra stem leaf)
        `(call-interactively (key-binding (kbd ,(concat stem leaf)))))))

(defun esm-cmd-3 (stem leaf)
  (or (when (string= leaf "u")
        #'esm-universal-arg)
      (esm-is-a-subhydra stem leaf)
      `(call-interactively (key-binding (kbd ,(concat stem leaf))))))

(defun esm-hint-3 (stem leaf)
  (let* ((sym (or (esm-is-a-subhydra stem leaf)
                  (esm-is-named-prefix stem leaf)
                  (global-key-binding (kbd (concat stem leaf)))))
         (name (if (symbolp sym) (symbol-name sym) " ")))
    (if (string= name "nil")
        " "
      (if (> (length name) esm-colwidth)
          (substring name 0 esm-colwidth)
        name))))

(defun esm-key-4 (stem leaf)
  "Return either LEAF or a string containing a single space. This
can be used to make a visibly blank spot in a hydra for hotkeys
that are unbound."
  (if (esm-is-unbound stem leaf) " " leaf))

(defun esm-key (stem leaf)
  "If the given hotkey is bound to a command, return LEAF,
otherwise return a space character. This can be used to
make a visibly blank spot in a hydra for hotkeys that are unbound."
  (if (stringp (esm-cmd-conservative stem leaf)) " " leaf))

(defun esm-exit-almost-never (stem leaf)
  (when (or (member (concat stem leaf) esm-quitters)
            (esm-is-unbound stem leaf))
    t))

(defun esm-exit-4 (stem leaf)
  (cond ((member (concat stem leaf) esm-quitters) '(:exit t))
        ((member (concat stem leaf) esm-noquitters) '(:exit nil))
        ((esm-is-a-subhydra stem leaf) '(:exit t)) ;; very important
        ((esm-is-unbound stem leaf) '(:exit t))
        (t '()))) ;; defer to hydra's default behavior

(defun esm-exit-2 (stem leaf)
  (cond ((member (concat stem leaf) esm-quitters) '(:exit t))
        ((member (concat stem leaf) esm-noquitters) '(:exit nil))
        ((esm-is-prefix-or-unbound stem leaf) '(:exit t))
        (t '()))) ;; defer to hydra's default behavior

(defun esm-head-4 (stem leaf)
  `( ,(esm-key-4 stem leaf) ,(esm-cmd-4 stem leaf) ,(esm-hint-3 stem leaf)
     ,@(esm-exit-4 stem leaf)))

(defun esm-head-3 (stem leaf)
  `( ,leaf ,(esm-cmd-3 stem leaf) ,(esm-hint-3 stem leaf)
           ,@(esm-exit-2 stem leaf)))

(defun esm-head-invisible (stem leaf)
  `( ,(esm-key stem leaf) ,(esm-cmd-conservative stem leaf) nil
     :exit ,(esm-exit-almost-never stem leaf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macro level: tying it all together.


(defmacro esm-defmode
    (name stem &optional doctitle pop-key parent exit keys)
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
     ,@(mapcar (lambda (char) (esm-head-4 stem (string char)))
               (or keys esm-hydra-keys))
     ,@(mapcar (lambda (leaf) (esm-head-invisible stem leaf))
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
           (stem (concat key " "))
           (parent-key (substring key 0 -2)) ;; Doesnt bug with <RET>, strange!
           (parent-symname (cdr (assoc parent-key esm-live-hydras)))
           (parent-body (cond ((string= parent-key "C") #'esm-control/body)
                              ((string= parent-key "M") #'esm-meta/body)
                              ((string= parent-key "C-M") #'esm-super/body)
                              ((assoc parent-key esm-live-hydras)
                               (intern (concat parent-symname "/body"))))))
      (eval `(progn
               (esm-defmode ,title       ,stem ,key nil ,parent-body t)
               (esm-defmode ,nonum-title ,stem ,(concat "\n\n" key) nil ,body t "qwertyuiopasdfghjkl;zxcvbnm,./")
               (define-key ,nonum-keymap "u" #'hydra--universal-argument)
               (dotimes (i 10)
                 (define-key ,keymap (int-to-string i) nil))))
      (set hint `(eval (hydra--format nil '(nil nil :columns 10)
                                      ,key (esm-update-hints ,heads)))))))


(defun esm-scan ()
  "Scan for global prefix keys to populate `esm-live-hydras'.
This may take a couple of seconds, so you may want to set the
variable in your init file directly, by copy-pasting the output
of this function."
  (setq esm-live-hydras nil)
  (dolist (x (esm-whole-keyboard))
    (when (keymapp (global-key-binding (kbd x)))
      (push `(,x . ,(esm-dub-from-key x))
            esm-live-hydras))))

(defun esm-generate-hydras ()
  (dolist (x esm-live-hydras)
    (esm-define-global-hydra-maybe (car x) (cdr x))))

;; I guess it's not really the whole keyboard
(defun esm-whole-keyboard ()
  (let (biglist)
    (dolist (chord esm-all-simple-chords)
      (dolist (key esm-all-keys-on-keyboard-except-shifted-symbols)
        (push (concat chord " " key) biglist))
      (push chord biglist))
    biglist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Change Emacs state. Should go in user init file.

;; Stage 1

(setq esm-debug "*Escape-modality debug*")
(setq esm-C-x-C-key-overwrites-C-x-key t)

;; Prevent clobbering useful commands. Should go in user init file.
(if esm-C-x-C-key-overwrites-C-x-key
    (progn
      (global-set-key (kbd "C-x C-a") (key-binding (kbd "C-x a"))) ;;abbrev-map
      (global-set-key (kbd "C-x C-h") #'mark-whole-buffer)
      (global-set-key (kbd "C-x C-n") #'narrow-to-region))
  (global-set-key (kbd "C-x e") #'eval-last-sexp)
  (global-set-key (kbd "C-x f") #'find-file)
  (global-set-key (kbd "C-x c") #'save-buffers-kill-emacs)
  (global-set-key (kbd "C-x x") #'exchange-point-and-mark)
  (global-set-key (kbd "C-x o") #'delete-blank-lines))

(esm-flatten-ctl-x)

;; Stage 2
(esm-kill-shift)

;; Stage 3
;; (esm-super-map-from-ctl-meta)
;; (esm-super-translate-to-ctl-meta)
(esm-super-from-ctl)

;; Stage 4

(esm-scan)
(esm-generate-hydras)

;; (add-hook 'buffer-list-update-hook #'esm-generate-hydras)

(esm-defmode esm-control       "C-"  "CONTROL" "C-<f13>")
(esm-defmode esm-meta          "M-"  "META"  "M-<f14>")
;; (esm-defmode esm-super         "C-M-"  "SUPER" "s-<f15>")
(esm-defmode esm-super         "s-"  "SUPER" "s-<f15>")

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

  ;; Figure out the prefix/"stem" for this hydra by iterating until a head is found.
  (let* ((example
          (catch t
            (dotimes (i 30)
              (let ((cmd (cadr (elt heads i))))
                (when (listp cmd) ;; when we find a (call-interactively ...)
                  (throw t (cadr (cadadr cmd))))))))
         (stem (substring example 0 -1)))

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
                    (esm-is-bound stem cmd))
               (esm-head-4 stem cmd)

             ;; Otherwise, check if it's a (call-interactively).
             (if (listp cmd)
                 (esm-head-4 stem (car head))

               ;; Otherwise, return head unmodified.
               head)
             head))))
     heads)))

;; How to detect a change in the keymap?
;; https://github.com/abo-abo/hydra/wiki/Conditional-Hydra
;; Instead of re-generating hydra, maybe you'll have luck with altering the /keymap.
(defun esm-update-hydra-maybe (heads)

  ;; Figure out the prefix/"stem" for this hydra by iterating until a head is found.
  (let* ((heads esm-Cx/heads)
         (example
          (catch t
            (dotimes (i 30)
              (let ((cmd (cadr (elt heads i))))
                (when (listp cmd) ;; when we find a (call-interactively ...)
                  (throw t (cadr (cadadr cmd))))))))
         (stem (substring example 0 -1)))

    (if (catch t
          (mapc (lambda (head)
                  (if (> (length (car head)) 1)
                      nil
                    (let ((cmd (cadr head)))
                      (cond ((and (stringp cmd) (esm-is-bound stem cmd))
                             (throw t t))
                            ((and (listp cmd) (esm-is-unbound stem (car head)))
                             (throw t t))))))
                heads))
        (eval '(esm-define-global-hydra-maybe (substring stem 0 -1))))))


;; let's use htis in place of the update-hints thing. Return a list of heads for
;; passing to hydra--format, but also actually set this thing globally.
(defun esm-update-heads (heads)

  ;; Figure out the prefix (aka "stem") for this hydra by iterating until a
  ;; head is found, any head.
  (let* ((example
          (catch t
            (dotimes (i 30)
              (let ((cmd (cadr (elt heads i))))
                (when (listp cmd) ;; when we find a (call-interactively) form
                  (throw t (cadr (cadadr cmd))))))))
         (stem (substring example 0 -1)))

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
                    (esm-is-bound stem cmd))
               (esm-head-4 stem cmd)

             ;; Otherwise, check if it's a (call-interactively).
             (if (listp cmd)
                 (esm-head-4 stem (car head))

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
