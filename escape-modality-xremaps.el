
;; (use-package escape-modality-xremaps
;;   :ensure nilx
;;   :load-path "~/active/code/lisp/remap"
;;   :config
;;   ;; Destroy LCtl and LMeta to learn to use the right-hand variations
;;   (dolist (var esmod-xmodmap-rules)
;;     (when (or (string-match "keycode 64" var)
;;               (string-match "keycode 133" var))
;;       (delq var esmod-xmodmap-rules)))
;;   (push "keycode 64 = VoidSymbol" esmod-xmodmap-rules)
;;   (push "keycode 133 = VoidSymbol" esmod-xmodmap-rules)
;;   (esmod-xmodmap-reload))


(require 'subr-x) ;; for string-join

;; Idea: Make C-m send Return and C-h send Backspace, via xmodmap? Cleaner?
;; Or use key-translation-map for C-h, and use the same method to make
;; Backspace send something else so you don't waste a key.
(defvar esmod-xmodmap-rules
  '("keycode 64 = Control_L"  ;; was Alt_L
    "keycode 108 = Control_R" ;; was Alt_R
    "keycode 133 = Meta_L"   ;; was Super_L
    "keycode 105 = Meta_R"   ;; was Control_R
    "keycode 66 = Super_L"  ;; was Caps Lock
    "keycode 36 = Super_R"  ;; was Return
    "keycode 23 = F18"     ;; was Tab
    "keycode 9 = F19"     ;; was Escape
    "keycode 22 = F20"   ;; was Backspace
    ;; necessary for exwm simulation keys to send them
    "keycode any = Return"
    "keycode any = Tab"
    "keycode any = Escape"
    "keycode any = BackSpace"
    ;; necessary for xcape to send them
    "keycode any = F13"
    "keycode any = F14"
    "keycode any = F15"
    "keycode any = F16"
    "keycode any = F17"
    "keycode any = F23"
    "keycode any = F24"
    "keycode any = F25"
    ;; the modifier table gets changed, restore.
    "clear control"
    "clear mod1"
    "clear mod2"
    "clear mod3"
    "clear mod4"
    "clear mod5"
    "add control = Control_L"
    "add control = Control_R"
    "add mod1 = Meta_L"
    "add mod3 = Alt_L"
    "add mod4 = Super_L"
    "add mod5 = Hyper_L"))

(defvar esmod-xcape-rules
  '("Control_L=F13"
    "Control_R=F13"
    "Meta_L=F14"
    "Meta_R=F14"
    "Super_L=F15"
    "Super_R=F25"
    "Hyper_L=F16"
    "Hyper_R=F16"
    "Alt_L=F17"
    "Alt_R=F17"))

(defun esmod-xmodmap-reload ()
  (when (executable-find "xmodmap")
    (message "Loading esmod-xmodmap-rules.")
    (shell-command
     (concat "xmodmap -e '" (string-join esmod-xmodmap-rules "' -e '") "'"))))

(defun esmod-xcape-reload ()
  (interactive) ;; user may need to call it because it can bug out
  (when (executable-find "xcape")
    (if (executable-find "pkill")
        (shell-command "pkill xcape")
      (if (executable-find "killall")
          (shell-command "killall -9 xcape")
        (message "Program pkill not found, so not killing previous instances of xcape.")))
    (shell-command
     (concat "xcape -e '" (string-join esmod-xcape-rules ";") "'"))))

(provide 'escape-modality-xremaps)
