;;; deianira-manual-tests.el --- description -*- lexical-binding: t; -*-

;; Manual tests
;; TODO: Figure out a test that doesn't need me to compare with a list thousands of sexps long
(let ((keys-to-hydraize '("C-x a i" "M-o")))
  (setq dei--requested-hydras
        (append
         (cl-loop for key in keys-to-hydraize
                  collect
                  (let ((stem (concat key " ")))
                    (cons (dei-dub-from-key key)
                          (append (dei--specify-visible-heads stem)
                                  (dei--specify-invisible-heads stem)
                                  (dei--specify-extra-heads stem)))))
         (cl-loop for key in keys-to-hydraize
                  collect
                  (let ((stem (concat key " ")))
                    (cons (concat (dei-dub-from-key key) "-nonum")
                          (append (dei--specify-visible-heads
                                   stem dei--hydra-keys-list-no-numbers)
                                  (dei--specify-invisible-heads stem)
                                  (dei--specify-extra-heads stem))))))))

;; test
(let ((x (car dei--requested-hydras)))
  (dei--define-dire-hydra (car x) (cdr x)))

;; test
(cl-loop for x in dei--requested-hydras
         collect (dei--define-dire-hydra (car x) (cdr x)))


;; EDUCATION -------------------------------------------
;; instructive prior art: which-key--get-keymap-bindings
;; Also see results of evaluating
;; (map-keymap 'readkey1 global-map)
;; (map-keymap 'readkey2 global-map)

(defun readkey1 (ev def)
  (print (key-description (list ev)) (get-buffer-create "*scratch*"))
  (print def (get-buffer-create "*scratch*")))

(defun readkey2 (ev def)
  (when (dei--of-interest-p def)
    (print (key-description (list ev)))
    (print def)))

;; Unfortunately C source, so you can't learn from them:
;; (substitute-command-keys "\\{org-mode-map}")
;; (accessible-keymaps global-map)

;; Other cool functions
;; (keyboard-translate)

;; map-keymap passes events to a function, and those events look like
;; 67108918 or f2
;; and it turns out you can append events instead of concatting strings:
;; (key-description (append (kbd "C-x") (list 67108918)))
;; => "C-x C-6"
;; --------------------------------------------------------



;;; deianira-manual-tests.el ends here
