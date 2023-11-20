;;; deianira-tests.el -*- lexical-binding: t; -*-

(require 'deianira)
(require 'ert)

(ert-deftest dei-test-keydesc-handling ()

  (should (equal (dei--corresponding-hydra (massmapper--parent-stem "C-x ")) #'dei-C/body))
  (should (equal (dei--corresponding-hydra (massmapper--parent-stem "M-x ")) #'dei-M/body))
  (should (equal (dei--corresponding-hydra (massmapper--parent-stem "s-x ")) #'dei-s/body))
  (should (equal (dei--corresponding-hydra (massmapper--parent-stem "s-x a ")) #'dei-sx/body))
  (should-error (dei--corresponding-hydra (massmapper--parent-stem "s-x a")))
  (should (equal (dei--corresponding-hydra (massmapper--parent-stem "C-")) nil))

  (should (massmapper--key-contains dei--all-shifted-symbols-list "M-F"))
  (should (massmapper--key-contains dei--all-shifted-symbols-list "C-M-F"))
  (should (massmapper--key-contains dei--all-shifted-symbols-list "F"))
  (should-not (massmapper--key-contains dei--all-shifted-symbols-list "M-f"))
  (should-not (massmapper--key-contains dei--all-shifted-symbols-list "C-M-f"))
  (should-not (massmapper--key-contains dei--all-shifted-symbols-list "RET"))

  ;; Not a big fan of the angle bracket protectors. Think I might bank on
  ;; mandating no capitals (or always rephrasing them to have S- out front),
  ;; then upcasing everything that'd normally go between angle brackets. Which
  ;; also means I have to mandate that you can't make hydras from both TAB and
  ;; <tab>, but that's cool bc it implies I can use RET as a shorthand for
  ;; <return>.
  (should (equal (dei--dub-hydra-from-key-or-stem "C-x 8 RET") "dei-Cx8<RET>"))
  (should (equal (dei--dub-hydra-from-key-or-stem "<f2> 8 RET") "dei-<f2>8<RET>"))
  (should (equal (dei--dub-hydra-from-key-or-stem "<f2> f r") "dei-<f2>fr"))
  (should (equal (dei--dub-hydra-from-key-or-stem "<f2> <f2>") "dei-<f2><f2>"))
  (should (equal (dei--dub-hydra-from-key-or-stem "ESC C-<down>") "dei-<ESC><down>"))
  (should (equal (dei--dub-hydra-from-key-or-stem "C-x RET C-\\") "dei-Cx<RET>\\"))
  (should (equal (dei--dub-hydra-from-key-or-stem "C-x RET C-> <") "dei-Cx<RET>><"))
  (should (equal (dei--dub-hydra-from-key-or-stem "M-<wheel-down>") "dei-M<wheel-down>"))
  (should (equal (dei--dub-hydra-from-key-or-stem "C-- - -") "dei-C---"))
  (should (equal (dei--dub-hydra-from-key-or-stem "C--") "dei-C-"))
  (should (equal (dei--dub-hydra-from-key-or-stem "C-") "dei-C"))
  (should (equal (dei--dub-hydra-from-key-or-stem "-") "dei--"))
  (should (equal (dei--dub-hydra-from-key-or-stem "TAB") "dei-<TAB>"))
  (should (equal (dei--dub-hydra-from-key-or-stem "<tab>") "dei-<tab>"))

  ;; apparent conflicts are nonissue due to homogenizing
  (should (equal (dei--dub-hydra-from-key-or-stem "A-T A-B") "dei-ATB"))
  (should (equal (dei--dub-hydra-from-key-or-stem "A-T A B") "dei-ATAB"))
  (should (equal (dei--dub-hydra-from-key-or-stem "C-x C-f") "dei-Cxf"))
  (should (equal (dei--dub-hydra-from-key-or-stem "C-x f") "dei-Cxf"))

  (should (equal (dei--dub-hydra-from-key-or-stem "s x s x") "dei-sxsx"))
  ;; a conflict that's likely to never play a role
  (should (equal (dei--dub-hydra-from-key-or-stem "s x s-x") "dei-sxx"))
  (should (equal (dei--dub-hydra-from-key-or-stem "s-x s-x") "dei-sxx"))

  ;; not sane result, but ok due to filters in
  ;; `dei--unnest-and-filter-current-bindings'
  (should (equal (dei--dub-hydra-from-key-or-stem "C-M-<return>") "dei-C<return>"))

  )

(ert-deftest dei-test-unnest ()
  (let ((foo (dei--unnest-and-filter-current-bindings)))
    (should (-none-p #'not (-map #'key-valid-p (-map #'car foo))))
    (should (-none-p #'keymapp (-map #'cdr foo)))
    (should (--all-p (member (car it) '(closure lambda))
                     (-filter #'listp (-map #'cdr foo))))
    ;; (should (not (null (seq-find (lambda (x) (dei--subhydra-or-nil (car ()))) foo))))
    ))

;; meant to run in emacs -Q
(ert-deftest dei-test-generate-heads ()
  (let ((dei--colwidth 30)
        (dei--filler (dei--filler-recalc 30)))
    (should (equal (length (dei--filler-recalc dei--colwidth)) 30))
    (should (equal (dei--head "C-" "f")
                   `("f" (call-interactively (key-binding ,(key-parse "C-f"))) "forward-char")))
    (should (equal (dei--head "C-x C-" "f")
                   `("f" (call-interactively (key-binding ,(key-parse "C-x C-f"))) "find-file")))
    (should (equal (dei--head-invisible "C-" "f")
                   `("f" (call-interactively (key-binding ,(key-parse "C-f"))))))
    (should (equal (dei--head-arg-hint "C-" "f")
                   (symbol-name (key-binding (key-parse "C-f")))))
    (should (equal (dei--head-arg-hint "C-" "<f12>")
                   dei--filler))
    (should (equal (dei--head-arg-hint "C-" "x")
                   "dei-Cx/body"))
    (should (equal (dei--specify-extra-heads "C-x ")
                   '(("<backspace>" dei-C/body :exit t))))
    (should (equal (dei--specify-extra-heads "C-")
                   '(("<backspace>" nil :exit t)
                     ("<katakana>" nil :exit t)
                     ("C-<katakana>" nil:exit t))))

    ;; Tests
    ;; (dei--specify-extra-heads "M-s ")
    ;; (dei--specify-extra-heads "M-s M-")
    ;; (dei--specify-extra-heads "M-")
    ;; (dei--specify-extra-heads "M-" t)
    ;; (setq foo (dei--specify-dire-hydra "M-s "))
    ;; (dei--specify-invisible-heads "C-")
    ;; (append nil (-map #'car (dei--specify-extra-heads "C-")))

    ))

(provide 'deianira-tests)
;;; deianira-tests.el ends here
