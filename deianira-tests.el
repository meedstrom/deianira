;;; deianira-tests.el --- description -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'deianira)
(require 'ert)

(ert-deftest test:dei--current-bindings ()
  (let ((foo (dei--current-bindings)))
    (should (-none-p #'null (-map #'dei--valid-keydesc (-map #'car foo))))
    ;; (should (not (null (seq-find (lambda (x) (dei--subhydra-or-nil (car ()))) foo))))
    ))

(ert-deftest keydesc-handling-1 ()

  ;; (dei--contains-upcase "C-x d")
  ;; (dei--key-starts-with-modifier "C-x d")
  ;; (dei--key-seq-is-allchord "C-x d")
  ;; (dei--key-has-more-than-one-modifier "C-x d")

  ;; (dei--key-has-more-than-one-modifier "s-i")
  ;;      (dei--contains-upcase keydesc)
  ;;      (dei--key-contains-multi-chords "s-i")
  ;;      (dei--key-seq-mixes-modifiers "s-i")
  ;; (dei--contains-upcase "s-i")
  ;; (dei--contains-upcase "s-i")

  (should-not (dei--key-seq-has-non-prefix-in-prefix "C-x v x"))
  (should-not (dei--key-seq-has-non-prefix-in-prefix "C-x C-v"))
  (should (dei--key-seq-has-non-prefix-in-prefix "C-x C-v C-x"))

  (should (dei--key-seq-mixes-modifiers "C-h M-o"))
  (should-not (dei--key-seq-mixes-modifiers "C-h C-o"))
  (should-not (dei--key-seq-mixes-modifiers "C-h o"))

  (should (dei--key-contains-multi-chords "C-M-f"))
  (should-not (dei--key-contains-multi-chords "C-f M-f"))
  ;; (should (dei--key-contains-multi-chords "C-<M-return>")) ;; fail ok b/c we assume normalized input

  (should (dei--key-has-more-than-one-modifier "C-x C-h"))
  (should (dei--key-has-more-than-one-modifier "C-M-x h"))
  (should-not (dei--key-has-more-than-one-modifier "<f1> F F"))
  (should-not (dei--key-has-more-than-one-modifier "C-x C"))
  (should-not (dei--key-has-more-than-one-modifier "C-x d"))

  (should (dei--key-seq-involves-shiftsym "C-x H"))
  (should (dei--key-seq-involves-shiftsym "C-F"))
  (should-not (dei--key-seq-involves-shiftsym "C-x h"))
  (should-not (dei--key-seq-involves-shiftsym "<RET>"))
  (should-not (dei--key-seq-involves-shiftsym "TAB"))

  (should (dei--key-starts-with-modifier "C-x"))
  (should (dei--key-starts-with-modifier "C-x c f b"))
  (should (dei--key-starts-with-modifier "C-<f1> f"))
  (should-not (dei--key-starts-with-modifier "<f1> C-f"))

  (should (equal (dei--dub-hydra-from-key "C-x ") "dei-Cx"))
  (should (equal (dei--hydra-from-stem "C-x ") #'dei-Cx/body))

  (should (equal (dei--parent-hydra "C-x ") #'dei-C/body))
  (should (equal (dei--parent-hydra "M-x ") #'dei-M/body))
  (should (equal (dei--parent-hydra "s-x ") #'dei-s/body))
  (should (equal (dei--parent-hydra "s-x a ") #'dei-sx/body))
  (should-error (dei--parent-hydra "s-x a") nil)

  (should (equal (dei--parent-stem "C-x ") "C-"))
  (should (equal (dei--stem-to-parent-keydesc "C-x ") "C-x"))

  ;; debatable if it should have these effects or be named this way
  (should (equal (dei--parent-key "C-x ") "C-x"))
  (should (equal (dei--parent-key "C-x C-") "C-x"))
  (should (equal (dei--parent-key "C-x C-x") "C-x"))

  (should (equal "C-x k e" (dei--ensure-chordonce "C-x C-k C-e")))
  (should (equal "x k e" (dei--ensure-chordonce "x C-k C-M-e")))

  (should (equal "C-x C-k C-e" (dei--ensure-permachord "C-x k C-e")))
  (should (equal "C-x C-k C-e" (dei--ensure-permachord "C-x k C-e")))
  (should (equal "C-x C-k C-e" (dei--ensure-permachord "C-x k e")))
  (should (equal "C-x C-k C-e" (dei--ensure-permachord "x k e")))
  (should (equal "C-x C-k C-e" (dei--ensure-permachord "C-x C-k C-e")))

  (should (dei--key-is-permachord "C-x C-k C-n"))
  (should-not (dei--key-is-permachord "C-x C-k n"))
  (should-not (dei--key-is-permachord "C-M-x C-k C-M-n"))
  (should-not (dei--key-is-permachord "<f3> C-k C-M-n"))

  (should (dei--immediate-child-p "C-x " "C-x f"))
  (should (dei--immediate-child-p "C-x C-" "C-x C-f"))
  ;; (should-not (dei--immediate-child-p "C-x C-" "C-x C-M-f"))
  (should-not (dei--immediate-child-p "C-x C-" "C-x f"))
  (should-not (dei--immediate-child-p "C-x C-" "C-x C-f f"))

  )

(ert-deftest components-of:dei--normalize ()
  (should (equal (dei--normalize-trim-segment "<next>") "next"))
  (should (equal (dei--normalize-trim-segment "<C-next>") "C-next"))
  (should (equal (dei--normalize-trim-segment "<") "<"))
  (should (equal (dei--normalize-trim-segment "C-<") "C-<"))
  (should (equal (dei--normalize-trim-segment "C->") "C->"))
  (should (equal (dei--normalize-trim-segment ">") ">"))

  (should (equal (dei--normalize-get-atoms "C--") '("C" "-")))
  (should (equal (dei--normalize-get-atoms "C-f") '("C" "f")))
  (should (equal (dei--normalize-get-atoms "M-wheel-down") '("M" "wheel-down")))
  (should (equal (dei--normalize-get-atoms "C-M-f") '("C" "M" "f")))

  (should (equal (dei--normalize-wrap-leaf-maybe '("C" "M" "next")) '("C" "M" "<next>")))
  (should (equal (dei--normalize-wrap-leaf-maybe '("C" "M" ">")) '("C" "M" ">")))
  (should-not (equal (dei--normalize-wrap-leaf-maybe '("C" "M" "f")) '("C" "M" "<f>")))

  (should (equal (dei--normalize-build-segments '("C" "M" "-")) "C-M--")))

(ert-deftest keydesc-handling-2 ()
  (let ((problematic-key-descriptions
         '(;; raw            normalized       squashed            leaf      1step?
           ("C-x 8 RET"      "C-x 8 <RET>"    "dei-Cx8<RET>"      "<RET>"      nil)
           ("<f2> 8 RET"     "<f2> 8 <RET>"   "dei-<f2>8<RET>"    "<RET>"      nil)
           ("<f2> f r"       "<f2> f r"       "dei-<f2>fr"        "r"          nil)
           ("<f2> <f2>"      "<f2> <f2>"      "dei-<f2><f2>"      "<f2>"       nil)
           ("ESC <C-down>"   "<ESC> C-<down>" "dei-<ESC>C<down>"  "<down>"     nil)
           ("C-x RET C-\\"   "C-x <RET> C-\\" "dei-Cx<RET>C\\"    "\\"         nil)
           ("TAB"            "TAB"            "dei-TAB"           "TAB"          t)
           ("A-T A-B"        "A-T A-B"        "dei-ATAB"          "B"          nil)
           ("A-T A B"        "A-T A B"        "dei-ATAB"          "B"          nil)
           ("A-TAB"          "A-TAB"          "dei-ATAB"          "TAB"          t)
           ("C-<M-return>"   "C-M-<return>"   "dei-CM<return>"    "<return>"     t)
           ("<C-M-return>"   "C-M-<return>"   "dei-CM<return>"    "<return>"     t)
           ("<M-wheel-down>" "M-<wheel-down>" "dei-M<wheel-down>" "<wheel-down>" t)
           ;; ("C-- - -"      "C-- - -"         "dei-C---"       "-"        nil  )
           ;; TODO: Because see  (kbd "TAB")  (kbd "<TAB>")
           ;; ("<TAB>"        "<TAB>"          "dei-<TAB>"        "<TAB>" t  )
           ;; ("s-S-M-H-C-A-<return>" "A-C-H-M-S-s-<return>" "dei-ACHMSs<return>" "<return>" t)
           )))
    (dolist (case problematic-key-descriptions)
      (seq-let (raw normalized squashed leaf 1step?) case
        (should (string= normalized (dei--normalize raw)))
        (should (string= squashed (dei--dub-from-key normalized)))
        (should (string= leaf (dei--get-leaf normalized)))
        (should (eq 1step? (dei--key-seq-steps=1 normalized)))))))

(ert-deftest keydesc-handling-limited ()
  "Test the functions that have strong assumptions on input."
  (let ((example-key-descriptions
         '(;; normalized        parent
           ("C-x 8 <RET>"   "C-x 8")
           ("<f2> 8 <RET>" "<f2> 8")
           ("<f2> f r"     "<f2> f")
           ("<f2> <f2>"    "<f2>")
           ("TAB"         nil)
           ("A-TAB"       "A-")
           ;; ("C-- - -"      "C-- - -"         "dei-C---"       "-"        nil  )
           ;; ("<TAB>"        "<TAB>"          "dei-<TAB>"        "<TAB>" t  )
           )))
    (dolist (case example-key-descriptions)
      (seq-let (normalized parent) case
        (should (string= parent (dei--parent-stem normalized)))))))


(ert-deftest generate-heads ()
  (setq dei--colwidth 30)
  (setq dei--filler (dei--filler-recalc))
  (should (equal (length (dei--filler-recalc)) 30))
  (should (equal (dei--head "C-" "f")
                 '("f" (call-interactively (key-binding (kbd "C-f"))) "forward-char")))
  (should (equal (dei--head "C-x " "f")
                 '("f" (call-interactively (key-binding (kbd "C-x f"))) "set-fill-column")))
  (should (equal (dei--head-invisible "C-" "f")
                 '("f" (call-interactively (key-binding (kbd "C-f"))) nil :exit nil)))
  (should (equal (dei--head-arg-hint "C-" "f")
                 (symbol-name (key-binding (kbd "C-f")))))
  (should (equal (dei--head-arg-hint "C-" "<f12>")
                 dei--filler))
  ;; weird case, behaves different interactively than when called by ert
  ;; (should (equal (dei-head-hint "C-" "x") "Control-X-prefi"))
  (should (equal (dei--specify-extra-heads "C-x ")
                 '(("<backspace>" dei-control/body nil :exit t))))
  (should (equal (dei--specify-extra-heads "C-")
                 '(("<backspace>" nil nil :exit t) ("<f35>" nil nil :exit t))))

  ;; Tests
;; (dei--specify-extra-heads "M-s ")
;; (dei--specify-extra-heads "M-s M-")
;; (dei--specify-extra-heads "M-")
;; (dei--specify-extra-heads "M-" t)
;; (setq foo (dei--specify-dire-hydra "M-s "))
;; (dei--specify-invisible-heads "C-")
;; (append nil (-map #'car (dei--specify-extra-heads "C-")))

  )


(ert-deftest homogenizing ()
  (setq dei-homogenizing-winners '(("C-c C-c")
                                ("C-x a")
                                ("C-x g")
                                ("C-x b")
                                ("C-c C-c" . org-mode-map)))

  ;; (dei--homogenize-binding-in-keymap "C-c C-c" 'org-mode-map)
  ;; (dei--homogenize-binding-in-keymap "C-c c" 'org-mode-map)
  ;; (dei--homogenize-binding "C-x f")
  ;; (dei--homogenize-binding "")
  ;; (dei--homogenize-binding "C-c c")
  ;; (dei--homogenize-binding "")
  )


(provide 'deianira-tests)
;;; deianira-tests.el ends here
