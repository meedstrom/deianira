;;; deianira-tests.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021
;;
;; Author:  <http://github/me>
;; Maintainer:  <me@debian>
;; Created: January 06, 2021
;; Modified: January 06, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/me/deianira-tests
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:


(ert-deftest test-dei--current-bindings ()
  (let ((foo (dei--current-bindings)))
    (should (-none-p #'null (-map #'dei--valid-keydesc (-map #'car foo))))
    ;; (should (not (null (seq-find (lambda (x) (dei--subhydra-or-nil (car ()))) foo))))
    ))

(ert-deftest keydesc-handling-1 ()
  (should (dei--key-seq-mixes-modifiers "C-h M-o"))
  (should-not (dei--key-seq-mixes-modifiers "C-h C-o"))
  (should-not (dei--key-seq-mixes-modifiers "C-h o"))
  (should (dei--key-contains-multi-chords "C-M-f"))
  (should-not (dei--key-contains-multi-chords "C-f M-f"))
  ;; (should (dei--key-contains-multi-chords "C-<M-return>")) ;; fail ok b/c we assume normalized input
  (should (dei--key-has-more-than-one-modifier "C-x C-h"))
  (should-not (dei--key-has-more-than-one-modifier "C-x H"))
    (should (equal (dei--get-parent "C-x ") #'dei-control/body))
  (should (equal (dei--get-parent "M-x ") #'dei-meta/body))
  (should (equal (dei--get-parent "s-x ") #'dei-super/body))
  (should (equal (dei--get-parent "s-x a") nil))
  (should (equal (dei--get-parent "s-x a ") #'dei-sx/body))
  ;; (should (equal (dei--get-parent "s-x <print>") nil)) ;; probably not a major problem
  (should (equal (dei--get-parent "s-x <print> ") #'dei-sx/body)))


(ert-deftest dei--normalize-components ()
  (should (equal (dei--normalize-trim-segment "<next>") "next"))
  (should (equal (dei--normalize-trim-segment "<C-next>") "C-next"))
  (should (equal (dei--normalize-trim-segment "<") "<"))
  (should (equal (dei--normalize-trim-segment "C-<") "C-<"))
  (should (equal (dei--normalize-trim-segment "C->") "C->"))
  (should (equal (dei--normalize-trim-segment ">") ">"))
  (should (equal (dei--normalize-get-atoms "C--") '("C" "-")))
  (should (equal (dei--normalize-get-atoms "C-f") '("C" "f")))
  (should (equal (dei--normalize-get-atoms "C-M-f") '("C" "M" "f")))
  (should (equal (dei--normalize-wrap-leaf-maybe '("C" "M" "next")) '("C" "M" "<next>")))
  (should (equal (dei--normalize-wrap-leaf-maybe '("C" "M" ">")) '("C" "M" ">")))
  (should-not (equal (dei--normalize-wrap-leaf-maybe '("C" "M" "f")) '("C" "M" "<f>")))
  (should (equal (dei--normalize-build-segments '("C" "M" "-")) "C-M--")))

(ert-deftest keydesc-handling-2 ()
  (let ((problematic-key-descriptions
         '(;; raw          normalized       squashed           leaf      1step?
           ("C-x 8 RET"    "C-x 8 <RET>"    "dei-Cx8<RET>"     "<RET>"   nil                                )
           ("<f2> 8 RET"   "<f2> 8 <RET>"   "dei-<f2>8<RET>"   "<RET>"   nil                                )
           ("<f2> f r"     "<f2> f r"       "dei-<f2>fr"       "r"       nil                                )
           ("<f2> <f2>"    "<f2> <f2>"      "dei-<f2><f2>"     "<f2>"    nil                              )
           ("ESC <C-down>" "<ESC> C-<down>" "dei-<ESC>C<down>" "<down>"  nil                             )
           ("C-x RET C-\\" "C-x <RET> C-\\" "dei-Cx<RET>C\\"   "\\"      nil                                 )
           ("TAB"          "TAB"            "dei-TAB"          "TAB"       t                        )
           ("A-T A-B"      "A-T A-B"        "dei-ATAB"         "B"       nil                                  )
           ("A-T A B"      "A-T A B"        "dei-ATAB"         "B"       nil                                  )
           ("A-TAB"        "A-TAB"          "dei-ATAB"         "TAB"       t                                  )
           ("C-<M-return>" "C-M-<return>"   "dei-CM<return>"   "<return>"  t                                  )
           ("<C-M-return>" "C-M-<return>"   "dei-CM<return>"   "<return>"  t                                  )
           ;; ("C-- - -"      "C-- - -"         "dei-C---"       "-"        nil  )
           ;; TODO: Because see  (kbd "TAB")  (kbd "<TAB>")
           ;; ("<TAB>"        "<TAB>"          "dei-<TAB>"        "<TAB>" t  )
           ;; ("s-S-M-H-C-A-<return>" "A-C-H-M-S-s-<return>" "dei-ACHMSs<return>" "<return>" t)
           )))
    (dolist (case problematic-key-descriptions)
      (seq-let (raw normalized squashed leaf 1step?) case
        (should (string= normalized (dei--normalize raw)))
        (should (string= squashed (dei-dub-from-key normalized)))
        (should (string= leaf (dei--get-leaf normalized)))
        (should (eq 1step? (dei--key-seq-steps=1 normalized)))))))

(ert-deftest keydesc-handling-limited ()
  "Functions that have strong assumptions on input, only on
such strings that meet the assumptions."
  (let ((example-key-descriptions
         '(;; normalized        parent
           ("C-x 8 <RET>"  "C-x 8"                              )
           ("<f2> 8 <RET>" "<f2> 8"                             )
           ("<f2> f r"   "<f2> f"                             )
           ("<f2> <f2>"  "<f2>"                             )
           ("TAB" nil)
           ("A-TAB" "A-"                             )
           ;; ("C-- - -"      "C-- - -"         "dei-C---"       "-"        nil  )
           ;; ("<TAB>"        "<TAB>"          "dei-<TAB>"        "<TAB>" t  )
           )))
    (dolist (case example-key-descriptions)
      (seq-let (normalized parent) case
        (should (string= parent (dei--parent-stem normalized)))))))


(ert-deftest generate-heads ()
  (should (equal (dei-head "C-" "f")
                 '("f" (call-interactively (key-binding (kbd "C-f"))) "forward-char")))
  (should (equal (dei-head "C-x " "f")
                 '("f" (call-interactively (key-binding (kbd "C-x f"))) "set-fill-column")))
  (should (equal (dei-head-invisible "C-" "f")
                 '("f" (call-interactively (key-binding (kbd "C-f"))) nil :exit nil)))
  (should (equal (dei-head-hint "C-" "f")
                 (symbol-name (key-binding (kbd "C-f")))))
  (should (equal (dei-head-hint "C-" "<f12>")
                 " "))
  ;; weird case, behaves different interactively than when called by ert
  ;; (should (equal (dei-head-hint "C-" "x") "Control-X-prefi"))
  (should (equal (dei--specify-extra-heads "C-x ")
                 '(("<backspace>" dei-control/body nil :exit t))))
  (should (equal (dei--specify-extra-heads "C-")
                 '(("<backspace>" nil nil :exit t) ("<f35>" nil nil :exit t)))))

(provide 'deianira-tests)
;;; deianira-tests.el ends here
