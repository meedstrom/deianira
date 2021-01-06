;;; escape-modality-tests.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021
;;
;; Author:  <http://github/me>
;; Maintainer:  <me@debian>
;; Created: January 06, 2021
;; Modified: January 06, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/me/escape-modality-tests
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:


(ert-deftest test-esm--current-bindings ()
  (let ((foo (esm--current-bindings)))
    (should (-none-p #'null (-map #'esm--valid-keydesc (-map #'car foo))))
    ;; (should (not (null (seq-find (lambda (x) (esm--subhydra-or-nil (car ()))) foo))))
    ))

(ert-deftest keydesc-handling-1 ()
  (should (esm--key-seq-mixes-modifiers "C-h M-o"))
  (should-not (esm--key-seq-mixes-modifiers "C-h C-o"))
  (should-not (esm--key-seq-mixes-modifiers "C-h o"))
  (should (esm--key-contains-multi-chords "C-M-f"))
  (should-not (esm--key-contains-multi-chords "C-f M-f"))
  ;; (should (esm--key-contains-multi-chords "C-<M-return>")) ;; fail ok b/c we assume normalized input
  (should (esm--key-has-more-than-one-modifier "C-x C-h"))
  (should-not (esm--key-has-more-than-one-modifier "C-x H"))
    (should (equal (esm--get-parent "C-x ") #'esm-control/body))
  (should (equal (esm--get-parent "M-x ") #'esm-meta/body))
  (should (equal (esm--get-parent "s-x ") #'esm-super/body))
  (should (equal (esm--get-parent "s-x a") nil))
  (should (equal (esm--get-parent "s-x a ") #'esm-sx/body))
  ;; (should (equal (esm--get-parent "s-x <print>") nil)) ;; probably not a major problem
  (should (equal (esm--get-parent "s-x <print> ") #'esm-sx/body)))


(ert-deftest esm--normalize-components ()
  (should (equal (esm--normalize-trim-segment "<next>") "next"))
  (should (equal (esm--normalize-trim-segment "<C-next>") "C-next"))
  (should (equal (esm--normalize-trim-segment "<") "<"))
  (should (equal (esm--normalize-trim-segment "C-<") "C-<"))
  (should (equal (esm--normalize-trim-segment "C->") "C->"))
  (should (equal (esm--normalize-trim-segment ">") ">"))
  (should (equal (esm--normalize-get-atoms "C--") '("C" "-")))
  (should (equal (esm--normalize-get-atoms "C-f") '("C" "f")))
  (should (equal (esm--normalize-get-atoms "C-M-f") '("C" "M" "f")))
  (should (equal (esm--normalize-wrap-leaf-maybe '("C" "M" "next")) '("C" "M" "<next>")))
  (should (equal (esm--normalize-wrap-leaf-maybe '("C" "M" ">")) '("C" "M" ">")))
  (should-not (equal (esm--normalize-wrap-leaf-maybe '("C" "M" "f")) '("C" "M" "<f>")))
  (should (equal (esm--normalize-build-segments '("C" "M" "-")) "C-M--")))

(ert-deftest keydesc-handling-2 ()
  (let ((problematic-key-descriptions
         '(;; raw          normalized       squashed           leaf      1step?
           ("C-x 8 RET"    "C-x 8 <RET>"    "esm-Cx8<RET>"     "<RET>"   nil                                )
           ("<f2> 8 RET"   "<f2> 8 <RET>"   "esm-<f2>8<RET>"   "<RET>"   nil                                )
           ("<f2> f r"     "<f2> f r"       "esm-<f2>fr"       "r"       nil                                )
           ("<f2> <f2>"    "<f2> <f2>"      "esm-<f2><f2>"     "<f2>"    nil                              )
           ("ESC <C-down>" "<ESC> C-<down>" "esm-<ESC>C<down>" "<down>"  nil                             )
           ("C-x RET C-\\" "C-x <RET> C-\\" "esm-Cx<RET>C\\"   "\\"      nil                                 )
           ("TAB"          "TAB"            "esm-TAB"          "TAB"       t                        )
           ("A-T A-B"      "A-T A-B"        "esm-ATAB"         "B"       nil                                  )
           ("A-T A B"      "A-T A B"        "esm-ATAB"         "B"       nil                                  )
           ("A-TAB"        "A-TAB"          "esm-ATAB"         "TAB"       t                                  )
           ("C-<M-return>" "C-M-<return>"   "esm-CM<return>"   "<return>"  t                                  )
           ("<C-M-return>" "C-M-<return>"   "esm-CM<return>"   "<return>"  t                                  )
           ;; ("C-- - -"      "C-- - -"         "esm-C---"       "-"        nil  )
           ;; TODO: Because see  (kbd "TAB")  (kbd "<TAB>")
           ;; ("<TAB>"        "<TAB>"          "esm-<TAB>"        "<TAB>" t  )
           ;; ("s-S-M-H-C-A-<return>" "A-C-H-M-S-s-<return>" "esm-ACHMSs<return>" "<return>" t)
           )))
    (dolist (case problematic-key-descriptions)
      (seq-let (raw normalized squashed leaf 1step?) case
        (should (string= normalized (esm--normalize raw)))
        (should (string= squashed (esm-dub-from-key normalized)))
        (should (string= leaf (esm--get-leaf normalized)))
        (should (eq 1step? (esm--key-seq-steps=1 normalized)))))))

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
           ;; ("C-- - -"      "C-- - -"         "esm-C---"       "-"        nil  )
           ;; ("<TAB>"        "<TAB>"          "esm-<TAB>"        "<TAB>" t  )
           )))
    (dolist (case example-key-descriptions)
      (seq-let (normalized parent) case
        (should (string= parent (esm--parent-stem normalized)))))))


(ert-deftest generate-heads ()
  (should (equal (esm-head "C-" "f")
                 '("f" (call-interactively (key-binding (kbd "C-f"))) "forward-char")))
  (should (equal (esm-head "C-x " "f")
                 '("f" (call-interactively (key-binding (kbd "C-x f"))) "set-fill-column")))
  (should (equal (esm-head-invisible "C-" "f")
                 '("f" (call-interactively (key-binding (kbd "C-f"))) nil :exit nil)))
  (should (equal (esm-head-hint "C-" "f")
                 (symbol-name (key-binding (kbd "C-f")))))
  (should (equal (esm-head-hint "C-" "<f12>")
                 " "))
  ;; weird case, behaves different interactively than when called by ert
  ;; (should (equal (esm-head-hint "C-" "x") "Control-X-prefi"))
  (should (equal (esm--specify-extra-heads "C-x ")
                 '(("<backspace>" esm-control/body nil :exit t))))
  (should (equal (esm--specify-extra-heads "C-")
                 '(("<backspace>" nil nil :exit t) ("<f35>" nil nil :exit t)))))

(provide 'escape-modality-tests)
;;; escape-modality-tests.el ends here
