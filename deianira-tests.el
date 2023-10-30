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

;; (provide 'deianira-tests)
;;; deianira-tests.el ends here
