;;; escape-modality-tests.el -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Martin Edström

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

;;; Code:

;; (require 'ert)

;; (key-description (kbd ...)) can get you far, but it refuses to normalize
;; some aspects when <> are involved, and I need <> to wrap leaf only. Compare:
;; (key-description (kbd "s-S-M-C-H-A-4"))
;; (key-description (kbd "s-S-M-C-H-A-<return>"))
(defun esm-test-keydesc-handling ()
  (let ((errors 0)
        (problematic-key-descriptions
         '(;; raw example  normalized       squashed           leaf
           ("C-x 8 RET"    "C-x 8 <RET>"    "esm-Cx8<RET>"     "<RET>")
           ("<f2> 8 RET"   "<f2> 8 <RET>"   "esm-<f2>8<RET>"   "<RET>")
           ("<f2> f r"     "<f2> f r"       "esm-<f2>fr"       "r")
           ("<f2> <f2>"    "<f2> <f2>"      "esm-<f2><f2>"     "<f2>")
           ("ESC <C-down>" "<ESC> C-<down>" "esm-<ESC>C<down>" "<down>")
           ("C-x RET C-\\" "C-x <RET> C-\\" "esm-Cx<RET>C\\"   "\\")
           ;; TODO: Because (kbd "TAB")  (kbd "<TAB>") are different
           ;; ("<TAB>"        "<TAB>"          "esm-<TAB>"        "<TAB>")
           ("TAB"          "TAB"            "esm-TAB"          "TAB")
           ("A-T A-B"      "A-T A-B"        "esm-ATAB"         "B")
           ("A-T A B"      "A-T A B"        "esm-ATAB"         "B")
           ("A-TAB"        "A-TAB"          "esm-ATAB"         "TAB")
           ("C-<M-return>" "C-M-<return>"   "esm-CM<return>"   "<return>")
           ("<C-M-return>" "C-M-<return>"   "esm-CM<return>"   "<return>")
           ;; ("s-S-M-H-C-A-<return>" "A-C-H-M-S-s-<return>" "esm-ACHMSs<return>" "<return>")
           )))
    (dolist (x problematic-key-descriptions t)
      (seq-let (raw normalized squashed leaf) x
        (unless (and (string= normalized (esm-normalize raw))
                     (string= squashed (esm-dub-from-key normalized))
                     (string= leaf (esm-get-leaf normalized)))
          (error (concat "Keydesc handling failed for test case: " raw)))))))
(esm-test-keydesc-handling)
