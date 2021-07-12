;;; minibuffer-tests.el --- Tests for completion functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2021 Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords:

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'ert-x)

(eval-when-compile (require 'cl-lib))

(ert-deftest completion-test1 ()
  (with-temp-buffer
    (cl-flet* ((test/completion-table (_string _pred action)
                                      (if (eq action 'lambda)
                                          nil
                                        "test: "))
               (test/completion-at-point ()
                                         (list (copy-marker (point-min))
                                               (copy-marker (point))
                                               #'test/completion-table)))
      (let ((completion-at-point-functions (list #'test/completion-at-point)))
        (insert "TEST")
        (completion-at-point)
        (should (equal (buffer-string)
                       "test: "))))))

(ert-deftest completion-table-with-predicate-test ()
  (let ((full-collection
         '("apple"                      ; Has A.
           "beet"                       ; Has B.
           "banana"                     ; Has A & B.
           "cherry"                     ; Has neither.
           ))
        (no-A (lambda (x) (not (string-match-p "a" x))))
        (no-B (lambda (x) (not (string-match-p "b" x)))))
    (should
     (member "cherry"
             (completion-table-with-predicate
              full-collection no-A t "" no-B t)))
    (should-not
     (member "banana"
             (completion-table-with-predicate
              full-collection no-A t "" no-B t)))
    ;; "apple" should still match when strict is nil.
    (should (eq t (try-completion
                   "apple"
                   (apply-partially
                    'completion-table-with-predicate
                    full-collection no-A nil)
                   no-B)))
    ;; "apple" should still match when strict is nil and pred2 is nil
    ;; (Bug#27841).
    (should (eq t (try-completion
                   "apple"
                   (apply-partially
                    'completion-table-with-predicate
                    full-collection no-A nil))))))

(ert-deftest completion-table-subvert-test ()
  (let* ((origtable '("A-hello" "A-there"))
         (subvtable (completion-table-subvert origtable "B" "A")))
    (should (equal (try-completion "B-hel" subvtable)
                   "B-hello"))
    (should (equal (all-completions "B-hel" subvtable) '("-hello")))
    (should (test-completion "B-hello" subvtable))
    (should (equal (completion-boundaries "B-hel" subvtable
                                          nil "suffix")
                   '(1 . 6)))))

(ert-deftest completion-table-test-quoting ()
  (let ((process-environment
         `("CTTQ1=ed" "CTTQ2=et/" ,@process-environment))
        (default-directory (ert-resource-directory)))
    (pcase-dolist (`(,input ,output)
                   '(
                     ;; Test that $ in files is properly $$ quoted.
                     ("data/m-cttq" "data/minibuffer-test-cttq$$tion")
                     ;; Test that $$ in input is properly unquoted.
                     ("data/m-cttq$$t" "data/minibuffer-test-cttq$$tion")
                     ;; Test that env-vars are preserved.
                     ("lisp/c${CTTQ1}et/se-u" "lisp/c${CTTQ1}et/semantic-utest")
                     ("lisp/ced${CTTQ2}se-u" "lisp/ced${CTTQ2}semantic-utest")
                     ;; Test that env-vars don't prevent partial-completion.
                     ;; FIXME: Ideally we'd like to keep the ${CTTQ}!
                     ("lis/c${CTTQ1}/se-u" "lisp/cedet/semantic-utest")
                     ))
      (should (equal (completion-try-completion input
                                                #'completion--file-name-table
                                                nil (length input))
                     (cons output (length output)))))))

(ert-deftest completion--insert-strings-faces ()
  (with-temp-buffer
    (completion--insert-strings
     '(("completion1" "suffix1")))
    (should (equal (get-text-property 12 'face) '(completions-annotations))))
  (with-temp-buffer
    (completion--insert-strings
     '(("completion1" #("suffix1" 0 7 (face shadow)))))
    (should (equal (get-text-property 12 'face) 'shadow)))
  (with-temp-buffer
    (completion--insert-strings
     '(("completion1" "prefix1" "suffix1")))
    (should (equal (get-text-property 19 'face) nil)))
  (with-temp-buffer
    (completion--insert-strings
     '(("completion1" "prefix1" #("suffix1" 0 7 (face shadow)))))
    (should (equal (get-text-property 19 'face) 'shadow))))

(ert-deftest completion-pcm--optimize-pattern ()
  (should (equal (completion-pcm--optimize-pattern '("buf" point "f"))
                 '("buf" point "f")))
  (should (equal (completion-pcm--optimize-pattern '(any "" any))
                 '(any))))

(defun test-completion-all-sorted-completions (base def history-var history-list)
  (with-temp-buffer
    (insert base)
    (cl-letf (((symbol-function #'minibufferp) (lambda (&rest _) t)))
      (let ((completion-styles '(basic))
            (completion-category-defaults nil)
            (completion-category-overrides nil)
            (minibuffer-history-variable history-var)
            (minibuffer-history history-list)
            (minibuffer-default def)
            (minibuffer-completion-table
             (lambda (str pred action)
               (pcase action
                 (`(boundaries . ,_) `(boundaries ,(length base) . 0))
                 (_ (complete-with-action
                     action
                     '("epsilon" "alpha" "gamma" "beta" "delta")
                     (substring str (length base)) pred))))))
        (completion-all-sorted-completions)))))

(ert-deftest completion-all-sorted-completions ()
  ;; No base, disabled history, no default
  (should (equal (test-completion-all-sorted-completions
                  "" nil t nil)
                 `("beta" "alpha" "delta" "gamma" "epsilon" . 0)))
  ;; No base, disabled history, default string
  (should (equal (test-completion-all-sorted-completions
                  "" "gamma" t nil)
                 `("gamma" "beta" "alpha" "delta" "epsilon" . 0)))
  ;; No base, empty history, default string
  (should (equal (test-completion-all-sorted-completions
                  "" "gamma" 'minibuffer-history nil)
                 `("gamma" "beta" "alpha" "delta" "epsilon" . 0)))
  ;; No base, empty history, default list
  (should (equal (test-completion-all-sorted-completions
                  "" '("gamma" "zeta") 'minibuffer-history nil)
                 `("gamma" "beta" "alpha" "delta" "epsilon" . 0)))
  ;; No base, history, default string
  (should (equal (test-completion-all-sorted-completions
                  "" "gamma" 'minibuffer-history '("other" "epsilon" "delta"))
                 `("gamma" "epsilon" "delta" "beta" "alpha"  . 0)))
  ;; Base, history, default string
  (should (equal (test-completion-all-sorted-completions
                  "base/" "base/gamma" 'minibuffer-history
                  '("some/alpha" "base/epsilon" "base/delta"))
                 `("gamma" "epsilon" "delta" "beta" "alpha"  . 5)))
  ;; Base, history, default string
  (should (equal (test-completion-all-sorted-completions
                  "base/" "gamma" 'minibuffer-history
                  '("some/alpha" "base/epsilon" "base/delta"))
                 `("epsilon" "delta" "beta" "alpha" "gamma"  . 5))))

(defun completion--pcm-first-difference-pos (comp)
  "Get `completions-first-difference' from COMP."
  (cl-loop for pos = (next-single-property-change 0 'face comp)
           then (next-single-property-change pos 'face comp)
           while pos
           when (eq (get-text-property pos 'face comp)
                    'completions-first-difference)
           return pos))

(ert-deftest completion-pcm-test-1 ()
  ;; Point is at end, this does not match anything
  (should (null
           (completion-pcm-all-completions
            "foo" '("hello" "world" "barfoobar") nil 3))))

(ert-deftest completion-pcm-test-2 ()
  ;; Point is at beginning, this matches "barfoobar"
  (should (equal
           (car (completion-pcm-all-completions
                 "foo" '("hello" "world" "barfoobar") nil 0))
           "barfoobar")))

(ert-deftest completion-pcm-test-3 ()
  ;; No match due to point being at the end
  (should (null
           (completion-pcm-all-completions
            "RO" '("RaOb") nil 2))))

(ert-deftest completion-pcm-test-4 ()
  ;; Since point is at the beginning, there is nothing that can really
  ;; be typed anymore
  (should (null
           (completion--pcm-first-difference-pos
            (car (completion-pcm-all-completions
                  "f" '("few" "many") nil 0))))))

(ert-deftest completion-pcm-test-5 ()
  ;; Wildcards and delimiters work
  (should (equal
           (car (completion-pcm-all-completions
                 "li-pac*" '("list-packages") nil 7))
           "list-packages"))
  (should (null
           (car (completion-pcm-all-completions
                 "li-pac*" '("do-not-list-packages") nil 7)))))

(ert-deftest completion-substring-test-1 ()
  (should (equal
           (car (completion-substring-all-completions
                 "foo" '("hello" "world" "barfoobar") nil 3))
           "barfoobar")))

(ert-deftest completion-substring-test-2 ()
  ;; Substring match
  (should (equal
           (car (completion-substring-all-completions
                 "custgroup" '("customize-group") nil 4))
           "customize-group"))
  (should (null
           (car (completion-substring-all-completions
                 "custgroup" '("customize-group") nil 5)))))

(ert-deftest completion-substring-test-3 ()
  ;; `completions-first-difference' should be at the right place
  (should (eql
           (completion--pcm-first-difference-pos
            (car (completion-substring-all-completions
                  "jab" '("dabjobstabby" "many") nil 1)))
           4))
  (should (null
           (completion--pcm-first-difference-pos
            (car (completion-substring-all-completions
                  "jab" '("dabjabstabby" "many") nil 1)))))
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-substring-all-completions
                  "jab" '("dabjabstabby" "many") nil 3)))
           6)))

(ert-deftest completion-flex-test-1 ()
  ;; Fuzzy match
  (should (equal
           (car (completion-flex-all-completions
                 "foo" '("hello" "world" "fabrobazo") nil 3))
           "fabrobazo")))

(ert-deftest completion-flex-test-2 ()
  ;; Another fuzzy match, but more of a "substring" one
  (should (equal
           (car (completion-flex-all-completions
                 "custgroup" '("customize-group-other-window") nil 4))
           "customize-group-other-window"))
  ;; `completions-first-difference' should be at the right place
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-flex-all-completions
                  "custgroup" '("customize-group-other-window") nil 4)))
           4))
  (should (equal
           (completion--pcm-first-difference-pos
            (car (completion-flex-all-completions
                  "custgroup" '("customize-group-other-window") nil 9)))
           15)))

(ert-deftest completion-flex-score-test-1 ()
  ;; Full match!
  (should (equal
           (completion--flex-score '(prefix "R") '("R"))
           (list (cons -1.0 "R")))))

(ert-deftest completion-flex-score-test-2 ()
  ;; One third and half of a match!
  (should (equal
           (completion--flex-score '(prefix "foo")
                                   '("barfoobar" "fooboo"))
           (list (cons (/ -1.0 3.0) "barfoobar")
                 (cons (/ -1.0 2.0) "fooboo")))))

(ert-deftest completion-flex-score-test-3 ()
  ;; One fourth of a match
  (should (eql
           (caar (completion--flex-score '(prefix "R" point "O")
                                         '("RaOb")))
           (/ -1.0 4.0))))

(ert-deftest completion-flex-score-test-4 ()
  ;; For quoted completion tables, score the unquoted completion string.
  (should (equal
           (completion--flex-score
            '(prefix "R")
            (list (propertize "X" 'completion--unquoted "R")))
           (list (cons -1.0 "R")))))

(defun completion--test-style (style string point table filtered)
  (let* ((completion-styles (list style))
         (pred (lambda (x) (not (string-search "!" x))))
         (result (completion-filter-completions
                  string table pred point nil)))
    (should (equal (alist-get 'base result) 0))
    (should (equal (alist-get 'end result) (length string)))
    (should (equal (alist-get 'completions result) filtered))
    (should (not (memq (alist-get 'highlight result) '(nil identity))))
    (should (equal (completion-all-completions string table pred point)
                   (append filtered 0)))))

(ert-deftest completion-basic-style-test-1 ()
  ;; point at the beginning |foo
  (completion--test-style 'basic "foo" 0
                          '("foobar" "foo!" "barfoo" "xfooy" "boobar")
                          '("foobar" "barfoo" "xfooy")))

(ert-deftest completion-basic-style-test-2 ()
  ;; point foo
  (completion--test-style 'basic "foo" 2
                          '("foobar" "foo!" "fobar" "barfoo" "xfooy" "boobar")
                          '("foobar")))

(ert-deftest completion-substring-style-test ()
  (completion--test-style 'substring "foo" 1
                          '("foobar" "foo!" "barfoo" "xfooy" "boobar")
                          '("foobar" "barfoo" "xfooy")))

(ert-deftest completion-emacs21-style-test ()
  (completion--test-style 'emacs21 "foo" 1
                          '("foobar" "foo!" "fobar" "barfoo" "xfooy" "boobar")
                          '("foobar")))

(ert-deftest completion-emacs22-style-test ()
  (completion--test-style 'emacs22 "fo0" 1
                          '("foobar" "foo!" "fobar" "barfoo" "xfooy" "boobar")
                          '("foobar" "fobar"))) ;; suffix ignored completely

(ert-deftest completion-flex-style-test ()
  (completion--test-style 'flex "abc" 1
                          '("abc" "abc!" "xaybzc" "xaybz")
                          '("abc" "xaybzc")))

(ert-deftest completion-initials-style-test ()
  (completion--test-style 'initials "abc" 1
                          '("a-b-c" "a-b-c!" "ax-by-cz" "xax-by-cz")
                          '("a-b-c" "ax-by-cz")))

(ert-deftest completion-pcm-style-test ()
  (completion--test-style 'partial-completion "ax-b-c" 1
                          '("ax-b-c" "ax-b-c!" "ax-by-cz" "xax-by-cz")
                          '("ax-b-c" "ax-by-cz")))

(ert-deftest completion-filter-completions-highlight-test ()
  ;; point at the beginning |foo
  (let* ((completion-styles '(basic))
         (result (completion-filter-completions
                  "foo" '("foobar" "fbarfoo" "fxfooy" "bar")
                  nil 1 nil)))
    (should (equal
             (format "%S" (alist-get 'completions result))
             (format "%S" '("foobar" "fbarfoo" "fxfooy"))))
    (should (equal
             (format "%S" (funcall (alist-get 'highlight result)
                                   (alist-get 'completions result)))
             (format "%S"
                     '(#("foobar" 0 1 (face (completions-common-part))
                         1 2 (face (completions-first-difference)))
                       #("fbarfoo" 0 1 (face (completions-common-part))
                         1 2 (face (completions-first-difference)))
                       #("fxfooy" 0 1 (face (completions-common-part))
                         1 2 (face (completions-first-difference)))))))))

(defun completion--test-boundaries (style string table result)
  (let ((table
         (lambda (str pred action)
           (pcase action
             (`(boundaries . ,suffix) `(boundaries
                                        ,(1+ (string-match-p "<\\|/" str))
                                        . ,(or (string-search ">" suffix) (length suffix))))
             (_ (complete-with-action action table
                                      (replace-regexp-in-string ".*[</]" "" str)
                                      pred)))))
        (point (string-search "|" string))
        (string (string-replace "|" "" string))
        (completion-styles (list style)))
    (should (equal
             (assq-delete-all
              (if (assq 'highlight result) '-does-not-exist 'highlight)
              (completion-filter-completions
               string table nil point nil))
             result))
    (should (equal
             (completion-all-completions
              string table nil point)
             (append (alist-get 'completions result)
                     (alist-get 'base result))))))

(ert-deftest completion-emacs21-boundaries-test ()
  (completion--test-boundaries 'emacs21 "before<in|put>after"
                               '("other") nil)
  (completion--test-boundaries 'emacs21 "before<in|put>after"
                               '("ainput>after" "input>after" "inpux>after"
                                 "inxputy>after" "input>after2")
                               '((base . 7)
                                 (end . 18)
                                 (completions "input>after" "input>after2"))))

(ert-deftest completion-emacs22-boundaries-test ()
  (completion--test-boundaries 'emacs22 "before<in|put>after"
                               '("other") nil)
  (completion--test-boundaries 'emacs22 "before<in|put>after"
                               '("ainxxx" "inyy" "inzzz")
                               '((base . 7)
                                 (end . 12)
                                 (completions "inyy" "inzzz"))))

(ert-deftest completion-basic-boundaries-test ()
  (completion--test-boundaries 'basic "before<in|put>after"
                               '("other") nil)
  (completion--test-boundaries 'basic "before<in|put>after"
                               '("ainput" "input" "inpux" "inxputy")
                               '((base . 7)
                                 (end . 12)
                                 (completions "input" "inxputy"))))

(ert-deftest completion-substring-boundaries-test ()
  (completion--test-boundaries 'substring "before<in|puts>after"
                               '("other") nil)
  (completion--test-boundaries 'substring "before<in|puts>after"
                               '("ainputs" "inputs" "inpux" "inxputsy")
                               '((base . 7)
                                 (end . 13)
                                 (completions "ainputs" "inputs" "inxputsy"))))

(ert-deftest completion-pcm-boundaries-test ()
  (completion--test-boundaries 'partial-completion "before<in-p|t>after"
                               '("other") nil)
  (completion--test-boundaries 'partial-completion "before<in-p|t>after"
                               '("ain-pu-ts" "in-pts" "in-pu-ts" "in-px" "inx-ptsy")
                               '((base . 7)
                                 (end . 12)
                                 (completions "in-pts" "in-pu-ts" "inx-ptsy"))))

(ert-deftest completion-initials-boundaries-test ()
  (completion--test-boundaries 'initials "/ip|t"
                               '("other") nil)
  (completion--test-boundaries 'initials "/ip|t"
                               '("ain/pu/ts" "in/pts" "in/pu/ts" "a/in/pu/ts"
                                 "in/pu/ts/foo" "in/px" "inx/ptsy")
                               '((base . 1)
                                 (end . 4)
                                 (completions "in/pu/ts" "in/pu/ts/foo"))))

(defun completion-emacs22orig-all-completions (string table pred point)
  (let ((beforepoint (substring string 0 point)))
    (completion-hilit-commonality
      (all-completions beforepoint table pred)
     point
     (car (completion-boundaries beforepoint table pred "")))))

(ert-deftest completion-upgrade-return-type-test ()
  ;; Test transparent upgrade of old completion style return value
  ;; to new return value format.
  (let ((completion-styles-alist
         '((emacs22orig completion-emacs22-try-completion
                        completion-emacs22orig-all-completions nil))))
  (completion--test-boundaries 'emacs22orig "before<in|put>after"
                               '("ainxxx" "inyy" "inzzz")
                               '((base . 7)
                                 ;; 18 is incorrect, should be 12!
                                 ;; But the information is not available
                                 ;; due to the completion-style upgrade.
                                 (end . 18)
                                 ;; Identity highlighting function.
                                 (highlight . identity)
                                 (completions "inyy" "inzzz")))))

(provide 'minibuffer-tests)
;;; minibuffer-tests.el ends here
