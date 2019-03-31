;;; separate-test.el --- Test of separate

;; Copyright (C) 2018  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'ert)
(require 'system-separate)
(eval-when-compile
  (require 'cl))

(ert-deftest system-separate--system-name ()
  (flet ((system-name () "windows-pc2"))
    (should (system-separate--system-name '("mac1" "windows-pc2" "linux3")))
    (should-not
     (system-separate--system-name '("mac1" "windows-pc21" "linux3")))    ))

(ert-deftest system-separate--emacs-version ()
  (let ((emacs-major-version 10)
        (emacs-minor-version 5))
    (should     (system-separate--emacs-version>= '(9)))
    (should     (system-separate--emacs-version>= '(10 5)))
    (should-not (system-separate--emacs-version>= '(10 6)))
    (should     (system-separate--emacs-version>= '(9 5)))
    (should     (system-separate--emacs-version>= '(9 6)))))

(ert-deftest system-separate--system-predicates ()
  (let ((system-separate-system-predicate-alist '((WIN1 . "windows-pc1")
                                                  (MAC1 . "mac-pc1")
                                                  (win2 . "windows-pc2")
                                                  (linux . "linux3"))))
    (flet ((system-name () "windows-pc2"))
      (should     (system-separate--system-predicates '(Wssx "some" win2)))
      (should-not (system-separate--system-predicates '(WIN1 linux "some"))))))

(ert-deftest system-separate--os ()
  (let ((system-type 'cygwin))
    (should     (system-separate--os '(ms-dos cygwin)))
    (should-not (system-separate--os '(ms-dos gnu/linux windows-nt)))))

(ert-deftest system-separate--eval ()
  ""
  (should (system-separate--eval '(nil t))))

(ert-deftest system-separate--package-available ()
  (should     (system-separate--package-available '(cl-lib system-separate)))
  (should-not (system-separate--package-available '(cl-lib not-defined-feature))))




(ert-deftest system-separate--function-assq ()
  (should (equal 2 (system-separate--function-assq t '((ignore . 1) (identity . 2)))))
  (should-not  (system-separate--function-assq t '((ignore . 1) (ignore . 2)))))

(ert-deftest system-separate--assq ()
  (should (equal 3 (system-separate--assq 'c '((a . 1) (b . 2) (c . 3)))))
  (should (equal 2 (system-separate--assq 'c '((a . 1) ((b c) . 2) (d . 3)))))
  (should (equal 1 (system-separate--assq 'c '(((q z) . 2) ((b c) . 1) ((a c) . 3)))))
  (should-not (system-separate--assq 'c '(((q z) . 1) ((b w) . 2) (a . 3)))))



(ert-deftest system-separate--system-predicates-p ()
  (should-not  (system-separate--system-predicates-p '(:alias system-predicate1 system-predicate2)))
  (should      (system-separate--system-predicates-p '(alias system-predicate1 system-predicate2))))

(ert-deftest system-separate--system-predicate-p ()
  (should (system-separate--system-predicate-p '(:alias system-predicate1 system-predicate2)))
  (should (system-separate--system-predicate-p 'system-predicate))
  (should (system-separate--system-predicate-p 3))
  (should (system-separate--system-predicate-p "PC-NAME")))


(ert-deftest system-separate--symbol-system-predicate-instance ()
  (let ((system-separate-system-predicate-alist '((WIN1 . "windows-pc1")
                              (MAC1 . "mac-pc1")
                              (win2 . "windows-pc2")
                              (wow  . (:alies WIN1)))))

    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))

      (should (equal "windows-pc1" (system-separate--symbol-system-predicate-instance 'WIN1)))
      (should-not  (system-separate--symbol-system-predicate-instance nil))
      (should (equal '(:alies WIN1) (system-separate--symbol-system-predicate-instance 'wow))))))

(ert-deftest system-separate--system-predicate-normalize ()
  (should (equal '(:system-name "a")
                 (system-separate--system-predicate-normalize "a")))
  (should (equal '(:emacs-version>= 3)
                 (system-separate--system-predicate-normalize 3)))
  (should-not (equal '(:alias a)
                     (system-separate--system-predicate-normalize 'a))))

(ert-deftest system-separate--symbol-system-predicate-current-p ()
  (let ((system-separate-system-predicate-alist '((WIN1 . "windows-pc1")
                              (MAC1 . "mac-pc1")
                              (win2 . "windows-pc2")
                              (wow  . (:alias win2)))))

    (flet ((system-name () "windows-pc2"))
      (should-not (system-separate--symbol-system-predicate-current-p 'WIN1))
      (should     (system-separate--symbol-system-predicate-current-p 'win2))
      (should     (system-separate--symbol-system-predicate-current-p 'wow)))))

(ert-deftest system-separate--current-system-predicate-p ()
  (let ((system-separate-system-predicate-alist '((WIN1 . "windows-pc1")
                              (MAC1 . "mac-pc1")
                              (win2 . "windows-pc2"))))
    (flet ((system-name () "windows-pc2"))
      (should (system-separate--current-system-predicate-p 'win2))
      (should (not (system-separate--current-system-predicate-p 'WIN1))))))

(ert-deftest system-separate-:eval ()
  (should (system-separate--current-system-predicate-p '(:eval nil t))))



(ert-deftest system-separate-set-no-eval ()
  (let ((var nil)
        (system-separate-system-predicate-alist
         '((WIN1 . "windows-pc1")
           (MAC1 . "mac-pc1")
           (win2 . "windows-pc2")
           (all  . (:eval t))
           (win  . (:system-predicates WIN1 win2))
           (seps . (:alias MAC1 WIN1))
           (ev1  . 5)
           (ev2  . (:emacs-version>= 5 3))
           (os   . (:os windows-nt))
           (os1  . windows-nt)
           (pkg  . (:package-available cl-lib system-separate))
           (sn   . (:system-name "windows-pc2"))
           (and  . (:and
                    win os ev1 pkg sn))))
        (emacs-major-version 6)
        (emacs-minor-version 2)
        (system-type 'windows-nt))
    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))

      (system-separate-set-no-eval 'var
                      (("windows-pc2" . (+ 1 1))
                       (win2 . (+ 3 1))))
      (should (equal var '(+ 1 1)))

      (system-separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       ("windows-pc2" . (+ 3 1))))
      (should (equal var '(+ 3 1)))

      (system-separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       ("windows-pc2" . (+ 3 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 3 1)))

      ;; all
      (system-separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (all  . (+ 3 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 3 1)))

      ;; win
      (system-separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (win  . (+ 5 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 5 1)))

      ;; seps
      (system-separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (seps . (+ 3 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 4 1)))

      ;; ev1
      (system-separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (ev1  . (+ 5 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 5 1)))

      ;; ev2
      (system-separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (ev2  . (+ 6 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 6 1)))

      ;; os
      (system-separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (os   . (+ 7 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 7 1)))

      ;; os1
      (system-separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (os1  . (+ 8 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 8 1)))

      ;; pkg
      (system-separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (pkg  . (+ 9 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 9 1)))

      ;; sn
      (system-separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (sn   . (+ 10 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 10 1)))

      ;; and
      (system-separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (and  . (+ 11 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 11 1)))

      ;; default
      (system-separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (MAC1 . (+ 3 1))
                       (default . (+ 12 1))))
      (should (equal var '(+ 12 1)))

      ;; default
      (system-separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1    . (+ 2 1))
                       (default . (+ 12 1))
                       (win2    . (+ 3 1))))
      (should (equal var '(+ 3 1))))))

(ert-deftest system-separate-setq-no-eval ()
  (let ((var nil)
        (system-separate-system-predicate-alist
         '((WIN1 . "windows-pc1")
           (MAC1 . "mac-pc1")
           (win2 . "windows-pc2")
           (all  . (:eval t))
           (win (:system-predicates WIN1 win2))
           (seps (:alias MAC1 WIN1))
           (ev1 . 5)
           (ev2 . (:emacs-version>= 5 3))
           (os . (:os windows-nt))
           (os1 . windows-nt)
           (pkg . (:package-available cl-lib system-separate))
           (sn . (:system-name "windows-pc2"))
           (and . (:and
                   win os ev1 pkg sn))))
        (emacs-major-version 6)
        (emacs-minor-version 2)
        (system-type 'windows-nt))
    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))

      (system-separate-setq-no-eval var
                       (("windows-pc2" . (+ 1 1))
                        (win2 . (+ 3 1))))
      (should (equal var '(+ 1 1)))

      (system-separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        ("windows-pc2" . (+ 3 1))))
      (should (equal var '(+ 3 1)))

      (system-separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        ("windows-pc2" . (+ 3 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 3 1)))

      ;; all
      (system-separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (all  . (+ 3 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 3 1)))

      ;; win
      (system-separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (win  . (+ 5 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 5 1)))

      ;; seps
      (system-separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (seps . (+ 3 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 4 1)))

      ;; ev1
      (system-separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (ev1  . (+ 5 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 5 1)))

      ;; ev2
      (system-separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (ev2  . (+ 6 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 6 1)))

      ;; os
      (system-separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (os   . (+ 7 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 7 1)))

      ;; os1
      (system-separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (os1  . (+ 8 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 8 1)))

      ;; pkg
      (system-separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (pkg  . (+ 9 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 9 1)))

      ;; sn
      (system-separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (sn   . (+ 10 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 10 1)))

      ;; and
      (system-separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (and  . (+ 11 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 11 1)))

      ;; default
      (system-separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (MAC1 . (+ 3 1))
                        (default . (+ 12 1))))
      (should (equal var '(+ 12 1)))

      ;; default
      (system-separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1    . (+ 2 1))
                        (default . (+ 12 1))
                        (win2    . (+ 3 1))))
      (should (equal var '(+ 3 1))))))



(ert-deftest system-separate-set ()
  (let ((var nil)
        (system-separate-system-predicate-alist
         '((WIN1 . "windows-pc1")
           (MAC1 . "mac-pc1")
           (win2 . "windows-pc2")
           (all  . (:eval t))
           (win (:system-predicates WIN1 win2))
           (seps (:alias MAC1 WIN1))
           (ev1 . 5)
           (ev2 . (:emacs-version>= 5 3))
           (os . (:os windows-nt))
           (os1 . windows-nt)
           (pkg . (:package-available cl-lib system-separate))
           (sn . (:system-name "windows-pc2"))
           (and . (:and
                   win os ev1 pkg sn))))
        (emacs-major-version 6)
        (emacs-minor-version 2)
        (system-type 'windows-nt))
    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))

      (system-separate-set 'var
              (("windows-pc2" . 1)
               (win2 . 3)))
      (should (equal var 1))

      (system-separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               ("windows-pc2" . 3)))
      (should (equal var 3))

      (system-separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               ("windows-pc2" . 3)
               (win2 . 4)))
      (should (equal var 3))

      ;; all
      (system-separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (all  . 3)
               (win2 . 4)))
      (should (equal var 3))

      ;; win
      (system-separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (win  . 5)
               (win2 . 4)))
      (should (equal var 5))

      ;; seps
      (system-separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (seps  . 3)
               (win2 . 4)))
      (should (equal var 4))

      ;; ev1
      (system-separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (ev1  . 5)
               (win2 . 4)))
      (should (equal var 5))

      ;; ev2
      (system-separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (ev2  . 6)
               (win2 . 4)))
      (should (equal var 6))

      ;; os
      (system-separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (os  . 7)
               (win2 . 4)))
      (should (equal var 7))

      ;; os1
      (system-separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (os1  . 8)
               (win2 . 4)))
      (should (equal var 8))

      ;; pkg
      (system-separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (pkg  . 9)
               (win2 . 4)))
      (should (equal var 9))

      ;; sn
      (system-separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (sn   . 10)
               (win2 . 4)))
      (should (equal var 10))

      ;; and
      (system-separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (and  . 11)
               (win2 . 4)))
      (should (equal var 11))

      ;; default
      (system-separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (MAC1 . 3)
               (default . 12)))
      (should (equal var 12))

      ;; default
      (system-separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (default . 12)
               (win2 . 3)))
      (should (equal var 3)))))

(ert-deftest system-separate-setq ()
  (let ((var nil)
        (system-separate-system-predicate-alist '((WIN1 . "windows-pc1")
                              (MAC1 . "mac-pc1")
                              (win2 . "windows-pc2"))))
    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))
      (system-separate-setq var
               (("windows-pc2" . 1)
                (win2 . 3)))

      (should (equal var 1))
      (system-separate-setq var
               (("windows-pc1" . 1)
                (WIN1 . 2)
                (win2 . 4)))
      (should (equal var 4)))))

(ert-deftest system-separate-cond ()
  (let ((var nil)
        (system-separate-system-predicate-alist '(("windows-pc1" . WIN1)
                              ("mac-pc1" . MAC1)
                              ("windows-pc2" . 1)
                              ("windows-pc2" . 5)
                              ("windows-pc2" . win2))))
    (flet ((system-name () "windows-pc2")
           (abc (arg) (setq var arg)))
      (should (equal (system-name) "windows-pc2"))

      (should
       (equal
        (system-separate-cond
         ("windows-pc2" 1)
         (5 3 2)
         (win2 6 3)
         (default 100))
        1))

      (system-separate-cond
       ("windows-pc2"
        (abc 1))
       (5
        (abc 2))
       (win2
        (abc 3)))
      (should (equal var 1)))))

;; (cl-loop for x in '(1 2 3 4)
;;          collect x)

;; (assoc "windows-pc2"
;;        '(("windows-pc1" . 1)
;;          (WIN1 . 2)
;;          ("windows-pc2" . 3)
;;          (win2 . 4)))

(provide 'separate-test)
;;; separate-test.el ends here
