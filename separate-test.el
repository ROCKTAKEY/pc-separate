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
(require 'separate)
(eval-when-compile
  (require 'cl))

(ert-deftest separate--system-name ()
  (flet ((system-name () "windows-pc2"))
    (should (separate--system-name '("mac1" "windows-pc2" "linux3")))
    (should-not
     (separate--system-name '("mac1" "windows-pc21" "linux3")))    ))

(ert-deftest separate--emacs-version ()
  (let ((emacs-major-version 10)
        (emacs-minor-version 5))
    (should     (separate--emacs-version>= '(9)))
    (should     (separate--emacs-version>= '(10 5)))
    (should-not (separate--emacs-version>= '(10 6)))
    (should     (separate--emacs-version>= '(9 5)))
    (should     (separate--emacs-version>= '(9 6)))))

(ert-deftest separate--system-predicates ()
  (let ((separate-system-predicate-alist '((WIN1 . "windows-pc1")
                                                  (MAC1 . "mac-pc1")
                                                  (win2 . "windows-pc2")
                                                  (linux . "linux3"))))
    (flet ((system-name () "windows-pc2"))
      (should     (separate--system-predicates '(Wssx "some" win2)))
      (should-not (separate--system-predicates '(WIN1 linux "some"))))))

(ert-deftest separate--os ()
  (let ((system-type 'cygwin))
    (should     (separate--os '(ms-dos cygwin)))
    (should-not (separate--os '(ms-dos gnu/linux windows-nt)))))

(ert-deftest separate--eval ()
  ""
  (should (separate--eval '(nil t))))

(ert-deftest separate--package-available ()
  (should     (separate--package-available '(cl-lib separate)))
  (should-not (separate--package-available '(cl-lib not-defined-feature))))




(ert-deftest separate--function-assq ()
  (should (equal 2 (separate--function-assq t '((ignore . 1) (identity . 2)))))
  (should-not  (separate--function-assq t '((ignore . 1) (ignore . 2)))))

(ert-deftest separate--assq ()
  (should (equal 3 (separate--assq 'c '((a . 1) (b . 2) (c . 3)))))
  (should (equal 2 (separate--assq 'c '((a . 1) ((b c) . 2) (d . 3)))))
  (should (equal 1 (separate--assq 'c '(((q z) . 2) ((b c) . 1) ((a c) . 3)))))
  (should-not (separate--assq 'c '(((q z) . 1) ((b w) . 2) (a . 3)))))



(ert-deftest separate--system-predicates-p ()
  (should-not  (separate--system-predicates-p '(:alias system-predicate1 system-predicate2)))
  (should      (separate--system-predicates-p '(alias system-predicate1 system-predicate2))))

(ert-deftest separate--system-predicate-p ()
  (should (separate--system-predicate-p '(:alias system-predicate1 system-predicate2)))
  (should (separate--system-predicate-p 'system-predicate))
  (should (separate--system-predicate-p 3))
  (should (separate--system-predicate-p "PC-NAME")))


(ert-deftest separate--symbol-system-predicate-instance ()
  (let ((separate-system-predicate-alist '((WIN1 . "windows-pc1")
                              (MAC1 . "mac-pc1")
                              (win2 . "windows-pc2")
                              (wow  . (:alies WIN1)))))

    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))

      (should (equal "windows-pc1" (separate--symbol-system-predicate-instance 'WIN1)))
      (should-not  (separate--symbol-system-predicate-instance nil))
      (should (equal '(:alies WIN1) (separate--symbol-system-predicate-instance 'wow))))))

(ert-deftest separate--system-predicate-normalize ()
  (should (equal '(:system-name "a")
                 (separate--system-predicate-normalize "a")))
  (should (equal '(:emacs-version>= 3)
                 (separate--system-predicate-normalize 3)))
  (should-not (equal '(:alias a)
                     (separate--system-predicate-normalize 'a))))

(ert-deftest separate--symbol-system-predicate-current-p ()
  (let ((separate-system-predicate-alist '((WIN1 . "windows-pc1")
                              (MAC1 . "mac-pc1")
                              (win2 . "windows-pc2")
                              (wow  . (:alias win2)))))

    (flet ((system-name () "windows-pc2"))
      (should-not (separate--symbol-system-predicate-current-p 'WIN1))
      (should     (separate--symbol-system-predicate-current-p 'win2))
      (should     (separate--symbol-system-predicate-current-p 'wow)))))

(ert-deftest separate--current-system-predicate-p ()
  (let ((separate-system-predicate-alist '((WIN1 . "windows-pc1")
                              (MAC1 . "mac-pc1")
                              (win2 . "windows-pc2"))))
    (flet ((system-name () "windows-pc2"))
      (should (separate--current-system-predicate-p 'win2))
      (should (not (separate--current-system-predicate-p 'WIN1))))))

(ert-deftest separate-:eval ()
  (should (separate--current-system-predicate-p '(:eval nil t))))



(ert-deftest separate-set-no-eval ()
  (let ((var nil)
        (separate-system-predicate-alist
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
           (pkg  . (:package-available cl-lib separate))
           (sn   . (:system-name "windows-pc2"))
           (and  . (:and
                    win os ev1 pkg sn))))
        (emacs-major-version 6)
        (emacs-minor-version 2)
        (system-type 'windows-nt))
    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))

      (separate-set-no-eval 'var
                      (("windows-pc2" . (+ 1 1))
                       (win2 . (+ 3 1))))
      (should (equal var '(+ 1 1)))

      (separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       ("windows-pc2" . (+ 3 1))))
      (should (equal var '(+ 3 1)))

      (separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       ("windows-pc2" . (+ 3 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 3 1)))

      ;; all
      (separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (all  . (+ 3 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 3 1)))

      ;; win
      (separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (win  . (+ 5 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 5 1)))

      ;; seps
      (separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (seps . (+ 3 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 4 1)))

      ;; ev1
      (separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (ev1  . (+ 5 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 5 1)))

      ;; ev2
      (separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (ev2  . (+ 6 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 6 1)))

      ;; os
      (separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (os   . (+ 7 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 7 1)))

      ;; os1
      (separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (os1  . (+ 8 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 8 1)))

      ;; pkg
      (separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (pkg  . (+ 9 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 9 1)))

      ;; sn
      (separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (sn   . (+ 10 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 10 1)))

      ;; and
      (separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (and  . (+ 11 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 11 1)))

      ;; default
      (separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       (MAC1 . (+ 3 1))
                       (default . (+ 12 1))))
      (should (equal var '(+ 12 1)))

      ;; default
      (separate-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1    . (+ 2 1))
                       (default . (+ 12 1))
                       (win2    . (+ 3 1))))
      (should (equal var '(+ 3 1))))))

(ert-deftest separate-setq-no-eval ()
  (let ((var nil)
        (separate-system-predicate-alist
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
           (pkg . (:package-available cl-lib separate))
           (sn . (:system-name "windows-pc2"))
           (and . (:and
                   win os ev1 pkg sn))))
        (emacs-major-version 6)
        (emacs-minor-version 2)
        (system-type 'windows-nt))
    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))

      (separate-setq-no-eval var
                       (("windows-pc2" . (+ 1 1))
                        (win2 . (+ 3 1))))
      (should (equal var '(+ 1 1)))

      (separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        ("windows-pc2" . (+ 3 1))))
      (should (equal var '(+ 3 1)))

      (separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        ("windows-pc2" . (+ 3 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 3 1)))

      ;; all
      (separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (all  . (+ 3 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 3 1)))

      ;; win
      (separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (win  . (+ 5 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 5 1)))

      ;; seps
      (separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (seps . (+ 3 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 4 1)))

      ;; ev1
      (separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (ev1  . (+ 5 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 5 1)))

      ;; ev2
      (separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (ev2  . (+ 6 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 6 1)))

      ;; os
      (separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (os   . (+ 7 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 7 1)))

      ;; os1
      (separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (os1  . (+ 8 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 8 1)))

      ;; pkg
      (separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (pkg  . (+ 9 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 9 1)))

      ;; sn
      (separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (sn   . (+ 10 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 10 1)))

      ;; and
      (separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (and  . (+ 11 1))
                        (win2 . (+ 4 1))))
      (should (equal var '(+ 11 1)))

      ;; default
      (separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1 . (+ 2 1))
                        (MAC1 . (+ 3 1))
                        (default . (+ 12 1))))
      (should (equal var '(+ 12 1)))

      ;; default
      (separate-setq-no-eval var
                       (("windows-pc1" . (+ 1 1))
                        (WIN1    . (+ 2 1))
                        (default . (+ 12 1))
                        (win2    . (+ 3 1))))
      (should (equal var '(+ 3 1))))))



(ert-deftest separate-set ()
  (let ((var nil)
        (separate-system-predicate-alist
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
           (pkg . (:package-available cl-lib separate))
           (sn . (:system-name "windows-pc2"))
           (and . (:and
                   win os ev1 pkg sn))))
        (emacs-major-version 6)
        (emacs-minor-version 2)
        (system-type 'windows-nt))
    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))

      (separate-set 'var
              (("windows-pc2" . 1)
               (win2 . 3)))
      (should (equal var 1))

      (separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               ("windows-pc2" . 3)))
      (should (equal var 3))

      (separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               ("windows-pc2" . 3)
               (win2 . 4)))
      (should (equal var 3))

      ;; all
      (separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (all  . 3)
               (win2 . 4)))
      (should (equal var 3))

      ;; win
      (separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (win  . 5)
               (win2 . 4)))
      (should (equal var 5))

      ;; seps
      (separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (seps  . 3)
               (win2 . 4)))
      (should (equal var 4))

      ;; ev1
      (separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (ev1  . 5)
               (win2 . 4)))
      (should (equal var 5))

      ;; ev2
      (separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (ev2  . 6)
               (win2 . 4)))
      (should (equal var 6))

      ;; os
      (separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (os  . 7)
               (win2 . 4)))
      (should (equal var 7))

      ;; os1
      (separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (os1  . 8)
               (win2 . 4)))
      (should (equal var 8))

      ;; pkg
      (separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (pkg  . 9)
               (win2 . 4)))
      (should (equal var 9))

      ;; sn
      (separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (sn   . 10)
               (win2 . 4)))
      (should (equal var 10))

      ;; and
      (separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (and  . 11)
               (win2 . 4)))
      (should (equal var 11))

      ;; default
      (separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (MAC1 . 3)
               (default . 12)))
      (should (equal var 12))

      ;; default
      (separate-set 'var
              (("windows-pc1" . 1)
               (WIN1 . 2)
               (default . 12)
               (win2 . 3)))
      (should (equal var 3)))))

(ert-deftest separate-setq ()
  (let ((var nil)
        (separate-system-predicate-alist '((WIN1 . "windows-pc1")
                              (MAC1 . "mac-pc1")
                              (win2 . "windows-pc2"))))
    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))
      (separate-setq var
               (("windows-pc2" . 1)
                (win2 . 3)))

      (should (equal var 1))
      (separate-setq var
               (("windows-pc1" . 1)
                (WIN1 . 2)
                (win2 . 4)))
      (should (equal var 4)))))

(ert-deftest separate-cond ()
  (let ((var nil)
        (separate-system-predicate-alist '(("windows-pc1" . WIN1)
                              ("mac-pc1" . MAC1)
                              ("windows-pc2" . 1)
                              ("windows-pc2" . 5)
                              ("windows-pc2" . win2))))
    (flet ((system-name () "windows-pc2")
           (abc (arg) (setq var arg)))
      (should (equal (system-name) "windows-pc2"))

      (should
       (equal
        (separate-cond
         ("windows-pc2" 1)
         (5 3 2)
         (win2 6 3)
         (default 100))
        1))

      (separate-cond
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
