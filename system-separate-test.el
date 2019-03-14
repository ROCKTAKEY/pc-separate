;;; system-separate-test.el --- Test of separate

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

(ert-deftest ss--system-name ()
  (flet ((system-name () "windows-pc2"))
    (should (ss--system-name '("mac1" "windows-pc2" "linux3")))
    (should-not
     (ss--system-name '("mac1" "windows-pc21" "linux3")))
    ))

(ert-deftest ss--emacs-version ()
  (let ((emacs-major-version 10)
        (emacs-minor-version 5))
    (should     (ss--emacs-version>= '(9)))
    (should     (ss--emacs-version>= '(10 5)))
    (should-not (ss--emacs-version>= '(10 6)))
    (should     (ss--emacs-version>= '(9 5)))
    (should     (ss--emacs-version>= '(9 6)))
    ))

(ert-deftest ss--separators ()
  (let ((ss-separator-alist '((WIN1 . "windows-pc1")
                              (MAC1 . "mac-pc1")
                              (win2 . "windows-pc2")
                              (linux . "linux3")
                              )))
    (flet ((system-name () "windows-pc2"))
      (should     (ss--separators '(Wssx "some" win2)))
      (should-not (ss--separators '(WIN1 linux "some")))
      ))
  )

(ert-deftest ss--os ()
  (let ((system-type 'cygwin))
    (should     (ss--os '(ms-dos cygwin)))
    (should-not (ss--os '(ms-dos gnu/linux windows-nt))))
  )

(ert-deftest ss--eval ()
  ""
  (should (ss--eval '(nil t)))
  )

(ert-deftest ss--package-available ()
  (should     (ss--package-available '(cl-lib system-separate)))
  (should-not (ss--package-available '(cl-lib not-defined-feature))))




(ert-deftest ss--function-assq ()
  (should (equal 2 (ss--function-assq t '((ignore . 1) (identity . 2)))))
  (should-not  (ss--function-assq t '((ignore . 1) (ignore . 2))))
  )

(ert-deftest ss--assq ()
  (should (equal 3 (ss--assq 'c '((a . 1) (b . 2) (c . 3)))))
  (should (equal 2 (ss--assq 'c '((a . 1) ((b c) . 2) (d . 3)))))
  (should (equal 1 (ss--assq 'c '(((q z) . 2) ((b c) . 1) ((a c) . 3)))))
  (should-not (ss--assq 'c '(((q z) . 1) ((b w) . 2) (a . 3))))
  )



(ert-deftest ss--separators-p ()
  (should-not  (ss--separators-p '(:alias separator1 separator2)))
  (should      (ss--separators-p '(alias separator1 separator2)))
  )

(ert-deftest ss--separator-p ()
  (should (ss--separator-p '(:alias separator1 separator2)))
  (should (ss--separator-p 'separator))
  (should (ss--separator-p 3))
  (should (ss--separator-p "PC-NAME")))


(ert-deftest ss--symbol-separator-instance ()
  (let ((ss-separator-alist '((WIN1 . "windows-pc1")
                              (MAC1 . "mac-pc1")
                              (win2 . "windows-pc2")
                              (wow  . (:alies WIN1)))))

    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))

      (should (equal "windows-pc1" (ss--symbol-separator-instance 'WIN1)))
      (should-not  (ss--symbol-separator-instance nil))
      (should (equal '(:alies WIN1) (ss--symbol-separator-instance 'wow)))
      )))

(ert-deftest ss--separator-normalize ()
  (should (equal '(:system-name "a")
                 (ss--separator-normalize "a")))
  (should (equal '(:emacs-version>= 3)
                 (ss--separator-normalize 3)))
  (should-not (equal '(:alias a)
                     (ss--separator-normalize 'a)))
  )

(ert-deftest ss--symbol-separator-current-p ()
  (let ((ss-separator-alist '((WIN1 . "windows-pc1")
                              (MAC1 . "mac-pc1")
                              (win2 . "windows-pc2")
                              (wow  . (:alias win2)))))

    (flet ((system-name () "windows-pc2"))
      (should-not (ss--symbol-separator-current-p 'WIN1))
      (should     (ss--symbol-separator-current-p 'win2))
      (should     (ss--symbol-separator-current-p 'wow))
      ))
  )

(ert-deftest ss--current-separator-p ()
  (let ((ss-separator-alist '((WIN1 . "windows-pc1")
                              (MAC1 . "mac-pc1")
                              (win2 . "windows-pc2"))))
    (flet ((system-name () "windows-pc2"))
      (should (ss--current-separator-p 'win2))
      (should (not (ss--current-separator-p 'WIN1)))
      )))

(ert-deftest ss-:eval ()
  (should (ss--current-separator-p '(:eval nil t))))



(ert-deftest ss-set-no-eval ()
  (let ((var nil)
        (ss-separator-alist
         '((WIN1 . "windows-pc1")
           (MAC1 . "mac-pc1")
           (win2 . "windows-pc2")
           (all  . (:eval t))
           (win  . (:separators WIN1 win2))
           (seps . (:alias MAC1 WIN1))
           (ev1  . 5)
           (ev2  . (:emacs-version>= 5 3))
           (os   . (:os windows-nt))
           (os1  . windows-nt)
           (pkg  . (:package-available cl-lib system-separate))
           (sn   . (:system-name "windows-pc2"))
           (and  . (:and
                   win os ev1 pkg sn))
           ))
        (emacs-major-version 6)
        (emacs-minor-version 2)
        (system-type 'windows-nt))
    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))

      (ss-set-no-eval 'var
                      (("windows-pc2" . (+ 1 1))
                       (win2 . (+ 3 1))))
      (should (equal var '(+ 1 1)))

      (ss-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       ("windows-pc2" . (+ 3 1))
                       ))
      (should (equal var '(+ 3 1)))

      (ss-set-no-eval 'var
                      (("windows-pc1" . (+ 1 1))
                       (WIN1 . (+ 2 1))
                       ("windows-pc2" . (+ 3 1))
                       (win2 . (+ 4 1))))
      (should (equal var '(+ 3 1)))

      ;; all
      (ss-set-no-eval 'var
                            (("windows-pc1" . (+ 1 1))
                             (WIN1 . (+ 2 1))
                             (all  . (+ 3 1))
                             (win2 . (+ 4 1))))
      (should (equal var '(+ 3 1)))

      ;; win
      (ss-set-no-eval 'var
                            (("windows-pc1" . (+ 1 1))
                             (WIN1 . (+ 2 1))
                             (win  . (+ 5 1))
                             (win2 . (+ 4 1))))
      (should (equal var '(+ 5 1)))

      ;; seps
      (ss-set-no-eval 'var
                            (("windows-pc1" . (+ 1 1))
                             (WIN1 . (+ 2 1))
                             (seps . (+ 3 1))
                             (win2 . (+ 4 1))))
      (should (equal var '(+ 4 1)))

      ;; ev1
      (ss-set-no-eval 'var
                            (("windows-pc1" . (+ 1 1))
                             (WIN1 . (+ 2 1))
                             (ev1  . (+ 5 1))
                             (win2 . (+ 4 1))))
      (should (equal var '(+ 5 1)))

      ;; ev2
      (ss-set-no-eval 'var
                            (("windows-pc1" . (+ 1 1))
                             (WIN1 . (+ 2 1))
                             (ev2  . (+ 6 1))
                             (win2 . (+ 4 1))))
      (should (equal var '(+ 6 1)))

      ;; os
      (ss-set-no-eval 'var
                            (("windows-pc1" . (+ 1 1))
                             (WIN1 . (+ 2 1))
                             (os   . (+ 7 1))
                             (win2 . (+ 4 1))))
      (should (equal var '(+ 7 1)))

      ;; os1
      (ss-set-no-eval 'var
                            (("windows-pc1" . (+ 1 1))
                             (WIN1 . (+ 2 1))
                             (os1  . (+ 8 1))
                             (win2 . (+ 4 1))))
      (should (equal var '(+ 8 1)))

      ;; pkg
      (ss-set-no-eval 'var
                            (("windows-pc1" . (+ 1 1))
                             (WIN1 . (+ 2 1))
                             (pkg  . (+ 9 1))
                             (win2 . (+ 4 1))))
      (should (equal var '(+ 9 1)))

      ;; sn
      (ss-set-no-eval 'var
                            (("windows-pc1" . (+ 1 1))
                             (WIN1 . (+ 2 1))
                             (sn   . (+ 10 1))
                             (win2 . (+ 4 1))))
      (should (equal var '(+ 10 1)))

      ;; and
      (ss-set-no-eval 'var
                            (("windows-pc1" . (+ 1 1))
                             (WIN1 . (+ 2 1))
                             (and  . (+ 11 1))
                             (win2 . (+ 4 1))))
      (should (equal var '(+ 11 1)))

      ;; default
      (ss-set-no-eval 'var
                            (("windows-pc1" . (+ 1 1))
                             (WIN1 . (+ 2 1))
                             (MAC1 . (+ 3 1))
                             (default . (+ 12 1))))
      (should (equal var '(+ 12 1)))

      ;; default
      (ss-set-no-eval 'var
                            (("windows-pc1" . (+ 1 1))
                             (WIN1    . (+ 2 1))
                             (default . (+ 12 1))
                             (win2    . (+ 3 1))
                             ))
      (should (equal var '(+ 3 1)))
      )))

(ert-deftest ss-setq-no-eval ()
  (let ((var nil)
        (ss-separator-alist
         '((WIN1 . "windows-pc1")
           (MAC1 . "mac-pc1")
           (win2 . "windows-pc2")
           (all  . (:eval t))
           (win (:separators WIN1 win2))
           (seps (:alias MAC1 WIN1))
           (ev1 . 5)
           (ev2 . (:emacs-version>= 5 3))
           (os . (:os windows-nt))
           (os1 . windows-nt)
           (pkg . (:package-available cl-lib system-separate))
           (sn . (:system-name "windows-pc2"))
           (and . (:and
                   win os ev1 pkg sn))
           ))
        (emacs-major-version 6)
        (emacs-minor-version 2)
        (system-type 'windows-nt))
    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))

      (ss-setq-no-eval var
                             (("windows-pc2" . (+ 1 1))
                              (win2 . (+ 3 1))))
      (should (equal var '(+ 1 1)))

      (ss-setq-no-eval var
                             (("windows-pc1" . (+ 1 1))
                              (WIN1 . (+ 2 1))
                              ("windows-pc2" . (+ 3 1))
                              ))
      (should (equal var '(+ 3 1)))

      (ss-setq-no-eval var
                             (("windows-pc1" . (+ 1 1))
                              (WIN1 . (+ 2 1))
                              ("windows-pc2" . (+ 3 1))
                              (win2 . (+ 4 1))))
      (should (equal var '(+ 3 1)))

      ;; all
      (ss-setq-no-eval var
                             (("windows-pc1" . (+ 1 1))
                              (WIN1 . (+ 2 1))
                              (all  . (+ 3 1))
                              (win2 . (+ 4 1))))
      (should (equal var '(+ 3 1)))

      ;; win
      (ss-setq-no-eval var
                             (("windows-pc1" . (+ 1 1))
                              (WIN1 . (+ 2 1))
                              (win  . (+ 5 1))
                              (win2 . (+ 4 1))))
      (should (equal var '(+ 5 1)))

      ;; seps
      (ss-setq-no-eval var
                             (("windows-pc1" . (+ 1 1))
                              (WIN1 . (+ 2 1))
                              (seps . (+ 3 1))
                              (win2 . (+ 4 1))))
      (should (equal var '(+ 4 1)))

      ;; ev1
      (ss-setq-no-eval var
                             (("windows-pc1" . (+ 1 1))
                              (WIN1 . (+ 2 1))
                              (ev1  . (+ 5 1))
                              (win2 . (+ 4 1))))
      (should (equal var '(+ 5 1)))

      ;; ev2
      (ss-setq-no-eval var
                             (("windows-pc1" . (+ 1 1))
                              (WIN1 . (+ 2 1))
                              (ev2  . (+ 6 1))
                              (win2 . (+ 4 1))))
      (should (equal var '(+ 6 1)))

      ;; os
      (ss-setq-no-eval var
                             (("windows-pc1" . (+ 1 1))
                              (WIN1 . (+ 2 1))
                              (os   . (+ 7 1))
                              (win2 . (+ 4 1))))
      (should (equal var '(+ 7 1)))

      ;; os1
      (ss-setq-no-eval var
                             (("windows-pc1" . (+ 1 1))
                              (WIN1 . (+ 2 1))
                              (os1  . (+ 8 1))
                              (win2 . (+ 4 1))))
      (should (equal var '(+ 8 1)))

      ;; pkg
      (ss-setq-no-eval var
                             (("windows-pc1" . (+ 1 1))
                              (WIN1 . (+ 2 1))
                              (pkg  . (+ 9 1))
                              (win2 . (+ 4 1))))
      (should (equal var '(+ 9 1)))

      ;; sn
      (ss-setq-no-eval var
                             (("windows-pc1" . (+ 1 1))
                              (WIN1 . (+ 2 1))
                              (sn   . (+ 10 1))
                              (win2 . (+ 4 1))))
      (should (equal var '(+ 10 1)))

      ;; and
      (ss-setq-no-eval var
                             (("windows-pc1" . (+ 1 1))
                              (WIN1 . (+ 2 1))
                              (and  . (+ 11 1))
                              (win2 . (+ 4 1))))
      (should (equal var '(+ 11 1)))

      ;; default
      (ss-setq-no-eval var
                             (("windows-pc1" . (+ 1 1))
                              (WIN1 . (+ 2 1))
                              (MAC1 . (+ 3 1))
                              (default . (+ 12 1))))
      (should (equal var '(+ 12 1)))

      ;; default
      (ss-setq-no-eval var
                             (("windows-pc1" . (+ 1 1))
                              (WIN1    . (+ 2 1))
                              (default . (+ 12 1))
                              (win2    . (+ 3 1))
                              ))
      (should (equal var '(+ 3 1)))
      )))



(ert-deftest ss-set ()
  (let ((var nil)
        (ss-separator-alist
         '((WIN1 . "windows-pc1")
           (MAC1 . "mac-pc1")
           (win2 . "windows-pc2")
           (all  . (:eval t))
           (win (:separators WIN1 win2))
           (seps (:alias MAC1 WIN1))
           (ev1 . 5)
           (ev2 . (:emacs-version>= 5 3))
           (os . (:os windows-nt))
           (os1 . windows-nt)
           (pkg . (:package-available cl-lib system-separate))
           (sn . (:system-name "windows-pc2"))
           (and . (:and
                   win os ev1 pkg sn))
           ))
        (emacs-major-version 6)
        (emacs-minor-version 2)
        (system-type 'windows-nt))
    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))

      (ss-set 'var
                    (("windows-pc2" . 1)
                     (win2 . 3)))
      (should (equal var 1))

      (ss-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     ("windows-pc2" . 3)
                     ))
      (should (equal var 3))

      (ss-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     ("windows-pc2" . 3)
                     (win2 . 4)))
      (should (equal var 3))

      ;; all
      (ss-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     (all  . 3)
                     (win2 . 4)))
      (should (equal var 3))

      ;; win
      (ss-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     (win  . 5)
                     (win2 . 4)))
      (should (equal var 5))

      ;; seps
      (ss-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     (seps  . 3)
                     (win2 . 4)))
      (should (equal var 4))

      ;; ev1
      (ss-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     (ev1  . 5)
                     (win2 . 4)))
      (should (equal var 5))

      ;; ev2
      (ss-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     (ev2  . 6)
                     (win2 . 4)))
      (should (equal var 6))

      ;; os
      (ss-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     (os  . 7)
                     (win2 . 4)))
      (should (equal var 7))

      ;; os1
      (ss-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     (os1  . 8)
                     (win2 . 4)))
      (should (equal var 8))

      ;; pkg
      (ss-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     (pkg  . 9)
                     (win2 . 4)))
      (should (equal var 9))

      ;; sn
      (ss-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     (sn   . 10)
                     (win2 . 4)))
      (should (equal var 10))

      ;; and
      (ss-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     (and  . 11)
                     (win2 . 4)))
      (should (equal var 11))

      ;; default
      (ss-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     (MAC1 . 3)
                     (default . 12)))
      (should (equal var 12))

      ;; default
      (ss-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     (default . 12)
                     (win2 . 3)
                     ))
      (should (equal var 3))
      )))

(ert-deftest ss-setq ()
  (let ((var nil)
        (ss-separator-alist '((WIN1 . "windows-pc1")
                                    (MAC1 . "mac-pc1")
                                    (win2 . "windows-pc2"))))
    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))
      (ss-setq var
                     (("windows-pc2" . 1)
                      (win2 . 3)))

      (should (equal var 1))
      (ss-setq var
                     (("windows-pc1" . 1)
                      (WIN1 . 2)
                      (win2 . 4)))
      (should (equal var 4))
      ))
  )

(ert-deftest ss-cond ()
  (let ((var nil)
        (ss-separator-alist '(("windows-pc1" . WIN1)
                                    ("mac-pc1" . MAC1)
                                    ("windows-pc2" . 1)
                                    ("windows-pc2" . 5)
                                    ("windows-pc2" . win2))))
    (flet ((system-name () "windows-pc2")
           (abc (arg) (setq var arg)))
      (should (equal (system-name) "windows-pc2"))

      (should
       (equal
        (ss-cond
         ("windows-pc2" 1)
         (5 3 2)
         (win2 6 3)
         (default 100))
        1))

      (ss-cond
       ("windows-pc2"
        (abc 1))
       (5
        (abc 2))
       (win2
        (abc 3)))
      (should (equal var 1))
      )))

;; (cl-loop for x in '(1 2 3 4)
;;          collect x)

;; (assoc "windows-pc2"
;;        '(("windows-pc1" . 1)
;;          (WIN1 . 2)
;;          ("windows-pc2" . 3)
;;          (win2 . 4)))

(provide 'system-separate-test)
;;; system-separate-test.el ends here
