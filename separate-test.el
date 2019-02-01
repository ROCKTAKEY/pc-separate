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

(ert-deftest separate-assoc-all ()
  (let ((alist
         '(("windows-pc1" . WIN1)
           ("mac-pc1" . MAC1)
           ("windows-pc2" . 1)
           ("windows-pc2" . 5)
           ("windows-pc2" . win2)
           (key . val)
           (key . val2)
           (foo . var))))
    (should (equal (separate--cdr-assoc-all "windows-pc2" alist)
                   '(1 5 win2)))
    (should (equal (separate--cdr-assoc-all 'key alist)
                   '(val val2)))
    (should (equal (separate--cdr-assoc-all 'foo alist)
                   '(var)))
    (should (equal (separate--cdr-assoc-all 'some alist)
                   nil))
    (should (equal (separate--cdr-assoc-all "some" alist)
                   nil))
    )
  )

(ert-deftest separate-assoc-or-eval ()
  (let ((alist
         '(("windows-pc1" . WIN1)
           ("mac-pc1" . MAC1)
           ("windows-pc2" . 1)
           ("windows-pc2" . 5)
           ("windows-pc2" . win2)
           (key . val)
           (key . val2)
           (foo . var)))
        )
    (should (equal (separate--assoc-or-eval "windows-pc2" alist)
                   '("windows-pc2" . 1)))
    (should (equal (separate--assoc-or-eval 'key alist)
                   '(key . val)))
    (should (equal (separate--assoc-or-eval 'foo alist)
                   '(foo . var)))
    (should (equal (separate--assoc-or-eval 'some alist)
                   nil))
    (should (equal (separate--assoc-or-eval "some" alist)
                   nil))
    ;; eval
    )
  )

(ert-deftest separate-sysname-to-sys ()
  (let ((separate-separator-alist '(("windows-pc1" . WIN1)
                                    ("mac-pc1" . MAC1)
                                    ("windows-pc2" . 1)
                                    ("windows-pc2" . 5)
                                    ("windows-pc2" . win2))))
    (should (equal (separate--system-name-to-separators "windows-pc2")
                   '(1 5 win2 "windows-pc2")))
    (should (equal (separate--system-name-to-separators "windows-pc1")
                   '(WIN1 "windows-pc1")))
    (should (equal (separate--system-name-to-separators "some")
                   '("some")))
    ))

(ert-deftest separate-sys-to-sysname ()
  (let ((separate-separator-alist '(("windows-pc1" . WIN1)
                                    ("mac-pc1" . MAC1)
                                    ("windows-pc2" . 1)
                                    ("windows-pc2" . 5)
                                    ("windows-pc2" . win2))))
    (should (equal (separate--separator-to-system-name 1)
                   "windows-pc2"))
    (should (equal (separate--separator-to-system-name 'win2)
                   "windows-pc2"))
    (should (equal (separate--separator-to-system-name 5)
                   "windows-pc2"))
    (should (equal (separate--separator-to-system-name 'MAC1)
                   "mac-pc1"))
    (should (equal (separate--separator-to-system-name 'some)
                   nil))
    ))

(ert-deftest separate-current-sys-p ()
  (let ((separate-separator-alist '(("windows-pc1" . WIN1)
                                    ("mac-pc1" . MAC1)
                                    ("windows-pc2" . 1)
                                    ("windows-pc2" . 5)
                                    ("windows-pc2" . win2))))
    (flet ((system-name () "windows-pc2"))
      (should (separate--current-separator-p 5))
      (should (separate--current-separator-p 1))
      (should (separate--current-separator-p 'win2))
      (should (not (separate--current-separator-p 'WIN1)))
      (should (not (separate--current-separator-p 4)))
      )))

(ert-deftest separate-set ()
  (let ((var nil)
        (separate-separator-alist '((WIN1 . "windows-pc1")
                                    (MAC1 . "mac-pc1")
                                    (win2 . "windows-pc2"))))

    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))

      (separate-set 'var
                    (("windows-pc2" . 1)
                     (5 . 2)
                     (win2 . 3)))
      (should (equal var 1))
      
      (separate-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     (5 . 3)
                     (win2 . 4)))
      (should (equal var 3))
      
      (separate-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     ("windows-pc2" . 3)
                     ))
      (should (equal var 3))
      
      (separate-set 'var
                    (("windows-pc1" . 1)
                     (WIN1 . 2)
                     ("windows-pc2" . 3)
                     (win2 . 4)))
      (should (equal var 3))
      ))
  )

(ert-deftest separate-setq ()
  (let ((var nil)
        (separate-separator-alist '(("windows-pc1" . WIN1)
                                    ("mac-pc1" . MAC1)
                                    ("windows-pc2" . 1)
                                    ("windows-pc2" . 5)
                                    ("windows-pc2" . win2))))
    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))
      (separate-setq var
                     (("windows-pc2" . 1)
                      (5 . 2)
                      (win2 . 3)))
      
      (should (equal var 1))
      (separate-setq var
                     (("windows-pc1" . 1)
                      (WIN1 . 2)
                      (5 . 3)
                      (win2 . 4)))
      (should (equal var 3))
      ))
  )

(ert-deftest separate-cond ()
  (let ((var nil)
        (separate-separator-alist '(("windows-pc1" . WIN1)
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
         (("windows-pc2" 1)
          (5 3 2)
          (win2 6 3)
          (default 100)))
        1))
      
      (separate-cond
       (("windows-pc2"
         (abc 1))
        (5
         (abc 2))
        (win2
         (abc 3))))
      (should (equal var 1))
      )))


;; (cl-loop for x in '(1 2 3 4)
;;          collect x)

;; (assoc "windows-pc2"
;;        '(("windows-pc1" . 1)
;;          (WIN1 . 2)
;;          ("windows-pc2" . 3)
;;          (win2 . 4)))

(provide 'separate-test)
;;; separate-test.el ends here

