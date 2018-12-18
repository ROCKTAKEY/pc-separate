;;; pc-separate-test.el --- Test of pc-separate

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
(require 'pc-separate)
(eval-when-compile
  (require 'cl))

(ert-deftest pc-separate-assoc-all ()
  (let ((alist
         '(("windows-pc1" . WIN1)
           ("mac-pc1" . MAC1)
           ("windows-pc2" . 1)
           ("windows-pc2" . 5)
           ("windows-pc2" . win2)
           (key . val)
           (key . val2)
           (foo . var))))
    (should (equal (pc-separate--cdr-assoc-all "windows-pc2" alist)
                   '(1 5 win2)))
    (should (equal (pc-separate--cdr-assoc-all 'key alist)
                   '(val val2)))
    (should (equal (pc-separate--cdr-assoc-all 'foo alist)
                   '(var)))
    (should (equal (pc-separate--cdr-assoc-all 'some alist)
                   nil))
    (should (equal (pc-separate--cdr-assoc-all "some" alist)
                   nil))
    )
  )

(ert-deftest pc-separate-assoc-or-eval ()
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
    (should (equal (pc-separate--assoc-or-eval "windows-pc2" alist)
                   '("windows-pc2" . 1)))
    (should (equal (pc-separate--assoc-or-eval 'key alist)
                   '(key . val)))
    (should (equal (pc-separate--assoc-or-eval 'foo alist)
                   '(foo . var)))
    (should (equal (pc-separate--assoc-or-eval 'some alist)
                   nil))
    (should (equal (pc-separate--assoc-or-eval "some" alist)
                   nil))
    ;; eval
    )
  )

(ert-deftest pc-separate-sysname-to-sys ()
  (let ((pc-separate-system-alist '(("windows-pc1" . WIN1)
                                    ("mac-pc1" . MAC1)
                                    ("windows-pc2" . 1)
                                    ("windows-pc2" . 5)
                                    ("windows-pc2" . win2))))
    (should (equal (pc-separate--system-name-to-system "windows-pc2")
                   '(1 5 win2 "windows-pc2")))
    (should (equal (pc-separate--system-name-to-system "windows-pc1")
                   '(WIN1 "windows-pc1")))
    (should (equal (pc-separate--system-name-to-system "some")
                   '("some")))
    ))

(ert-deftest pc-separate-sys-to-sysname ()
  (let ((pc-separate-system-alist '(("windows-pc1" . WIN1)
                                    ("mac-pc1" . MAC1)
                                    ("windows-pc2" . 1)
                                    ("windows-pc2" . 5)
                                    ("windows-pc2" . win2))))
    (should (equal (pc-separate--system-to-system-name 1)
                   "windows-pc2"))
    (should (equal (pc-separate--system-to-system-name 'win2)
                   "windows-pc2"))
    (should (equal (pc-separate--system-to-system-name 5)
                   "windows-pc2"))
    (should (equal (pc-separate--system-to-system-name 'MAC1)
                   "mac-pc1"))
    (should (equal(pc-separate--system-to-system-name 'some)
                  nil))
    ))

(ert-deftest pc-separate-current-sys-p ()
  (let ((pc-separate-system-alist '(("windows-pc1" . WIN1)
                                    ("mac-pc1" . MAC1)
                                    ("windows-pc2" . 1)
                                    ("windows-pc2" . 5)
                                    ("windows-pc2" . win2))))
    (flet ((system-name () "windows-pc2"))
      (should (pc-separate--current-system-p 5))
      (should (pc-separate--current-system-p 1))
      (should (pc-separate--current-system-p 'win2))
      (should (not (pc-separate--current-system-p 'WIN1)))
      (should (not (pc-separate--current-system-p 4)))
      )))

(ert-deftest pc-separate-set ()
  (let ((var nil)
        (pc-separate-system-alist '(("windows-pc1" . WIN1)
                                    ("mac-pc1" . MAC1)
                                    ("windows-pc2" . 1)
                                    ("windows-pc2" . 5)
                                    ("windows-pc2" . win2))))
    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))
      (pc-separate-set 'var
                       '(("windows-pc2" . 1)
                         (5 . 2)
                         (win2 . 3)))      
      (should (equal var 2))
      
      (pc-separate-set 'var
                       '(("windows-pc1" . 1)
                         (WIN1 . 2)
                         (5 . 3)
                         (win2 . 4)))
      (should (equal var 3))
      
      (pc-separate-set 'var
                       '(("windows-pc1" . 1)
                         (WIN1 . 2)
                         ("windows-pc2" . 3)
                         ))
      (should (equal var 3))
      
      (pc-separate-set 'var
                       '(("windows-pc1" . 1)
                         (WIN1 . 2)
                         ("windows-pc2" . 3)
                         (win2 . 4)))   ;symbol have higher priority
      (should (equal var 4))
      ))
  )

(ert-deftest pc-separate-setq ()
  (let ((var nil)
        (pc-separate-system-alist '(("windows-pc1" . WIN1)
                                    ("mac-pc1" . MAC1)
                                    ("windows-pc2" . 1)
                                    ("windows-pc2" . 5)
                                    ("windows-pc2" . win2))))
    (flet ((system-name () "windows-pc2"))
      (should (equal (system-name)"windows-pc2"))
      (pc-separate-setq var
                        '(("windows-pc2" . 1)
                          (5 . 2)
                          (win2 . 3)))
      
      (should (equal var 2))
      (pc-separate-setq var
                        '(("windows-pc1" . 1)
                          (WIN1 . 2)
                          (5 . 3)
                          (win2 . 4)))
      (should (equal var 3))
      ))
  )

(ert-deftest pc-separate-cond ()
  (let ((var nil)
        (pc-separate-system-alist '(("windows-pc1" . WIN1)
                                    ("mac-pc1" . MAC1)
                                    ("windows-pc2" . 1)
                                    ("windows-pc2" . 5)
                                    ("windows-pc2" . win2))))
    (flet ((system-name () "windows-pc2")
           (abc (arg) (setq var arg)))
      (should (equal (system-name) "windows-pc2"))

      (should
       (equal
        (pc-separate-cond
         '(("windows-pc2"
            1)
           (5
            3
            2)
           (win2
            6
            3)))
        2))
      
      (pc-separate-cond
       '(("windows-pc2"
          (abc 1))
         (5
          (abc 2))
         (win2
          (abc 3))))
      (should (equal var 2))
      )))


;; (cl-loop for x in '(1 2 3 4)
;;          collect x)

;; (assoc "windows-pc2"
;;        '(("windows-pc1" . 1)
;;          (WIN1 . 2)
;;          ("windows-pc2" . 3)
;;          (win2 . 4)))

(provide 'pc-separate-test)
;;; pc-separate-test.el ends here

