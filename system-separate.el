;;; system-separate.el --- separate setting by pc system -*- lexical-binding: t -*-

;; Copyright (C) 2018  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: tools

;; URL: https://github.com/ROCKTAKEY/pc-separate

;; Package-Requires: ((cl-lib "0.6.1") (emacs "24.3") (dash "2.15.0"))

;; Version: 0.0.6

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

;; * Change action by pc system
;;   "system-separate" provide function that help you to separate setting
;;   by system name given by "(system-name)".
;; * Macros
;; ** =system-separate-set (variable alist)=
;;   - Set value of =VARIABLE= depend on =SYSTEM-SEPARATOR= below.
;;   - Each element of =ALIST= is =(SYSTEM-SEPARATOR . VALUE)=,
;;     and =VARIABLE= is set to =VALUE=
;;     if =SYSTEM-SEPARATOR= is valid.
;;   - If there are some cons cells whose car (= =SYSTEM-SEPARATOR=) is valid,
;;     upstream element is used, and rest of them is not evaluated.
;;   - in the cons cell whose =SYSTEM-SEPARATOR= is =default=,
;;     its =VALUE= is used only when any other =SYSTEM-SEPARATOR= isn't valid.
;;   - =(system-separate-set 'a ((b . c) ...))= is absolutely same as
;;     =(system-separate-setq a ((b . c) ...))=.
;; ** =system-separate-setq (variable alist)=
;;   - Same as =system-separate-set=, but =VARIABLE= doesn't have to be quoted.
;;   - See [[#HowToUse][How to Use Section]] as example.
;; ** =system-separate-set-no-eval (variable alist)=
;;   - Same as =system-separate-set-no-eval=, but =VALUE= are NOT evalueted.
;; ** =system-separate-setq-no-eval (variable alist)=
;;   - Same as =system-separate-setq-no-eval=, but =VALUE= are NOT evalueted.
;; ** =system-separate-cond (&body clauses)=
;;   - Similar to =cond=, but use =SYSTEM-SEPARATOR= instead of =CANDICATE=.
;;     If =SYSTEM-SEPARATOR= is valid, evaluate =BODY=.
;;   - Priority of each clause is same as =system-separate-set=.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'dash)

(defgroup system-separate nil
  "system-separate group."
  :group 'tools
  :prefix "system-separate-")

(defcustom system-separate-system-separator-alist '()
  "Relationship of \"number or symbol\", and system-name on separate.
You can add (system-name . number-or-symbol).
And you can use the number instead of system-name on separate."
  :group 'system-separate
  :type '(set (cons (choice integer symbol) string)))



(defvar system-separate--function-alist
  '(((:alias :system-separators :or) . system-separate--system-separators)
    (:and                 . system-separate--and)
    (:system-name         . system-separate--system-name)
    (:emacs-version>=     . system-separate--emacs-version>=)
    (:os                  . system-separate--os)
    (:eval                . system-separate--eval)
    (:package-available   . system-separate--package-available)))

(defun system-separate--system-name (args)
  "Return non-nil if ARGS contain value returned by function `system-name'."
  (-any?
   (lambda (a) nil nil
     (string= a (system-name)))
   args))

(defun system-separate--emacs-version>= (args)
  "Return non-nil if ARG is same as or higher than variable `emacs-version'.
ARGS can have 1 or 2 element(s).  First is MAJOR, second is MINOR (optional).
If MAJOR.MINOR is same and or higher than variable `emacs-version', return non-nil."
  (let ((major (car args))
        (minor (cadr args)))
    (or (> emacs-major-version major)
        (and
         (= emacs-major-version major)
         (or (not minor) (>= emacs-minor-version minor))))))

(defun system-separate--system-separators (args)
  "Return non-nil if one of ARGS elements is a valid system-separator."
  (-any? 'system-separate--current-system-separator-p args))

(defun system-separate--and (args)
  "Return non-nil if all of ARGS elements are valid system-separator."
  (-all? 'system-separate--current-system-separator-p args))

(defun system-separate--os (args)
  "Return non-nil if one of ARGS elements is same as `system-type'."
  (-any?
   (lambda (a) nil nil
     (eq a system-type))
   args))

(defun system-separate--eval (args)
  "Return result of evaluating ARGS."
  (eval (cons 'progn args)))

(defun system-separate--package-available (args)
  "Return non-nil if all of ARGS elements are return non-nil when passed to `featurep'."
  (-all? 'featurep args))



(defun system-separate--function-assq (arg alist)
  "Return VALUE if (apply FUNC ARG) return t.
Each element of ALIST is (FUNC . VALUE)."
  (cl-loop
   for (x . y) in alist
   if (funcall x arg)
   return y
   end
   finally
   return nil))

(defun system-separate--assq (key alist)
  "Same as assq, but if car of element of ALIST is list, compare KEY to element of that, too."
  (cl-loop
   for (x . y) in alist
   if (or (eq key x) (and (listp x) (memq key x)))
   return y
   end
   finally return nil))



(defvar system-separate--default-alist
  '((stringp . :system-name)
    (numberp . :emacs-version>=))
  "Use VALUE to car of system-separator, if (apply KEY system-separator) return non-nil.
`listp' and `symbolp' is invalid for key, because these system-separator are
trapped before applied this variable to.")

(defun system-separate--system-separators-p (object)
  "Return t if OBJECT is system-separator list."
  (when (listp object)
    (not
     (let ((c (car object)))
       (when (symbolp c)
         (string= ":" (substring (symbol-name c) 0 1)))))))

(defun system-separate--system-separator-p (object)
  "Return non-nil if OBJECT is system-separator."
  (or
   (listp object)
   (symbolp object)
   (system-separate--function-assq object system-separate--default-alist)))

(defun system-separate--symbol-system-separator-instance (symbol-system-separator)
  "Return instance of SYMBOL-SYSTEM-SEPARATOR in `system-separate-system-separator-alist'."
  (cdr (assq symbol-system-separator system-separate-system-separator-alist)))

(defun system-separate--system-separator-normalize (system-separator)
  "Normarize SYSTEM-SEPARATOR.
Change SYSTEM-SEPARATOR to be listed one whose car is :foobar."
  (let ((hidden-kind
         (system-separate--function-assq system-separator system-separate--default-alist)))
    (cond
     ;; trap system-separators-list
     ((system-separate--system-separators-p system-separator) (cons ':system-separators system-separator))
     ;; make listed-separaor
     (hidden-kind (list hidden-kind system-separator))
     ;; fall out symbol-system-separator
     (t system-separator))))

(defun system-separate--symbol-system-separator-current-p (symbol-system-separator)
  "Return non-nil if SYMBOL-SYSTEM-SEPARATOR is valid system-separator."
  (or (system-separate--os (list symbol-system-separator))
      (system-separate--current-system-separator-p
       (system-separate--symbol-system-separator-instance symbol-system-separator))))

(defun system-separate--current-system-separator-p (system-separator)
  "Return non-nil if SYSTEM-SEPARATOR is valid system-separator."
  (cond
   ((null system-separator) nil)
   ((symbolp system-separator)
    (system-separate--symbol-system-separator-current-p system-separator))
   (t
    (setq system-separator (system-separate--system-separator-normalize system-separator))
    (funcall (system-separate--assq (car system-separator) system-separate--function-alist)
             (cdr system-separator)))))


;;;###autoload
(defmacro system-separate-set-no-eval (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SYSTEM-SEPARATOR . VALUE), and VARIABLE is set to VALUE
if (system-separate-current-system-separator-p SYSTEM-SEPARATOR) return non-nil.
VALUE is NOT evaluated.

\(fn VARIABLE ((SYSTEM-SEPARATOR . VALUE)...))"
  (declare (debug (form (&rest (system-separate-system-separator-p . [&rest sexp])))))
  (let ((valid-cons (gensym "valid-cons"))
        (system-separator (gensym "system-separator"))
        (value (gensym "value"))
        (default (gensym "default")))
    ;; throw error if ALIST is NOT both alist and symbol.
    `(let (,valid-cons (,default nil))
       (setq ,valid-cons
             (cl-loop
              for (,system-separator . ,value) in ',alist
              if (eq ,system-separator 'default)
              do (setq ,default (cons ,system-separator ,value))
              else if (system-separate--current-system-separator-p ,system-separator)
              return (cons ,system-separator ,value)
              end
              ;; If never eval "return form", return default
              ;; which is nil if no "default" system-separator.
              finally return ,default))
       ;; If no valid-cons, return nil because of `when' macro.
       (when ,valid-cons
         (set ,variable (cdr ,valid-cons)))
       ,valid-cons)))

;;;###autoload
(defmacro system-separate-setq-no-eval (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SYSTEM-SEPARATOR . VALUE), and VARIABLE is set to VALUE
if (system-separate-current-system-separator-p SYSTEM-SEPARATOR) return non-nil.
variable have to be non-quoted.
VALUE is NOT evaluated.

\(fn VARIABLE ((SYSTEM-SEPARATOR . VALUE)...))"
  (declare (debug (symbolp (&rest (system-separate-system-separator-p . [&rest sexp])))))
  `(system-separate-set-no-eval (quote ,variable) ,alist))



;;;###autoload
(defmacro system-separate-set (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SYSTEM-SEPARATOR . VALUE), and VARIABLE is set to VALUE
if (system-separate-current-system-separator-p SYSTEM-SEPARATOR) return non-nil.
VALUE is evaluated.

\(fn VARIABLE ((SYSTEM-SEPARATOR . VALUE)...))"
  (declare (debug (form (&rest (system-separate-system-separator-p . [&rest form])))))
  (let ((valid-cons (gensym "valid-cons"))
        (system-separator (gensym "system-separator"))
        (value (gensym "value"))
        (default (gensym "default")))
    `(let (,valid-cons (,default nil))
       (setq ,valid-cons
             (cl-loop
              for (,system-separator . ,value) in ',alist
              if (eq ,system-separator 'default)
              do (setq ,default (cons ,system-separator ,value))
              else if (system-separate--current-system-separator-p ,system-separator)
              return (cons ,system-separator ,value)
              end
              ;; If never eval "return form", return default
              ;; which is nil if no "default" system-separator.
              finally return ,default))
       ;; If no valid-cons, return nil because of `when' macro.
       (when ,valid-cons
         (set ,variable (eval (cdr ,valid-cons))))
       ,valid-cons)))

;;;###autoload
(defmacro system-separate-setq (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SYSTEM-SEPARATOR . VALUE), and VARIABLE is set to VALUE
if (system-separate-current-system-separator-p SYSTEM-SEPARATOR) return non-nil.
variable have to be non-quoted.
VALUE is evaluated.

\(fn VARIABLE ((SYSTEM-SEPARATOR . VALUE)...))"
  (declare (debug (symbolp (&rest (system-separate-system-separator-p . [&rest form])))))
  `(system-separate-set (quote ,variable) ,alist))

;;;###autoload
(defmacro system-separate-cond (&rest clauses)
  "Eval BODY if SYSTEM-SEPARATOR accords current system.
Each element of CLAUSES looks like (SYSTEM-SEPARATOR BODY...).  BODY is evaluate
if (system-separate-current-system-separator-p SYSTEM-SEPARATOR) return non-nil.

\(fn (SYSTEM-SEPARATOR BODY...)...)"
  (declare (debug (&rest (system-separate-system-separator-p [&rest form]))))
  (let ((c (gensym)))
    `(let (,c)
       (system-separate-setq-no-eval ,c ,clauses)
       (eval (cons 'progn ,c)))))

(provide 'system-separate)
;;; system-separate.el ends here
