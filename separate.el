;;; separate.el --- separate setting by pc system -*- lexical-binding: t -*-

;; Copyright (C) 2018  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: tools

;; URL: https://github.com/ROCKTAKEY/pc-separate

;; Package-Requires: ((cl-lib "0.6.1") (emacs "24.3") (dash "2.15.0"))

;; Version: 0.0.8

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
;;   "separate" provide function that help you to separate setting
;;   by system name given by "(system-name)".
;; * Macros
;; ** =separate-set (variable alist)=
;;   - Set value of =VARIABLE= depend on =SYSTEM-PREDICATE= below.
;;   - Each element of =ALIST= is =(SYSTEM-PREDICATE . VALUE)=,
;;     and =VARIABLE= is set to =VALUE=
;;     if =SYSTEM-PREDICATE= is valid.
;;   - If there are some cons cells whose car (= =SYSTEM-PREDICATE=) is valid,
;;     upstream element is used, and rest of them is not evaluated.
;;   - in the cons cell whose =SYSTEM-PREDICATE= is =default=,
;;     its =VALUE= is used only when any other =SYSTEM-PREDICATE= isn't valid.
;;   - =(separate-set 'a ((b . c) ...))= is absolutely same as
;;     =(separate-setq a ((b . c) ...))=.
;; ** =separate-setq (variable alist)=
;;   - Same as =separate-set=, but =VARIABLE= doesn't have to be quoted.
;;   - See [[#HowToUse][How to Use Section]] as example.
;; ** =separate-set-no-eval (variable alist)=
;;   - Same as =separate-set-no-eval=, but =VALUE= are NOT evalueted.
;; ** =separate-setq-no-eval (variable alist)=
;;   - Same as =separate-setq-no-eval=, but =VALUE= are NOT evalueted.
;; ** =separate-cond (&body clauses)=
;;   - Similar to =cond=, but use =SYSTEM-PREDICATE= instead of =CANDICATE=.
;;     If =SYSTEM-PREDICATE= is valid, evaluate =BODY=.
;;   - Priority of each clause is same as =separate-set=.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'dash)

(defgroup separate nil
  "separate group."
  :group 'tools
  :prefix "separate-")

(defcustom separate-system-predicate-alist '()
  "Relationship of \"number or symbol\", and system-name on separate.
You can add (system-name . number-or-symbol).
And you can use the number instead of system-name on separate."
  :group 'separate
  :type '(set (cons (choice integer symbol) string)))



(defvar separate--function-alist
  '(((:alias :system-predicates :or) . separate--system-predicates)
    (:and                 . separate--and)
    (:system-name         . separate--system-name)
    (:emacs-version>=     . separate--emacs-version>=)
    (:os                  . separate--os)
    (:eval                . separate--eval)
    (:package-available   . separate--package-available)))

(defun separate--system-name (args)
  "Return non-nil if ARGS contain value returned by function `system-name'."
  (-any?
   (lambda (a) nil nil
     (string= a (system-name)))
   args))

(defun separate--emacs-version>= (args)
  "Return non-nil if ARG is same as or higher than variable `emacs-version'.
ARGS can have 1 or 2 element(s).  First is MAJOR, second is MINOR (optional).
If MAJOR.MINOR is same and or higher than variable `emacs-version', return non-nil."
  (let ((major (car args))
        (minor (cadr args)))
    (or (> emacs-major-version major)
        (and
         (= emacs-major-version major)
         (or (not minor) (>= emacs-minor-version minor))))))

(defun separate--system-predicates (args)
  "Return non-nil if one of ARGS elements is a valid system-predicate."
  (-any? 'separate--current-system-predicate-p args))

(defun separate--and (args)
  "Return non-nil if all of ARGS elements are valid system-predicate."
  (-all? 'separate--current-system-predicate-p args))

(defun separate--os (args)
  "Return non-nil if one of ARGS elements is same as `system-type'."
  (-any?
   (lambda (a) nil nil
     (eq a system-type))
   args))

(defun separate--eval (args)
  "Return result of evaluating ARGS."
  (eval (cons 'progn args)))

(defun separate--package-available (args)
  "Return non-nil if all of ARGS elements are return non-nil when passed to `featurep'."
  (-all? 'featurep args))



(defun separate--function-assq (arg alist)
  "Return VALUE if (apply FUNC ARG) return t.
Each element of ALIST is (FUNC . VALUE)."
  (cl-loop
   for (x . y) in alist
   if (funcall x arg)
   return y
   end
   finally
   return nil))

(defun separate--assq (key alist)
  "Same as assq, but if car of element of ALIST is list, compare KEY to element of that, too."
  (cl-loop
   for (x . y) in alist
   if (or (eq key x) (and (listp x) (memq key x)))
   return y
   end
   finally return nil))



(defvar separate--default-alist
  '((stringp . :system-name)
    (numberp . :emacs-version>=))
  "Use VALUE to car of system-predicate, if (apply KEY system-predicate) return non-nil.
`listp' and `symbolp' is invalid for key, because these system-predicate are
trapped before applied this variable to.")

(defun separate--system-predicates-p (object)
  "Return t if OBJECT is system-predicate list."
  (when (listp object)
    (not
     (let ((c (car object)))
       (when (symbolp c)
         (string= ":" (substring (symbol-name c) 0 1)))))))

(defun separate--system-predicate-p (object)
  "Return non-nil if OBJECT is system-predicate."
  (or
   (listp object)
   (symbolp object)
   (separate--function-assq object separate--default-alist)))

(defun separate--symbol-system-predicate-instance (symbol-system-predicate)
  "Return instance of SYMBOL-SYSTEM-PREDICATE in `separate-system-predicate-alist'."
  (cdr (assq symbol-system-predicate separate-system-predicate-alist)))

(defun separate--system-predicate-normalize (system-predicate)
  "Normarize SYSTEM-PREDICATE.
Change SYSTEM-PREDICATE to be listed one whose car is :foobar."
  (let ((hidden-kind
         (separate--function-assq system-predicate separate--default-alist)))
    (cond
     ;; trap system-predicates-list
     ((separate--system-predicates-p system-predicate) (cons ':system-predicates system-predicate))
     ;; make listed-separaor
     (hidden-kind (list hidden-kind system-predicate))
     ;; fall out symbol-system-predicate
     (t system-predicate))))

(defun separate--symbol-system-predicate-current-p (symbol-system-predicate)
  "Return non-nil if SYMBOL-SYSTEM-PREDICATE is valid system-predicate."
  (or (separate--os (list symbol-system-predicate))
      (separate--current-system-predicate-p
       (separate--symbol-system-predicate-instance symbol-system-predicate))))

(defun separate--current-system-predicate-p (system-predicate)
  "Return non-nil if SYSTEM-PREDICATE is valid system-predicate."
  (cond
   ((null system-predicate) nil)
   ((symbolp system-predicate)
    (separate--symbol-system-predicate-current-p system-predicate))
   (t
    (setq system-predicate (separate--system-predicate-normalize system-predicate))
    (funcall (separate--assq (car system-predicate) separate--function-alist)
             (cdr system-predicate)))))


;;;###autoload
(defmacro separate-set-no-eval (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SYSTEM-PREDICATE . VALUE), and VARIABLE is set to VALUE
if (separate-current-system-predicate-p SYSTEM-PREDICATE) return non-nil.
VALUE is NOT evaluated.

\(fn VARIABLE ((SYSTEM-PREDICATE . VALUE)...))"
  (declare (debug (form (&rest (separate-system-predicate-p . [&rest sexp])))))
  (let ((valid-cons (cl-gensym "valid-cons"))
        (system-predicate (cl-gensym "system-predicate"))
        (value (cl-gensym "value"))
        (default (cl-gensym "default")))
    ;; throw error if ALIST is NOT both alist and symbol.
    `(let (,valid-cons (,default nil))
       (setq ,valid-cons
             (cl-loop
              for (,system-predicate . ,value) in ',alist
              if (eq ,system-predicate 'default)
              do (setq ,default (cons ,system-predicate ,value))
              else if (separate--current-system-predicate-p ,system-predicate)
              return (cons ,system-predicate ,value)
              end
              ;; If never eval "return form", return default
              ;; which is nil if no "default" system-predicate.
              finally return ,default))
       ;; If no valid-cons, return nil because of `when' macro.
       (when ,valid-cons
         (set ,variable (cdr ,valid-cons)))
       ,valid-cons)))

;;;###autoload
(defmacro separate-setq-no-eval (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SYSTEM-PREDICATE . VALUE), and VARIABLE is set to VALUE
if (separate-current-system-predicate-p SYSTEM-PREDICATE) return non-nil.
variable have to be non-quoted.
VALUE is NOT evaluated.

\(fn VARIABLE ((SYSTEM-PREDICATE . VALUE)...))"
  (declare (debug (symbolp (&rest (separate-system-predicate-p . [&rest sexp])))))
  `(separate-set-no-eval (quote ,variable) ,alist))



;;;###autoload
(defmacro separate-set (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SYSTEM-PREDICATE . VALUE), and VARIABLE is set to VALUE
if (separate-current-system-predicate-p SYSTEM-PREDICATE) return non-nil.
VALUE is evaluated.

\(fn VARIABLE ((SYSTEM-PREDICATE . VALUE)...))"
  (declare (debug (form (&rest (separate-system-predicate-p . [&rest form])))))
  (let ((valid-cons (cl-gensym "valid-cons"))
        (system-predicate (cl-gensym "system-predicate"))
        (value (cl-gensym "value"))
        (default (cl-gensym "default")))
    `(let (,valid-cons (,default nil))
       (setq ,valid-cons
             (cl-loop
              for (,system-predicate . ,value) in ',alist
              if (eq ,system-predicate 'default)
              do (setq ,default (cons ,system-predicate ,value))
              else if (separate--current-system-predicate-p ,system-predicate)
              return (cons ,system-predicate ,value)
              end
              ;; If never eval "return form", return default
              ;; which is nil if no "default" system-predicate.
              finally return ,default))
       ;; If no valid-cons, return nil because of `when' macro.
       (when ,valid-cons
         (set ,variable (eval (cdr ,valid-cons))))
       ,valid-cons)))

;;;###autoload
(defmacro separate-setq (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SYSTEM-PREDICATE . VALUE), and VARIABLE is set to VALUE
if (separate-current-system-predicate-p SYSTEM-PREDICATE) return non-nil.
variable have to be non-quoted.
VALUE is evaluated.

\(fn VARIABLE ((SYSTEM-PREDICATE . VALUE)...))"
  (declare (debug (symbolp (&rest (separate-system-predicate-p . [&rest form])))))
  `(separate-set (quote ,variable) ,alist))

;;;###autoload
(defmacro separate-cond (&rest clauses)
  "Eval BODY if SYSTEM-PREDICATE accords current system.
Each element of CLAUSES looks like (SYSTEM-PREDICATE BODY...).  BODY is evaluate
if (separate-current-system-predicate-p SYSTEM-PREDICATE) return non-nil.

\(fn (SYSTEM-PREDICATE BODY...)...)"
  (declare (debug (&rest (separate-system-predicate-p [&rest form]))))
  (let ((c (cl-gensym)))
    `(let (,c)
       (separate-setq-no-eval ,c ,clauses)
       (eval (cons 'progn ,c)))))

(provide 'separate)
;;; separate.el ends here
