;;; separate.el --- separate setting by pc system -*- lexical-binding: t -*-

;; Copyright (C) 2018  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: tools

;; URL: https://github.com/ROCKTAKEY/pc-separate

;; Package-Requires: ((cl-lib "0.6.1") (emacs "24.3") (dash "2.15.0"))

;; Version: 0.0.3

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
;;   - Set value of =VARIABLE= depend on =SEPARATOR= below.
;;   - Each element of =ALIST= is =(SEPARATOR . VALUE)=,
;;     and =VARIABLE= is set to =VALUE=
;;     if =SEPARATOR= is valid.
;;   - If there are some cons cells whose car (= =SEPARATOR=) is valid,
;;     upstream element is used, and rest of them is not evaluated.
;;   - in the cons cell whose =SEPARATOR= is =default=,
;;     its =VALUE= is used only when any other =SEPARATOR= isn't valid.
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
;;   - Similar to =cond=, but use =SEPARATOR= instead of =CANDICATE=.
;;     If =SEPARATOR= is valid, evaluate =BODY=.
;;   - Priority of each clause is same as =separate-set=.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'dash)

(defgroup separate nil
  "separate group."
  :group 'tools)

(defcustom separate-separator-alist '()
  "Relationship of \"number or symbol\", and system-name on separate.
You can add (system-name . number-or-symbol).
And you can use the number instead of system-name on separate."
  :group 'separate
  :type '(set (cons (choice integer symbol) string))
  )



(defvar separate--function-alist
  '(((:alias :separators :or) . separate--separators)
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
         (or (not minor) (>= emacs-minor-version minor)))))
  )

(defun separate--separators (args)
  "Return non-nil if one of ARGS elements is a valid separator."
  (-any? 'separate--current-separator-p args))

(defun separate--and (args)
  "Return non-nil if all of ARGS elements are valid separator."
  (-all? 'separate--current-separator-p args))

(defun separate--os (args)
  "Return non-nil if one of ARGS elements is same as `system-type'."
  (-any?
   (lambda (a) nil nil
     (eq a system-type))
   args)
  )

(defun separate--eval (args)
  "Return result of evaluating ARGS."
  (eval (cons 'progn args))
  )

(defun separate--package-available (args)
  "Return non-nil if all of ARGS elements are return non-nil when passed to `featurep'."
  (-all? 'featurep args)
  )



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
   finally return nil
   )
  )



(defvar separate--default-alist
  '((stringp . :system-name)
    (numberp . :emacs-version>=))
  "Use VALUE to car of separator, if (apply KEY separator) return non-nil.
`listp' and `symbolp' is invalid for key, because these separator are
trapped before applied this variable to.")

(defun separate--separators-p (object)
  "Return t if OBJECT is separator list."
  (when (listp object)
    (not
     (let ((c (car object)))
       (when (symbolp c)
         (string= ":" (substring (symbol-name c) 0 1)))))))

(defun separate--symbol-separator-instance (symbol-separator)
  "Return instance of SYMBOL-SEPARATOR in `separate-separator-alist'."
  (cdr (assq symbol-separator separate-separator-alist))
  )

(defun separate--separator-normalize (separator)
  "Normarize SEPARATOR.
Change SEPARATOR to be listed one whose car is :foobar."
  (let ((hidden-kind
         (separate--function-assq separator separate--default-alist)))
    (cond
     ;; trap separators-list
     ((separate--separators-p separator) (cons ':separators separator))
     ;; make listed-separaor
     (hidden-kind (list hidden-kind separator))
     ;; fall out symbol-separator
     (t separator))
    ))

(defun separate--symbol-separator-current-p (symbol-separator)
  "Return non-nil if SYMBOL-SEPARATOR is valid separator."
  (or (separate--os (list symbol-separator))
      (separate--current-separator-p
       (separate--symbol-separator-instance symbol-separator)))
  )

(defun separate--current-separator-p (separator)
  "Return non-nil if SEPARATOR is valid separator."
  (cond
   ((null separator) nil)
   ((symbolp separator)
    (separate--symbol-separator-current-p separator))
   (t
    (setq separator (separate--separator-normalize separator))
    (funcall (separate--assq (car separator) separate--function-alist)
             (cdr separator)))))


;;;###autoload
(defmacro separate-set-no-eval (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SEPARATOR . VALUE), and VARIABLE is set to VALUE
if (separate-current-separator-p SEPARATOR) return non-nil.
VALUE is NOT evaluated.

\(fn VARIABLE ((SEPARATOR . VALUE)...))"
  (let ((valid-cons (gensym "valid-cons"))
        (separator (gensym "separator"))
        (value (gensym "value"))
        (default (gensym "default"))
        )
    ;; throw error if ALIST is NOT both alist and symbol.
    `(let (,valid-cons (,default nil))
       (setq ,valid-cons
             (cl-loop
              for (,separator . ,value) in ',alist
              if (eq ,separator 'default)
              do (setq ,default (cons ,separator ,value))
              else if (separate--current-separator-p ,separator)
              return (cons ,separator ,value)
              end
              ;; If never eval "return form", return default
              ;; which is nil if no "default" separator.
              finally return ,default
              ))
       ;; If no valid-cons, return nil because of `when' macro.
       (when ,valid-cons
         (set ,variable (cdr ,valid-cons)))
       ,valid-cons)))

;;;###autoload
(defmacro separate-setq-no-eval (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SEPARATOR . VALUE), and VARIABLE is set to VALUE
if (separate-current-separator-p SEPARATOR) return non-nil.
variable have to be non-quoted.
VALUE is NOT evaluated.

\(fn VARIABLE ((SEPARATOR . VALUE)...))"
  `(separate-set-no-eval (quote ,variable) ,alist))



;;;###autoload
(defmacro separate-set (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SEPARATOR . VALUE), and VARIABLE is set to VALUE
if (separate-current-separator-p SEPARATOR) return non-nil.
VALUE is evaluated.

\(fn VARIABLE ((SEPARATOR . VALUE)...))"
  (let ((valid-cons (gensym "valid-cons"))
        (separator (gensym "separator"))
        (value (gensym "value"))
        (default (gensym "default"))
        )
    `(let (,valid-cons (,default nil))
       (setq ,valid-cons
             (cl-loop
              for (,separator . ,value) in ',alist
              if (eq ,separator 'default)
              do (setq ,default (cons ,separator ,value))
              else if (separate--current-separator-p ,separator)
              return (cons ,separator ,value)
              end
              ;; If never eval "return form", return default
              ;; which is nil if no "default" separator.
              finally return ,default
              ))
       ;; If no valid-cons, return nil because of `when' macro.
       (when ,valid-cons
         (set ,variable (eval (cdr ,valid-cons))))
       ,valid-cons)))

;;;###autoload
(defmacro separate-setq (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SEPARATOR . VALUE), and VARIABLE is set to VALUE
if (separate-current-separator-p SEPARATOR) return non-nil.
variable have to be non-quoted.
VALUE is evaluated.

\(fn VARIABLE ((SEPARATOR . VALUE)...))"
  `(separate-set (quote ,variable) ,alist))

;;;###autoload
(defmacro separate-cond (&rest clauses)
  "Eval BODY if SEPARATOR accords current system.
Each element of CLAUSES looks like (SEPARATOR BODY...).  BODY is evaluate
if (separate-current-separator-p SEPARATOR) return non-nil.

\(fn (SEPARATOR BODY...)...)"
  (let ((c (gensym)))
    `(let (,c)
       (separate-setq-no-eval ,c ,clauses)
       (eval (cons 'progn ,c)))))

(provide 'separate)
;;; separate.el ends here
