;;; separate.el --- separate setting by pc system -*- lexical-binding: t -*-

;; Copyright (C) 2018  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: tools

;; Package-Requires: ((cl-lib "1.0") (emacs "26.4"))

;; Version: 0.0.1

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
;; * Words
;;   -  "separator" means system-name or symbol which is defined in
;;      `separate-system-alist'.
;; * Variables
;; ** `separate-system-alist'
;;    an associated list. Each element is cons cell,
;;    "(system-name . number-or-symbol)". in this package, you can use
;;    "number-or-symbol" as "separator" instead of "system-name".
;; * Macros
;; ** "`separate-set' (variable alist)
;;   - Set value of "VARIABLE" depend on "SEPARATOR".
;;   - Each element of "ALIST" is "(SEPARATOR . VALUE)", and "VARIABLE" is
;;     set to "VALUE" if "SEPARATOR" accords current system.
;;   - If there are some cons cells whose car accords current system,
;;     "number-or-symbol" defined on upper stream in "separate-system-alist"
;;     is used. System-name is the lowest priority.
;;   - in the cons cell whose "SEPARATOR" is "default", its "VALUE" is used only
;;     when any other "SEPARATOR" doesn't accord current system.
;; ** `separate-setq' (variable alist)
;;    Same as separate-set, but "VARIABLE" doesn't have to be quoted.


;;; Code:

(eval-when-compile
  (require 'cl-lib))

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
  ""
  (separate--mapc-or
   (lambda (a) nil nil
     (string= a (system-name)))
   args))

(defun separate--emacs-version>= (args)
  ""
  (let ((major (car args))
        (minor (cadr args)))
    (or (> emacs-major-version major)
        (and
         (= emacs-major-version major)
         (or (not minor) (>= emacs-minor-version minor)))))
  )

(defun separate--separators (args)
  ""
  (separate--mapc-or 'separate--current-separator-p args))

(defun separate--and (args)
  ""
  (separate--mapc-and 'separate--current-separator-p args))

(defun separate--os (args)
  ""
  (separate--mapc-or
   (lambda (a) nil nil
     (eq a system-type))
   args)
  )

(defun separate--eval (args)
  ""
  (eval (cons 'progn args))
  )

(defun separate--package-available (args)
  ""
  (separate--mapc-and 'featurep args)
  )



(defun separate--function-assq (arg alist)
  "Return VALUE if (apply FUNC ARG) returns t.

\(fn ARG '((FUNC . VALUE)...))"
  (cl-loop
   for (x . y) in alist
   if (funcall x arg)
   return y
   end
   finally
   return nil))

(defun separate--mapc-or (func list)
  "If (funcall FUNC ELEMENT) return t on one or more ELEMENT of LIST, return t."
  (cl-loop
   for x in list
   if (funcall func x)
   return t
   end
   finally
   return nil)
  )

(defun separate--mapc-and (func list)
  "If (funcall FUNC ELEMENT) return t on all ELEMENT of LIST, return t."
  (cl-loop
   for x in list
   unless (funcall func x)
   return nil
   end
   finally
   return t)
  )

(defun separate--assq (key alist)
  "Same as assq, but if car of element of ALIST is list,
  compare key to element of that, too."
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
  ""
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
  ""
  (or (separate--os (list symbol-separator))
      (separate--current-separator-p
       (separate--symbol-separator-instance symbol-separator)))
  )

(defun separate--current-separator-p (separator)
  "Return non-nil if SEPARATOR is representing current system."
  (cond
   ((null separator) nil)
   ((symbolp separator)
    (separate--symbol-separator-current-p separator))
   (t
    (setq separator (separate--separator-normalize separator))
    (funcall (separate--assq (car separator) separate--function-alist)
             (cdr separator)))))



;;;###autoload
(defmacro separate-set (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SEPARATOR . VALUE), and VARIABLE is set to VALUE
if (separate-current-separator-p SEPARATOR) return non-nil.

\(fn VARIABLE ((SEPARATOR . VALUE)...))"
  (let ((valid-cons (gensym "valid-cons"))
        (separator (gensym "separator"))
        (value (gensym "value"))
        (default (gensym "default"))
        )
    ;; throw error if ALIST is NOT both alist and symbol.
    (ignore (cl-loop for (x . y) in alist))
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
(defmacro separate-setq (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SEPARATOR . VALUE), and VARIABLE is set to VALUE
if (separate-current-separator-p SEPARATOR) return non-nil.
variable have to be non-quoted.

\(fn VARIABLE ((SEPARATOR . VALUE)...))"
  `(separate-set (quote ,variable) ,alist))

;;;###autoload
(defmacro separate-cond (&rest clauses)
  "Eval BODY if SEPARATOR accords current system.
Each clause looks like (SEPARATOR BODY...). BODY is evaluate
if (separate-current-separator-p SEPARATOR) return non-nil.

\(fn (SEPARATOR BODY...)...)"
  (let ((c (gensym)))
    `(let (,c)
       (separate-setq ,c ,clauses)
       (eval (cons 'progn ,c)))))

(provide 'separate)
;;; separate.el ends here
