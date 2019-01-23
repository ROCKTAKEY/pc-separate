;;; separate.el --- separate setting by pc system -*- lexical-binding: t -*-

;; Copyright (C) 2018  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: tools

;; Package-Requires: ((cl-lib "1.0") (emacs "26.4"))

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

(require 'cl-lib)

(defgroup separate nil
  "separate group."
  :group 'tools)

(defcustom separate-separator-alist '()
  "Relationship of \"number or symbol\", and system-name on separate.
You can add (system-name . number-or-symbol).
And you can use the number instead of system-name on separate."
  :group 'separate
  :type '(set (cons string (choice integer symbol)))
  )



(defun separate--cdr-assoc-all (key alist &optional func)
  ""
  (cl-loop for (k . v) in alist
           if (funcall (if func func 'equal) key k)
           collect v))

(defun separate--assoc-or-eval (key alist)
  "Same as assoc, but if car of element of ALIST is list, and eval it.
If the value is t, treat the element as matched."
  (cl-loop
   with candicate = nil
   with x
   for y in alist
   do
   (setq x (car y))
   (setq candicate
         (if (listp x)
             (eval x)
           (equal x key)))
   when candicate
   return y)
  )

(defun separate--separator-to-system-name (separator)
  "Return system-name which SEPARATOR is representing."
  (let
      ((sys-name (if (stringp separator)
                     separator
                   ;; if no system-name, return nil
                   (car (rassq separator separate-separator-alist)))))
    (when sys-name
      sys-name)))

(defun separate--system-name-to-separators (system-name)
  "Return list whose all element is representing SYSTEM-NAME."
  (let* ((p (separate--cdr-assoc-all system-name separate-separator-alist))
         (separator-list (if p
                             (append p (list system-name))
                           (list system-name))))
    separator-list))

(defun separate--current-separator-p (separator)
  "Return non-nil if SEPARATOR is representing current system."
  (string= (separate--separator-to-system-name separator) (system-name)))



;;;###autoload
(defmacro separate-set (variable alist)
  "Set value of VARIABLE each system.
each element of ALIST is (SEPARATOR . VALUE), and VARIABLE is set to VALUE
if (separate-current-separator-p SEPARATOR) return non-nil."
  (let ((valid-cons (gensym "valid-cons"))
        (separator (gensym "separator"))
        (value (gensym "value"))
        (alst alist))
    ;; throw error if ALIST is NOT both alist and symbol.
    (ignore (cl-loop for (x . y) in (eval alst))) 
    `(let (,valid-cons)
       (setq ,valid-cons
             (cl-loop
              for (,separator . ,value) in ,alst
              if (separate--current-separator-p ,separator)
              return (cons ,separator ,value)
              end
              finally return nil   ;if never eval "return form" return nil
              ))
       (when ,valid-cons
         (set ,variable (cdr ,valid-cons)))
       ,valid-cons)))

;;;###autoload
(defmacro separate-setq (variable alist)
  "Set value of VARIABLE each system.
each element of ALIST is (SEPARATOR . VALUE), and VARIABLE is set to VALUE
if (separate-current-SEPARATOR-p SEPARATOR) return non-nil.
variable have to be non-quoted."
  `(separate-set (quote ,variable) ,alist))

;;;###autoload
(defmacro separate-cond (clauses)
  "Eval BODY if SEPARATOR accords current system.
Each clause looks like (SEPARATOR BODY...). BODY is evaluate
if (separate-current-separator-p SEPARATOR) return non-nil."
  (let ((c (gensym)))
    `(let (,c)
       (separate-setq ,c ',clauses)
       (eval (cons 'progn ,c)))))

(provide 'separate)
;;; separate.el ends here
