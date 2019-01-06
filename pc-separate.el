;;; pc-separate.el --- separate setting by pc system

;; Copyright (C) 2018  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: local

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
;;   "pc-separate" provide function that help you to separate setting
;;   by system name given by "(system-name)". 
;; * Words
;;   -  "system" means system-name or symbol which is defined in `pc-separate-system-alist'.
;; * Variables
;; ** `pc-separate-system-alist'
;;    an associated list. Each element is cons cell,
;;    "(system-name . number-or-symbol)". in this package, you can use
;;    "number-or-symbol" as "system" instead of "system-name".
;; * Macros
;; ** "`pc-separate-set' (variable alist)
;;   - Set value of "VARIABLE" depend on "system".
;;   - Each element of "ALIST" is "(SYSTEM . VALUE)", and "VARIABLE" is set to "VALUE"
;;     if "SYSTEM" accords current system.
;;   - If there are some cons cells whose car accords current system, "number-or-symbol"
;;     defined on upper stream in "pc-separate-system-alist" is used. System-name is the 
;;     lowest priority.
;;   - in the cons cell whose "SYSTEM" is "default", its "VALUE" is used only when any
;;     other "SYSTEM" doesn't accord current system.
;; ** `pc-separate-setq' (variable alist)
;;    Same as pc-separate-set, but "VARIABLE" doesn't have to be quoted.


;;; Code:

(require 'cl-lib)

(defgroup pc-separate nil
  "pc-separate group.")

(defcustom pc-separate-system-alist '()
  "Relationship of \"number or symbol\", and system-name on pc-separate.
You can add (system-name . number-or-symbol).
And you can use the number instead of system-name on pc-separate."
  :group 'pc-separate
  :type '(set (cons string (choice integer symbol)))
  )



(defun pc-separate--cdr-assoc-all (key alist &optional func)
  ""
  (cl-loop for (k . v) in alist
           if (funcall (if func func 'equal) key k)
           collect v))

(defun pc-separate--assoc-or-eval (key alist)
  "Same as assq, but if car of element of ALIST is list, eval it, and if t, treat the element as matched."
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

(defun pc-separate--system-to-system-name (system)
  "Return system-name which SYSTEM is representing."
  (let
      ((sys-name (if (stringp system)
                     system
                   (car (rassq system pc-separate-system-alist))))) ;if no system-name, return nil
    (when sys-name
      sys-name)))

(defun pc-separate--system-name-to-system (sys-name)
  "Return list whose all element is representing SYS-NAME."
  (let* ((p (pc-separate--cdr-assoc-all sys-name pc-separate-system-alist))
         (system-list (if p
                          (append p (list sys-name))
                        (list sys-name))))
    system-list))

(defun pc-separate--current-system-p (system)
  "Return non-nil if SYSTEM is representing current system."
  (string= (pc-separate--system-to-system-name system) (system-name)))



;;;###autoload
(defmacro pc-separate-set (variable alist)
  "Set value of VARIABLE each system.
each element of ALIST is (SYSTEM . VALUE), and VARIABLE is set to VALUE
if (pc-separate-current-system-p SYSTEM) return non-nil."
  (let* ((--pc-separate--setq-candicate-- (gensym "condicate"))
         (result (gensym "result"))
         (system-list (gensym "system-list"))
         (s (gensym "s"))
         (alst alist))
    (ignore (cl-loop for (x . y) in (eval alst))) ;throw error if ALIST is NOT both alist and symbol.    
    `(let (,--pc-separate--setq-candicate--)
       (setq ,--pc-separate--setq-candicate--
             (cl-loop
              with ,result = nil
              with ,system-list = (pc-separate--system-name-to-system (system-name))
              for ,s in (append ,system-list (list 'default))
              do (setq ,result (assoc ,s ,alst)) ;assq throw error if ALIST is NOT list.
              if ,result
              return ,result
              end
              finally return nil   ;if never eval "return form" return nil
              ))
       (when ,--pc-separate--setq-candicate-- (set ,variable (cdr ,--pc-separate--setq-candicate--)))
       ,--pc-separate--setq-candicate--)))

;;;###autoload
(defmacro pc-separate-setq (variable alist)
  "Set value of VARIABLE each system.
each element of ALIST is (SYSTEM . VALUE), and VARIABLE is set to VALUE
if (pc-separate-current-system-p SYSTEM) return non-nil.
variable have to be non-quoted."
  `(pc-separate-set (quote ,variable) ,alist))

;;;###autoload
(defmacro pc-separate-cond (clauses)
  "Eval BODY if SYSTEM accords current system.
Each clause looks like (SYSTEM BODY...).BODY is evaluate
if (pc-separate-current-system-p SYSTEM) return non-nil."
  (let ((c (gensym)))
    `(let (,c)
       (pc-separate-setq ,c ',clauses)
       (eval (cons 'progn ,c)))))

(provide 'pc-separate)
;;; pc-separate.el ends here
