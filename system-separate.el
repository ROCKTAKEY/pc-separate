;;; system-separate.el --- separate setting by pc system -*- lexical-binding: t -*-

;; Copyright (C) 2018  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: tools

;; URL: https://github.com/ROCKTAKEY/pc-separate

;; Package-Requires: ((cl-lib "0.6.1") (emacs "24.3") (dash "2.15.0"))

;; Version: 0.0.4

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
;; ** =ss-set (variable alist)=
;;   - Set value of =VARIABLE= depend on =SEPARATOR= below.
;;   - Each element of =ALIST= is =(SEPARATOR . VALUE)=,
;;     and =VARIABLE= is set to =VALUE=
;;     if =SEPARATOR= is valid.
;;   - If there are some cons cells whose car (= =SEPARATOR=) is valid,
;;     upstream element is used, and rest of them is not evaluated.
;;   - in the cons cell whose =SEPARATOR= is =default=,
;;     its =VALUE= is used only when any other =SEPARATOR= isn't valid.
;;   - =(ss-set 'a ((b . c) ...))= is absolutely same as
;;     =(ss-setq a ((b . c) ...))=.
;; ** =ss-setq (variable alist)=
;;   - Same as =ss-set=, but =VARIABLE= doesn't have to be quoted.
;;   - See [[#HowToUse][How to Use Section]] as example.
;; ** =ss-set-no-eval (variable alist)=
;;   - Same as =ss-set-no-eval=, but =VALUE= are NOT evalueted.
;; ** =ss-setq-no-eval (variable alist)=
;;   - Same as =ss-setq-no-eval=, but =VALUE= are NOT evalueted.
;; ** =ss-cond (&body clauses)=
;;   - Similar to =cond=, but use =SEPARATOR= instead of =CANDICATE=.
;;     If =SEPARATOR= is valid, evaluate =BODY=.
;;   - Priority of each clause is same as =ss-set=.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'dash)

(defgroup system-separate nil
  "system-separate group."
  :group 'tools
  :prefix "ss-")

(defcustom ss-separator-alist '()
  "Relationship of \"number or symbol\", and system-name on separate.
You can add (system-name . number-or-symbol).
And you can use the number instead of system-name on separate."
  :group 'system-separate
  :type '(set (cons (choice integer symbol) string)))



(defvar ss--function-alist
  '(((:alias :separators :or) . ss--separators)
    (:and                 . ss--and)
    (:system-name         . ss--system-name)
    (:emacs-version>=     . ss--emacs-version>=)
    (:os                  . ss--os)
    (:eval                . ss--eval)
    (:package-available   . ss--package-available)))

(defun ss--system-name (args)
  "Return non-nil if ARGS contain value returned by function `system-name'."
  (-any?
   (lambda (a) nil nil
     (string= a (system-name)))
   args))

(defun ss--emacs-version>= (args)
  "Return non-nil if ARG is same as or higher than variable `emacs-version'.
ARGS can have 1 or 2 element(s).  First is MAJOR, second is MINOR (optional).
If MAJOR.MINOR is same and or higher than variable `emacs-version', return non-nil."
  (let ((major (car args))
        (minor (cadr args)))
    (or (> emacs-major-version major)
        (and
         (= emacs-major-version major)
         (or (not minor) (>= emacs-minor-version minor))))))

(defun ss--separators (args)
  "Return non-nil if one of ARGS elements is a valid separator."
  (-any? 'ss--current-separator-p args))

(defun ss--and (args)
  "Return non-nil if all of ARGS elements are valid separator."
  (-all? 'ss--current-separator-p args))

(defun ss--os (args)
  "Return non-nil if one of ARGS elements is same as `system-type'."
  (-any?
   (lambda (a) nil nil
     (eq a system-type))
   args))

(defun ss--eval (args)
  "Return result of evaluating ARGS."
  (eval (cons 'progn args)))

(defun ss--package-available (args)
  "Return non-nil if all of ARGS elements are return non-nil when passed to `featurep'."
  (-all? 'featurep args))



(defun ss--function-assq (arg alist)
  "Return VALUE if (apply FUNC ARG) return t.
Each element of ALIST is (FUNC . VALUE)."
  (cl-loop
   for (x . y) in alist
   if (funcall x arg)
   return y
   end
   finally
   return nil))

(defun ss--assq (key alist)
  "Same as assq, but if car of element of ALIST is list, compare KEY to element of that, too."
  (cl-loop
   for (x . y) in alist
   if (or (eq key x) (and (listp x) (memq key x)))
   return y
   end
   finally return nil))



(defvar ss--default-alist
  '((stringp . :system-name)
    (numberp . :emacs-version>=))
  "Use VALUE to car of separator, if (apply KEY separator) return non-nil.
`listp' and `symbolp' is invalid for key, because these separator are
trapped before applied this variable to.")

(defun ss--separators-p (object)
  "Return t if OBJECT is separator list."
  (when (listp object)
    (not
     (let ((c (car object)))
       (when (symbolp c)
         (string= ":" (substring (symbol-name c) 0 1)))))))

(defun ss--symbol-separator-instance (symbol-separator)
  "Return instance of SYMBOL-SEPARATOR in `ss-separator-alist'."
  (cdr (assq symbol-separator ss-separator-alist)))

(defun ss--separator-normalize (separator)
  "Normarize SEPARATOR.
Change SEPARATOR to be listed one whose car is :foobar."
  (let ((hidden-kind
         (ss--function-assq separator ss--default-alist)))
    (cond
     ;; trap separators-list
     ((ss--separators-p separator) (cons ':separators separator))
     ;; make listed-separaor
     (hidden-kind (list hidden-kind separator))
     ;; fall out symbol-separator
     (t separator))))

(defun ss--symbol-separator-current-p (symbol-separator)
  "Return non-nil if SYMBOL-SEPARATOR is valid separator."
  (or (ss--os (list symbol-separator))
      (ss--current-separator-p
       (ss--symbol-separator-instance symbol-separator))))

(defun ss--current-separator-p (separator)
  "Return non-nil if SEPARATOR is valid separator."
  (cond
   ((null separator) nil)
   ((symbolp separator)
    (ss--symbol-separator-current-p separator))
   (t
    (setq separator (ss--separator-normalize separator))
    (funcall (ss--assq (car separator) ss--function-alist)
             (cdr separator)))))


;;;###autoload
(defmacro ss-set-no-eval (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SEPARATOR . VALUE), and VARIABLE is set to VALUE
if (ss-current-separator-p SEPARATOR) return non-nil.
VALUE is NOT evaluated.

\(fn VARIABLE ((SEPARATOR . VALUE)...))"
  (let ((valid-cons (gensym "valid-cons"))
        (separator (gensym "separator"))
        (value (gensym "value"))
        (default (gensym "default")))
    ;; throw error if ALIST is NOT both alist and symbol.
    `(let (,valid-cons (,default nil))
       (setq ,valid-cons
             (cl-loop
              for (,separator . ,value) in ',alist
              if (eq ,separator 'default)
              do (setq ,default (cons ,separator ,value))
              else if (ss--current-separator-p ,separator)
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
(defmacro ss-setq-no-eval (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SEPARATOR . VALUE), and VARIABLE is set to VALUE
if (ss-current-separator-p SEPARATOR) return non-nil.
variable have to be non-quoted.
VALUE is NOT evaluated.

\(fn VARIABLE ((SEPARATOR . VALUE)...))"
  `(ss-set-no-eval (quote ,variable) ,alist))



;;;###autoload
(defmacro ss-set (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SEPARATOR . VALUE), and VARIABLE is set to VALUE
if (ss-current-separator-p SEPARATOR) return non-nil.
VALUE is evaluated.

\(fn VARIABLE ((SEPARATOR . VALUE)...))"
  (let ((valid-cons (gensym "valid-cons"))
        (separator (gensym "separator"))
        (value (gensym "value"))
        (default (gensym "default")))
    `(let (,valid-cons (,default nil))
       (setq ,valid-cons
             (cl-loop
              for (,separator . ,value) in ',alist
              if (eq ,separator 'default)
              do (setq ,default (cons ,separator ,value))
              else if (ss--current-separator-p ,separator)
              return (cons ,separator ,value)
              end
              ;; If never eval "return form", return default
              ;; which is nil if no "default" separator.
              finally return ,default))
       ;; If no valid-cons, return nil because of `when' macro.
       (when ,valid-cons
         (set ,variable (eval (cdr ,valid-cons))))
       ,valid-cons)))

;;;###autoload
(defmacro ss-setq (variable alist)
  "Set value of VARIABLE to VALUE each system.
each element of ALIST is (SEPARATOR . VALUE), and VARIABLE is set to VALUE
if (ss-current-separator-p SEPARATOR) return non-nil.
variable have to be non-quoted.
VALUE is evaluated.

\(fn VARIABLE ((SEPARATOR . VALUE)...))"
  `(ss-set (quote ,variable) ,alist))

;;;###autoload
(defmacro ss-cond (&rest clauses)
  "Eval BODY if SEPARATOR accords current system.
Each element of CLAUSES looks like (SEPARATOR BODY...).  BODY is evaluate
if (ss-current-separator-p SEPARATOR) return non-nil.

\(fn (SEPARATOR BODY...)...)"
  (let ((c (gensym)))
    `(let (,c)
       (ss-setq-no-eval ,c ,clauses)
       (eval (cons 'progn ,c)))))

(provide 'system-separate)
;;; system-separate.el ends here
