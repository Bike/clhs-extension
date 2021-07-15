#|
Here is how the example in the MOP page on MAKE-METHOD-LAMBDA (and in "make-method-lambda considered harmful") would work with this new system. This code defines a new kind of method which, in addition to CALL-NEXT-METHOD and NEXT-METHOD-P, can use a function THIS-METHOD to get the current method.
|#

(defclass my-method (standard-method)
  ((%function :initarg :function :reader my-method-function)))

;;; This function could probably be exported by MOP, since implementations of
;;; CLOS probably need it to implement DEFMETHOD anyway.
(defun specializer-creation-forms (specializer-exprs)
  (loop for spec in specializer-exprs
        if (symbolp spec)
          collect `(find-class ',spec)
        else if (and (consp spec) (eq (car spec) 'eql)
                     (consp (cdr spec)) (null (cddr spec)))
               collect `(intern-eql-specializer ,(second spec))
        else (error ...)))

(defun my-make-method-lambda (lambda-list body)
  `(lambda (continuation this-method &rest arguments)
     ;; The parameters should be gensyms; skipped for simplicity
     (flet ((this-method () this-method)
            (next-method-p () (not (null continuation)))
            (call-next-method (&rest cnm-args)
              (if continuation
                  (apply continuation (or cnm-args arguments))
                  (apply #'no-next-method
                         (method-generic-function this-method)
                         this-method (or cnm-args arguments)))))
       ;; BODY should be processed for declarations relating to the local
       ;; functions; skipped for simplicity
       (destructuring-bind ,(extract-lambda-list lambda-list) arguments
         ,@body))))

(defmacro defmymethod (name lambda-list &body body)
  ;; For simplicity, we do not allow qualifiers. Adding qualifiers would only
  ;; complicate this macro and not the rest, though. We don't include
  ;; documentation for the same reason.
  `(make-instance 'my-method
     :qualifiers ()
     :specializers `(list ,@(specializer-creation-forms
                             (extract-specializer-names lambda-list)))
     :lambda-list (extract-lambda-list lambda-list)
     :function ,(my-make-method-lambda (extract-lambda-list lambda-list) body)))

(defmethod expand-apply-method ((method my-method) marguments arguments env)
  (declare (ignore env))
  (destructuring-bind (&optional ((&rest next-methods)) &rest more-margs)
      marguments
    `(apply (load-time-value (my-method-function ,method))
            ,(if next-methods
                 `(lambda (&rest args)
                    (apply-method ,(first next-methods)
                                  (,(rest next-methods) ,@more-margs)
                                  args))
                 'nil)
            ,method ,@arguments)))

#|
The filtered reader example from "make-method-lambda considered harmful" is simpler:
|#

(defclass filter-method (standard-method)
  ((%filter :initarg :filter :reader filter-method-filter :type function)))

(defmethod expand-apply-method ((method filter-method) marguments arguments env)
  (declare (ignore env))
  (destructuring-bind (&optional ((&rest next-methods)) &rest more) marguments
    (if next-methods
        `(funcall ,(filter-method-filter method)
                  (apply-method ,(first next-methods)
                                (,(rest next-methods) ,@more)
                                ,@arguments))
        (error "filter needs a next method"))))

(defun add-reader-filter (gf filter)
  (add-method gf (make-instance 'filter-method
                   :qualifiers ()
                   :lambda-list '(object)
                   :specializers (list (find-class 't))
                   :filter filter)))

#|
Here is an example implementation of compute-effective-method-function, which exists internally in many CLOS implementations to compute the actual effective method function for a given set of applicable methods. For simplicity, the options returned by COMPUTE-EFFECTIVE-METHOD are ignored here.
|#

(defun compute-effective-method-function (generic-function method-combination
                                          applicable-methods)
  (let* ((effective-method
           (compute-effective-method generic-function method-combination
                                     applicable-methods))
         (argsym (gensym "ARGS"))
         (wrapped
           `(macrolet ((call-method (method &rest method-arguments)
                         ;; since argsym is just a symbol, it has no side
                         ;; effects, so we can skip the once-only stuff.
                         (list* 'apply-method method method-arguments argsym)))
              (progn ,effective-method)))
         (lambda-expression
           `(lambda (&rest ,argsym) ,wrapped)))
    (coerce lambda-expression 'function)))

#|
A more efficient implementation could use the fact that APPLY-METHOD takes forms for a spreadable argument list generator rather than just a list of arguments. Imagine we have functions GENERIC-FUNCTION-REQUIRED-COUNT and GENERIC-FUNCTION-NON-REQUIRED-P that return the number of required parameters in the generic function's lambda list and whether it has parameters other than required parameters, respectively. Then we could write
|#

(defun compute-effective-method-function (generic-function method-combination
                                          applicable-methods)
  (let* ((effective-method
           (compute-effective-method generic-function method-combination
                                     applicable-methods))
         (params (loop for r repeat (generic-function-required-count
                                     generic-function)
                       collect (gensym "REQUIRED-ARG")))
         (restsym (if (generic-function-non-required-p generic-function)
                      (gensym "REST")
                      nil))
         (wrapped
           `(macrolet ((call-method (method &rest method-arguments)
                         (append '(apply-method) (list method method-arguments)
                                 params (list restsym))))
              (progn ,effective-method)))
         (lambda-expression
           `(lambda (,@params ,@(when restsym '(&rest ,restsym))) ,wrapped)))
    (coerce lambda-expression 'function)))

#|
Now the effective method function does not need to cons a &rest list. A sophisticated compiler may be able to recognize (apply ... nil) forms expanded from APPLY-METHOD forms and reduce these to more efficient code.
|#
