(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cl-ppcre))

(defmacro jimport (fqcn)
  (let ((fqcn (string fqcn)))
    (ppcre:register-groups-bind (package-name class-name)
        ("(.*)\\.(.*)" fqcn)
      (print (list package-name class-name))
      (let ((class (jclass fqcn)))
        `(progn
           (defparameter ,(intern fqcn) ,class)
           (defparameter ,(intern class-name) ,class))))))

(defun new (class &rest args)
  (apply #'jnew
         (apply #'jconstructor class (mapcar #'jclass-of args))
         args))

(jimport |java.lang.Object|)
(jimport |java.lang.String|)
|Object|
|java.lang.Object|
(jnew (jconstructor |Object|))

(new |Object|)
(new |String|)
(new |String| "まみむめも♪")
