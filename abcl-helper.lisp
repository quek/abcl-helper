(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cl-ppcre))

(defmacro jimport (fqcn &optional (package *package*))
  (let ((fqcn (string fqcn))
        (package package))
    (ppcre:register-groups-bind (class-name)
        (".*\\.(.*)" fqcn)
      (let ((class (jclass fqcn)))
        `(progn
           (defparameter ,(intern fqcn package) ,class)
           (defparameter ,(intern class-name package) ,class)
           ,@(map 'list
                  (lambda (method)
                    (let ((symbol (intern (jmethod-name method) package))
                          (fn (if (jmember-static-p method)
                                  #'jstatic
                                  #'jcall)))
                      `(progn
                         (defun ,symbol (&rest args)
                           (apply ,fn ,(symbol-name symbol) args))
                         (defparameter ,symbol #',symbol))))
                  (jclass-methods class)))))))

(defun new (class &rest args)
  (apply #'jnew
         (apply #'jconstructor class (mapcar #'jclass-of args))
         args))


(defmacro jcc (receiver message &rest rest)
  (loop for i in rest
        with args = nil
        if (and (symbolp i)
                (typep (symbol-value i) 'function))
          do (setf receiver `(,message ,receiver ,@(nreverse args))
                   message i
                   args nil)
        else
          do (push i args)
        finally (return `(,message ,receiver ,@(nreverse args)))))

(defmacro jcs (receiver message &rest rest)
  (let ((yourself (gensym "yourself")))
    `(let ((,yourself ,receiver))
       ,@(let (result)
           (loop for i in rest
                 with args = nil
                 if (and (symbolp i)
                         (typep (symbol-value i) 'function))
                   do (progn
                        #1=(push `(,message ,yourself ,@(nreverse args))
                                 result)
                        (setf message i
                              args nil))
                 else
                   do (push i args)
                 finally (if (eq i :yourself)
                             (progn
                               (push `(,message ,yourself
                                                ,@(nreverse (cdr args)))
                                     result)
                               (push yourself result))
                             #1#))
           (nreverse result)))))
#|
(jimport |java.lang.String|)
(|toUpperCase| "Hello")
(|replaceAll| "Hello" "l" "*ま*")
(jcc "Hello" |toUpperCase| |replaceAll| "L" "*L*")

(jimport |java.lang.Integer|)
(|parseInt| |Integer| "123")

(jimport |java.util.ArrayList|)
(jcs (new |ArrayList|) |add| "a" |add| "b" |toString|)
(|toString| (jcs (new |ArrayList|) |add| "a" |add| "b" |add| "c" :yourself))



(jimport |javax.swing.JOptionPane|)
(|showMessageDialog| |JOptionPane| nil "Hello World! まみむめも♪")
↓は Clojure
(. javax.swing.JOptionPane (showMessageDialog nil "Hello World"))
|#
