(in-package #:3b-ovr-bindings)

(defmacro define-cffi-translators ((pointer-var value-var type class)
                                   &body body &key read write)
  (declare (ignore body))
  `(progn
     ,@(when write
         `((defmethod cffi::expand-to-foreign-dyn-indirect (,value-var
                                                            ,pointer-var
                                                            body
                                                            (type ,class))
             `(with-foreign-object (,,pointer-var ',',type)
                ,,write
                ,@body))
           #++
           (defmethod expand-to-foreign (,value-var (type ,class))
             `(let ((,',pointer-var (foreign-alloc ',',type)))
                ,(let ((,pointer-var ',pointer-var))
                   ,write)
                ,',pointer-var))
           (macrolet ((w (,value-var ,pointer-var)
                        ,write))
             (defmethod translate-into-foreign-memory (,value-var (type ,class)
                                                       ,pointer-var)
               (w ,value-var ,pointer-var))
             (defmethod translate-to-foreign (,value-var (type ,class))
               (let ((,pointer-var (foreign-alloc ',type)))
                 (w ,value-var ,pointer-var)
                 ,pointer-var)))))
     ,@(when read
         `((defmethod expand-from-foreign (,pointer-var (type ,class))
             (alexandria:once-only (,pointer-var) ,read))
           (macrolet ((r (,pointer-var)
                        ,read))
             (defmethod translate-from-foreign (,pointer-var (type ,class))
               (r ,pointer-var)))))))

