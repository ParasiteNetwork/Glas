(in-package :glas)

(defun key= (sdlkey symkey)
  (sdl2:scancode= (sdl2:scancode-value sdlkey) symkey))

(defmacro keycase (key &body body)
  `(cond
     ,@(let ((statements))
         (dolist (clause body statements)
           (setf statements 
                 (append statements
                         (if (eq (car clause) t)
                             `((t ,@(cdr clause)))
                             `(((key= ,key ,(car clause)) ,@(cdr clause) t)))))))))

(defun initialize-struct-slot (thestruct package prefix key value)
  (let ((postfix (symbol-name key)))
    (let ((accessor (intern (concatenate 'string prefix "-" postfix) package)))
      (let ((writer (fdefinition (list 'setf accessor))))
        (funcall writer value thestruct)))))

(defmacro initialize-struct (thestruct &rest args &key &allow-other-keys)
  (let ((name (gensym))(package (gensym))(target (gensym)))
    `(let* ((,target ,thestruct)
            (,name (symbol-name (type-of ,target)))
            (,package (symbol-package (type-of ,target))))
       (prog1
         ,target
         ,@(loop for (key value) on args by #'cddr
                 collect `(initialize-struct-slot ,target 
                                                  ,package 
                                                  ,name 
                                                  ,key 
                                                  ,value))))))
