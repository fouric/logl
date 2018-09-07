(in-package :logl)

(proclaim '(optimize (speed 0) (safety 0) (space 0) (debug 3)))

(defmacro with-bind-buffer ((target buffer) &body body)
  "bind BUFFER to TARGET around the given BODY"
  (alexandria:once-only (target)
    `(multiple-value-prog1
         (progn
           (gl:bind-buffer ,target ,buffer)
           ,@body)
       (gl:bind-buffer ,target 0))))

(defmacro with-vertex-attrib-array ((index) &body body)
  "enable vertex array with index INDEX around BODY, then disable"
  (alexandria:once-only (index)
    `(multiple-value-prog1
         (progn
           (gl:enable-vertex-attrib-array ,index)
           ,@body)
       (gl:disable-vertex-attrib-array ,index))))

(defmacro with-program ((program) &body body)
  "use PROGRAM around BODY"
  `(multiple-value-prog1
       (progn
         (gl:use-program ,program)
         ,@body)
     (gl:use-program 0)))

(defmacro with-vao ((vao) &body body)
  "bind vertex array object VAO around BODY, then unbind"
  `(multiple-value-prog1
       (progn
         (gl:bind-vertex-array ,vao)
         ,@body)
     (gl:bind-vertex-array 0)))

(defmacro with-shader ((name type filename) &body body)
  "create the specified shader around BODY, then destroy"
  `(let ((,name (make-shader-from-file ,type ,filename)))
     (multiple-value-prog1
         (progn
           ,@body)
       (gl:delete-shader ,name))))
