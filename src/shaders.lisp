(in-package :logl)

(proclaim '(optimize (speed 0) (safety 0) (space 0) (debug 3)))

(defun make-shader (type filename)
  (assert (or (eq type :vertex-shader) (eq type :fragment-shader))
          (type) "~S is not a valid shader type" type)
  (let ((shader-id (gl:create-shader type))
        (source (fouriclib:read-file filename)))
    (gl:shader-source shader-id source)
    (gl:compile-shader shader-id)
    (unless (gl:get-shader shader-id :compile-status)
      (format t "shader info log for type ~s: ~s~%shader: ~s~%~%" type (gl:get-shader-info-log shader-id) source)
      (sb-ext:exit))
    shader-id))

(defun make-program (vertex-shader-filename fragment-shader-filename)
  "create a program object by sourcing vertex and fragment shaders from the given filenames"
  (let ((program (gl:create-program)))
    (let ((vertex-shader (make-shader :vertex-shader vertex-shader-filename))
          (fragment-shader (make-shader :fragment-shader fragment-shader-filename)))
      (gl:attach-shader program vertex-shader)
      (gl:attach-shader program fragment-shader)
      (gl:link-program program)
      (unless (gl:get-program program :link-status)
        (format t "program info log: ~s~%" (gl:get-program-info-log program))
        (sb-ext:exit))
      (gl:delete-shader vertex-shader)
      (gl:delete-shader fragment-shader))
    program))
