(in-package #:logl)

(defmacro with-bind-buffer ((target buffer) &body body)
  `(progn
     (gl:bind-buffer ,target ,buffer)
     ,@body
     (gl:bind-buffer ,target 0)))

(defmacro with-vertex-attrib-array ((index) &body body)
  `(progn
     (gl:enable-vertex-attrib-array ,index)
     ,@body
     (gl:disable-vertex-attrib-array ,index)))

(defmacro with-program ((program) &body body)
  `(progn
     (gl:use-program ,program)
     ,@body
     (gl:use-program 0)))

(defmacro with-vao ((vao) &body body)
  `(progn
     (gl:bind-vertex-array ,vao)
     ,@body
     (gl:bind-vertex-array 0)))

(defun make-shader (type source)
  (assert (or (eq type :vertex-shader) (eq type :fragment-shader))
	  (type) "~S is not a valid shader type" type)
   (let ((shader-id (gl:create-shader type)))
    (gl:shader-source shader-id source)
    (gl:compile-shader shader-id)
    (unless (gl:get-shader shader-id :compile-status)
      (format t "shader info log for type ~s: ~s~%shader: ~s~%~%" type (gl:get-shader-info-log shader-id) source)
      (sb-ext:exit))
    shader-id))

(defun make-shader-from-file (type filename)
  (make-shader type (fouriclib:read-file filename)))

(defmacro with-shader ((name type filename) &body body)
  `(let ((,name (make-shader-from-file ,type ,filename)))
     ,@body
     (gl:delete-shader ,name)))

(defun make-program (vertex-shader-filename fragment-shader-filename)
  (let ((program (gl:create-program)))
    (let ((vertex-shader (make-shader-from-file :vertex-shader vertex-shader-filename))
          (fragment-shader (make-shader-from-file :fragment-shader fragment-shader-filename)))
      (gl:attach-shader program vertex-shader)
      (gl:attach-shader program fragment-shader)
      (gl:link-program program)
      (unless (gl:get-program program :link-status)
        (format t "program info log: ~s~%" (gl:get-program-info-log program))
        (sb-ext:exit))
      (gl:delete-shader vertex-shader)
      (gl:delete-shader fragment-shader))
    program))

(defparameter *vertex-data* #(0.25  0.25 -1.25 1.0
			      0.25 -0.25 -1.25 1.0
			      -0.25  0.25 -1.25 1.0

			      0.25 -0.25 -1.25 1.0
			      -0.25 -0.25 -1.25 1.0
			      -0.25  0.25 -1.25 1.0

			      0.25  0.25 -2.75 1.0
			      -0.25  0.25 -2.75 1.0
			      0.25 -0.25 -2.75 1.0

			      0.25 -0.25 -2.75 1.0
			      -0.25  0.25 -2.75 1.0
			      -0.25 -0.25 -2.75 1.0

			      -0.25  0.25 -1.25 1.0
			      -0.25 -0.25 -1.25 1.0
			      -0.25 -0.25 -2.75 1.0

			      -0.25  0.25 -1.25 1.0
			      -0.25 -0.25 -2.75 1.0
			      -0.25  0.25 -2.75 1.0

			      0.25  0.25 -1.25 1.0
			      0.25 -0.25 -2.75 1.0
			      0.25 -0.25 -1.25 1.0

			      0.25  0.25 -1.25 1.0
			      0.25  0.25 -2.75 1.0
			      0.25 -0.25 -2.75 1.0

			      0.25  0.25 -2.75 1.0
			      0.25  0.25 -1.25 1.0
			      -0.25  0.25 -1.25 1.0

			      0.25  0.25 -2.75 1.0
			      -0.25  0.25 -1.25 1.0
			      -0.25  0.25 -2.75 1.0

			      0.25 -0.25 -2.75 1.0
			      -0.25 -0.25 -1.25 1.0
			      0.25 -0.25 -1.25 1.0

			      0.25 -0.25 -2.75 1.0
			      -0.25 -0.25 -2.75 1.0
			      -0.25 -0.25 -1.25 1.0




			      0.0 0.0 1.0 1.0
			      0.0 0.0 1.0 1.0
			      0.0 0.0 1.0 1.0

			      0.0 0.0 1.0 1.0
			      0.0 0.0 1.0 1.0
			      0.0 0.0 1.0 1.0

			      0.8 0.8 0.8 1.0
			      0.8 0.8 0.8 1.0
			      0.8 0.8 0.8 1.0

			      0.8 0.8 0.8 1.0
			      0.8 0.8 0.8 1.0
			      0.8 0.8 0.8 1.0

			      0.0 1.0 0.0 1.0
			      0.0 1.0 0.0 1.0
			      0.0 1.0 0.0 1.0

			      0.0 1.0 0.0 1.0
			      0.0 1.0 0.0 1.0
			      0.0 1.0 0.0 1.0

			      0.5 0.5 0.0 1.0
			      0.5 0.5 0.0 1.0
			      0.5 0.5 0.0 1.0

			      0.5 0.5 0.0 1.0
			      0.5 0.5 0.0 1.0
			      0.5 0.5 0.0 1.0

			      1.0 0.0 0.0 1.0
			      1.0 0.0 0.0 1.0
			      1.0 0.0 0.0 1.0

			      1.0 0.0 0.0 1.0
			      1.0 0.0 0.0 1.0
			      1.0 0.0 0.0 1.0

			      0.0 1.0 1.0 1.0
			      0.0 1.0 1.0 1.0
			      0.0 1.0 1.0 1.0

			      0.0 1.0 1.0 1.0
			      0.0 1.0 1.0 1.0
			      0.0 1.0 1.0 1.0))

(defun make-gl-array (lisp-array)
  (let ((gl-array (gl:alloc-gl-array :float (length lisp-array))))
    (dotimes (i (length lisp-array))
      (setf (gl:glaref gl-array i) (aref lisp-array i)))
    gl-array))

(defun initialize-vertex-buffer ()
  ;; buffer doesn't actually have memory allocated to it until it's bound
  (let* ((buffer (first (gl:gen-buffers 1)))
	 (array (make-gl-array *vertex-data*)))
    (with-bind-buffer (:array-buffer buffer)
      ;; actually allocate memory for the given buffer object and then load vertex data into it
      (gl:buffer-data :array-buffer :stream-draw array))
    (gl:free-gl-array array)
    buffer))

(defun run ()
  (fouriclib:with-init-window-gl ((:everything) (window :flags '(:opengl :resizable)) (context window))
    (sdl2:gl-make-current window context)
    (gl:viewport 0 0 800 600)

    (sdl2:with-event-loop (:method :poll)
      (:keyup (:keysym keysym)
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                (sdl2:push-event :quit)))
      (:windowevent (:event event)
                    (cond
                      ((= event sdl2-ffi:+sdl-windowevent-resized+)
                       nil)))
      (:idle ()
             (gl:clear-color 0.2 0.3 0.3 1.0)
             (gl:clear :color-buffer)

             (sdl2:gl-swap-window window))
      (:quit () t))))
