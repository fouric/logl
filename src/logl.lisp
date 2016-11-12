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

(defmacro with-shader ((name type filename) &body body)
  `(let ((,name (make-shader-from-file ,type ,filename)))
     ,@body
     (gl:delete-shader ,name)))

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

(defparameter *vertex-data* #(-0.5 -0.5 0.0
                              0.5 -0.5 0.0
                              0.0 0.5 0.0))

(defun make-gl-array (lisp-array)
  (let ((gl-array (gl:alloc-gl-array :float (length lisp-array))))
    (dotimes (i (length lisp-array))
      (setf (gl:glaref gl-array i) (aref lisp-array i)))
    gl-array))

(defun transfer-data-to-gpu (data &optional (buffer-type :array-buffer) (buffer-usage :static-draw))
  (check-type data array)
  ;; buffer doesn't actually have memory allocated to it until it's bound
  (let ((buffer-object (gl:gen-buffer))
        (gl-array (make-gl-array data)))
    (gl:bind-buffer buffer-type buffer-object)
    ;; actually allocate memory for the given buffer object and then load vertex data into it
    (gl:buffer-data buffer-type buffer-usage gl-array)
    (gl:bind-buffer buffer-type 0)
    (gl:free-gl-array gl-array)))

(defun run ()
  (sdl2:with-init (:everything)
    (sdl2:gl-set-attr :context-major-version 3)
    (sdl2:gl-set-attr :context-minor-version 3)
    (sdl2:with-window (window :flags '(:opengl :resizable))
      (sdl2:with-gl-context (context window)
        (format t "opengl version: ~s~%" (gl:get-string :version))
        (sdl2:gl-make-current window context)
        (cl-opengl-bindings:viewport 0 0 800 600)
        (let ((program (make-program (resource "src/vertex-shader" 'logl)
                                     (resource "src/fragment-shader" 'logl)))
              (triangle-vbo (transfer-data-to-gpu *vertex-data*)))

          (sdl2:with-event-loop (:method :poll)
            (:keyup (:keysym keysym)
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                      (sdl2:push-event :quit)))
            (:windowevent (:event event)
                          (cond ((= event sdl2-ffi:+sdl-windowevent-resized+) nil)))
            (:idle nil (cl-opengl-bindings:clear-color 0.2 0.3 0.3 1.0)
                   (cl-opengl:clear :color-buffer) (sdl2:gl-swap-window window))
            (:quit nil t)))))))
