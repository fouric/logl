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

(defun transfer-data-to-gpu (data &optional (buffer-type :array-buffer) (buffer-usage :static-draw))
  (check-type data array)
  ;; buffer doesn't actually have memory allocated to it until it's bound
  (let ((buffer-object (gl:gen-buffer))
        (gl-array (make-gl-array data :foo)))
    (gl:bind-buffer buffer-type buffer-object)
    ;; actually allocate memory for the given buffer object and then load vertex data into it
    (gl:buffer-data :array-buffer buffer-usage gl-array)
    (gl:bind-buffer buffer-type 0)
    (gl:free-gl-array gl-array)))

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

(defparameter *vertices* #(0.5 0.5 0.0
                           0.5 -0.5 0.0
                           -0.5 -0.5 0.0
                           -0.5 0.5 0.0))
(defparameter *indices* #(0 1 3
                          1 2 3))

(defun make-gl-array (lisp-array type)
  (let ((gl-array (gl:alloc-gl-array type (length lisp-array))))
    (dotimes (i (length lisp-array))
      (setf (gl:glaref gl-array i) (aref lisp-array i)))
    gl-array))

(defun buffer-data-from-lisp-array (target usage array array-element-type)
  (let ((gl-array (make-gl-array array array-element-type)))
    (gl:buffer-data target usage gl-array)
    (gl:free-gl-array gl-array)))

(defun run ()
  (sdl2:with-init (:everything)
    (sdl2:gl-set-attrs :context-major-version 3
                       :context-minor-version 3
                       :context-core-profile sdl2-ffi:+sdl-gl-context-profile-core+)
    (sdl2:with-window (window :w 800 :h 600 :flags '(:opengl :resizable))
      (sdl2:with-gl-context (context window)
        (format t "opengl version: ~s~%" (gl:get-string :version))
        (sdl2:gl-make-current window context)
        (cl-opengl-bindings:viewport 0 0 800 600)
        (let ((program (make-program (resource "src/vertex-shader" 'logl)
                                     (resource "src/fragment-shader" 'logl)))
              (triangle-vbo (gl:gen-buffer))
              (vao (gl:gen-vertex-array))
              (ebo (gl:gen-buffer)))

          (gl:use-program program)

          (with-vao (vao)
            (gl:bind-buffer :array-buffer triangle-vbo)
            (buffer-data-from-lisp-array :array-buffer :static-draw *vertices* :float)
            (gl:enable-vertex-attrib-array 0)
            (gl:bind-buffer :element-array-buffer ebo)
            (buffer-data-from-lisp-array :element-array-buffer :static-draw *indices* :unsigned-int)
            (gl:vertex-attrib-pointer 0 3 :float nil (* 4 3) 0))

          (sdl2:with-event-loop (:method :poll)
            (:keyup (:keysym keysym)
                    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                      (sdl2:push-event :quit)))
            (:windowevent (:event event)
                          (cond ((= event sdl2-ffi:+sdl-windowevent-resized+) nil)))
            (:idle ()
                   (gl:clear-color 0.2 0.3 0.3 1.0)
                   (gl:clear :color-buffer)

                   (with-program (program)
                     (with-vao (vao)
                       (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 6)))

                   (sdl2:gl-swap-window window))
            (:quit ()
                   t)))))))
