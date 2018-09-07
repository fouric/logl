(in-package :logl)

(proclaim '(optimize (speed 0) (safety 0) (space 0) (debug 3)))

(defparameter +opengl-float-size+ 4)

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

(defparameter *vertices* #(;; positions        colors
                           0.5  -0.5  0.0   1.0 0.0 0.0 ;; bottom right
                           -0.5 -0.5  0.0   0.0 1.0 0.0 ;; bottom left
                           0.0   0.5  0.0   0.0 0.0 1.0 ;; top
                           ))
(defparameter *indices* #(0 1 2))

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
    ;; make sure that we get 3.3 core profile
    (sdl2:gl-set-attrs :context-major-version 3
                       :context-minor-version 3
                       :context-profile-mask sdl2-ffi:+sdl-gl-context-profile-core+)
    (sdl2:with-window (window :w 800 :h 600 :flags '(:opengl :resizable))
      (sdl2:with-gl-context (context window)
        (sdl2:gl-make-current window context)
        (gl:viewport 0 0 800 600)
        (let* ((program (make-program (fouriclib:resource "src/vertex-shader" 'logl)
                                      (fouriclib:resource "src/fragment-shader" 'logl)))
               (triangle-vbo (gl:gen-buffer))
               (vao (gl:gen-vertex-array))
               (ebo (gl:gen-buffer))
               (null-array (gl:make-null-gl-array :unsigned-short)))

          (gl:use-program program)

          (with-vao (vao)
            (gl:bind-buffer :array-buffer triangle-vbo)
            (buffer-data-from-lisp-array :array-buffer :static-draw *vertices* :float)
            (gl:bind-buffer :element-array-buffer ebo)
            (buffer-data-from-lisp-array :element-array-buffer :static-draw *indices* :unsigned-short)
            ;; vertex position components; attribute index 0, 3 floats at a time, interleaved with 3 color floats
            (gl:vertex-attrib-pointer 0 3 :float nil (* +opengl-float-size+ 6) 0)
            (gl:enable-vertex-attrib-array 0)
            ;; vertex position components; attribute index 1, 3 floats at a time, starting after 3 vertex floats
            (gl:vertex-attrib-pointer 1 3 :float nil (* +opengl-float-size+ 6) (* +opengl-float-size+ 3))
            (gl:enable-vertex-attrib-array 1))

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
                       (gl:draw-elements :triangles null-array :count 3)))

                   (sdl2:gl-swap-window window))
            (:quit ()
                   t)))))))
