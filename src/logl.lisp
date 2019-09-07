(in-package :logl)

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
    ;; make sure that we get 4.5 core profile
    (sdl2:gl-set-attrs :context-major-version 4
                       :context-minor-version 5
                       :context-profile-mask sdl2-ffi:+sdl-gl-context-profile-core+
                       ;; double-buffering on by default on my machine but it doesn't hurt to explicitly turn it on
                       :doublebuffer 1)
    (sdl2:with-window (window :w 800 :h 600 :flags '(:opengl :resizable))
      (sdl2:with-gl-context (context window)
        (sdl2:gl-make-current window context)
        (gl:viewport 0 0 800 600)
        (let* ((program (make-program (fouric:resource "src/vertex-shader" 'logl)
                                      (fouric:resource "src/fragment-shader" 'logl)))
               (triangle-vbo (gl:gen-buffer))
               (vao (gl:gen-vertex-array))
               (ebo (gl:gen-buffer))
               (null-array (gl:make-null-gl-array :unsigned-short))
               (position-index (gl:get-attrib-location program "position"))
               (color-index (gl:get-attrib-location program "color")))

          ;; expands into (gl:use-program program) ... (gl:use-program 0)
          (with-program (program)

            ;; (gl:bind-vertex-array vao)
            (with-vao (vao)
              ;; a buffer object is a handle to a region of memory on the GPU where we can store stuff
              ;; a vertex buffer object is a particular *type* of buffer object used for storing vertices. we name it with :array-buffer
              ;; we can bind multiple buffer objects as long as they refer to different kinds of data and we bind to different targets
              (gl:bind-buffer :array-buffer triangle-vbo)
              ;; this is where we actually load the data into the buffer - calls gl:buffer-data with the :array-buffer target
              (buffer-data-from-lisp-array :array-buffer :static-draw *vertices* :float)
              ;; what's an element array buffer?
              (gl:bind-buffer :element-array-buffer ebo)
              (buffer-data-from-lisp-array :element-array-buffer :static-draw *indices* :unsigned-short)
              ;; vertex position components; attribute index 0, 3 floats at a time, interleaved with 3 color floats
              ;; vertex-attrib-pointer tells opengl how to interpret the data in vertex attribute array 0 from now until the next time we update said interpretation information
              ;; that is, it updates the opengl state, such that when we call gl:draw-elements, this information is used to interpret the data in the array-buffer target?
              ;; the first argument is "index" - the index of the vertex attribute that this data will be fed into. practically, this means that whatever is assigned to location 0 in our vertex shader will get this data
              ;; this actually forms a persistent connection between the vertex buffer object storing our triangle data and vertex attribute 0, because we called it when our triangle-vbo was bound to :array-buffer
              ;; now, even if we rebind the array-buffer _target_, the reference is kept between vertex attribute 0 and our VBO
              (gl:vertex-attrib-pointer position-index 3 :float nil (* +opengl-float-size+ 6) 0)
              (gl:enable-vertex-attrib-array 0)
              ;; vertex position components; attribute index 1, 3 floats at a time, starting after 3 vertex floats
              (gl:vertex-attrib-pointer color-index 3 :float nil (* +opengl-float-size+ 6) (* +opengl-float-size+ 3))
              (gl:enable-vertex-attrib-array 1))

            (sdl2:with-event-loop (:method :poll)
              (:keyup (:keysym keysym)
                      (when (or (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                                (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-q))
                        (sdl2:push-event :quit)))
              (:windowevent (:event event)
                            (cond ((= event sdl2-ffi:+sdl-windowevent-resized+) nil)))
              (:idle ()
                     (gl:clear-color 0.2 0.3 0.3 1.0)
                     (gl:clear :color-buffer)

                     (with-vao (vao)
                       (gl:draw-elements :triangles null-array :count 3))

                     (sdl2:gl-swap-window window))
              (:quit ()
                     t)))))))
  )
