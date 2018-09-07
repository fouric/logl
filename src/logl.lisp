(defparameter +opengl-float-size+ 4)

;; ...why is this not being used???
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

(defun make-circle-points (count inner-r outer-r)
  (let ((array (make-array (list (* count 12)))))
    (dotimes (i count)
      (let ((angle (- (/ pi 2) (* i (/ (* 2 pi) count)))))
        (let ((inner-x (coerce (* inner-r (cos angle)) 'single-float))
              (inner-y (coerce (* inner-r (sin angle)) 'single-float))
              (outer-x (coerce (* outer-r (cos angle)) 'single-float))
              (outer-y (coerce (* outer-r (sin angle)) 'single-float)))
          (setf (aref array (+ 0 (* i 12))) outer-x)
          (setf (aref array (+ 1 (* i 12))) outer-y)
          (setf (aref array (+ 2 (* i 12))) 0.0)
          (setf (aref array (+ 3 (* i 12))) 0.8)
          (setf (aref array (+ 4 (* i 12))) 0.2)
          (setf (aref array (+ 5 (* i 12))) 0.8)

          (setf (aref array (+ 6 (* i 12))) inner-x)
          (setf (aref array (+ 7 (* i 12))) inner-y)
          (setf (aref array (+ 8 (* i 12))) 0.0)
          (setf (aref array (+ 9 (* i 12))) 0.8)
          (setf (aref array (+ 10 (* i 12))) 0.2)
          (setf (aref array (+ 11 (* i 12))) 0.8)
          ;;(list outer-x outer-y 0.0 inner-x inner-y 0.0)
          )))
    array))

(defun make-indices (count)
  (let ((array (make-array (list (* count 6)))))
    (dotimes (i count)
      (setf (aref array (+ 0 (* i 6))) (+ 0 (* 2 i)))
      (setf (aref array (+ 1 (* i 6))) (+ 1 (* 2 i)))
      (setf (aref array (+ 2 (* i 6))) (+ 2 (* 2 i)))
      (setf (aref array (+ 3 (* i 6))) (+ 2 (* 2 i)))
      (setf (aref array (+ 4 (* i 6))) (+ 1 (* 2 i)))
      (setf (aref array (+ 5 (* i 6))) (+ 3 (* 2 i))))
    array))

#++(defparameter *vertices* #(;; positions        colors
                              0.5   0.5  0.0   0.8 0.2 0.8 ;; top right
                              -0.5   0.5  0.0   0.8 0.2 0.8 ;; top left
                              -0.5  -0.5  0.0   0.8 0.2 0.8 ;; bottom left
                              0.5  -0.5  0.0   0.8 0.2 0.8 ;; bottom right
                              ))
(defparameter *vertices* (make-circle-points 200 0.45 0.6))
#++(defparameter *indices* #(0 1 2
                             2 1 3
                             2 3 4
                             4 3 5
                             4 5 6
                             6 5 7))
(defparameter *indices* (make-indices 200))

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
  (let ((frames 0)
        (start-time 0)
        (end-time 0))
    (sdl2:with-init (:everything)
      ;; make sure that we get 3.3 core profile
      (sdl2:gl-set-attrs :context-major-version 3
                         :context-minor-version 3
                         :context-profile-mask sdl2-ffi:+sdl-gl-context-profile-core+)
      (sdl2:with-window (window :w 600 :h 600 :flags '(:opengl :resizable))
        (sdl2:with-gl-context (context window)
          (sdl2:gl-make-current window context)
          (gl:viewport 0 0 600 600)
          (let* ((program (make-program (fouriclib:resource "src/vertex-shader" 'logl)
                                        (fouriclib:resource "src/fragment-shader" 'logl)))
                 (triangle-vbo (gl:gen-buffer))
                 (vao (gl:gen-vertex-array))
                 (ebo (gl:gen-buffer))
                 (null-array (gl:make-null-gl-array :unsigned-short))

                 (progress 0.0)
                 (increment 0.004))
            (setf start-time (sdl2:get-ticks))

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
                        (sdl2:push-event :quit))
                      (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-up)
                        (incf increment 0.001))
                      (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-down)
                        (decf increment 0.001)))
              (:windowevent (:event event)
                            (cond ((= event sdl2-ffi:+sdl-windowevent-resized+) nil)))
              (:idle ()
                     (gl:clear-color 0.1 0.1 0.1 1.0)
                     (gl:clear :color-buffer)

                     (incf frames)
                     (setf progress (mod (+ progress increment) 1.0))

                     ;;(format t "to draw: ~a~%" (1+ (round (* 200 progress))))

                     (with-program (program)
                       (with-vao (vao)
                         (gl:draw-elements :triangles null-array :count (* 6 (1+ (round (* 200 progress)))))))

                     (sdl2:gl-swap-window window))
              (:quit ()
                     (setf end-time (sdl2:get-ticks))
                     t))))))
    (format t "FPS: ~a~%" (/ frames (/ (- end-time start-time) 1000.0)))))
