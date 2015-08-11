;; minimal example using glop
(defpackage #:3bovr-test
  (:use #:cl))
(in-package #:3bovr-test)

(defclass 3bovr-test (glop:window)
  ((hmd :reader hmd :initarg :hmd)
   (fbo :reader fbo :initarg :fbo)))



(defmethod glop:on-event ((window 3bovr-test) (event glop:key-event))
  ;; exit on ESC key
  (when (eq (glop:keysym event) :escape)
    (glop:push-close-event window)))

(defmethod glop:on-event ((window 3bovr-test) event)
  ;; ignore any other events
  (declare (ignore window event)))

(defun cube (x y z r)
  (let* ((x (coerce x 'single-float))
         (y (coerce y 'single-float))
         (z (coerce z 'single-float))
         (r (coerce r 'single-float))
         (a (sb-cga:vec (- r) (- r) (- r)))
         (b (sb-cga:vec (- r) (+ r) (- r)))
         (c (sb-cga:vec (+ r) (+ r) (- r)))
         (d (sb-cga:vec (+ r) (- r) (- r)))
         (fpi (coerce pi 'single-float)))
    (loop for m in (list (sb-cga:rotate* 0.0 0.0 0.0)
                         (sb-cga:rotate* 0.0 (* fpi 1/2) 0.0)
                         (sb-cga:rotate* 0.0 (* fpi 2/2) 0.0)
                         (sb-cga:rotate* 0.0 (* fpi 3/2) 0.0)
                         (sb-cga:rotate* (* fpi 1/2) 0.0 0.0)
                         (sb-cga:rotate* (* fpi 3/2) 0.0 0.0))
          do (let ((n (sb-cga:transform-point (sb-cga:vec 0.0 0.0 1.0) m)))
               (gl:normal (aref n 0) (aref n 1) (aref n 2)))
             (flet ((v (v)
                                 (let ((v (sb-cga:transform-point v m)))
                                   (gl:vertex (+ x (aref v 0))
                                              (+ y (aref v 1))
                                              (+ z (aref v 2))))))
                  (v a)
                  (v b)
                  (v c)
                  (v a)
                  (v c)
                  (v d)))))

(defun draw-world ()
  (gl:clear :color-buffer :depth-buffer)
  (gl:disable :texture-2d)
  (gl:enable :framebuffer-srgb
             :line-smooth :blend :point-smooth :depth-test
             :lighting :light0 :color-material)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)
  (gl:light :light0 :position '(100.0 100.0 10.0 0.0))
  ;; draw a checkerboard ground
  (gl:with-primitives :quads
    (loop for i from -8 below 8
          do (loop for j from -8 below 8
                   for p = (oddp (+ i j))
                   do (if p
                          (gl:color 0.0 0.9 0.9 1.0)
                          (gl:color 0.1 0.1 0.1 1.0))
                      (gl:vertex i -0.66 j)
                      (gl:vertex (1+ i) -0.66 j)
                      (gl:vertex (1+ i) -0.66 (1+ j))
                      (gl:vertex i -0.66 (1+ j)))))

  ;; and some random cubes
  (let ((*random-state* (make-random-state *random-state*))
        (r 20.0))
    (gl:point-size 8)
    (gl:with-primitives :triangles
     (flet ((r () (- (random r) (/ r 2))))
       (loop for i below 1000
             do (gl:color (random 1.0) (random 1.0) (random 1.0) 1.0)
                (cube (+ 0.0 (r)) (- (r)) (+ 1.5 (r)) (+ 0.05 (random 0.10))))))))


(defun draw-frame (hmd &key eye-render-desc fbo eye-textures)
  (assert (and eye-render-desc fbo eye-textures))
  (let* ((timing (%ovrhmd::begin-frame hmd
                                       ;; don't need to pass index
                                       ;; unless we use
                                       ;; get-frame-timing
                                       0))
         (props (%ovr::dump-hmd-to-plist hmd))
         ;; get current hmd position/orientation
         ;;(state (%ovrhmd::get-tracking-state hmd))
         ;;(pose (getf state :head-pose))
         ;;(pos (getf (getf pose :the-pose) :position))
         ;;(or (getf (getf pose :the-pose) :orientation))
         ;;(lac (getf pose :linear-acceleration))
         ;;(lv (getf pose :linear-velocity))
         ;;(cam (getf state :camera-pose))
         ;;(cam-pos (getf cam :position))
         ;; set camera orientation from rift
         #++(camera ))
    (declare (ignorable timing))
    ;; get position of eyes
    (multiple-value-bind (head-pose tracking-state)
        (%ovr::get-eye-poses hmd (mapcar (lambda (a)
                                           (getf a :hmd-to-eye-view-offset))
                                         eye-render-desc))

      (let ((status (getf tracking-state :status-flags)))
        ;; change clear color depending on tracking state
        ;; red = no tracking
        ;; blue = orientation only
        ;; green = good
;        (print status)
        (cond
          ((and (member :orientation-tracked status)
                (member :position-tracked status))
           (gl:clear-color 0.1 0.5 0.2 1))
          ((and (member :orientation-tracked status))
           (gl:clear-color 0.1 0.2 0.5 1))
          (t
           (gl:clear-color 0.5 0.1 0.1 1))))
      ;; draw view from each eye
      (gl:bind-framebuffer :framebuffer fbo)
      (loop
        for index below 2
        ;; sdk specifies preferred drawing order, so it can predict
        ;; timing better in case one eye will be displayed before
        ;; other
        for eye = index ;(elt (getf props :eye-render-order) index)
        ;; get position/orientation for specified eye
        for pose = (elt head-pose eye)
        for orientation = (getf pose :orientation)
        for position = (getf pose :position)
        ;; get projection matrix from sdk
        for projection = (%ovr::matrix4f-projection
                          (getf (elt eye-render-desc eye)
                                :fov)
                          0.1 1000.0 ;; near/far
                          ;; request GL style matrix
                          '(:right-handed
 :clip-range-open-gl))
        ;; draw scene to fbo for 1 eye
        do (flet ((viewport (x)
                    ;; set viewport and scissor from texture config we
                    ;; will pass to sdk so rendering matches
                    (destructuring-bind (&key pos size) x
                      (gl:viewport (elt pos 0) (elt pos 1)
                                   (getf size :w) (getf size :h))
                      (gl:scissor (elt pos 0) (elt pos 1)
                                  (getf size :w)
                                  (getf size :h)))))
             (viewport (getf (elt eye-textures index) :render-viewport)))
           (gl:enable :scissor-test)
           ;; configure matrices
           (gl:with-pushed-matrix* (:projection)
             (gl:load-transpose-matrix projection)
             (gl:with-pushed-matrix* (:modelview)
               (gl:load-identity)
               (gl:mult-transpose-matrix
                (kit.math::quat-rotate-matrix
                 ;; kit.math quaternions are w,x,y,z but libovr quats
                 ;; are x,y,z,w
                 (kit.math::quaternion (aref orientation 3)
                                       (aref orientation 0)
                                       (aref orientation 1)
                                       (aref orientation 2) )))
               (gl:translate (- (aref position 0))
                             (- (aref position 1))
                             (- (aref position 2)))
               (draw-world))))
      (gl:bind-framebuffer :framebuffer 0)
      ;; pass textures to SDK for distortion, display and vsync
      (%ovr::end-frame hmd head-pose eye-textures))))


(defun test-3bovr ()
  ;; initialize library
  (%ovr::with-ovr ok (:debug t :timeout-ms 500)
    (unless ok
      (format t "couldn't initialize libovr~%")
      (return-from test-3bovr nil))
    ;; print out some info
    (format t "version: ~s~%" (%ovr::get-version-string))
    (format t "time = ~,3f~%" (%ovr::get-time-in-seconds))
    (format t "detect: ~s HMDs available~%" (%ovrhmd::detect))
    ;; try to open an HMD
    (%ovr::with-hmd (hmd)
      (unless hmd
        (format t "couldn't open hmd 0~%")
        (return-from test-3bovr nil))
      ;; print out info aobut the HMD
      (let ((props (%ovr::dump-hmd-to-plist hmd)) ;; decode the HMD struct
            w h x y
            eye-render-desc)
        (format t "got hmd ~{~s ~s~^~%        ~}~%" props)
        ;; turn on the tracking
        (%ovrhmd::configure-tracking hmd
                                     ;; desired tracking capabilities
                                     '(:orientation :mag-yaw-correction
                                       :position)
                                     ;; required tracking capabilities
                                     nil)
        ;; figure out where to put the window
        (setf w (getf (getf props :resolution) :w))
        (setf h (getf (getf props :resolution) :h))
        (setf x (aref (getf props :window-pos) 0))
        (setf y (aref (getf props :window-pos) 1))
        ;; create window
        (format t "opening ~sx~s window at ~s,~s~%" w h x y)
        (glop:with-window (win
                           "3bovr test window"
                           w h
                           :x x :y y
                           :win-class '3bovr-test
                           :fullscreen t
                           :depth-size 16)
          ;; configure rendering and save eye render params
          ;; todo: linux/mac versions
          (setf eye-render-desc
                (%ovr::configure-rendering hmd
                                           (glop::win32-window-id win)
                                           (glop::win32-window-dc win)))
          ;; attach libovr runtime to window
          (%ovrhmd::attach-to-window hmd
                                     (glop::win32-window-id win)
                                     (cffi:null-pointer) (cffi:null-pointer))
          ;; configure FBO for offscreen rendering of the eye views
          (let* ((fbo (gl:gen-framebuffer))
                 (texture (gl:gen-texture))
                 (renderbuffer (gl:gen-renderbuffer))
                 ;; get recommended sizes of eye textures
                 (ls (%ovrhmd::get-fov-texture-size hmd %ovr::+eye-left+
                                                    ;; use default fov
                                                    (elt (getf props
                                                               :default-eye-fov)
                                                         %ovr::+eye-left+)
                                                    ;; and no scaling
                                                    1.0))
                 (rs (%ovrhmd::get-fov-texture-size hmd %ovr::+eye-right+
                                                    (elt (getf props
                                                               :default-eye-fov)
                                                         %ovr::+eye-right+)
                                                    1.0))
                 ;; storing both eyes in 1 texture, so figure out combined size
                 (fbo-w (+ (getf ls :w) (getf rs :w)))
                 (fbo-h (max (getf ls :h) (getf rs :h)))
                 ;; describe the texture configuration for libovr
                 (eye-textures
                   (loop for v in (list (list :pos #(0 0)
                                              :size ls)
                                        (list :pos (vector (getf ls :w) 0)
                                              :size rs))
                         collect
                         `(:texture ,texture
                           :render-viewport ,v
                           :texture-size (:w ,fbo-w :h ,fbo-h)
                           :api :opengl))))
            ;; configure the fbo/texture
            (format t "left eye tex size = ~s, right = ~s~% total =~sx~a~%"
                    ls rs fbo-w fbo-h)
            (gl:bind-texture :texture-2d texture)
            (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
            (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
            (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
            (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
            (gl:tex-image-2d :texture-2d 0 :srgb8-alpha8 fbo-w fbo-h
                             0 :rgba :unsigned-int (cffi:null-pointer))
            (gl:bind-framebuffer :framebuffer fbo)
            (gl:framebuffer-texture-2d :framebuffer :color-attachment0
                                       :texture-2d texture 0)
            (gl:bind-renderbuffer :renderbuffer renderbuffer)
            (gl:renderbuffer-storage :renderbuffer :depth-component24
                                     fbo-w fbo-h)
            (gl:framebuffer-renderbuffer :framebuffer :depth-attachment
                                         :renderbuffer renderbuffer)
            (format t "created renderbuffer status = ~s~%"
                    (gl:check-framebuffer-status :framebuffer))
            (gl:bind-framebuffer :framebuffer 0)

            ;; main loop
            (loop while (glop:dispatch-events win :blocking nil :on-foo nil)
                  do (draw-frame hmd :eye-render-desc eye-render-desc
                                     :fbo fbo :eye-textures eye-textures))
            ;; clean up
            (gl:delete-framebuffers (list fbo))
            (gl:delete-textures (list texture))
            (gl:delete-renderbuffers (list renderbuffer))))))))



#++
(test-3bovr)
