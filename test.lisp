;; minimal example using glop
(defpackage #:3bovr-test
  (:use #:cl))
(in-package #:3bovr-test)

(defclass 3bovr-test (glop:window)
  ((hmd :reader hmd :initarg :hmd)
   (world-vao :accessor world-vao)
   (count :accessor world-count)
   (hud-vbo :accessor hud-vbo :initform nil)
   (hud-vao :accessor hud-vao :initform nil)
   (hud-count :accessor hud-count)
   (hud-texture :accessor hud-texture)
   (font :accessor font)))

(defparameter *tex-size* 256)
(defparameter *mirror* nil)
(defparameter *hud* 0)
(defparameter *hud-stereo* 0)

(defmethod glop:on-event :around ((window 3bovr-test) event)
  (with-simple-restart (continue "continue")
    (call-next-method)))

(defmethod glop:on-event ((window 3bovr-test) (event glop:key-event))
  (when (glop:pressed event)
    (case (glop:keysym event)
      (:escape
       (glop:push-close-event window))
      (:space
       (format t "latency = ~{~,3,3f ~,3,3f ~,3,3f ~,3,3f ~,3,3f~}~%"
               (%ovr::get-float-array (hmd window) :dk2-latency 5)))
      (:m (setf *mirror* (not *mirror*)))
      (:h
       (incf *hud*)
       (%ovr::set-int (hmd window) :perf-hud-mode (mod *hud* 5)))
      (:j
       (incf *hud-stereo*)
       (%ovr::set-int (hmd window) :debug-hud-stereo-mode (mod *hud-stereo* 4))))))


(defun hud-text (win hmd)
  (declare (ignorable win))
  (format nil "fps: ~s~%~
latency = ~{m2p:~,3,3f ren:~,3,3f tWrp:~,3,3f~%~
          PostPresent: ~,3,3f Err: ~,3,3f~}"
          "??"
          (%ovr::get-float-array
           hmd :dk2-latency 5)))

(defmethod glop:on-event ((window 3bovr-test) event)
  ;; ignore any other events
  (declare (ignore window event)))

(defun init-hud (win)
  (let ((vbo (gl:gen-buffer))
        (vao (hud-vao win)))
    (setf (hud-vbo win) vbo)
    (setf (hud-count win) 0)
    (let ((stride (* 4 4))) ;; x,y,u,v * float
      (gl:bind-buffer :array-buffer vbo)
      (%gl:buffer-data :array-buffer (* 0 stride) (cffi:null-pointer)
                       :static-draw)
      (gl:bind-vertex-array vao)
      (gl:enable-client-state :vertex-array)
      (%gl:vertex-pointer 2 :float stride (cffi:null-pointer))
      (gl:enable-client-state :texture-coord-array)
      (%gl:tex-coord-pointer 2 :float stride (* 2 4)))))

(defun update-hud (win string atl)
  (let* ((strings (split-sequence:split-sequence #\newline string))
         (count (reduce '+ strings :key 'length))
         (stride (* (+ 2 2) 6)) ;; x,y,u,v * 2 tris
         (i 0)
         (scale 0.01))
    (gl:bind-buffer :array-buffer (hud-vbo win))
    (%gl:buffer-data :array-buffer (* count stride 4) (cffi:null-pointer)
                     :static-draw)
    (let ((p (%gl:map-buffer :array-buffer :write-only)))
      (unwind-protect
           (loop for line in strings
                 for baseline from 0 by (* 30 scale)
                 when line
                   do (flet ((c (x y u v)
                               (let ((x (* x scale))
                                     (y (+ baseline (* y scale))))
                                 (setf (cffi:mem-aref p :float (+ 0 (* i 4))) x
                                       (cffi:mem-aref p :float (+ 1 (* i 4))) (- y)
                                       (cffi:mem-aref p :float (+ 2 (* i 4))) v
                                       (cffi:mem-aref p :float (+ 3 (* i 4))) u)
                                 (incf i))))
                        (texatl.cl:do-texatl-string (line
                                                     x0 y0 x1 y1
                                                     u0 v0 u1 v1
                                                     :tex-width *tex-size*
                                                     :tex-height *tex-size*)
                                                    atl
                          (c x0 y0 u0 v0)
                          (c x0 y1 u0 v1)
                          (c x1 y1 u1 v1)

                          (c x0 y0 u0 v0)
                          (c x1 y1 u1 v1)
                          (c x1 y0 u1 v0)))
                 finally (setf (hud-count win) i))
        (%gl:unmap-buffer :array-buffer)))))

(defun build-world (vao)
  (let ((vbo (gl:gen-buffer))
        (color (vector 0 0 0 1))
        (normal (vector 1 0 0))
        (buf (make-array '(1024) :element-type 'single-float
                                 :fill-pointer 0 :adjustable t))
        (count 0))
    (labels ((color (r g b &optional (a 1))
               (setf color (vector r g b a)))
             (normal (x y z)
               (setf normal (vector x y z)))
             (vertex (x y z &optional (w 1))
               (loop for i in (list x y z w)
                     do (vector-push-extend (float i 0.0) buf))
               (loop for i across color
                     do (vector-push-extend (float i 0.0) buf))
               (loop for i across normal
                     do (vector-push-extend (float i 0.0) buf))
               (incf count))
             (cube (x y z r)
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
                       do (let ((n (sb-cga:transform-point
                                    (sb-cga:vec 0.0 0.0 1.0) m)))
                            (normal (aref n 0) (aref n 1) (aref n 2)))
                          (flet ((v (v)
                                   (let ((v (sb-cga:transform-point v m)))
                                     (vertex (+ x (aref v 0))
                                             (+ y (aref v 1))
                                             (+ z (aref v 2))))))
                            (v a)
                            (v b)
                            (v c)
                            (v a)
                            (v c)
                            (v d))))))
      ;; checkerboard ground
      (loop for i from -8 below 8
            do (loop for j from -8 below 8
                     for p = (oddp (+ i j))
                     do (if p
                            (color 0.0 0.9 0.9 1.0)
                            (color 0.1 0.1 0.1 1.0))
                        (vertex i -0.66 j)
                        (vertex (1+ i) -0.66 j)
                        (vertex (1+ i) -0.66 (1+ j))
                        (vertex i -0.66 j)
                        (vertex (1+ i) -0.66 (1+ j))
                        (vertex i -0.66 (1+ j))))
      ;; and some random cubes
      (let ((*random-state* (make-random-state *random-state*))
            (r 20.0))
        (flet ((r () (- (random r) (/ r 2))))
          (loop for i below 5000
                do (color (random 1.0) (+ 0.5 (random 0.5)) (random 1.0) 1.0)
                   (cube (+ 0.0 (r)) (- (r)) (+ 1.5 (r)) (+ 0.05 (random 0.10))))))
      (let ((stride (* 11 4)))
        (gl:bind-buffer :array-buffer vbo)
        (%gl:buffer-data :array-buffer (* count stride) (cffi:null-pointer)
                         :static-draw)
        (gl:bind-vertex-array vao)
        (gl:enable-client-state :vertex-array)
        (%gl:vertex-pointer 4 :float stride (cffi:null-pointer))
        (gl:enable-client-state :normal-array)
        (%gl:normal-pointer :float stride (* 8 4))
        (gl:enable-client-state :color-array)
        (%gl:color-pointer 4 :float stride (* 4 4)))
      (let ((p (%gl:map-buffer :array-buffer :write-only)))
        (unwind-protect
             (loop for i below (fill-pointer buf)
                   do (setf (cffi:mem-aref p :float i)
                            (aref buf i)))
          (%gl:unmap-buffer :array-buffer)))
      (gl:bind-vertex-array 0)
      (gl:delete-buffers (list vbo))
      count)))

(defparameter *w* nil)
(defun draw-world (win)
  (setf *w* win)
  (gl:clear :color-buffer :depth-buffer)
  (gl:enable :framebuffer-srgb
             :line-smooth :blend :point-smooth :depth-test
             :lighting :light0 :color-material)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:polygon-mode :front-and-back :fill)
  (gl:light :light0 :position '(100.0 -120.0 -10.0 0.0))
  (when (world-count win)
    (gl:disable :texture-2d)
    (gl:bind-vertex-array (world-vao win))
    (%gl:draw-arrays :triangles 0 (world-count win)))
  (gl:point-size 10)
  (gl:with-pushed-matrix* (:modelview)
    (gl:translate -2 0.2 -2.5)
    (when (and (hud-count win) (plusp (hud-count win)))
      (gl:enable :texture-2d)
      (gl:bind-texture :texture-2d (hud-texture win))
      (gl:bind-vertex-array (hud-vao win))
      (%gl:draw-arrays :triangles 0 (hud-count win))))
  (gl:bind-vertex-array 0))


(defparameter *x* nil)

(defun draw-frame (hmd &key fbo eye-layer win eye-offsets texture-set
                         mirror)
  (assert (and eye-layer fbo eye-offsets texture-set))
  (let* ((timing (%ovr::get-frame-timing hmd 0))
         (tracking-state (%ovr::get-tracking-state
                          hmd (getf timing :display-midpoiont-seconds)))
         (eye-poses (%ovr::calc-eye-poses
                     (getf (getf tracking-state :head-pose) :the-pose)
                     eye-offsets)))
    (setf *x* eye-layer)

    (let ((status (getf tracking-state :status-flags)))
      ;; change clear color depending on tracking state
      ;; red = no tracking
      ;; blue = orientation only
      ;; green = good
      (cond
        ((and (member :orientation-tracked status)
              (member :position-tracked status))
         (gl:clear-color 0.1 0.5 0.2 1))
        ((and (member :orientation-tracked status))
         (gl:clear-color 0.1 0.2 0.5 1))
        (t
         (gl:clear-color 0.5 0.1 0.1 1))))
    ;; draw view from each eye
    (let ((id (%ovr::texture-set-next-texture-id texture-set)))
      (gl:bind-framebuffer :framebuffer fbo)
      (gl:framebuffer-texture-2d :framebuffer :color-attachment0
                                 :texture-2d
                                 id
                                 0)

      (loop
        for index below 2
        ;; sdk specifies preferred drawing order, so it can predict
        ;; timing better in case one eye will be displayed before
        ;; other
        for eye = index  ;(elt (getf props :eye-render-order) index)
                         ;; get position/orientation for specified eye
        for pose = (elt eye-poses eye)
        for orientation = (getf pose :orientation)
        for position = (getf pose :position)
        ;; get projection matrix from sdk
        for projection = (%ovr::matrix4f-projection
                          (elt (getf eye-layer :fov)
                               eye)
                          0.1 1000.0 ;; near/far
                          ;; request GL style matrix
                          '(:right-handed :clip-range-open-gl))
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
             (viewport (elt (getf eye-layer :viewport) eye)))
           (setf (elt (getf eye-layer :render-pose) eye)
                 pose)
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
                                       (aref orientation 2))))
               (gl:translate (- (aref position 0))
                             (- (aref position 1))
                             (- (aref position 2)))
               (draw-world win))))
      (gl:bind-framebuffer :framebuffer 0)
      ;; pass textures to SDK for distortion, display and vsync
      (%ovr::submit-frame hmd (list eye-layer))
      ;; draw something to the main window
      (gl:draw-buffer :front)
      (gl:disable :scissor-test)
      (gl:clear :color-buffer)
      (when *mirror*
        (gl:bind-texture :texture-2d (getf mirror :texture-id))
        (gl:color 1 1 1 1)
        (gl:enable :texture-2d)
        (gl:disable :cull-face :depth-test :blend)
        (gl:viewport 0 0 (glop:window-width win) (glop:window-height win))
        (when mirror
          (gl:with-pushed-matrix* (:modelview)
            (gl:load-identity)
            (gl:scale 0.9 0.9 0.9)
            (gl:with-pushed-matrix* (:projection)
              (gl:load-identity)
              (gl:with-primitives :quads
                (gl:tex-coord 0 1)
                (gl:vertex -1 -1 0)
                (gl:tex-coord 0 0)
                (gl:vertex -1 1 0)
                (gl:tex-coord 1 0)
                (gl:vertex 1 1 0)
                (gl:tex-coord 1 1)
                (gl:vertex 1 -1 0)))))
        #++(glop:swap-buffers win)))))

(defparameter *once* nil)

(defun test-3bovr (&key (x 0) (y 0))
  (when *once*
    ;; running it twice at once breaks things, so try to avoid that...
    (format t "already running?~%")
    (return-from test-3bovr nil))
  ;; initialize library
  (setf *once* t)
  (unwind-protect
       (%ovr::with-ovr ok (:debug t :timeout-ms 500)
         (unless ok
           (format t "couldn't initialize libovr~%")
           (return-from test-3bovr nil))
         ;; print out some info
         (format t "version: ~s~%" (%ovr::get-version-string))
         (format t "time = ~,3f~%" (%ovr::get-time-in-seconds))
         #++(format t "detect: ~s HMDs available~%" (%ovrhmd::detect))
         ;; try to open an HMD
         (%ovr::with-hmd (hmd luid props)
           (unless hmd
             (format t "couldn't open hmd 0~%")
             (format t "error = ~s~%" (%ovr::get-last-error))
             (return-from test-3bovr nil))
           ;; print out info about the HMD
           (format t "got hmd ~{~s ~s~^~%        ~}~%" props)
           (format t "enabled caps = ~s~%" (%ovr::get-enabled-caps hmd))
           #++(%ovr::set-enabled-caps hmd '(:low-persistence
                                            :dynamic-prediction))
           (format t "             -> ~s~%" (%ovr::get-enabled-caps hmd))
           ;; turn on the tracking
           (%ovr::configure-tracking hmd
                                     ;; desired tracking capabilities
                                     '(:orientation :mag-yaw-correction
                                       :position)
                                     ;; required tracking capabilities
                                     nil)
           (format t "tracking state = ~{~s ~s~^~%                 ~}~%"
                   (%ovr::get-tracking-state hmd))
           ;; create window
           (glop:with-window (win
                              "3bovr test window"
                              1600 900
                              :x x :y y
                              :win-class '3bovr-test
                              :depth-size 16)
             (setf (slot-value win 'hmd) hmd)
             (format t "openend window, gl = ~a / ~a~%"
                     (gl:get* :vendor) (gl:get* :renderer))
             ;; configure FBO for offscreen rendering of the eye views
             (let* ((vaos (gl:gen-vertex-arrays 2))
                    (fbo (gl:gen-framebuffer))
                    (textures (gl:gen-textures 2))
                    (renderbuffer (gl:gen-renderbuffer))
                    (default-fov (getf props :default-eye-fov))
                    ;; get recommended sizes of eye textures
                    (ls (%ovr::get-fov-texture-size hmd %ovr::+eye-left+
                                                    ;; use default fov
                                                    (elt default-fov
                                                         %ovr::+eye-left+)
                                                    ;; and no scaling
                                                    1.0))
                    (rs (%ovr::get-fov-texture-size hmd %ovr::+eye-right+
                                                    (elt default-fov
                                                         %ovr::+eye-right+)
                                                    1.0))
                    ;; put space between eyes to avoid interference
                    (padding 16)
                    ;; storing both eyes in 1 texture, so figure out combined size
                    (fbo-w (+ (getf ls :w) (getf rs :w) (* 3 padding)))
                    (fbo-h (+ (* 2 padding)
                              (max (getf ls :h) (getf rs :h))))
                    ;; describe the texture configuration for libovr
                    (eye-layer
                      `(:render-pose ((:position #(0.0 0.0 0.0)
                                       :orientation #(0.0 0.0 0.0 0.0))
                                      (:position #(0.0 0.0 0.0)
                                       :orientation #(0.0 0.0 0.0 0.0)))
                        :fov ,default-fov
                        :viewport #((:pos #(,padding , padding)
                                     :size ,ls)
                                    (:pos #(,(+ (* 2 padding)
                                              (getf ls :w))
                                            ,padding)
                                     :size ,rs))
                        :color-texture (,(cffi:null-pointer)
                                        ,(cffi:null-pointer))
                        :flags (:high-quality :texture-origin-at-bottom-left)
                        :type :eye-fov))
                    (font (car
                           (conspack:decode-file
                            (asdf:system-relative-pathname '3b-ovr
                                                           "font.met"))))
                    (eye-offsets
                      (coerce
                       (loop for i below 2
                             for fov across default-fov
                             for erd = (%ovr::get-render-desc hmd i fov)
                             do (format t "eyeRenderDesc[~s] = ~s~%" i erd)
                             collect (getf erd :hmd-to-eye-view-offset))
                       'vector)))
               (format t "user = ~s~%"
                       (%ovr::get-string hmd :user "?unknown?"))
               (format t "IPD = ~s, calculated = ~s~%"
                       (%ovr::get-float hmd :ipd 0.0)
                       (sb-cga:vec-length
                        (sb-cga:vec- (elt eye-offsets 0)
                                     (elt eye-offsets 1))))
               (%ovr::with-swap-texture-set texture-set (hmd fbo-w fbo-h)
                 (%ovr::with-mirror-texture mirror (hmd 1920 1090)
                   (setf (elt (getf eye-layer :color-texture) 0)
                         texture-set)
                   (format t "created texture set ~s~%" texture-set)
                   (format t "  = ~s / ~s~%"
                           (%ovr::swap-texture-set-index texture-set)
                           (%ovr::swap-texture-set-count texture-set))
                   (format t "  = ~s~%"
                           (%ovr::swap-texture-set-textures texture-set))

                   ;; configure the fbo/texture
                   (format t "left eye tex size = ~s, right = ~s~% total =~sx~a~%"
                           ls rs fbo-w fbo-h)
                   (gl:bind-framebuffer :framebuffer fbo)
                   ;; we will bind the texture later, since we have to cycle
                   ;; through the allocated textures in the texture set
                   ;; todo: preallocate an fbo per texture in texture-set?
                   (gl:bind-renderbuffer :renderbuffer renderbuffer)
                   (gl:renderbuffer-storage :renderbuffer :depth-component24
                                            fbo-w fbo-h)
                   (gl:framebuffer-renderbuffer :framebuffer :depth-attachment
                                                :renderbuffer renderbuffer)
                   (format t "created renderbuffer status = ~s~%"
                           (gl:check-framebuffer-status :framebuffer))
                   (gl:bind-framebuffer :framebuffer 0)

                   ;; load font texture
                   (gl:bind-texture :texture-2d (second textures))
                   (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
                   (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
                   (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
                   (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
                   (let ((png (png-read:read-png-file
                               (asdf:system-relative-pathname '3b-ovr
                                                              "font.png"))))
                     (gl:tex-image-2d :texture-2d 0 :rgb
                                      (png-read:width png) (png-read:height png)
                                      0 :rgb :unsigned-byte
                                      (make-array (* 3
                                                     (png-read:width png)
                                                     (png-read:height png))
                                                  :element-type
                                                  '(unsigned-byte 8)
                                                  :displaced-to
                                                  (png-read:image-data png)))
                     (gl:generate-mipmap :texture-2d)
                     (gl:bind-texture :texture-2d 0))
                   (setf (hud-texture win) (second textures))

                   ;; set up a vao containing a simple 'world' geometry,
                   ;; and hud geometry
                   (setf (world-vao win) (first vaos)
                         (world-count win) (build-world (first vaos))
                         (hud-vao win) (second vaos))
                   (init-hud win)

                   ;; main loop
                   (loop while (glop:dispatch-events win :blocking nil
                                                         :on-foo nil)
                         when font
                           do (with-simple-restart (continue "continue")
                                (update-hud win (hud-text win hmd)
                                            font))
                         do (with-simple-restart (continue "continue")
                              (draw-frame hmd :fbo fbo
                                              :eye-layer eye-layer
                                              :win win
                                              :eye-offsets eye-offsets
                                              :texture-set texture-set
                                              :mirror mirror)))
                   ;; clean up
                   (gl:delete-vertex-arrays vaos)
                   (gl:delete-framebuffers (list fbo))
                   (gl:delete-textures textures)
                   (gl:delete-renderbuffers (list renderbuffer))
                   (format t "done~%")
                   (sleep 1)))))))
    (progn
      (format t "done2~%")
      (setf *once* nil)
      (format t "done3 ~s~%" *once*))))



#++
(asdf:load-systems '3b-ovr-sample)

#++
(test-3bovr)
#++
(test-3bovr :x 1920)

#++
(let ((*default-pathname-defaults* (asdf:system-relative-pathname '3b-ovr "./")))
  (texatl:make-font-atlas-files "font.png" "font.met" 256 256
                                "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
                                16
                                :dpi 128
                                :padding 4
                                :string "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.,;:?!@#$%^&*()-_<>'\"$[]= "))

