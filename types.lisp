(in-package #:3b-ovr-bindings)


(defctype int8-t :char)

(defctype int16-t :short)

(defctype int32-t :int)

(defctype int64-t :long)

(defctype uint8-t :unsigned-char)

(defctype uint16-t :unsigned-short)

(defctype uint32-t :unsigned-int)

(defctype uint64-t :unsigned-long)

(defctype int-least8-t :char)

(defctype int-least16-t :short)

(defctype int-least32-t :int)

(defctype int-least64-t :long)

(defctype uint-least8-t :unsigned-char)

(defctype uint-least16-t :unsigned-short)

(defctype uint-least32-t :unsigned-int)

(defctype uint-least64-t :unsigned-long)

(defctype int-fast8-t :char)

(defctype int-fast16-t :long)

(defctype int-fast32-t :long)

(defctype int-fast64-t :long)

(defctype uint-fast8-t :unsigned-char)

(defctype uint-fast16-t :unsigned-long)

(defctype uint-fast32-t :unsigned-long)

(defctype uint-fast64-t :unsigned-long)

(defctype intptr-t :long)

(defctype uintptr-t :unsigned-long)

(defctype intmax-t :long)

(defctype uintmax-t :unsigned-long)

(defctype bool (:boolean :char))

(defcstruct (vector2i- :class vector2i)
  (x :int)
  (y :int))

(define-cffi-translators (p v (:struct vector2i-) vector2i)
  :write `(etypecase ,v
            ((simple-array (unsigned-byte 32)(2))
             (setf (cffi:mem-aref ,p :int 0) (aref ,v 0)
                   (cffi:mem-aref ,p :int 1) (aref ,v 1)))
            (sequence
             (setf (cffi:mem-aref ,p :int 0) (elt ,v 0)
                   (cffi:mem-aref ,p :int 1) (elt ,v 1))))
  :read `(make-array '(2)
                     :element-type '(unsigned-byte 32)
                     :initial-contents (list (cffi:mem-aref ,p :int 0)
                                             (cffi:mem-aref ,p :int 1))))

(defctype vector2i (:struct vector2i-))

;; not sure if this should be same as vector* or not?
(defcstruct sizei-
  (w :int)
  (h :int))

(defctype sizei (:struct sizei-))

(defcstruct recti-
  (pos vector2i)
  (size sizei))

(defctype recti (:struct recti-))

(defcstruct (quatf- :class quatf)
  (x :float)
  (y :float)
  (z :float)
  (w :float))

(defctype quatf (:struct quatf-))

(define-cffi-translators (p v (:struct quatf-) quatf)
  :write `(etypecase ,v
            ((simple-array single-float (4))
             (setf (cffi:mem-aref ,p :float 0) (aref ,v 0)
                   (cffi:mem-aref ,p :float 1) (aref ,v 1)
                   (cffi:mem-aref ,p :float 2) (aref ,v 2)
                   (cffi:mem-aref ,p :float 3) (aref ,v 3)))
            (sequence
             (setf (cffi:mem-aref ,p :float 0) (float (elt ,v 0) 1.0)
                   (cffi:mem-aref ,p :float 1) (float (elt ,v 1) 1.0)
                   (cffi:mem-aref ,p :float 2) (float (elt ,v 2) 1.0)
                   (cffi:mem-aref ,p :float 3) (float (elt ,v 3) 1.0))))
  :read `(make-array '(4)
                     :element-type 'single-float
                     :initial-contents (list (cffi:mem-aref ,p :float 0)
                                             (cffi:mem-aref ,p :float 1)
                                             (cffi:mem-aref ,p :float 2)
                                             (cffi:mem-aref ,p :float 3))))

(defcstruct (vector2f- :class vector2f)
  (x :float)
  (y :float))

(defctype vector2f (:struct vector2f-))

(define-cffi-translators (p v (:struct vector2f-) vector2f)
  :write `(etypecase ,v
            ((simple-array single-float (2))
             (setf (cffi:mem-aref ,p :float 0) (aref ,v 0)
                   (cffi:mem-aref ,p :float 1) (aref ,v 1)))
            (sequence
             (setf (cffi:mem-aref ,p :float 0) (float (elt ,v 0) 1.0)
                   (cffi:mem-aref ,p :float 1) (float (elt ,v 1) 1.0))))
  :read `(make-array '(2)
                     :element-type 'single-float
                     :initial-contents (list (cffi:mem-aref ,p :float 0)
                                             (cffi:mem-aref ,p :float 1))))

(defcstruct (vector3f- :class vector3f)
  (:x :float)
  (:y :float)
  (:z :float))

(defctype vector3f (:struct vector3f-))

(define-cffi-translators (p v (:struct vector3f-) vector3f)
  :write `(etypecase ,v
            ((simple-array single-float (3))
             (setf (cffi:mem-aref ,p :float 0) (aref ,v 0)
                   (cffi:mem-aref ,p :float 1) (aref ,v 1)
                   (cffi:mem-aref ,p :float 2) (aref ,v 2)))
            (sequence
             (setf (cffi:mem-aref ,p :float 0) (float (elt ,v 0) 1.0)
                   (cffi:mem-aref ,p :float 1) (float (elt ,v 1) 1.0)
                   (cffi:mem-aref ,p :float 2) (float (elt ,v 2) 1.0))))
  :read `(sb-cga:vec (cffi:mem-aref ,p :float 0)
                     (cffi:mem-aref ,p :float 1)
                     (cffi:mem-aref ,p :float 2)))


(defcstruct (matrix4f- :class matrix4f)
  (m :float :count 16))

(defctype matrix4f (:struct matrix4f-))

(define-cffi-translators (p v (:struct matrix4f-) matrix4f)
  :write `(etypecase ,v
            ((simple-array single-float (16))
             ;; not sure if loop or unrolling would be better here?
             (loop for i below 16
                   do (cffi:mem-aref ,p :float i) (aref ,v i)))
            (sequence
             (loop for i below 16
                   do (cffi:mem-aref ,p :float i) (float (elt ,v i) 1.0))))
  :read `(make-array 16
                     :element-type 'single-float
                     :initial-contents
                     (list
                      ,@(loop for i below 16
                              collect `(cffi:mem-aref ,p :float ,i)))))

(defcstruct posef-
  (orientation quatf)
  (position vector3f))

(defctype posef (:struct posef-))

(defcstruct pose-statef-
  (the-pose posef)
  (angular-velocity vector3f)
  (linear-velocity vector3f)
  (angular-acceleration vector3f)
  (linear-acceleration vector3f)
  (pad :float)
  (time-in-seconds :double))

(defctype pose-statef (:struct pose-statef-))

(defcstruct fov-port-
  (up-tan :float)
  (down-tan :float)
  (left-tan :float)
  (right-tan :float))

(defctype fov-port (:struct fov-port-))

(defcenum (%ovrhmd::type- :unsigned-int)
  (:none 0)
  (:dk1 3)
  (:dkhd 4)
  (:dk2 6)
  (:black-star 7)
  (:cb 8)
  (:other 9))

(defctype %ovrhmd::type  %ovrhmd::type-)

(defbitfield (%ovrhmd::caps- :unsigned-int)
  (:present 1)
  (:available 2)
  (:captured 4)
  (:extend-desktop 8)
  (:debug-device 16)
  (:no-mirror-to-window 8192)
  (:display-off 64)
  (:low-persistence 128)
  (:dynamic-prediction 512)
  (:no-vsync 4096)
  (:writable-mask 12992)
  (:service-mask 8896))

(defctype %ovrhmd::caps %ovrhmd::caps-)

(defbitfield (tracking-caps- :unsigned-int)
  (:orientation 16)
  (:mag-yaw-correction 32)
  (:position 64)
  (:idle 256))

(defctype tracking-caps tracking-caps-)

(defbitfield (distortion-caps- :unsigned-int)
  (:time-warp 2)
  (:vignette 8)
  (:no-restore 16)
  (:flip-input 32)
  (:srgb 64)
  (:overdrive 128)
  (:hq-distortion 256)
  (:linux-dev-fullscreen 512)
  (:compute-shader 1024)
  (:timewarp-jit-delay 4096)
  (:profile-no-spin-waits 65536))

(defctype distortion-caps distortion-caps-)

(defcenum (eye-type- :unsigned-int)
  (:left 0)
  (:right 1)
  (:count 2))

(defctype eye-type eye-type-)

(defcstruct %ovrhmd::struct
)

(defcstruct %ovrhmd::desc-
  (handle (:pointer (:struct %ovrhmd::struct)))
  (type %ovrhmd::type)
  (product-name :string)
  (manufacturer :string)
  (vendor-id :short)
  (product-id :short)
  (serial-number :char :count 24)
  (firmware-major :short)
  (firmware-minor :short)
  (camera-frustum-hfov-in-radians :float)
  (camera-frustum-vfov-in-radians :float)
  (camera-frustum-near-zin-meters :float)
  (camera-frustum-far-zin-meters :float)
  (hmd-caps %ovrhmd::caps)
  (tracking-caps tracking-caps)
  (distortion-caps distortion-caps)
  (default-eye-fov fov-port :count 2)
  (max-eye-fov fov-port :count 2)
  (eye-render-order :unsigned-int :count 2) ;; eye-type
  (resolution sizei)
  (windows-pos vector2i)
  (display-device-name :string)
  (display-id :int))

(defctype %ovrhmd::desc (:struct %ovrhmd::desc-))

(defctype hmd (:pointer %ovrhmd::desc))

(defbitfield (status-bits- :unsigned-int)
  (:orientation-tracked 1)
  (:position-tracked 2)
  (:camera-pose-tracked 4)
  (:position-connected 32)
  (:hmd-connected 128))

(defctype status-bits status-bits-)

(defcstruct sensor-data-
  (accelerometer vector3f)
  (gyro vector3f)
  (magnetometer vector3f)
  (temperature :float)
  (time-in-seconds :float))

(defctype sensor-data (:struct sensor-data-))

(defcstruct tracking-state-
  (head-pose pose-statef)
  (camera-pose posef)
  (leveled-camera-pose posef)
  (raw-sensor-data sensor-data)
  (status-flags status-bits)
  (last-camera-frame-counter uint32-t)
  (pad uint32-t))

(defctype tracking-state (:struct tracking-state-))

(defcstruct frame-timing-
  (delta-seconds :float)
  (pad :float)
  (this-frame-seconds :double)
  (timewarp-point-seconds :double)
  (next-frame-seconds :double)
  (scanout-midpoint-seconds :double)
  (eye-scanout-seconds :double :count 2))

(defctype frame-timing (:struct frame-timing-))

(defcstruct eye-render-desc-
  (eye eye-type)
  (fov fov-port)
  (distorted-viewport recti)
  (pixels-per-tan-angle-at-center vector2f)
  (hmd-to-eye-view-offset vector3f))

(defctype eye-render-desc (:struct eye-render-desc-))

(defcstruct position-timewarp-desc-
  (hmd-to-eye-view-offset vector3f :count 2)
  (near-clip :float)
  (far-clip :float))

(defctype position-timewarp-desc (:struct position-timewarp-desc-))

(defcenum (render-apitype- :unsigned-int)
  (:none 0)
  (:open-gl 1)
  (:android-gles 2)
  (:d3d9 3)
  (:d3d10 4)
  (:d3d11 5)
  (:count 6))

(defctype render-apitype render-apitype-)

(defcstruct render-apiconfig-header-
  (api render-apitype)
  (back-buffer-size sizei)
  (multisample :int))

(defctype render-apiconfig-header (:struct render-apiconfig-header-))

(defcstruct render-apiconfig-
  (header render-apiconfig-header)
  (platform-data uintptr-t :count 8))

(defctype render-apiconfig (:struct render-apiconfig-))

(defcstruct texture-header-
  (api render-apitype)
  (texture-size sizei)
  (render-viewport recti))

(defctype texture-header (:struct texture-header-))

(defcstruct texture-
  (header texture-header)
  (platform-data uintptr-t :count 8))

(defctype texture (:struct texture-))



(defbitfield (init-flags- :unsigned-int)
  (:debug 1)
  (:server-optional 2)
  (:request-version 4)
  (:force-no-debug 8))

(defctype init-flags init-flags-)

(defcenum (log-level- :unsigned-int)
  (:debug 0)
  (:info 1)
  (:error 2))

(defctype log-level log-level-)

(defctype log-callback :pointer)

(defcstruct init-params-
  (flags init-flags)
  (requested-minor-version uint32-t)
  (log-callback log-callback)
  (connection-timeout-ms uint32-t))

(defctype init-params (:struct init-params-))



(defcstruct distortion-vertex-
  (screen-pos-ndc vector2f)
  (time-warp-factor :float)
  (vignette-factor :float)
  (tan-eye-angles-r vector2f)
  (tan-eye-angles-g vector2f)
  (tan-eye-angles-b vector2f))

(defctype distortion-vertex (:struct distortion-vertex-))

(defcstruct distortion-mesh-
  (p-vertex-data (:pointer distortion-vertex))
  (p-index-data (:pointer :unsigned-short))
  (vertex-count :unsigned-int)
  (index-count :unsigned-int))

(defctype distortion-mesh (:struct distortion-mesh-))

(defcstruct hswdisplay-state-
  (displayed bool)
  (pad :char :count 7)
  (start-time :double)
  (dismissible-time :double))

(defctype hswdisplay-state (:struct hswdisplay-state-))



(defbitfield (projection-modifier- :unsigned-int)
  (:none 0)
  (:right-handed 1)
  (:far-less-than-near 2)
  (:far-clip-at-infinity 4)
  (:clip-range-open-gl 8))

(defctype projection-modifier projection-modifier-)
