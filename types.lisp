(in-package #:3b-ovr-bindings)


(defctype int8-t :int8)

(defctype int16-t :int16)

(defctype int32-t :int32)

(defctype int64-t :int64)

(defctype uint8-t :uint8)

(defctype uint16-t :uint16)

(defctype uint32-t :uint32)

(defctype uint64-t :uint64)

(defctype int-least8-t :char)

(defctype int-least16-t :short)

(defctype int-least32-t :int)

(defctype int-least64-t :int64)

(defctype uint-least8-t :unsigned-char)

(defctype uint-least16-t :unsigned-short)

(defctype uint-least32-t :unsigned-int)

(defctype uint-least64-t :uint64)

(defctype int-fast8-t :char)

(defctype int-fast16-t :long)

(defctype int-fast32-t :long)

(defctype int-fast64-t :int64)

(defctype uint-fast8-t :unsigned-char)

(defctype uint-fast16-t :unsigned-long)

(defctype uint-fast32-t :unsigned-long)

(defctype uint-fast64-t :uint64)

(defctype intptr-t #. (ecase (foreign-type-size :pointer)
                        (4 :int32)
                        (8 :int64)))

(defctype uintptr-t #. (ecase (foreign-type-size :pointer)
                        (4 :uint32)
                        (8 :uint64)))

(defctype intmax-t :int64)

(defctype uintmax-t :uint64)

(defctype bool (:boolean :char))

;; non-negative = success, negative = failure
(defcenum (ovr-result  :int32)
  ;; general "success"
  (:success 0)
  ;; returned from submit-frame, succeeded but didn't actually display anything?
  (:success-not-visible 1000) ;; something else is using the HMD, stop rendering anything and try again periodically until submit-frame returns :success
  (:success-hmd-firmware-mismatch 4100)
  (:success-tracker-firmware-mismatch 4101)
  (:success-controller-firmware-mismatch 4104)
  ;; error codes
  (:Memory-Allocation-Failure -1000) ;; Failure to allocate memory.
  (:Socket-Creation-Failure -1001)   ;; Failure to create a socket.
  (:Invalid-Hmd -1002)              ;; Invalid HMD parameter provided.
  (:Timeout -1003)                 ;; The operation timed out.
  (:Not-Initialized -1004) ;; The system or component has not been initialized.
  (:Invalid-Parameter -1005) ;; Invalid parameter provided. See error info or log for details.
  (:Service-Error -1006) ;; Generic service error. See error info or log for details.
  (:No-Hmd -1007)        ;; The given HMD doesn't exist.

  ;; Audio error range, reserved for Audio errors.
  (:Audio-Reserved-Begin -2000) ;; First Audio error.
  (:Audio-Reserved-End -2999)   ;; Last Audio error.

  ;; Initialization errors.
  (:Initialize -3000)        ;; Generic initialization error.
  (:Lib-Load -3001)           ;; Couldn't load LibOVRRT.
  (:Lib-Version -3002)        ;; LibOVRRT version incompatibility.
  (:Service-Connection -3003) ;; Couldn't connect to the OVR Service.
  (:Service-Version -3004)    ;; OVR Service version incompatibility.
  (:Incompatible-OS -3005) ;; The operating system version is incompatible.
  (:Display-Init -3006)    ;; Unable to initialize the HMD display.
  (:Server-Start -3007) ;; Unable to start the server. Is it already running?
  (:Reinitialization -3008) ;; Attempting to re-initialize with a different version.
  (:Mismatched-Adapters -3009) ;; Chosen rendering adapters between client and service do not match
  (:Leaking-Resources -3010) ;; Calling application has leaked resources
  (:Client-Version -3011) ;; Client version too old to connect to service

  ;;Hardware Errors
  (:Invalid-Bundle-Adjustment -4000) ;; Headset has no bundle adjustment data.
  (:USB-Bandwidth -4001) ;; The USB hub cannot handle the camera frame bandwidth.
  (:USB-Enumerated-Speed -4002) ;; The USB camera is not enumerating at the correct device speed.
  (:Image-Sensor-CommError -4003) ;; Unable to communicate with the image sensor.
  (:General-Tracker-Failure -4004) ;; We use this to report various tracker issues that don't fit in an easily classifiable bucket.
  (:Excessive-Frame-Truncation -4005) ;; A more than acceptable number of frames are coming back truncated.
  (:Excessive-Frame-Skipping -4006) ;; A more than acceptable number of frames have been skipped.
  (:Sync-Disconnected -4007) ;; The tracker is not receiving the sync signal (cable disconnected?)
  (:Tracker-Memory-ReadFailure -4008) ;; Failed to read memory from the tracker
  (:Tracker-Memory-WriteFailure -4009) ;; Failed to write memory from the tracker
  (:Tracker-Frame-Timeout -4010) ;; Timed out waiting for a camera frame
  (:Tracker-Truncated-Frame -4011) ;; Truncated frame returned from tracker

  (:HMD-Firmware-Mismatch -4100) ;; The HMD Firmware is out of date and is unacceptable.
  (:Tracker-Firmware-Mismatch -4101) ;; The Tracker Firmware is out of date and is unacceptable.
  (:Bootloader-Device-Detected -4102) ;; A bootloader HMD is detected by the service
  (:Tracker-Calibration-Error -4103) ;; The tracker calibration is missing or incorrect
  (:Controller-Firmware-Mismatch -4104) ;; The controller firmware is out of date and is unacceptable

  ;;Synchronization Errors
  (:Incomplete -5000) ;; Requested async work not yet complete.
  (:Abandoned -5001) ;; Requested async work was abandoned and result is incomplete.

  ;;Rendering Errors
  (:Display-Lost -6000) ;; In the event of a system-wide graphics reset or cable unplug this is returned to the app
  )

(define-foreign-type checked-result ()
  ()
  (:actual-type ovr-result)
  (:simple-parser checked-result))


(defcstruct error-info-
  (:result ovr-result)
  (:error-string :char :count 512))

(defcfun ("ovr_GetLastErrorInfo" get-last-error-info) :void
  (error-info :pointer))

(defun get-last-error ()
  (with-foreign-object (e '(:struct error-info-))
    (get-last-error-info e)
    (values
     (foreign-string-to-lisp (foreign-slot-pointer e '(:struct error-info-)
                                                   :error-string)
                             :max-chars 512
                             :encoding :utf-8))))

(defmethod translate-from-foreign (v (type checked-result))
  ;; todo: expand-from-foreign/-dyn
  (if (zerop v) ;; optimize common case
      :success
      (let ((enum (foreign-enum-keyword 'ovr-result v :errorp nil)))
        (when (and (not enum)
                   (<= (foreign-enum-value 'ovr-result :audio-reserved-end)
                       v
                       (foreign-enum-value 'ovr-result :audio-reserved-begin)))
          (setf enum :unknown-audio-error))
        (when (< v 0)
          (error "libOVR error ~s (~s) = ~a" v (or enum "??")
                 (get-last-error)))
        (or enum v))))
#++
(with-foreign-object (a 'ovr-result)
  (setf (mem-aref a 'ovr-result) -6000)
  (mem-aref a 'checked-result))

(defcstruct (vector2i- :class vector2i)
  (x :int)
  (y :int))

(define-cffi-translators (p v (:struct vector2i-) vector2i)
  :write `(etypecase ,v
            ((simple-array (signed-byte 32)(2))
             (setf (cffi:mem-aref ,p :int 0) (aref ,v 0)
                   (cffi:mem-aref ,p :int 1) (aref ,v 1)))
            (sequence
             (setf (cffi:mem-aref ,p :int 0) (elt ,v 0)
                   (cffi:mem-aref ,p :int 1) (elt ,v 1))))
  :read `(make-array '(2)
                     :element-type '(signed-byte 32)
                     :initial-contents (list (cffi:mem-aref ,p :int 0)
                                             (cffi:mem-aref ,p :int 1))))

;(defctype vector2i (:struct vector2i-))

;; not sure if this should be same as vector* or not?
(defcstruct sizei-
  (:w :int)
  (:h :int))

;(defctype sizei (:struct sizei-))

(defcstruct recti-
  (:pos (:struct vector2i-))
  (:size (:struct sizei-)))

;(defctype recti (:struct recti-))

(defcstruct (quatf- :class quatf)
  (x :float)
  (y :float)
  (z :float)
  (w :float))

;(defctype quatf (:struct quatf-))

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

;(defctype vector2f (:struct vector2f-))

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

;(defctype vector3f (:struct vector3f-))

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

(defcstruct (posef- :class posef)
  (:orientation (:struct quatf-))
  (:position (:struct vector3f-)))

;(defctype posef (:struct posef-))

#++
(defmethod translate-from-foreign (value (type posef))
  (call-next-method)
  #++(let ((v (call-next-method)))
    (setf (getf v :position) (cffi:mem-ref (getf v :position)
                                           '(:struct vector3f-)))
    (setf (getf v :orientation) (cffi:mem-ref (getf v :orientation)
                                           '(:struct quatf-)))
    v))

(defcstruct (pose-statef- :class pose-statef)
  (:the-pose (:struct posef-))
  (:angular-velocity (:struct vector3f-))
  (:linear-velocity (:struct vector3f-))
  (:angular-acceleration (:struct vector3f-))
  (:linear-acceleration (:struct vector3f-))
  (:pad :float)
  (:time-in-seconds :double))

;(defctype pose-statef (:struct pose-statef-))

#++
(defmethod translate-from-foreign (value (type pose-statef))
  (call-next-method)
  #++(let ((v (call-next-method)))
       (flet ((v3 (k)
                (let ((x (getf v k)))
                  (setf (getf v k) (cffi:mem-ref x '(:struct vector3f-))))))
         (v3 :angular-acceleration)
         (v3 :angular-velocity)
         (v3 :linear-acceleration)
         (v3 :linear-velocity)
         (remf v :pad)
         (setf (getf v :the-pose) (mem-ref (getf v :the-pose) '(:struct posef-)))
         v)))

(defcstruct fov-port-
  (:up-tan :float)
  (:down-tan :float)
  (:left-tan :float)
  (:right-tan :float))

;(defctype fov-port (:struct fov-port-))

(defcenum (hmd-type- :unsigned-int)
  (:none 0)
  (:dk1 3)
  (:dkhd 4)
  (:dk2 6)
  (:black-star 7)
  (:cb 8)
  (:other 9)
  (:e3-2015 10)
  (:es06 11))

;(defctype %ovrhmd::type  %ovrhmd::type-)

(defbitfield (hmd-caps- :unsigned-int)
  ;(:present 1)
  ;(:available 2)
  ;(:captured 4)
  ;(:extend-desktop 8)
  (:debug-device 16)
  ;(:no-mirror-to-window 8192)
  ;(:display-off 64)
  ;(:low-persistence 128)
  ;(:dynamic-prediction 512)
  ;(:no-vsync 4096)
  ;; fixme: convert 'writable-mask' into a constant or something
  ;; for bitfields that have it?
  #++(:writable-mask 0)
  #++(:service-mask 0))

;(defctype %ovrhmd::caps %ovrhmd::caps-)

(defbitfield (tracking-caps- :unsigned-int)
  (:orientation 16)
  (:mag-yaw-correction 32)
  (:position 64)
  (:idle 256))

;(defctype tracking-caps tracking-caps-)

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

;(defctype distortion-caps distortion-caps-)

(defcenum (eye-type- :unsigned-int)
  (:left 0)
  (:right 1)
  (:count 2))
;; some constants to work around a cffi bug for now
;; (and possibly for indexing per-eye data from API)
(defconstant +eye-left+ 0)
(defconstant +eye-right+ 1)

;(defctype eye-type eye-type-)

(defcstruct graphics-luid-
  (reserved :char :count 8))

(defcstruct (char64- :class char64)
  (string :char :count 64))

(defmethod translate-from-foreign (v (type char64))
  (cffi:foreign-string-to-lisp v :max-chars 64))

(defcstruct (fov-port-2- :class fov-port-2)
  (fov (:struct fov-port-) :count 2))

(define-cffi-translators (p v (:struct fov-port-2-) fov-port-2)
  :write `(progn
            (setf (mem-aref ,p '(:struct fov-port-) 0)
                       (elt ,v 0))
            (setf (mem-aref ,p '(:struct fov-port-) 1)
                  (elt ,v 1)))
  :read `(vector
          (mem-aref ,p '(:struct fov-port-) 0)
          (mem-aref ,p '(:struct fov-port-) 1)))

(defcstruct hmd-desc-
  (:type hmd-type-)
  (pad :char :count 4)
  (:product-name (:struct char64-))
  (:manufacturer (:struct char64-))
  (:vendor-id :short)
  (:product-id :short)
  (:serial-number :char :count 24) ;; todo: do something like for product-name?
  (:firmware-major :short)
  (:firmware-minor :short)
  (:camera-frustum-hfov-in-radians :float)
  (:camera-frustum-vfov-in-radians :float)
  (:camera-frustum-near-z-in-meters :float)
  (:camera-frustum-far-z-in-meters :float)
  (:available-hmd-caps hmd-caps-)
  (:default-hmd-caps hmd-caps-)
  (:available-tracking-caps tracking-caps-)
  (:default-tracking-caps tracking-caps-)
  (:default-eye-fov (:struct fov-port-2-))
  (:max-eye-fov (:struct fov-port-2-))
  (:resolution (:struct sizei-))
  (:display-refresh-rate :float)
  (pad1 :char :count 4)
  )

;(defctype %ovrhmd::desc (:struct %ovrhmd::desc-))

(defcstruct hmd-struct
  )
(defctype hmd (:pointer (:struct hmd-struct)))

(defbitfield (status-bits- :unsigned-int)
  (:orientation-tracked 1)
  (:position-tracked 2)
  (:camera-pose-tracked 4)
  (:position-connected #x20)
  (:hmd-connected #x80))

;(defctype status-bits status-bits-)

(defcstruct (sensor-data- :class sensor-data)
  (:accelerometer (:struct vector3f-))
  (:gyro (:struct vector3f-))
  (:magnetometer (:struct vector3f-))
  (:temperature :float)
  (:time-in-seconds :float))

;(defctype sensor-data (:struct sensor-data-))

#++
(defmethod translate-from-foreign (value (type sensor-data))
  (call-next-method)
  #++
  (let ((v (call-next-method)))
    (flet ((v3 (k)
             (let ((x (getf v k)))
               (setf (getf v k) (cffi:mem-ref x '(:struct vector3f-))))))
      (v3 :magnetometer)
      (v3 :accelerometer)
      (v3 :gyro)
      v)))

(defcstruct tracking-state-
  (:head-pose (:struct pose-statef-))
  (:camera-pose (:struct posef-))
  (:leveled-camera-pose (:struct posef-))
  (:hand-poses (:struct pose-statef-) :count 2)
  (:raw-sensor-data (:struct sensor-data-))
  (:status-flags status-bits-)
  (:last-camera-frame-counter uint32-t)
  (:pad uint32-t))

;(defctype tracking-state (:struct tracking-state-))

(defcstruct frame-timing-
  (:display-midpoiont-seconds :double)
  (:frame-interval-seconds :double)
  (:app-frame-index :unsigned-int)
  (:display-frame-index :unsigned-int))

;(defctype frame-timing (:struct frame-timing-))

(defcstruct (eye-render-desc- :class eye-render-desc)
  ;; not sure if this is more useful as an int or :left/:right ?
  ;; leaving as int for now, since we probably want to index into sequence
  ;; more often than describe it
  (:eye :unsigned-int) ;; eye-type-
  (:fov (:struct fov-port-))
  (:distorted-viewport (:struct recti-))
  (:pixels-per-tan-angle-at-center (:struct vector2f-))
  (:hmd-to-eye-view-offset (:struct vector3f-)))

;(defctype eye-render-desc (:struct eye-render-desc-))

#++
(defmethod translate-from-foreign (value (type eye-render-desc))
  (call-next-method)
  #++(let ((v (call-next-method)))
    (setf (getf v :fov) (cffi:mem-ref (getf v :fov) '(:struct fov-port-)))
    (setf (getf v :distorted-viewport)
          (cffi:mem-ref (getf v :distorted-viewport) '(:struct recti-)))
    (setf (getf v :pixels-per-tan-angle-at-center)
          (cffi:mem-ref (getf v :pixels-per-tan-angle-at-center)
                        '(:struct vector2f-)))
    (setf (getf v :hmd-to-eye-view-offset)
          (cffi:mem-ref (getf v :hmd-to-eye-view-offset) '(:struct vector3f-)))
    v))

(defcstruct timewarp-projection-desc-
  (:projection-22 :float)
  (:projection-23 :float)
  (:projection-32 :float))

(defcstruct view-scale-desc-
  (:hmd-to-eye-view-offset (:struct vector3f-) :count 2)
  (:hmd-space-to-world-scale-in-meters :float))

(defcenum (render-apitype- :unsigned-int)
  (:none 0)
  (:opengl 1)
  (:android-gles 2)
  (:d3d11 5))

;(defctype render-apitype render-apitype-)

(defcstruct render-api-config-header-
  (:api render-apitype-)
  (:back-buffer-size (:struct sizei-))
  (:multisample :int))

;(defctype render-api-config-header (:struct render-api-config-header-))

(defcstruct render-api-config-
  (:header (:struct render-api-config-header-))
  (:platform-data uintptr-t :count 8))

;(defctype render-api-config (:struct render-api-config-))

(defcstruct gl-config-data-
  (:header (:struct render-api-config-header-))
  #+windows (:window :pointer)
  #+windows (:dc :pointer)
  #+linux (:x-display :pointer))

;(defctype gl-config-data (:struct gl-config-data-))
;; not sure if this should define ovrGLConfig or not?
#++ (defcunion gl-config
      (config render-api-config)
      (gl gl-config-data))

(defcstruct texture-header-
  (:api render-apitype-)
  (:texture-size (:struct sizei-)))

;(defctype texture-header (:struct texture-header-))


(defcstruct texture-x
  (:header (:struct texture-header-))
  (:pad :char :count 4)
  (:platform-data uintptr-t :count 8))

;; just defining the GL version for now...
(defcstruct texture-
  (:api render-apitype-)
  (:texture-size (:struct sizei-))
  (:texture-id :unsigned-int)
  (pad uintptr-t :count 8))

;(defctype texture (:struct texture-))

(defcstruct swap-texture-set-
  (:textures (:pointer (:struct texture-)))
  (:texture-count :int)
  (:current-index :int))

(defbitfield (button :unsigned-int)
  (:A #x00000001)
  (:B #x00000002)
  (:Right-Thumb #x00000004)
  (:Right-Shoulder #x00000008)
  (:X #x00000100)
  (:Y #x00000200)
  (:Left-Thumb #x00000400)
  (:Left-Shoulder #x00000800)
  ;; Navigation through DPad.
  (:Up         #x00010000)
  (:Down       #x00020000)
  (:Left       #x00040000)
  (:Right      #x00080000)
  (:Enter      #x00100000) ;; Start on XBox controller.
  (:Back       #x00200000) ;; Back on Xbox controller.
  )

(defbitfield (touch :unsigned-int)
  (:A #x00000001)
  (:B #x00000002)
  (:Right-Thumb #x00000004)
  (:Right-index-trigger #x00000010)
  (:X #x00000100)
  (:Y #x00000200)
  (:Left-Thumb #x00000400)
  (:Left-index-trigger #x00001000)
  ;; Navigation through DPad.
  (:right-index-pointing #x0020)
  (:right-thumb-up #x0040)
  (:left-index-pointing #x2000)
  (:left-thumb-up #x4000))

(defbitfield (controller-type :unsigned-int)
  (:left-touch #x01)
  (:right-touch #x02)
  (:touch #x03)
  (:all #xff))

(defcstruct input-state-
  (:time-in-seconds :double)
  (:connected-controller-types controller-type)
  (:buttons button)
  (:touches touch)
  (:index-trigger :float :count 2)
  (:hand-trigger :float :count 2)
  (:thumb-stick (:struct vector2f-) :count 2)
)

(defbitfield (init-flags- :unsigned-int)
  (:debug 1)
  (:server-optional 2)
  (:request-version 4)
  ;;(:force-no-debug 8)
  (:writable-bits #x00ffffff)
  )


;(defctype init-flags init-flags-)

(defcenum (log-level- :unsigned-int)
  (:debug 0)
  (:info 1)
  (:error 2))

;(defctype log-level log-level-)

;; typedef void (OVR_CDECL* ovrLogCallback)(uintptr_t userData, int level, const char* message);
(defctype log-callback :pointer)

(defcstruct init-params-
  (flags init-flags-)
  (requested-minor-version uint32-t)
  (log-callback log-callback)
  (user-data uintptr-t)
  (connection-timeout-ms uint32-t)
  (pad :char :count 4))

;(defctype init-params (:struct init-params-))


#++
(defcstruct distortion-vertex-
  (screen-pos-ndc vector2f)
  (time-warp-factor :float)
  (vignette-factor :float)
  (tan-eye-angles-r vector2f)
  (tan-eye-angles-g vector2f)
  (tan-eye-angles-b vector2f))
#++
(defctype distortion-vertex (:struct distortion-vertex-))
#++
(defcstruct distortion-mesh-
  (p-vertex-data (:pointer distortion-vertex))
  (p-index-data (:pointer :unsigned-short))
  (vertex-count :unsigned-int)
  (index-count :unsigned-int))
#++
(defctype distortion-mesh (:struct distortion-mesh-))
#++
(defcstruct hswdisplay-state-
  (displayed bool)
  (pad :char :count 7)
  (start-time :double)
  (dismissible-time :double))
#++
(defctype hswdisplay-state (:struct hswdisplay-state-))


;; constants for get-*
(defparameter *ovr-key*
  (alexandria:plist-hash-table
   '(:user "User" ;; string
     :name "Name" ;; string
     :gender "Gender" ;; string "Male","Female","Unknown"
     :player-height "PlayerHeight" ;; float meters
     :eye-height "EyeHeight" ;; float meters
     :ipd "IPD" ;; float meters
     :neck-to-eye-distance "NeckEyeDistance" ;; float[2] meters
     :eye-relief-dial "EyeReliefDial" ;; int
     :eye-to-nose-distance "EyeToNoseDist" ;; float[2] meters
     :max-eye-to-plate=distance "MaxEyeToPlateDist" ;; float[2] meters
     :eye-cup "EyeCup" ;; char[16] "A","B","C"
     :custom-eye-render "CustomEyeRender" ;; bool
     :camera-position-1 "CenteredFromWorld" ;;double[7] quat rot, vec3 translate
     :camera-position-2 "CenteredFromWorld2";;double[7] quat rot, vec3 translate
     :dk2-latency "DK2Latency" ;; float[5]?
     :perf-hud-mode "PerfHudMode" ;; int, see enum ovrPerfHudMode
     :debug-hud-stereo-mode "DebugHudStereoMode" ;; int, see enum ovrDebugHudStereoMode
     :debug-hud-stereo-guide-size "DebugHudStereoGuideSize2f" ;; float[2]
     :debug-hud-stereo-guide-position "DebugHudStereoGuidePosition3f" ;; float[3]
     :debug-hud-stereo-guide-yawpitchroll "DebugHudStereoGuideYawPitchRoll3f" ;; float[3]
     :debug-hud-stereo-guide-color "DebugHudStereoGuideColor4f" ;; float[4]
     )))

(define-foreign-type ovr-key ()
  ()
  (:actual-type :string)
  (:simple-parser ovr-key))
#++
(defmethod translate-to-foreign (value (type ovr-key))
  (if (symbolp value)
      (gethash value *ovr-key* "")
      value))

(defmethod expand-to-foreign-dyn (value var body (type ovr-key))
  `(with-foreign-string (,var
                         ,(cond
                            ((and (constantp value) (symbolp value))
                             (gethash value *ovr-key* ""))
                            ((and (constantp value) (stringp value))
                             value)
                            (t
                             (alexandria:once-only (value)
                               `(if (symbolp ,value)
                                    (gethash ,value *ovr-key* "")
                                    ,value)))))
     ,@body))


;; todo:
;; #define
;; #define

(defcstruct (recti-2- :class recti-2)
  (rects (:struct recti-) :count 2))

(define-cffi-translators (p v (:struct recti-2-) recti-2)
  :write `(progn
            (setf (mem-aref ,p '(:struct recti-) 0)
                  (elt ,v 0))
            (setf (mem-aref ,p '(:struct recti-) 1)
                  (elt ,v 1)))
  :read `(vector
          (mem-aref ,p '(:struct recti-) 0)
          (mem-aref ,p '(:struct recti-) 1)))


(defcstruct (posef-2- :class posef-2)
  (poses (:struct posef-) :count 2))

(define-cffi-translators (p v (:struct posef-2-) posef-2)
  :write `(progn
            (setf (mem-aref ,p '(:struct posef-) 0)
                  (elt ,v 0))
            (setf (mem-aref ,p '(:struct posef-) 1)
                  (elt ,v 1)))
  :read `(vector
          (mem-aref ,p '(:struct posef-) 0)
          (mem-aref ,p '(:struct posef-) 1)))



(defcstruct (pointer-2- :class pointer-2)
  (pointers :pointer :count 2))

(define-cffi-translators (p v (:struct pointer-2-) pointer-2)
  :write `(progn
            (setf (mem-aref ,p :pointer 0)
                  (elt ,v 0))
            (setf (mem-aref ,p :pointer 1)
                  (elt ,v 1)))
  :read `(vector
          (mem-aref ,p ':pointer 0)
          (mem-aref ,p ':pointer 1)))


(defcenum (layer-type :unsigned-int)
  (:disabled 0)
  (:eye-fov 1)
  (:eye-fov-depth 2)
  (:quad-in-world 3)
  (:quad-head-locked 4)
  (:direct 6))

(defbitfield (layer-flags :unsigned-int)
  (:high-quality 1)
  (:texture-origin-at-bottom-left 2))

(defcstruct layer-header-
  (:type layer-type)
  (:flags layer-flags))

(defcstruct layer-eye-fov-
  ;; expanding header inline to simplify access...
  (:type layer-type) ;; must be eye-fov
  (:flags layer-flags)
  (:color-texture (:struct pointer-2-)) ;; (:pointer (:struct swap-texture-set-)) :count 2
  (:viewport (:struct recti-2-))
  (:fov (:struct fov-port-2-))
  (:render-pose (:struct posef-2-)))

(defcstruct layer-eye-fov-depth-
  ;; expanding header inline to simplify access...
  (:type layer-type) ;; must be eye-fov-depth
  (:flags layer-flags)
  (:color-texture (:pointer (:struct swap-texture-set-)) :count 2)
  (:viewport (:struct recti-2-))
  (:fov (:struct fov-port-2-))
  (:render-pose (:struct posef-2-))
  (:depth-texture (:pointer (:struct swap-texture-set-)) :count 2)
  (:projection-desc (:struct timewarp-projection-desc-)))

(defcstruct layer-eye-quad-
  ;; expanding header inline to simplify access...
  (:type layer-type) ;; must be quad-in-world or quad-head-locked
  (:flags layer-flags)
  (:color-texture (:pointer (:struct swap-texture-set-)))
  (:viewport (:struct recti-))
  (:quad-pose-center (:struct posef-))
  (:quad-size (:struct vector2f-)))

(defcstruct layer-eye-direct-
  ;; expanding header inline to simplify access...
  (:type layer-type) ;; must be eye-direct
  (:flags layer-flags)
  (:color-texture (:pointer (:struct swap-texture-set-)) :count 2)
  (:viewport (:struct recti-2-)))


(defcenum (perf-hud-mode :unsigned-int)
  (:off 0)
  (:latency-timing 1) ;;Shows latency related timing info
  (:render-timing 2)  ;;Shows CPU & GPU timing info
  (:perf-headroom 3) ;;Shows available performance headroom in a "consumer-friendly" way
  (:version-info 4)) ;;Shows SDK Version Info

(defcenum (debug-hud-stereo-mode :unsigned-int)
  (:off 0)
  (:quad 1) ;;Renders Quad in world for Stereo Debugging
  (:quad-with-crosshair 2) ;;Renders Quad+crosshair in world for Stereo Debugging
  (:crosshair-at-infinity 3)) ;;Renders screen-space crosshair at infinity for Stereo Debugging



;;; from OVR_CAPI_Util.h

(defcstruct (matrix4f- :class matrix4f)
  (m :float :count 16))

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

(defbitfield (projection-modifier- :unsigned-int)
  (:none 0)
  (:right-handed 1)
  (:far-less-than-near 2)
  (:far-clip-at-infinity 4)
  (:clip-range-open-gl 8))
