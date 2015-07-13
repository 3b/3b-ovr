(in-package #:3b-ovr-bindings)


(defcfun ("ovr_InitializeRenderingShimVersion" initialize-rendering-shim-version) bool
  (requested-minor-version :int))

(defcfun ("ovr_InitializeRenderingShim" initialize-rendering-shim) bool
)


(defcfun ("ovr_Initialize" %initialize) bool
  (params (:pointer init-params)))

(defun initialize (&key debug no-debug minor-version timeout-ms log-callback
                     server-optional)
  (with-foreign-object (ip 'init-params)
    (setf (foreign-slot-value ip 'init-params 'flags)
          (remove nil (list (when debug :debug)
                            (when minor-version :request-version)
                            (when server-optional :server-optional)
                            (when no-debug :force-no-debug))))
    (setf (foreign-slot-value ip 'init-params 'requested-minor-version)
          (or minor-version 0))
    (setf (foreign-slot-value ip 'init-params 'log-callback)
          (or log-callback (null-pointer)))
    (setf (foreign-slot-value ip 'init-params 'connection-timeout-ms)
          (or timeout-ms 0))
    (%initialize ip))
  )

(defcfun ("ovr_Shutdown" shutdown) :void
)

(defmacro with-ovr (initialized-p-var
                    (&rest options
                     &key debug no-debug minor-version timeout-ms log-callback
                       server-optional)
                    &body body)
  (declare (ignore debug no-debug minor-version timeout-ms log-callback
                   server-optional))
  (let ((init (gensym "INITIALIZED")))
    `(let* ((,init (initialize ,@options))
            ,@(when initialized-p-var
                `((,initialized-p-var ,init))))
       (unwind-protect
            (progn
              ,@body)
         (when ,init
           (shutdown))))))

(defcfun ("ovr_GetVersionString" get-version-string) :string
)

(defcfun ("ovrHmd_Detect" %ovrhmd::detect) :int
)

(defcfun ("ovrHmd_Create" %ovrhmd::create) hmd
  (index :int))

(defcfun ("ovrHmd_Destroy" %ovrhmd::destroy) :void
  (hmd hmd))

(defmacro with-hmd ((var &optional (index 0)) &body body)
  (let ((hmd (gensym "HMD")))
    `(let* ((,hmd (%ovrhmd::create ,index))
            (,var (if (null-pointer-p ,hmd)
                      nil
                      ,hmd)))
       (unwind-protect
            (progn ,@body)
         (unless (null-pointer-p ,hmd)
           (%ovrhmd::destroy ,hmd))))))

(defun dump-hmd-to-plist (hmd)
  ;; mostly for debugging, convert a hmd pointer into readable lisp data
  (macrolet ((slot (x &optional type n)
               (let ((hmdt ''(:struct %ovrhmd::desc-)))
                 (if n
                     `(let ((s (foreign-slot-value hmd ,hmdt ',x)))
                        (loop for i below ,n
                              collect (cffi:mem-aref s ',type i)))
                     (if type
                         `(convert-from-foreign
                           (foreign-slot-value hmd ,hmdt ',x)
                           ',type)
                         `(foreign-slot-value hmd ,hmdt ',x))))))
    (list :handle (slot handle)
          :type (slot type) ;;  %ovrhmd::type
          :product-name (slot product-name)
          :manufacturer (slot manufacturer)
          :vendor-id (slot vendor-id)
          :product-id (slot product-id)
          :serial-number (slot serial-number)
          #++(coerce (mapcar 'code-char (slot serial-number :char 24))
                                    'string)
          :firmware-major (slot firmware-major)
          :firmware-minor (slot firmware-minor)
          :camera-frustum-hfov-in-radians (slot camera-frustum-hfov-in-radians)
          :camera-frustum-vfov-in-radians (slot camera-frustum-vfov-in-radians)
          :camera-frustum-near-zin-meters (slot camera-frustum-near-zin-meters)
          :camera-frustum-far-zin-meters (slot camera-frustum-far-zin-meters)
          :hmd-caps (slot hmd-caps)
          :tracking-caps (slot tracking-caps)
          :distortion-caps (slot distortion-caps)
          :default-eye-fov (slot default-eye-fov (:struct fov-port-) 2)
          :max-eye-fov (slot max-eye-fov (:struct fov-port-) 2)
          :eye-render-order (slot eye-render-order eye-type 2)
          :resolution (slot resolution (:struct sizei-))
          :windows-pos (slot windows-pos (:struct vector2i-))
          :display-device-name (slot display-device-name)
          :display-id (slot display-id))))

(defcfun ("ovrHmd_CreateDebug" %ovrhmd::create-debug) hmd
  (type %ovrhmd::type))

(defcfun ("ovrHmd_GetLastError" %ovrhmd::get-last-error) :string
  (hmd hmd))

(defcfun ("ovrHmd_AttachToWindow" %ovrhmd::attach-to-window) bool
  (hmd hmd)
  (window (:pointer :void))
  (dest-mirror-rect (:pointer recti))
  (source-render-target-rect (:pointer recti)))

(defcfun ("ovrHmd_GetEnabledCaps" %ovrhmd::get-enabled-caps) %ovrhmd::caps
  (hmd hmd))

(defcfun ("ovrHmd_SetEnabledCaps" %ovrhmd::set-enabled-caps) :void
  (hmd hmd)
  (hmd-caps %ovrhmd::caps))

(defcfun ("ovrHmd_ConfigureTracking" %ovrhmd::configure-tracking) bool
  (hmd hmd)
  (supported-tracking-caps tracking-caps)
  (required-tracking-caps tracking-caps))

(defcfun ("ovrHmd_RecenterPose" %ovrhmd::recenter-pose) :void
  (hmd hmd))

(defcfun ("ovrHmd_GetTrackingState" %ovrhmd::%get-tracking-state) (:struct tracking-state-)
  (hmd hmd)
  (abs-time :double))

(defun %ovrhmd::get-tracking-state (hmd
                                    &optional (abs-time (get-time-in-seconds)))
  (let ((state (%ovrhmd::%get-tracking-state hmd abs-time)))
    (macrolet ((slot (x &optional type n)
                 (if n
                     `(let ((s (getf state ',x)))
                        (loop for i below ,n
                              collect (cffi:mem-aref s ',type i)))
                     (if type
                         `(convert-from-foreign
                           (getf state ',x)
                           ',type)
                         `(getf state ',x)))))
      (list
       :head-pose (slot head-pose (:struct pose-statef-))
       :camera-pose (slot camera-pose (:struct posef-))
       :leveled-camera-pose (slot leveled-camera-pose (:struct posef-))
       :raw-sensor-data (slot raw-sensor-data (:struct sensor-data-))
       :status-flags (slot status-flags status-bits)
       :last-camera-frame-counter (slot last-camera-frame-counter)
       :pad (slot pad)))))

(defcfun ("ovrHmd_GetFovTextureSize" %ovrhmd::get-fov-texture-size) (:struct sizei-)
  (hmd hmd)
  (eye eye-type)
  (fov fov-port)
  (pixels-per-display-pixel :float))

(defcfun ("ovrHmd_ConfigureRendering" %ovrhmd::configure-rendering)  bool
  (hmd hmd)
  (api-config (:pointer render-apiconfig))
  (distortion-caps distortion-caps)
  (eye-fov-in (:pointer fov-port)) ;; fov-port :count 2 ?
  (eye-render-desc-out (:pointer eye-render-desc))) ;; eye-render-desc :count 2

(defcfun ("ovrHmd_BeginFrame" %ovrhmd::begin-frame) (:struct frame-timing-)
  (hmd hmd)
  (frame-index :unsigned-int))

(defcfun ("ovrHmd_EndFrame" %ovrhmd::end-frame) :void
  (hmd hmd)
  (render-pose (:pointer posef)) ;; posef :count 2
  (eye-texture (:pointer texture))) ;; texture :count 2

(defcfun ("ovrHmd_GetEyePoses" %ovrhmd::get-eye-poses) :void
  (hmd hmd)
  (frame-index :unsigned-int)
  (hmd-to-eye-view-offset (:pointer vector3f)) ;; vector3f :count 2
  (out-eye-poses (:pointer posef)) ;; posef :count 2
  (out-hmd-tracking-state (:pointer tracking-state)))

(defcfun ("ovrHmd_GetHmdPosePerEye" %ovrhmd::get-hmd-pose-per-eye) (:struct posef-)
  (hmd hmd)
  (eye eye-type))

(defcfun ("ovrHmd_GetRenderDesc" %ovrhmd::get-render-desc) (:struct eye-render-desc-)
  (hmd hmd)
  (eye-type eye-type)
  (fov fov-port))

(defcfun ("ovrHmd_CreateDistortionMesh" %ovrhmd::create-distortion-mesh)
    :char ;;bool
  (hmd hmd)
  (eye-type eye-type)
  (fov fov-port)
  (distortion-caps distortion-caps)
  (mesh-data (:pointer distortion-mesh)))

(defcfun ("ovrHmd_CreateDistortionMeshDebug" %ovrhmd::create-distortion-mesh-debug) :char ;;bool
  (hmddesc hmd)
  (eye-type eye-type)
  (fov fov-port)
  (distortion-caps distortion-caps)
  (mesh-data (:pointer distortion-mesh))
  (debug-eye-relief-override-in-metres :float))

(defcfun ("ovrHmd_DestroyDistortionMesh" %ovrhmd::destroy-distortion-mesh) :void
  (mesh-data (:pointer distortion-mesh)))

(defcfun ("ovrHmd_GetRenderScaleAndOffset" %ovrhmd::get-render-scale-and-offset) :void
  (fov fov-port)
  (texture-size sizei)
  (render-viewport recti)
  (uv-scale-offset-out (:pointer vector2f))) ;; vector2f :count 2

(defcfun ("ovrHmd_GetFrameTiming" %ovrhmd::get-frame-timing) (:struct frame-timing-)
  (hmd hmd)
  (frame-index :unsigned-int))

(defcfun ("ovrHmd_BeginFrameTiming" %ovrhmd::begin-frame-timing) (:struct frame-timing-)
  (hmd hmd)
  (frame-index :unsigned-int))

(defcfun ("ovrHmd_EndFrameTiming" %ovrhmd::end-frame-timing) :void
  (hmd hmd))

(defcfun ("ovrHmd_ResetFrameTiming" %ovrhmd::reset-frame-timing) :void
  (hmd hmd)
  (frame-index :unsigned-int))

(defcfun ("ovrHmd_GetEyeTimewarpMatrices" %ovrhmd::get-eye-timewarp-matrices) :void
  (hmd hmd)
  (eye eye-type)
  (render-pose posef)
  (twm-out (:pointer matrix4f))) ;; matrix4f :count 2

(defcfun ("ovrHmd_GetEyeTimewarpMatricesDebug" %ovrhmd::get-eye-timewarp-matrices-debug) :void
  (hmddesc hmd)
  (eye eye-type)
  (render-pose posef)
  (player-torso-motion quatf)
  (twm-out (:pointer matrix4f)) ;; matrix4f :count 2
  (debug-timing-offset-in-seconds :double))

(defcfun ("ovr_GetTimeInSeconds" get-time-in-seconds) :double
)

(defcfun ("ovrHmd_ProcessLatencyTest" %ovrhmd::process-latency-test) bool
  (hmd hmd)
  (rgb-color-out (:pointer :unsigned-char))) ;; :unsigned-char :count 3

(defcfun ("ovrHmd_GetLatencyTestResult" %ovrhmd::get-latency-test-result) (:pointer
                                                                           :char)
  (hmd hmd))

(defcfun ("ovrHmd_GetLatencyTest2DrawColor" %ovrhmd::get-latency-test2draw-color) bool
  (hmddesc hmd)
  (rgb-color-out (:pointer :unsigned-char))) ;; :unsigned-char :count 3

(defcfun ("ovrHmd_GetHSWDisplayState" %ovrhmd::get-hswdisplay-state) :void
  (hmd hmd)
  (has-warning-state (:pointer hswdisplay-state)))

(defcfun ("ovrHmd_DismissHSWDisplay" %ovrhmd::dismiss-hswdisplay) bool
  (hmd hmd))

(defcfun ("ovrHmd_GetBool" %ovrhmd::get-bool) bool
  (hmd hmd)
  (property-name :string)
  (default-val bool))

(defcfun ("ovrHmd_SetBool" %ovrhmd::set-bool) bool
  (hmd hmd)
  (property-name :string)
  (value bool))

(defcfun ("ovrHmd_GetInt" %ovrhmd::get-int) :int
  (hmd hmd)
  (property-name :string)
  (default-val :int))

(defcfun ("ovrHmd_SetInt" %ovrhmd::set-int) bool
  (hmd hmd)
  (property-name :string)
  (value :int))

(defcfun ("ovrHmd_GetFloat" %ovrhmd::get-float) :float
  (hmd hmd)
  (property-name :string)
  (default-val :float))

(defcfun ("ovrHmd_SetFloat" %ovrhmd::set-float) bool
  (hmd hmd)
  (property-name :string)
  (value :float))

(defcfun ("ovrHmd_GetFloatArray" %ovrhmd::get-float-array) :unsigned-int
  (hmd hmd)
  (property-name :string)
  (values (:pointer :float))
  (array-size :unsigned-int))

(defcfun ("ovrHmd_SetFloatArray" %ovrhmd::set-float-array) bool
  (hmd hmd)
  (property-name :string)
  (values (:pointer :float))
  (array-size :unsigned-int))

(defcfun ("ovrHmd_GetString" %ovrhmd::get-string) :string
  (hmd hmd)
  (property-name :string)
  (default-val :string))

(defcfun ("ovrHmd_SetString" %ovrhmd::set-string) bool
  (hmddesc hmd)
  (property-name :string)
  (value :string))

(defcfun ("ovr_TraceMessage" trace-message) :int
  (level :int)
  (message :string))

(defcfun ("ovrHmd_StartPerfLog" %ovrhmd::start-perf-log) bool
  (hmd hmd)
  (file-name :string)
  (user-data1 :string))

(defcfun ("ovrHmd_StopPerfLog" %ovrhmd::stop-perf-log) bool
  (hmd hmd))

(defcfun ("ovrMatrix4f_Projection" matrix4f-projection) (:struct matrix4f-)
  (fov fov-port)
  (znear :float)
  (zfar :float)
  (projection-mod-flags projection-modifier))

(defcfun ("ovrMatrix4f_OrthoSubProjection" matrix4f-ortho-sub-projection) (:struct matrix4f-)
  (projection matrix4f)
  (ortho-scale (:struct vector2f-))
  (ortho-distance :float)
  (hmd-to-eye-view-offset-x :float))

(defcfun ("ovr_WaitTillTime" wait-till-time) :double
  (abs-time :double))
