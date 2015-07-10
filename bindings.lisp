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

(defcstruct vector2i-
  (x :int)
  (y :int))

(defctype vector2i (:struct vector2i-))

(defcstruct sizei-
  (w :int)
  (h :int))

(defctype sizei (:struct sizei-))

(defcstruct recti-
  (pos vector2i)
  (size sizei))

(defctype recti (:struct recti-))

(defcstruct quatf-
  (x :float)
  (y :float)
  (z :float)
  (w :float))

(defctype quatf (:struct quatf-))

(defcstruct vector2f-
  (x :float)
  (y :float))

(defctype vector2f (:struct vector2f-))

(defcstruct vector3f-
  (x :float)
  (y :float)
  (z :float))

(defctype vector3f (:struct vector3f-))

(defcstruct matrix4f-
  (m :float :count 16))

(defctype matrix4f (:struct matrix4f-))

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
  (hmd-caps :unsigned-int)
  (tracking-caps :unsigned-int)
  (distortion-caps :unsigned-int)
  (default-eye-fov fov-port :count 2)
  (max-eye-fov fov-port :count 2)
  (eye-render-order eye-type :count 2)
  (resolution sizei)
  (windows-pos vector2i)
  (display-device-name :string)
  (display-id :int))

(defctype %ovrhmd::desc (:struct %ovrhmd::desc-))

(defctype ovrhmd (:pointer %ovrhmd::desc))

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
  (status-flags :unsigned-int)
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

(defcfun ("ovr_InitializeRenderingShimVersion" initialize-rendering-shim-version) bool
  (requested-minor-version :int))

(defcfun ("ovr_InitializeRenderingShim" initialize-rendering-shim) bool
)

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

(defcfun ("ovrHmd_Create" %ovrhmd::create) ovrhmd
  (index :int))

(defcfun ("ovrHmd_Destroy" %ovrhmd::destroy) :void
  (hmd ovrhmd))

(defcfun ("ovrHmd_CreateDebug" %ovrhmd::create-debug) ovrhmd
  (type %ovrhmd::type))

(defcfun ("ovrHmd_GetLastError" %ovrhmd::get-last-error) :string
  (hmd ovrhmd))

(defcfun ("ovrHmd_AttachToWindow" %ovrhmd::attach-to-window) bool
  (hmd ovrhmd)
  (window (:pointer :void))
  (dest-mirror-rect (:pointer recti))
  (source-render-target-rect (:pointer recti)))

(defcfun ("ovrHmd_GetEnabledCaps" %ovrhmd::get-enabled-caps) :unsigned-int
  (hmd ovrhmd))

(defcfun ("ovrHmd_SetEnabledCaps" %ovrhmd::set-enabled-caps) :void
  (hmd ovrhmd)
  (hmd-caps :unsigned-int))

(defcfun ("ovrHmd_ConfigureTracking" %ovrhmd::configure-tracking) bool
  (hmd ovrhmd)
  (supported-tracking-caps :unsigned-int)
  (required-tracking-caps :unsigned-int))

(defcfun ("ovrHmd_RecenterPose" %ovrhmd::recenter-pose) :void
  (hmd ovrhmd))

(defcfun ("ovrHmd_GetTrackingState" %ovrhmd::get-tracking-state) (:struct tracking-state-)
  (hmd ovrhmd)
  (abs-time :double))

(defcfun ("ovrHmd_GetFovTextureSize" %ovrhmd::get-fov-texture-size) (:struct sizei-)
  (hmd ovrhmd)
  (eye eye-type)
  (fov fov-port)
  (pixels-per-display-pixel :float))

(defcfun ("ovrHmd_ConfigureRendering" %ovrhmd::configure-rendering)  bool
  (hmd ovrhmd)
  (api-config (:pointer render-apiconfig))
  (distortion-caps :unsigned-int)
  (eye-fov-in (:pointer fov-port)) ;; fov-port :count 2 ?
  (eye-render-desc-out (:pointer eye-render-desc))) ;; eye-render-desc :count 2

(defcfun ("ovrHmd_BeginFrame" %ovrhmd::begin-frame) (:struct frame-timing-)
  (hmd ovrhmd)
  (frame-index :unsigned-int))

(defcfun ("ovrHmd_EndFrame" %ovrhmd::end-frame) :void
  (hmd ovrhmd)
  (render-pose (:pointer posef)) ;; posef :count 2
  (eye-texture (:pointer texture))) ;; texture :count 2

(defcfun ("ovrHmd_GetEyePoses" %ovrhmd::get-eye-poses) :void
  (hmd ovrhmd)
  (frame-index :unsigned-int)
  (hmd-to-eye-view-offset (:pointer vector3f)) ;; vector3f :count 2
  (out-eye-poses (:pointer posef)) ;; posef :count 2
  (out-hmd-tracking-state (:pointer tracking-state)))

(defcfun ("ovrHmd_GetHmdPosePerEye" %ovrhmd::get-hmd-pose-per-eye) (:struct posef-)
  (hmd ovrhmd)
  (eye eye-type))

(defcfun ("ovrHmd_GetRenderDesc" %ovrhmd::get-render-desc) (:struct eye-render-desc-)
  (hmd ovrhmd)
  (eye-type eye-type)
  (fov fov-port))

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

(defcfun ("ovrHmd_CreateDistortionMesh" %ovrhmd::create-distortion-mesh)
    :char ;;bool
  (hmd ovrhmd)
  (eye-type eye-type)
  (fov fov-port)
  (distortion-caps :unsigned-int)
  (mesh-data (:pointer distortion-mesh)))

(defcfun ("ovrHmd_CreateDistortionMeshDebug" %ovrhmd::create-distortion-mesh-debug) :char ;;bool
  (hmddesc ovrhmd)
  (eye-type eye-type)
  (fov fov-port)
  (distortion-caps :unsigned-int)
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
  (hmd ovrhmd)
  (frame-index :unsigned-int))

(defcfun ("ovrHmd_BeginFrameTiming" %ovrhmd::begin-frame-timing) (:struct frame-timing-)
  (hmd ovrhmd)
  (frame-index :unsigned-int))

(defcfun ("ovrHmd_EndFrameTiming" %ovrhmd::end-frame-timing) :void
  (hmd ovrhmd))

(defcfun ("ovrHmd_ResetFrameTiming" %ovrhmd::reset-frame-timing) :void
  (hmd ovrhmd)
  (frame-index :unsigned-int))

(defcfun ("ovrHmd_GetEyeTimewarpMatrices" %ovrhmd::get-eye-timewarp-matrices) :void
  (hmd ovrhmd)
  (eye eye-type)
  (render-pose posef)
  (twm-out (:pointer matrix4f))) ;; matrix4f :count 2

(defcfun ("ovrHmd_GetEyeTimewarpMatricesDebug" %ovrhmd::get-eye-timewarp-matrices-debug) :void
  (hmddesc ovrhmd)
  (eye eye-type)
  (render-pose posef)
  (player-torso-motion quatf)
  (twm-out (:pointer matrix4f)) ;; matrix4f :count 2
  (debug-timing-offset-in-seconds :double))

(defcfun ("ovr_GetTimeInSeconds" get-time-in-seconds) :double
)

(defcfun ("ovrHmd_ProcessLatencyTest" %ovrhmd::process-latency-test) bool
  (hmd ovrhmd)
  (rgb-color-out (:pointer :unsigned-char))) ;; :unsigned-char :count 3

(defcfun ("ovrHmd_GetLatencyTestResult" %ovrhmd::get-latency-test-result) (:pointer
                                                                           :char)
  (hmd ovrhmd))

(defcfun ("ovrHmd_GetLatencyTest2DrawColor" %ovrhmd::get-latency-test2draw-color) bool
  (hmddesc ovrhmd)
  (rgb-color-out (:pointer :unsigned-char))) ;; :unsigned-char :count 3

(defcstruct hswdisplay-state-
  (displayed bool)
  (pad :char :count 7)
  (start-time :double)
  (dismissible-time :double))

(defctype hswdisplay-state (:struct hswdisplay-state-))

(defcfun ("ovrHmd_GetHSWDisplayState" %ovrhmd::get-hswdisplay-state) :void
  (hmd ovrhmd)
  (has-warning-state (:pointer hswdisplay-state)))

(defcfun ("ovrHmd_DismissHSWDisplay" %ovrhmd::dismiss-hswdisplay) bool
  (hmd ovrhmd))

(defcfun ("ovrHmd_GetBool" %ovrhmd::get-bool) bool
  (hmd ovrhmd)
  (property-name :string)
  (default-val bool))

(defcfun ("ovrHmd_SetBool" %ovrhmd::set-bool) bool
  (hmd ovrhmd)
  (property-name :string)
  (value bool))

(defcfun ("ovrHmd_GetInt" %ovrhmd::get-int) :int
  (hmd ovrhmd)
  (property-name :string)
  (default-val :int))

(defcfun ("ovrHmd_SetInt" %ovrhmd::set-int) bool
  (hmd ovrhmd)
  (property-name :string)
  (value :int))

(defcfun ("ovrHmd_GetFloat" %ovrhmd::get-float) :float
  (hmd ovrhmd)
  (property-name :string)
  (default-val :float))

(defcfun ("ovrHmd_SetFloat" %ovrhmd::set-float) bool
  (hmd ovrhmd)
  (property-name :string)
  (value :float))

(defcfun ("ovrHmd_GetFloatArray" %ovrhmd::get-float-array) :unsigned-int
  (hmd ovrhmd)
  (property-name :string)
  (values (:pointer :float))
  (array-size :unsigned-int))

(defcfun ("ovrHmd_SetFloatArray" %ovrhmd::set-float-array) bool
  (hmd ovrhmd)
  (property-name :string)
  (values (:pointer :float))
  (array-size :unsigned-int))

(defcfun ("ovrHmd_GetString" %ovrhmd::get-string) :string
  (hmd ovrhmd)
  (property-name :string)
  (default-val :string))

(defcfun ("ovrHmd_SetString" %ovrhmd::set-string) bool
  (hmddesc ovrhmd)
  (property-name :string)
  (value :string))

(defcfun ("ovr_TraceMessage" trace-message) :int
  (level :int)
  (message :string))

(defcfun ("ovrHmd_StartPerfLog" %ovrhmd::start-perf-log) bool
  (hmd ovrhmd)
  (file-name :string)
  (user-data1 :string))

(defcfun ("ovrHmd_StopPerfLog" %ovrhmd::stop-perf-log) bool
  (hmd ovrhmd))

(defcenum (projection-modifier- :unsigned-int)
  (:none 0)
  (:right-handed 1)
  (:far-less-than-near 2)
  (:far-clip-at-infinity 4)
  (:clip-range-open-gl 8))

(defctype projection-modifier projection-modifier-)

(defcfun ("ovrMatrix4f_Projection" matrix4f-projection) (:struct matrix4f-)
  (fov fov-port)
  (znear :float)
  (zfar :float)
  (projection-mod-flags :unsigned-int))

(defcfun ("ovrMatrix4f_OrthoSubProjection" matrix4f-ortho-sub-projection) (:struct matrix4f-)
  (projection matrix4f)
  (ortho-scale vector2f)
  (ortho-distance :float)
  (hmd-to-eye-view-offset-x :float))

(defcfun ("ovr_WaitTillTime" wait-till-time) :double
  (abs-time :double))
