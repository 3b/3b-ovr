(in-package #:3b-ovr-bindings)

#++
(defcfun ("ovr_InitializeRenderingShimVersion" initialize-rendering-shim-version) bool
  (requested-minor-version :int))
#++
(defcfun ("ovr_InitializeRenderingShim" initialize-rendering-shim) bool
)


(defcfun ("ovr_Initialize" %initialize) checked-result
  (params (:pointer init-params-)))

(defun initialize (&key debug no-debug minor-version timeout-ms log-callback
                     server-optional)
  (with-foreign-object (ip '(:struct init-params-))
    (setf (foreign-slot-value ip '(:struct init-params-) 'flags)
          (remove nil (list (when debug :debug)
                            (when minor-version :request-version)
                            (when server-optional :server-optional)
                            (when no-debug :force-no-debug))))
    (setf (foreign-slot-value ip '(:struct init-params-)
                              'requested-minor-version)
          (or minor-version 0))
    (setf (foreign-slot-value ip '(:struct init-params-) 'log-callback)
          (or log-callback (null-pointer)))
    (setf (foreign-slot-value ip '(:struct init-params-) 'connection-timeout-ms)
          (or timeout-ms 0))
    (%initialize ip)))

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

(defcfun ("ovr_TraceMessage" trace-message) :int
  (level :int)
  (message :string))



(defcfun ("ovr_Create" %create) checked-result
  (hmd (:pointer hmd))
  (luid (:pointer (:struct graphics-luid-))))


(defcfun ("ovr_GetHmdDesc" get-hmd-desc) (:struct %ovrhmd::desc-)
  (hmd hmd))

(defcfun ("ovr_Destroy" destroy) :void
  (hmd hmd))

(defun create-hmd ()
  (with-foreign-objects ((p :pointer)
                         (luid '(:struct graphics-luid-)))
    (%create p luid)
    (values (cffi:mem-aref p :pointer)
            (cffi:mem-aref luid '(:struct graphics-luid-)))))

(defmacro with-hmd ((hmd-var luid-var &optional desc-var)
                    &body body)
  (let ((hmd (gensym "HMD")))
    `(multiple-value-bind (,hmd ,luid-var)
         (create-hmd)
       (declare (ignorable ,luid-var))
       (let* ((,hmd-var (if (null-pointer-p ,hmd)
                        nil
                        ,hmd))
              ,@(when desc-var
                  `((,desc-var (get-hmd-desc ,hmd)))))
         (unwind-protect
              (progn ,@body)
           (unless (null-pointer-p ,hmd)
             (destroy ,hmd)))))))

#++
(defun dump-hmd-to-plist (hmd) ;; use get-hmd-desc instead?
  ;; mostly for debugging, convert a hmd pointer into readable lisp data
  (let ((hmd (%ovrhmd::get-hmd-desc hmd)))
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
            ;; we tend to use eye as index into things, and can't pass
            ;; enums to some functions due to cffi bug anyway, so just
            ;; return it as a number
            :eye-render-order (slot eye-render-order :unsigned-int 2)
            ;; but keep the enum version around in case anyone wants it
            :eye-render-order-symbol (slot eye-render-order eye-type 2)
            :resolution (slot resolution #++(:struct sizei-))
            :window-pos (slot window-pos #+=(:struct vector2i-))
            :display-device-name (slot display-device-name)
            :display-id (slot display-id)))))

(defcfun ("ovr_GetEnabledCaps" get-enabled-caps) %ovrhmd::caps-
  (hmd hmd))

(defcfun ("ovr_SetEnabledCaps" set-enabled-caps) :void
  (hmd hmd)
  (hmd-caps %ovrhmd::caps-))

(defcfun ("ovr_ConfigureTracking" configure-tracking) checked-result
  (hmd hmd)
  (requested-tracking-caps tracking-caps-)
  (required-tracking-caps tracking-caps-))

(defcfun ("ovr_RecenterPose" recenter-pose) :void
  (hmd hmd))

(defcfun ("ovr_GetTrackingState" %get-tracking-state) (:struct tracking-state-)
  (hmd hmd)
  (abs-time :double))

(defun get-tracking-state (hmd
                                    &optional (abs-time 0d0))
  (%get-tracking-state hmd (float abs-time 0d0)))

(defcfun ("ovr_GetInputState" %get-input-state) checked-result
  (hmd hmd)
  (controller-type-mask controller-type)
  (input-state (:pointer (:struct input-state-))))

(defun %ovrhmd::get-input-state (hmd controller-types)
  (with-foreign-object (p '(:struct input-state-))
   (%get-input-state hmd controller-types p)
    (mem-aref p '(:struct input-state-))))

(defcfun ("ovr_SetControllerVibration" set-controller-vibration) checked-result
  (hmd hmd)
  (controller-type-mask controller-type)
  (frequency :float)
  (amplitude :float))

(defcfun ("ovr_DestroySwapTextureSet" destroy-swap-texture-set) :void
  (hmd hmd)
  (texture-set (:pointer (:struct swap-texture-set-))))

(defcfun ("ovr_DestroyMirrorTexture" destroy-mirror-texture) :void
  (hmd hmd)
  (mirror-texture (:pointer (:struct texture-))))

(defcfun ("ovr_GetFovTextureSize" get-fov-texture-size) (:struct sizei-)
  (hmd hmd)
  (eye :unsigned-int) ; eye-type
  (fov (:struct fov-port-))
  (pixels-per-display-pixel :float))

(defcfun ("ovr_GetRenderDesc" get-render-desc) (:struct eye-render-desc-)
  (hmd hmd)
  (eye-type :unsigned-int ) ;; eye-type
  (fov (:struct fov-port-)))

(defcfun ("ovr_SubmitFrame" %submit-frame) checked-result
  (hmd hmd)
  (frame-index :unsigned-int)
  (view-scale-desc (:pointer (:struct view-scale-desc-)))
  (layer-pointer-list (:pointer (:pointer (:struct layer-header-))))
  (layer-count :unsigned-int))

(defcfun ("ovr_GetFrameTiming" get-frame-timing) (:struct frame-timing-)
  (hmd hmd)
  (frame-index :unsigned-int))

(defcfun ("ovr_GetTimeInSeconds" get-time-in-seconds) :double
)

(defcfun ("ovr_ResetBackOfHeadTracking" reset-back-of-head-tracking) :void
  (hmd hmd))

(defcfun ("ovr_ResetMulticameraTracking" reset-multicamera-tracking) :void
  (hmd hmd))


(defcfun ("ovr_GetBool" %ovr::get-bool) bool
  (hmd hmd)
  (property-name ovr-key)
  (default-val bool))

(defcfun ("ovr_SetBool" %ovr::set-bool) bool
  (hmd hmd)
  (property-name ovr-key)
  (value bool))

(defcfun ("ovr_GetInt" %ovr::get-int) :int
  (hmd hmd)
  (property-name ovr-key)
  (default-val :int))

(defcfun ("ovr_SetInt" %ovr::set-int) bool
  (hmd hmd)
  (property-name ovr-key)
  (value :int))

(defcfun ("ovr_GetFloat" %ovr::get-float) :float
  (hmd hmd)
  (property-name ovr-key)
  (default-val :float))

(defcfun ("ovr_SetFloat" %ovr::set-float) bool
  (hmd hmd)
  (property-name ovr-key)
  (value :float))

(defcfun ("ovr_GetFloatArray" %ovr::%get-float-array) :unsigned-int
  (hmd hmd)
  (property-name ovr-key)
  (values (:pointer :float))
  (array-size :unsigned-int))

(defun get-float-array (hmd key count)
  (cffi:with-foreign-object (p :float count)
    (let ((w (%ovr::%get-float-array hmd key p count)))
      ;; possibly should return a (typed?) vector?
      ;; possibly should return COUNT values even if fewer are returned?
      ;;   (if so, should return W as 2nd value)
      (loop for i below w
            collect (mem-aref p :float i)))))

(defcfun ("ovr_SetFloatArray" %ovr::set-float-array) bool
  (hmd hmd)
  (property-name ovr-key)
  (values (:pointer :float))
  (array-size :unsigned-int))

(defcfun ("ovr_GetString" %ovr::get-string) :string
  (hmd hmd)
  (property-name ovr-key)
  (default-val :string))

(defcfun ("ovr_SetString" %ovr::set-string) bool
  (hmddesc hmd)
  (property-name ovr-key)
  (value :string))





#++
(defcfun ("ovr_ConfigureRendering" %ovr::configure-rendering)  bool
  (hmd hmd)
  (api-config (:pointer render-api-config))
  (distortion-caps distortion-caps)
  (eye-fov-in (:pointer fov-port)) ;; fov-port :count 2 ?
  (eye-render-desc-out (:pointer eye-render-desc))) ;; eye-render-desc :count 2


#++
(defun configure-rendering (hmd
                            &key
                              back-buffer-size
                              (multisample 0)
                              (distortion-caps
                               '(:overdrive :time-warp :vignette))
                              eye-fov-in
                              ;; optional settings used only on windows
                              win-dc
                              win-window
                              ;; optional settings used only on linux
                              ;; (no corresponding option for osx?)
                              linux-display)
  (declare (ignorable win-dc win-window linux-display))
  (setf back-buffer-size
        (or back-buffer-size
            (foreign-slot-value hmd '%ovr::desc 'resolution)
            #++(mem-ref (foreign-slot-value hmd '%ovr::desc 'resolution)
                        '(:struct sizei-))))
  (setf eye-fov-in
        (or eye-fov-in
            (list
             (mem-aref (foreign-slot-value hmd '%ovr::desc 'default-eye-fov)
                       '(:struct fov-port-) 0)
             (mem-aref (foreign-slot-value hmd '%ovr::desc 'default-eye-fov)
                       '(:struct fov-port-) 1))))
  (format t "~&configure rendering, fov=-~s~%" eye-fov-in)

  (with-foreign-objects ((config 'render-api-config)
                         (out '(:struct eye-render-desc-) 2)
                         (fov '(:struct fov-port-) 2))
    (setf (foreign-slot-value config 'render-api-config-header :api) :opengl)
    (setf (mem-ref (foreign-slot-pointer config
                                         'render-api-config-header
                                         :back-buffer-size)
                   '(:struct sizei-))
          back-buffer-size)
    (setf (foreign-slot-value config 'render-api-config-header :multisample)
          multisample)
    #+windows
    (progn
      (setf (foreign-slot-value config 'gl-config-data :window)
            (or win-window (cffi:null-pointer)))
      (setf (foreign-slot-value config 'gl-config-data :dc)
            (or win-dc (cffi:null-pointer))))
    #+linux
    (setf (foreign-slot-value config 'gl-config-data :x-display)
          (or linux-display (cffi:null-pointer)))
    (loop for i below 2
          do (setf (mem-aref fov '(:struct fov-port-) i)
                   (elt eye-fov-in i)))
    (format t "~&hmd ~s config ~s~% distortion caps :~s~% eye-fov-in ~s~% out ~s~%"hmd config distortion-caps
            (or eye-fov-in (cffi:null-pointer))
            out)
    (print (loop for i below 8 collect (mem-aref fov :float i)))
    (print (%ovr::get-last-error hmd))
    (print
     (%ovr::configure-rendering hmd config distortion-caps
                                   fov
                                   out))
    (print (%ovr::get-last-error hmd))
    (format t "~&got desc ~s~%"
            (loop for i below 2
                  collect (cffi:mem-aref out '(:struct eye-render-desc-) i)))
    (loop for i below 2
          collect (cffi:mem-aref out '(:struct eye-render-desc-) i))
    ))

;; linux sdk 0.5.0.1 tries to unconfigure rendering, which segfaults
;; if the window is already closed, so add a wrapper to make it easier
;; to unconfigure it sooner by hand
#++(defmacro with-configure-rendering (desc-var
                                    (hmd
                                     &rest args
                                     &key
                                       back-buffer-size
                                       (multisample 0)
                                       (distortion-caps
                                        '(:overdrive :time-warp :vignette))
                                       eye-fov-in
                                       ;; optional settings used only on windows
                                       win-dc
                                       win-window
                                       ;; optional settings used only on linux
                                       ;; (no corresponding option for osx?)
                                       linux-display)
                                    &body body)
  (declare (ignore back-buffer-size multisample distortion-caps
                   eye-fov-in win-dc win-window linux-display))
  (let ((desc-var (or desc-var (gensym))))
    (alexandria:once-only (hmd)
      `(unwind-protect
            (let ((,desc-var (configure-rendering ,hmd ,@args)))
              ,@body)
         (%ovr::configure-rendering ,hmd (cffi:null-pointer) 0
                                       (cffi:null-pointer)
                                       (cffi:null-pointer))))))

#++
(defcfun ("ovr_BeginFrame" %ovr::begin-frame) (:struct frame-timing-)
  (hmd hmd)
  (frame-index :unsigned-int))

#++
(defcfun ("ovr_EndFrame" %ovr::%end-frame) :void
  (hmd hmd)
  (render-pose (:pointer posef)) ;; posef :count 2
  (eye-texture (:pointer texture))) ;; texture :count 2

(defmacro without-fp-traps (&body body)
  #+(and sbcl (or x86 x86-64))
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
     ,@body)
  #-(and sbcl (or x86 x86-64))
  `(progn ,@body))
#++
(defparameter *foo* 10)
#++(incf *foo*)
#++
(defun end-frame (hmd render-pose eye-textures)
  (cffi:with-foreign-objects ((poses '(:struct posef-) 2)
                              (textures '(:struct texture-x) 2))
    (loop for i below 2
          do (setf (mem-aref poses '(:struct posef-) i)
                   (elt render-pose i))
             ;; using mem-ref and manual size calculation since GL struct
             ;; doesn't match size of generic struct
             (setf (mem-ref textures '(:struct texture-)
                            (* i #. (foreign-type-size '(:struct texture-x))))
                   (elt eye-textures i)))
    (without-fp-traps
     (%ovr::%end-frame hmd poses textures))))

#++
(defcfun ("ovr_GetEyePoses" %ovr::%get-eye-poses) :void
  (hmd hmd)
  (frame-index :unsigned-int)
  (hmd-to-eye-view-offset (:pointer vector3f)) ;; vector3f :count 2
  (out-eye-poses (:pointer posef)) ;; posef :count 2
  (out-hmd-tracking-state (:pointer tracking-state)))
#++
(defun get-eye-poses (hmd hmd-to-eye-view-offsets &key (frame-index 0))
  (with-foreign-objects ((poses 'posef 2)
                         (state 'tracking-state)
                         (offsets 'vector3f 2))
    (loop for i below 2
          for o = (elt hmd-to-eye-view-offsets i)
          do (setf (mem-aref offsets '(:struct vector3f-) i) o))
    (%ovr::%get-eye-poses hmd frame-index offsets poses state)
    (values (loop for i below 2
                  collect  (mem-aref poses '(:struct posef-) i))
            (mem-ref state '(:struct tracking-state-)))))

#++
(defcfun ("ovr_GetHmdPosePerEye" %ovr::get-hmd-pose-per-eye) (:struct posef-)
  (hmd hmd)
  (eye eye-type))

#++
(defcfun ("ovr_CreateDistortionMesh" %ovr::create-distortion-mesh)
    :char ;;bool
  (hmd hmd)
  (eye-type eye-type)
  (fov fov-port)
  (distortion-caps distortion-caps)
  (mesh-data (:pointer distortion-mesh)))

#++
(defcfun ("ovr_CreateDistortionMeshDebug" %ovr::create-distortion-mesh-debug) :char ;;bool
  (hmddesc hmd)
  (eye-type eye-type)
  (fov fov-port)
  (distortion-caps distortion-caps)
  (mesh-data (:pointer distortion-mesh))
  (debug-eye-relief-override-in-metres :float))
#++
(defcfun ("ovr_DestroyDistortionMesh" %ovr::destroy-distortion-mesh) :void
  (mesh-data (:pointer distortion-mesh)))
#++
(defcfun ("ovr_GetRenderScaleAndOffset" %ovr::get-render-scale-and-offset) :void
  (fov fov-port)
  (texture-size sizei)
  (render-viewport recti)
  (uv-scale-offset-out (:pointer vector2f))) ;; vector2f :count 2


#++(defcfun ("ovr_BeginFrameTiming" %ovr::begin-frame-timing) (:struct frame-timing-)
  (hmd hmd)
  (frame-index :unsigned-int))
#++
(defcfun ("ovr_EndFrameTiming" %ovr::end-frame-timing) :void
  (hmd hmd))
#++
(defcfun ("ovr_ResetFrameTiming" %ovr::reset-frame-timing) :void
  (hmd hmd)
  (frame-index :unsigned-int))
#++
(defcfun ("ovr_GetEyeTimewarpMatrices" %ovr::get-eye-timewarp-matrices) :void
  (hmd hmd)
  (eye eye-type)
  (render-pose posef)
  (twm-out (:pointer matrix4f))) ;; matrix4f :count 2
#++
(defcfun ("ovr_GetEyeTimewarpMatricesDebug" %ovr::get-eye-timewarp-matrices-debug) :void
  (hmddesc hmd)
  (eye eye-type)
  (render-pose posef)
  (player-torso-motion quatf)
  (twm-out (:pointer matrix4f)) ;; matrix4f :count 2
  (debug-timing-offset-in-seconds :double))

#++
(defcfun ("ovr_ProcessLatencyTest" %ovr::process-latency-test) bool
  (hmd hmd)
  (rgb-color-out (:pointer :unsigned-char))) ;; :unsigned-char :count 3

#++
(defcfun ("ovr_GetLatencyTestResult" %ovr::get-latency-test-result) (:pointer
                                                                           :char)
  (hmd hmd))

#++
(defcfun ("ovr_GetLatencyTest2DrawColor" %ovr::get-latency-test2draw-color) bool
  (hmddesc hmd)
  (rgb-color-out (:pointer :unsigned-char))) ;; :unsigned-char :count 3
#++
(defcfun ("ovr_GetHSWDisplayState" %ovr::get-hswdisplay-state) :void
  (hmd hmd)
  (has-warning-state (:pointer hswdisplay-state)))

#++
(defcfun ("ovr_DismissHSWDisplay" %ovr::dismiss-hswdisplay) bool
  (hmd hmd))

#++
(defcfun ("ovr_StartPerfLog" %ovr::start-perf-log) bool
  (hmd hmd)
  (file-name :string)
  (user-data1 :string))

#++
(defcfun ("ovr_StopPerfLog" %ovr::stop-perf-log) bool
  (hmd hmd))
#++
(defcfun ("ovrMatrix4f_Projection" %matrix4f-projection) (:struct matrix4f-)
  (fov (:struct fov-port-))
  (znear :float)
  (zfar :float)
  (projection-mod-flags :unsigned-int)) ;;projection-modifier
#++
(defun matrix4f-projection (fov znear zfar projection-mod-flags)
  (%matrix4f-projection fov znear zfar
                        (foreign-bitfield-value
                         'projection-modifier-
                         projection-mod-flags))
  )
#++
(defcfun ("ovrMatrix4f_OrthoSubProjection" matrix4f-ortho-sub-projection) (:struct matrix4f-)
  (projection matrix4f)
  (ortho-scale (:struct vector2f-))
  (ortho-distance :float)
  (hmd-to-eye-view-offset-x :float))
#++
(defcfun ("ovr_WaitTillTime" wait-till-time) :double
  (abs-time :double))


(defcfun ("ovr_CreateSwapTextureSetGL" %create-swap-texture-set-gl) checked-result
  (hmd hmd)
  (format %gl:enum)
  (width :int)
  (height :int)
  (texture-set (:pointer (:pointer (:struct swap-texture-set-)))))

(defun create-swap-texture-set-gl (hmd width height &key (format :srgb8-alpha8))
  (with-foreign-object (out :pointer)
    (%create-swap-texture-set-gl hmd format width height out)
    (mem-aref out :pointer)))

(defmacro with-swap-texture-set (var (hmd width height &key (format :srgb8-alpha8))
                                 &body body)
  (let ((p (gensym "SWAP-TEXTURE-SET")))
    `(let ((,p (create-swap-texture-set-gl ,hmd ,width ,height
                                           :format ,format)))
       (unwind-protect
            (let ((,var (unless (null-pointer-p ,p) ,p)))
              ,@body)
         (unless (null-pointer-p ,p)
           (destroy-swap-texture-set ,hmd ,p))))))

;; we have to keep the pointer around to destroy a swap-texture-set properly
;; not sure if it would be better to translate it to lisp then add the pointer
;; into that, or just write accessors that work directly on the pointer?
(defun swap-texture-set-count (swap-texture-set)
  (foreign-slot-value swap-texture-set '(:struct swap-texture-set-)
                      :texture-count))
(defun swap-texture-set-index (swap-texture-set)
  (foreign-slot-value swap-texture-set '(:struct swap-texture-set-)
                      :current-index))
(defun (setf swap-texture-set-index) (new swap-texture-set)
  ;; should this just MOD instead of erroring?
  (assert (< -1 new (swap-texture-set-count swap-texture-set)))
  (setf (foreign-slot-value swap-texture-set '(:struct swap-texture-set-)
                            :current-index)
        new))
(defun swap-texture-set-next-index (swap-texture-set)
  (setf (foreign-slot-value swap-texture-set
                            '(:struct swap-texture-set-)
                            :current-index)
        (mod (1+ (foreign-slot-value swap-texture-set
                                     '(:struct swap-texture-set-)
                                     :current-index))
             (swap-texture-set-count swap-texture-set))))

(defun swap-texture-set-textures (swap-texture-set)
  (coerce
   (loop for i below (swap-texture-set-count swap-texture-set)
         collect (mem-aref (foreign-slot-pointer swap-texture-set
                                                 '(:struct swap-texture-set-)
                                                 :textures)
                           '(:struct texture-)
                           i))
   'vector))

(defun texture-set-next-texture (swap-texture-set)
  (let* ((i (swap-texture-set-next-index swap-texture-set)))
    (mem-aref (foreign-slot-pointer swap-texture-set
                                    '(:struct swap-texture-set-)
                                    :textures)
              '(:struct texture-)
              i)))





(initialize :debug t)
;(format t "error = ~s~%"(get-last-error))
;(shutdown)
