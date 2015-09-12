;; quick hack to verify struct sizes...
;; probably should just use cffi-grovel or something instead
(in-package #:3b-ovr-bindings)

(defparameter *structs*
  (with-open-file (f (asdf:system-relative-pathname '3b-ovr "types.lisp"))
    (loop with eof = (gensym)
          for n = (read f nil eof)
          do (format t "~s~%" (if (consp n) (car n) n))
          until (eq n eof)
          when (and (consp n)
                    (eq (car n) 'defcstruct))
          collect (if (consp (second n))
                      (first (second n))
                      (second n)))))

(defun c-name (f)
  (concatenate 'string
               "ovr"
               (loop with n = (symbol-name f)
                     for prev = #\- then c
                     for c across (if (alexandria:ends-with #\- n)
                                      (substitute #\_ #\- n :from-end t :count 1)
                                      n)
                     if (eql prev #\-)
                       collect (char-upcase c)
                     else unless (eql c #\-)
                            collect (char-downcase c))))

(c-name (first *structs*))

(defparameter *skip* '(char64- fov-port-2- texture-x hmd-struct
                       layer-eye-direct- layer-eye-quad-
                       render-api-config- render-api-config-header-
                       gl-config-data-))

(defmethod cffi::slot-count (a)
  1)
(with-open-file (f (asdf:system-relative-pathname '3b-ovr "check.c")
                   :direction :output :if-exists :supersede
                   :if-does-not-exist :create)
  (format f "// generated file, will be overwritten~%")
  (format f "#include <stdio.h>~%")
  (format f "#include <assert.h>~%")
  (format f "#include <OVR_CAPI.h>~%")
  (format f "#include <OVR_CAPI_GL.h>~%")
  ;;(format f "#define check(n,c,l) printf(\"%s = %d %d\\n\",n,c,l)~%")
  (format f "#define check(n,c,l) assert(c==l)~%")
  (format f "int main (int a,char** b) {~%")
  (loop for struct in *structs*
        unless (member struct *skip*)
        do (format f "  check(~s,sizeof(struct ~a),~d);~%"
                   (c-name struct)
                   (c-name struct)
                   (foreign-type-size `(:struct ,struct))))
  ;; some special cases
  (format f "  check(\"ovrGLTextureData_s.TexId\",offsetof(struct ovrGLTextureData_s,TexId),~d);~%"
         (foreign-slot-offset '(:struct texture-) :texture-id))
  (format f "ovrHmdDesc foo;")
  (loop for (i j) on '("Type" :type
                       "pad0" %ovr::pad
                       "ProductName" :product-name
                       "Manufacturer" :manufacturer
                       "VendorId" :vendor-id
                       "ProductId" :product-id
                       "SerialNumber" :serial-number
                       "FirmwareMajor" :firmware-major
                       "FirmwareMinor" :firmware-minor
                       "CameraFrustumHFovInRadians" :camera-frustum-hfov-in-radians
                       "CameraFrustumVFovInRadians" :camera-frustum-vfov-in-radians
                       "CameraFrustumNearZInMeters" :camera-frustum-near-z-in-meters
                       "CameraFrustumFarZInMeters" :camera-frustum-far-z-in-meters
                       "AvailableHmdCaps" :available-hmd-caps
                       "DefaultHmdCaps" :default-hmd-caps
                       "AvailableTrackingCaps" :available-tracking-caps
                       "DefaultTrackingCaps" :default-tracking-caps
                       "DefaultEyeFov" :default-eye-fov
                       "MaxEyeFov" :max-eye-fov
                       "Resolution" :resolution
                       "DisplayRefreshRate" :display-refresh-rate
                       "pad1" %ovr::pad1)
        by #'cddr
        do (format f "  check(\"ovrHmdDesc.~a\",offsetof(struct ovrHmdDesc_,~a),~d);~%"
                   i i (foreign-slot-offset '(:struct hmd-desc-) j))
        do (format f "  check(\"ovrHmdDesc.~a\",sizeof(foo.~a),~d);~%"
                   i i (* (foreign-type-size (foreign-slot-type '(:struct hmd-desc-) j))
                          (foreign-slot-count '(:struct hmd-desc-) j))))
  (format f "}~%"))
