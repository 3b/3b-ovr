(in-package #:3b-ovr-bindings)

(define-foreign-library libovr
  (:unix "libOVR")
  (:windows #+x86-64 (:or "LibOVRRT64_0_5.dll")
            #-x86-64 (:or "LibOVRRT32_0_5.dll")))

(use-foreign-library libovr)

