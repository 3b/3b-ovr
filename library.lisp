(in-package #:3b-ovr-bindings)

(define-foreign-library libovr
  (:unix #+x86-64 (:or "libOVRRT64_0.so.5"
                       "libOVRRT64_0.so.5.0.1")
         #-x86-64 (:or "LibOVRRT32_0.so.5"
                       "LibOVRRT32_0.so.5.0.1"))
  (:windows #+x86-64 (:or "LibOVRRT64_0_5.dll")
            #-x86-64 (:or "LibOVRRT32_0_5.dll")))

(use-foreign-library libovr)

