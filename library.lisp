(in-package #:3b-ovr-bindings)

(define-foreign-library libovr
  (:unix #+x86-64 (:or "libOVRRT64_0.so.7"
                       "libOVRRT64_0.so.7.0.0")
         #-x86-64 (:or "LibOVRRT32_0.so.7"
                       "LibOVRRT32_0.so.7.0.0"))
  (:windows #+x86-64 (:or "LibOVRRT64_0_7.dll")
            #-x86-64 (:or "LibOVRRT32_0_7.dll")))

(use-foreign-library libovr)
;(close-foreign-library 'libovr)

