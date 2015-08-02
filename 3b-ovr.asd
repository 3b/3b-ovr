(defsystem :3b-ovr
  :depends-on (alexandria sb-cga cffi-libffi)
  :serial t
  :components ((:file "package")
               (:file "library")
               (:file "util")
               (:file "types")
               (:file "bindings")))

