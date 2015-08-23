(defsystem :3b-ovr-sample
  :depends-on (3b-ovr cl-opengl glop mathkit texatl-client png-read
                      split-sequence)
  :serial t
  :components ((:file "test")))
