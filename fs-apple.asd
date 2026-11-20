;;;; fs-apple.asd -- Apple disk and file formats in portable Common Lisp.
;;;;
;;;;   (asdf:load-system "fs-apple")
;;;;
;;;; One package, FS-APPLE.  So far: lib/apm.lisp, the Apple Partition Map (a
;;;; disk's driver descriptor and partition map).  To come, each its own file:
;;;; HFS volumes, resource forks, MacBinary, AppleDouble, BinHex.  The tests
;;;; under test/ are plain CL scripts (see each one's header).

(defsystem "fs-apple"
  :description "Pure Common Lisp readers and writers for Apple disk and file formats."
  :author "ynniv"
  :version "0.1.0"
  :serial t
  :components ((:file "lib/apm")))
