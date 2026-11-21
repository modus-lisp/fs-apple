;;;; fs-apple.asd -- Apple disk and file formats in portable Common Lisp.
;;;;
;;;;   (asdf:load-system "fs-apple")
;;;;
;;;; One package, FS-APPLE (lib/package.lisp).  So far: lib/apm.lisp, the Apple
;;;; Partition Map (a disk's driver descriptor and partition map); lib/packbits.lisp,
;;;; PackBits; lib/resource-fork.lisp, a file's resource fork; lib/macbinary.lisp,
;;;; MacBinary II (with cram's CRC-16).  To come, each its own file: HFS volumes,
;;;; AppleDouble, BinHex.  The tests
;;;; under test/ are plain CL scripts (see each one's header).

(defsystem "fs-apple"
  :description "Pure Common Lisp readers and writers for Apple disk and file formats."
  :author "ynniv"
  :version "0.1.0"
  :depends-on ("cram")
  :serial t
  :components ((:file "lib/package")
               (:file "lib/macroman")
               (:file "lib/apm")
               (:file "lib/packbits")
               (:file "lib/resource-fork")
               (:file "lib/macbinary")
               (:file "lib/appledouble")
               (:file "lib/diskcopy")
               (:file "lib/hfs")))
