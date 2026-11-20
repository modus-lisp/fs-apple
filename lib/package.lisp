;;;; lib/package.lisp -- the FS-APPLE package: every format's public names.

(defpackage #:fs-apple
  (:use #:cl)
  (:export ;; big-endian fields (lib/apm.lisp)
           #:u16-ref #:u32-ref #:set-u16 #:set-u32 #:set-ascii #:ascii-ref
           ;; the Apple Partition Map (lib/apm.lisp)
           #:apm-boot-checksum #:apm-write-ddm #:apm-write-map #:apm-read
           ;; PackBits (lib/packbits.lisp)
           #:packbits #:unpackbits))
