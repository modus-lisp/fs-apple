;;;; lib/package.lisp -- the FS-APPLE package: every format's public names.

(defpackage #:fs-apple
  (:use #:cl)
  (:export ;; big-endian fields (lib/apm.lisp)
           #:u16-ref #:u32-ref #:set-u16 #:set-u32 #:set-ascii #:ascii-ref
           ;; the Apple Partition Map (lib/apm.lisp)
           #:apm-boot-checksum #:apm-write-ddm #:apm-write-map #:apm-read
           ;; PackBits (lib/packbits.lisp)
           #:packbits #:unpackbits
           ;; resource forks (lib/resource-fork.lisp)
           #:resource-fork-write #:resource-fork-read
           ;; MacBinary II (lib/macbinary.lisp; its CRC is cram's)
           #:macbinary-write #:macbinary-read
           ;; AppleDouble (lib/appledouble.lisp)
           #:appledouble-read #:appledouble-write
           ;; Disk Copy images (lib/diskcopy.lisp; NDIF's ADC is cram's)
           #:diskcopy42-read #:ndif-read
           ;; Mac Roman (lib/macroman.lisp)
           #:mac-roman-decode #:mac-roman-encode
           ;; HFS volumes (lib/hfs.lisp)
           #:hfs-read #:hfs-write #:hfs-ref #:hfs-set #:hfs-remove #:hfs-paths
           #:hfs-folder #:make-hfs-folder #:hfs-folder-p #:hfs-folder-name #:hfs-folder-entries
           #:hfs-folder-flags #:hfs-folder-x #:hfs-folder-y
           #:hfs-folder-crdate #:hfs-folder-mddate #:hfs-folder-bkdate
           #:hfs-file #:make-hfs-file #:hfs-file-p #:hfs-file-type #:hfs-file-creator #:hfs-file-flags
           #:hfs-file-x #:hfs-file-y #:hfs-file-crdate #:hfs-file-mddate #:hfs-file-bkdate
           #:hfs-file-aliastarget #:hfs-file-data #:hfs-file-rsrc))
