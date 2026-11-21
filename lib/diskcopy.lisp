;;;; lib/diskcopy.lisp -- Disk Copy's disk images, as the raw disk.
;;;;
;;;; Disk Copy 4.2: an 84-byte header (the name as a Pascal string, the data and
;;;; tag sizes at 64 and 68, checksums, the format), then the data, then tags.
;;;; DISKCOPY42-READ is the data.
;;;;
;;;; Disk Copy 6, NDIF: the data fork holds the disk in chunks, the resource fork
;;;; a 'bcem' resource that maps them -- its chunk count at #x7C, then from #x80
;;;; twelve bytes a chunk: the first sector (24 bits), the type, the chunk's offset
;;;; in the data fork and its length.  Type #x83 is ADC (cram:adc-decompress), #x02
;;;; raw, #xFF the end (its sector is the disk's size); any other is zeros.

(in-package #:fs-apple)

(defun diskcopy42-read (v)
  "A Disk Copy 4.2 image's disk."
  (let ((n (u32-ref v 64)))
    (when (> (+ 84 n) (length v)) (error "fs-apple: a Disk Copy 4.2 image shorter than its data size"))
    (subseq v 84 (+ 84 n))))

(defun ndif-read (data rsrc)
  "The disk in an NDIF image: DATA its data fork, RSRC its resource fork."
  (let* ((bcem (or (third (find "bcem" (resource-fork-read rsrc) :key #'first :test #'string=))
                   (error "fs-apple: no 'bcem' resource: not an NDIF image")))
         (count (u32-ref bcem #x7C))
         (nsec (ash (u32-ref bcem (+ #x80 (* 12 (1- count)))) -8))
         (disk (make-array (* nsec 512) :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for q from #x80 by 12
          for sec = (ash (u32-ref bcem q) -8)
          for type = (aref bcem (+ q 3))
          until (= type #xFF)
          do (let* ((off (u32-ref bcem (+ q 4))) (len (u32-ref bcem (+ q 8)))
                    (next (ash (u32-ref bcem (+ q 12)) -8))
                    (room (* 512 (- next sec)))
                    (chunk (case type
                             (#x83 (cram:adc-decompress data :start off :end (+ off len)))
                             (#x02 (subseq data off (+ off len)))
                             (t nil))))
               (when chunk
                 (replace disk chunk :start1 (* 512 sec) :end2 (min (length chunk) room)))))
    disk))
