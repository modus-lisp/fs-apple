;;;; lib/apm.lisp -- the Apple Partition Map: a disk's driver descriptor and map.
;;;;
;;;; A Macintosh SCSI disk starts with the DRIVER DESCRIPTOR in block 0 ('ER':
;;;; the block size, the disk's block count, and where each driver is), then the
;;;; PARTITION MAP from block 1: one 512-byte entry a block ('PM'), each naming a
;;;; partition's start, length, name and type -- the map's own entry
;;;; (Apple_partition_map) among them.  A driver partition (Apple_Driver43, say)
;;;; also carries the code's size and the BOOT CHECKSUM the ROM compares before
;;;; it trusts the code.  Inside Macintosh: Devices, "SCSI Manager"; Apple's pdisk.
;;;;
;;;; A disk is a byte vector, read and written in place, so the caller saves it.
;;;; Every field is big-endian; a 32-bit one may be a bignum on a 30-bit-fixnum
;;;; Lisp (Modus on the 68k), so nothing here assumes 32 bits fit a fixnum.
;;;;
;;;; Public API:
;;;;   (apm-boot-checksum bytes &key start end)      -> the driver checksum
;;;;   (apm-write-ddm disk block-count drivers &key block-size)
;;;;       DRIVERS: a list of (FIRST-BLOCK BLOCK-COUNT TYPE); TYPE 1 = Mac OS
;;;;   (apm-write-map disk entries)
;;;;       ENTRIES: a list of plists, in map order, block 1 on --
;;;;         :start :count :name :type, optional :status (default #x37),
;;;;         :data-start (0) :data-count (= :count), and for a driver
;;;;         :boot-size :boot-checksum :processor ("68000", say)
;;;;   (apm-read disk)  -> (values DDM ENTRIES): DDM a plist (:block-size
;;;;       :block-count :drivers), ENTRIES plists as above (also :boot-start,
;;;;       :boot-address, :boot-entry); NIL if block 0 is not a driver descriptor
;;;;
;;;; Style follows fs-fat: plain loops and AREF, any vector of byte values.

(defpackage #:fs-apple
  (:use #:cl)
  (:export #:apm-boot-checksum #:apm-write-ddm #:apm-write-map #:apm-read
           #:u16-ref #:u32-ref #:set-u16 #:set-u32 #:set-ascii #:ascii-ref))
(in-package #:fs-apple)

;;; ---- big-endian fields --------------------------------------------------------

(defun u16-ref (v i)
  (+ (* 256 (aref v i)) (aref v (+ i 1))))

(defun u32-ref (v i)
  (+ (* 65536 (u16-ref v i)) (u16-ref v (+ i 2))))

(defun set-u16 (v i x)
  (setf (aref v i) (logand (ash x -8) 255)
        (aref v (+ i 1)) (logand x 255))
  x)

(defun set-u32 (v i x)
  (set-u16 v i (logand (ash x -16) 65535))
  (set-u16 v (+ i 2) (logand x 65535))
  x)

(defun set-ascii (v i string max)
  "STRING's characters at I, at most MAX, NUL-padded to MAX."
  (dotimes (k max)
    (setf (aref v (+ i k)) (if (< k (length string)) (char-code (char string k)) 0)))
  string)

(defun ascii-ref (v i max)
  "The NUL-terminated string of at most MAX characters at I."
  (let ((n 0))
    (loop while (and (< n max) (/= 0 (aref v (+ i n)))) do (incf n))
    (let ((s (make-string n)))
      (dotimes (k n s) (setf (char s k) (code-char (aref v (+ i k))))))))

;;; ---- the driver checksum ------------------------------------------------------

(defun apm-boot-checksum (bytes &key (start 0) (end (length bytes)))
  "pmBootCksum: each byte added to a 16-bit sum that is then rotated left one
   bit; a sum of 0 is written as #xFFFF (Apple's pdisk)."
  (let ((s 0))
    (loop for i from start below end
          do (setq s (logand (+ s (aref bytes i)) #xFFFF)
                   s (logior (logand (ash s 1) #xFFFF) (ash s -15))))
    (if (zerop s) #xFFFF s)))

;;; ---- writing ------------------------------------------------------------------

(defun apm-write-ddm (disk block-count drivers &key (block-size 512))
  "Block 0: the driver descriptor ('ER'): SBBLKSIZE, SBBLKCOUNT, the device
   type and id (1, 1), data 0, then SBDRVRCOUNT and each driver's block, size
   and type."
  (dotimes (i 512) (setf (aref disk i) 0))
  (set-u16 disk 0 #x4552)
  (set-u16 disk 2 block-size)
  (set-u32 disk 4 block-count)
  (set-u16 disk 8 1)
  (set-u16 disk 10 1)
  (set-u32 disk 12 0)
  (set-u16 disk 16 (length drivers))
  (let ((at 18))
    (dolist (d drivers)
      (destructuring-bind (first count type) d
        (set-u32 disk at first)
        (set-u16 disk (+ at 4) count)
        (set-u16 disk (+ at 6) type)
        (incf at 8))))
  disk)

(defun apm-write-map (disk entries &key (block-size 512))
  "The partition map: entry I in block 1 + I ('PM', the map's entry count,
   then the partition)."
  (let ((n (length entries)))
    (loop for e in entries
          for i from 0
          for at = (* (+ 1 i) block-size)
          do (dotimes (k block-size) (setf (aref disk (+ at k)) 0))
             (set-u16 disk at #x504D)                         ; 'PM'
             (set-u32 disk (+ at 4) n)                        ; pmMapBlkCnt
             (set-u32 disk (+ at 8) (getf e :start))          ; pmPyPartStart
             (set-u32 disk (+ at 12) (getf e :count))         ; pmPartBlkCnt
             (set-ascii disk (+ at 16) (getf e :name) 32)     ; pmPartName
             (set-ascii disk (+ at 48) (getf e :type) 32)     ; pmParType
             (set-u32 disk (+ at 80) (getf e :data-start 0))  ; pmLgDataStart
             (set-u32 disk (+ at 84) (getf e :data-count (getf e :count)))
             (set-u32 disk (+ at 88) (getf e :status #x37))   ; pmPartStatus
             (when (getf e :boot-size)
               (set-u32 disk (+ at 92) (getf e :boot-start 0))     ; pmLgBootStart
               (set-u32 disk (+ at 96) (getf e :boot-size))        ; pmBootSize
               (set-u32 disk (+ at 100) (getf e :boot-address 0))  ; pmBootAddr
               (set-u32 disk (+ at 108) (getf e :boot-entry 0))    ; pmBootEntry
               (set-u32 disk (+ at 116) (getf e :boot-checksum 0)) ; pmBootCksum
               (set-ascii disk (+ at 120) (getf e :processor "") 16))))
  disk)

;;; ---- reading ------------------------------------------------------------------

(defun apm-read (disk)
  "The driver descriptor and the partition map, or NIL."
  (when (and (>= (length disk) 1024) (= (u16-ref disk 0) #x4552))
    (let* ((bs (u16-ref disk 2))
           (ddm (list :block-size bs :block-count (u32-ref disk 4)
                      :drivers (loop for k below (u16-ref disk 16)
                                     for at = (+ 18 (* 8 k))
                                     collect (list (u32-ref disk at) (u16-ref disk (+ at 4))
                                                   (u16-ref disk (+ at 6))))))
           (entries nil))
      (when (= (u16-ref disk bs) #x504D)
        (let ((n (u32-ref disk (+ bs 4))))
          (dotimes (i n)
            (let ((at (* (+ 1 i) bs)))
              (when (and (<= (+ at bs) (length disk)) (= (u16-ref disk at) #x504D))
                (push (list :start (u32-ref disk (+ at 8)) :count (u32-ref disk (+ at 12))
                            :name (ascii-ref disk (+ at 16) 32) :type (ascii-ref disk (+ at 48) 32)
                            :data-start (u32-ref disk (+ at 80)) :data-count (u32-ref disk (+ at 84))
                            :status (u32-ref disk (+ at 88))
                            :boot-start (u32-ref disk (+ at 92)) :boot-size (u32-ref disk (+ at 96))
                            :boot-address (u32-ref disk (+ at 100)) :boot-entry (u32-ref disk (+ at 108))
                            :boot-checksum (u32-ref disk (+ at 116))
                            :processor (ascii-ref disk (+ at 120) 16))
                      entries))))))
      (values ddm (nreverse entries)))))
