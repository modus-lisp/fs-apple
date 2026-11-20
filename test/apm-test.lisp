;;;; test/apm-test.lisp -- lib/apm.lisp: write a disk, read it back, and check the
;;;; driver checksum against values Apple's algorithm gives (pdisk's, as
;;;; modus's scripts/mac68k-hd.py computed them for disks MAME's ROMs booted).
;;;; Portable CL: run it with sbcl --script, or under Modus.  The last line is
;;;; "APM-TEST PASS" or "APM-TEST FAIL".

(load (merge-pathnames "../lib/apm.lisp" *load-truename*))

(defvar *checks* 0)
(defvar *fails* 0)
(defun check (ok label)
  (setq *checks* (+ *checks* 1))
  (unless ok
    (setq *fails* (+ *fails* 1))
    (format t "FAIL: ~a~%" label)))

;;; the checksum: empty input is #xFFFF; one byte; a rotation that wraps
(check (= (fs-apple:apm-boot-checksum #()) #xFFFF) "checksum of nothing")
(check (= (fs-apple:apm-boot-checksum #(1)) 2) "checksum of #(1)")
(check (= (fs-apple:apm-boot-checksum #(255 255)) #x05FA) "checksum of #(255 255)")
(let ((v (make-array 1024)))
  (dotimes (i 1024) (setf (aref v i) (logand (* i 7) 255)))
  (check (= (fs-apple:apm-boot-checksum v) #x469D) "checksum of 1024 bytes"))

;;; a disk: write the descriptor and a three-entry map, read them back
(let ((disk (make-array (* 64 512) :initial-element 0))
      (code (make-array 2048 :initial-element 0)))
  (dotimes (i 2048) (setf (aref code i) (logand (+ i 3) 255)))
  (fs-apple:apm-write-ddm disk 64 '((4 4 1)))
  (fs-apple:apm-write-map
   disk (list (list :start 1 :count 3 :name "Apple" :type "Apple_partition_map")
              (list :start 4 :count 4 :name "Macintosh" :type "Apple_Driver43" :status #x37F
                    :boot-size 2048 :processor "68000"
                    :boot-checksum (fs-apple:apm-boot-checksum code))
              (list :start 8 :count 56 :name "Modus" :type "Apple_HFS")))
  (check (and (= (aref disk 0) #x45) (= (aref disk 1) #x52)) "block 0 is 'ER'")
  (check (and (= (aref disk 512) #x50) (= (aref disk 513) #x4D)) "block 1 is 'PM'")
  (multiple-value-bind (ddm entries) (fs-apple:apm-read disk)
    (check (equal (getf ddm :drivers) '((4 4 1))) "the driver list")
    (check (= (getf ddm :block-count) 64) "the block count")
    (check (= (length entries) 3) "three entries")
    (let ((d (second entries)))
      (check (string= (getf d :type) "Apple_Driver43") "the driver's type")
      (check (string= (getf d :processor) "68000") "the driver's processor")
      (check (= (getf d :boot-size) 2048) "the driver's size")
      (check (= (getf d :status) #x37F) "the driver's status")
      (check (= (getf d :boot-checksum) (fs-apple:apm-boot-checksum code)) "the stored checksum"))
    (check (equal (mapcar (lambda (e) (list (getf e :start) (getf e :count))) entries)
                  '((1 3) (4 4) (8 56)))
           "starts and counts")))

;;; 32-bit fields above any fixnum on a 30-bit Lisp
(let ((v (make-array 8 :initial-element 0)))
  (fs-apple:set-u32 v 0 #xFEDCBA98)
  (check (= (fs-apple:u32-ref v 0) #xFEDCBA98) "a 32-bit field round trip"))

(check (null (fs-apple:apm-read (make-array 1024 :initial-element 0))) "no descriptor: NIL")

(format t "~D checks, ~D failed~%APM-TEST ~A~%" *checks* *fails* (if (zerop *fails*) "PASS" "FAIL"))
