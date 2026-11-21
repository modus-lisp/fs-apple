;;;; test/appledouble-diskcopy-test.lisp -- lib/appledouble.lisp and
;;;; lib/diskcopy.lisp on images built here from the formats: AppleDouble both
;;;; ways; a Disk Copy 4.2 image; an NDIF image with an ADC chunk, a raw chunk and
;;;; a zero chunk behind a 'bcem' map.  (Checked separately on real Disk Copy 6
;;;; images, Allegro CL 1.2.2's two floppies: byte for byte the disks modus's
;;;; ndif.py made, which machfs read as HFS.)  Needs cram (CRAM, else ../cram).
;;;; sbcl --script; the last line is "APPLEDOUBLE-DISKCOPY-TEST PASS" or "... FAIL".

(unless (find-package "CRAM")
  (let* ((env (sb-ext:posix-getenv "CRAM"))
         (dir (if (and env (plusp (length env)))
                  (pathname (if (char= (char env (1- (length env))) #\/) env (concatenate 'string env "/")))
                  (merge-pathnames "../../cram/" *load-truename*))))
    (load (merge-pathnames "src/packages.lisp" dir))
    (load (merge-pathnames "src/crc16.lisp" dir))
    (load (merge-pathnames "src/adc.lisp" dir))))
(dolist (f '("package" "apm" "packbits" "resource-fork" "macbinary" "appledouble" "diskcopy"))
  (load (merge-pathnames (format nil "../lib/~A.lisp" f) *load-truename*)))

(defvar *fails* 0)
(defun check (ok label) (unless ok (incf *fails*) (format t "FAIL: ~a~%" label)))
(defun octets (n &optional (f (lambda (i) (declare (ignore i)) 0)))
  (let ((v (make-array n :element-type '(unsigned-byte 8)))) (dotimes (i n v) (setf (aref v i) (funcall f i)))))
(defun same (a b) (and (= (length a) (length b)) (every #'= a b)))

;;; AppleDouble
(let* ((rsrc (octets 300 (lambda (i) (mod (* 5 i) 256))))
       (ad (fs-apple:appledouble-write :rsrc rsrc :type "APPL" :creator "CCL2" :flags #x2100))
       (r (fs-apple:appledouble-read ad)))
  (check (same (getf r :rsrc) rsrc) "AppleDouble: the resource fork")
  (check (and (string= (getf r :type) "APPL") (string= (getf r :creator) "CCL2") (= (getf r :flags) #x2100))
         "AppleDouble: type, creator, flags")
  (check (equal (mapcar #'car (getf r :entries)) '(9 2)) "AppleDouble: entries 9 and 2")
  (let ((bad (copy-seq ad))) (setf (aref bad 3) 0)
    (check (handler-case (progn (fs-apple:appledouble-read bad) nil) (error () t)) "AppleDouble: a bad magic refused")))
(let ((r (fs-apple:appledouble-read (fs-apple:appledouble-write))))
  (check (and (= (length (getf r :rsrc)) 0) (string= (getf r :type) "????")) "AppleDouble: empty"))

;;; Disk Copy 4.2: an 84-byte header, then the data
(let* ((disk (octets 1024 (lambda (i) (mod i 199))))
       (img (octets (+ 84 1024 12))))
  (fs-apple:set-u32 img 64 1024) (replace img disk :start1 84)
  (check (same (fs-apple:diskcopy42-read img) disk) "Disk Copy 4.2: the disk"))

;;; NDIF: sectors 0-1 ADC ("AB" x 512 = #x81 A B, then long matches of 67 from 1 back),
;;; sectors 2 raw, 3-4 zeros (type 0), 5 the end
(let* ((adc (let ((v (list #x81 65 66)))
              ;; 1022 more bytes, each a repeat of the byte 2 back: matches of 67 at offset 1
              (loop with left = 1022 while (> left 0)
                    do (let ((n (min left 67)))
                         (if (>= n 4) (setq v (append v (list (+ #x40 (- n 4)) 0 1)))
                             (setq v (append v (list (+ #x80 (1- n))) (loop for k below n collect (if (evenp k) 65 66)))))
                         (decf left n)))
              (coerce v '(vector (unsigned-byte 8)))))
       (raw (octets 512 (lambda (i) (mod (* 3 i) 256))))
       (data (concatenate '(vector (unsigned-byte 8)) adc raw))
       (bcem (octets (+ #x80 (* 12 4)))))
  (fs-apple:set-u32 bcem #x7C 4)
  (flet ((entry (k sec type off len)
           (let ((q (+ #x80 (* 12 k))))
             (fs-apple:set-u32 bcem q (logior (ash sec 8) type))
             (fs-apple:set-u32 bcem (+ q 4) off) (fs-apple:set-u32 bcem (+ q 8) len))))
    (entry 0 0 #x83 0 (length adc))
    (entry 1 2 #x02 (length adc) 512)
    (entry 2 3 #x00 0 0)
    (entry 3 5 #xFF 0 0))
  (let* ((rsrc (fs-apple:resource-fork-write (list (list "bcem" 128 bcem))))
         (disk (fs-apple:ndif-read data rsrc)))
    (check (= (length disk) (* 5 512)) "NDIF: five sectors")
    (check (every (lambda (i) (= (aref disk i) (if (evenp i) 65 66))) (loop for i below 1024 collect i)) "NDIF: the ADC chunk")
    (check (same (subseq disk 1024 1536) raw) "NDIF: the raw chunk")
    (check (every #'zerop (subseq disk 1536)) "NDIF: the zero chunk")
    (check (handler-case (progn (fs-apple:ndif-read data (fs-apple:resource-fork-write (list (list "vers" 1 #(1 0))))) nil)
             (error () t))
           "NDIF: no bcem refused")))

(format t "~&APPLEDOUBLE-DISKCOPY-TEST ~A~%" (if (zerop *fails*) "PASS" "FAIL"))
