;;;; test/rsrc-macbinary-test.lisp -- lib/resource-fork.lisp and lib/macbinary.lisp.
;;;; The fixed bytes are what modus's mac68k-chain.py wrote -- an application whose
;;;; MacBinary file and resource fork Mac OS 7.5.3 launched (MAME's Quadra 950) --
;;;; for the same input; then round trips, names, and a bad CRC.  Needs cram (CRAM,
;;;; else the workspace's cram/ beside fs-apple).  Portable CL (sbcl --script).  The
;;;; last line is "RSRC-MACBINARY-TEST PASS" or "RSRC-MACBINARY-TEST FAIL".

(unless (find-package "CRAM")
  (let* ((env (sb-ext:posix-getenv "CRAM"))
         (dir (if (and env (plusp (length env)))
                  (pathname (if (char= (char env (1- (length env))) #\/) env (concatenate 'string env "/")))
                  (merge-pathnames "../../cram/" *load-truename*))))
    (load (merge-pathnames "src/packages.lisp" dir))
    (load (merge-pathnames "src/crc16.lisp" dir))))
(dolist (f '("package" "macroman" "apm" "packbits" "resource-fork" "macbinary"))
  (load (merge-pathnames (format nil "../lib/~A.lisp" f) *load-truename*)))

(defvar *fails* 0)
(defun check (ok label) (unless ok (incf *fails*) (format t "FAIL: ~a~%" label)))
(defun hex (s)
  (let ((v (make-array (/ (length s) 2) :element-type '(unsigned-byte 8))))
    (dotimes (i (length v) v) (setf (aref v i) (parse-integer s :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))
(defun octets (&rest xs) (coerce xs '(vector (unsigned-byte 8))))
(defun same (a b) (and (= (length a) (length b)) (every #'= a b)))

(defparameter *resources*
  (list (list "CODE" 0 (octets 1 2 3 4 5))
        (list "SIZE" -1 (octets 0 #x80 0 1 0 0 0 0 #x80 0))
        (list "CODE" 1 (octets #x4E #x75))
        (list "ics#" -16455 (octets 0 1 2 3 4 5 6 7 8 9))))

(defparameter *fork*
  (hex (concatenate 'string
    "000001000000012b0000002b00000066" (make-string 480 :initial-element #\0)
    "000000050102030405" "0000000a00800001000000008000" "000000024e75" "0000000a00010203040506070809"
    "000001000000012b0000002b000000660000000000000000001c00660002434f44450001001a53495a4500000032"
    "696373230000003e0000ffff00000000000000000001ffff0000001700000000ffffffff0000000900000000bfb9ffff0000001d00000000")))

(let ((rf (fs-apple:resource-fork-write *resources*)))
  (check (same rf *fork*) "the resource fork is mac68k-chain.py's, byte for byte")
  ;; read back: grouped by type, in type order
  (let ((back (fs-apple:resource-fork-read rf)))
    (check (equal (mapcar (lambda (r) (list (first r) (second r))) back)
                  '(("CODE" 0) ("CODE" 1) ("SIZE" -1) ("ics#" -16455)))
           "read back: types and IDs")
    (check (every (lambda (r) (same (third r) (third (find-if (lambda (o) (and (string= (first o) (first r))
                                                                                (= (second o) (second r))))
                                                               *resources*))))
                  back)
           "read back: every resource's data"))
  ;; MacBinary II around it
  (let ((mb (fs-apple:macbinary-write "Modus" "APPL" "MDUS" (map '(vector (unsigned-byte 8)) #'char-code "data fork!") rf)))
    (check (same (subseq mb 0 128)
                 (hex (concatenate 'string
                        "00054d6f64757300000000000000000000000000000000000000000000000000"
                        "0000000000000000000000000000000000000000000000000000000000000000"
                        "004150504c4d445553000000000000000000000000000a00000191a0000000a0"
                        "000000000000000000000000000000000000000000000000000081813b940000")))
           "the MacBinary header is mac68k-chain.py's, CRC included")
    (check (= (length mb) 768) "MacBinary: forks padded to 128")
    (multiple-value-bind (name type creator data rsrc flags) (fs-apple:macbinary-read mb)
      (check (and (string= name "Modus") (string= type "APPL") (string= creator "MDUS") (= flags 0))
             "MacBinary read: name, type, creator, flags")
      (check (and (same data (map 'vector #'char-code "data fork!")) (same rsrc rf)) "MacBinary read: both forks"))
    (setf (aref mb 3) (logxor (aref mb 3) 1))
    (check (handler-case (progn (fs-apple:macbinary-read mb) nil) (error () t)) "a bad header CRC is refused")))

;; names, and Finder flags across both header bytes
(let* ((rs (list (list "STR " 128 (octets 5 104 101 108 108 111) "greeting")
                 (list "STR " 129 (octets 0))
                 (list "vers" 1 (octets 1 0) "v")))
       (back (fs-apple:resource-fork-read (fs-apple:resource-fork-write rs))))
  (check (equal (mapcar (lambda (r) (list (first r) (second r) (fourth r))) back)
                '(("STR " 128 "greeting") ("STR " 129 nil) ("vers" 1 "v")))
         "named resources round trip")
  (multiple-value-bind (n ty cr d r flags)
      (fs-apple:macbinary-read (fs-apple:macbinary-write "x" "TEXT" "ttxt" (octets) (octets) :finder-flags #x2101))
    (declare (ignore n ty cr d r))
    (check (= flags #x2101) "Finder flags, both bytes")))

(format t "~&RSRC-MACBINARY-TEST ~A~%" (if (zerop *fails*) "PASS" "FAIL"))
