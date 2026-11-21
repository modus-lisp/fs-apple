;;;; lib/macbinary.lisp -- MacBinary II: one Mac file, both forks and its Finder
;;;; information, as one byte stream for anything that is not a Mac.
;;;;
;;;; The 128-byte header (the MacBinary II standard, 1987): 0 version 0; 1 the
;;;; name's length, 2..64 the name; 65 type, 69 creator; 73 Finder flags (high
;;;; byte); 75 vertical, 77 horizontal position; 79 window or folder ID; 81
;;;; protected; 83 the data fork's length, 87 the resource fork's; 91 created,
;;;; 95 modified (Mac seconds, from 1904); 99 the Get Info comment's length; 101
;;;; Finder flags (low byte); 116 total unpacked length; 120 secondary header's
;;;; length; 122 the writer's version (129), 123 the version needed to read it
;;;; (129); 124 the CRC-16/XMODEM of bytes 0..123 (cram:crc16).  Then the data
;;;; fork and the resource fork, each padded to a multiple of 128.

(in-package #:fs-apple)

(defun macbinary-write (name type creator data rsrc &key (created #xA0000000)
                                                          (modified created) (finder-flags 0))
  "MacBinary II bytes of a file NAME (at most 63 characters), TYPE and CREATOR
   (4-character strings), DATA and RSRC its forks (octets), FINDER-FLAGS its
   Finder flags (16 bits)."
  (when (> (length name) 63) (error "fs-apple: a Mac file name is at most 63 characters"))
  (flet ((pad (n) (* 128 (ceiling n 128))))
    (let ((out (make-array (+ 128 (pad (length data)) (pad (length rsrc)))
                           :element-type '(unsigned-byte 8) :initial-element 0)))
      (setf (aref out 1) (length name))
      (set-ascii out 2 name (length name))
      (set-ascii out 65 type 4)
      (set-ascii out 69 creator 4)
      (setf (aref out 73) (ldb (byte 8 8) finder-flags)
            (aref out 101) (ldb (byte 8 0) finder-flags))
      (set-u32 out 83 (length data))
      (set-u32 out 87 (length rsrc))
      (set-u32 out 91 created)
      (set-u32 out 95 modified)
      (setf (aref out 122) 129 (aref out 123) 129)
      (set-u16 out 124 (cram:crc16 out :end 124))
      (replace out data :start1 128)
      (replace out rsrc :start1 (+ 128 (pad (length data))))
      out)))

(defun macbinary-read (v)
  "A MacBinary II file's (values name type creator data rsrc finder-flags);
   an error when its header's CRC is wrong."
  (unless (= (u16-ref v 124) (cram:crc16 v :end 124))
    (error "fs-apple: not MacBinary II (the header's CRC is wrong)"))
  (let* ((dl (u32-ref v 83)) (rl (u32-ref v 87))
         (rsrc-at (+ 128 (* 128 (ceiling dl 128)))))
    (values (map 'string #'code-char (subseq v 2 (+ 2 (aref v 1))))
            (ascii-ref v 65 4) (ascii-ref v 69 4)
            (subseq v 128 (+ 128 dl)) (subseq v rsrc-at (+ rsrc-at rl))
            (logior (ash (aref v 73) 8) (aref v 101)))))
