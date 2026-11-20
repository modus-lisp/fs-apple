;;;; lib/packbits.lisp -- PackBits, Apple's run-length encoding (Technical Note
;;;; TN1023; QuickDraw's _PackBits / _UnpackBits, MacPaint, resource data).
;;;;
;;;; A packed stream is a header byte N, then: 0..127, N + 1 literal bytes;
;;;; 129..255 (-127..-1), the next byte 257 - N times; 128, nothing (skipped).
;;;;
;;;; Public API:
;;;;   (packbits bytes &key start end)  -> a fresh vector of packed bytes
;;;;   (unpackbits bytes size &key start) -> (values a fresh SIZE-byte vector,
;;;;                                         the index after what was used)
;;;;
;;;; PACKBITS chooses greedily: a run of three or more equal bytes (up to 128)
;;;; becomes a repeat, anything else a literal of up to 128 bytes that stops
;;;; before the next run of three -- the choices modus's mac68k-disk.py made, so
;;;; the boot blocks' splash packs to the same bytes.

(in-package #:fs-apple)

(defun packbits (bytes &key (start 0) (end (length bytes)))
  (let ((out (make-array (+ 16 (* 2 (- end start))) :fill-pointer 0))
        (i start))
    (flet ((emit (b) (vector-push b out)))
      (loop while (< i end)
            do (let ((j i))
                 (loop while (and (< (+ j 1) end) (= (aref bytes (+ j 1)) (aref bytes i))
                                  (< (- j i) 127))
                       do (incf j))
                 (if (>= (+ (- j i) 1) 3)
                     (progn (emit (logand (- 257 (+ (- j i) 1)) 255))
                            (emit (aref bytes i))
                            (setq i (+ j 1)))
                     (let ((k i))
                       (loop while (and (< k end) (< (- k i) 128)
                                        (not (and (< (+ k 2) end)
                                                  (= (aref bytes k) (aref bytes (+ k 1)))
                                                  (= (aref bytes k) (aref bytes (+ k 2))))))
                             do (incf k))
                       (emit (- k i 1))
                       (loop for m from i below k do (emit (aref bytes m)))
                       (setq i k))))))
    (let ((v (make-array (length out))))
      (replace v out)
      v)))

(defun unpackbits (bytes size &key (start 0))
  (let ((out (make-array size :initial-element 0)) (n 0) (i start))
    (loop while (< n size)
          do (let ((h (aref bytes i)))
               (incf i)
               (cond ((< h 128)
                      (loop repeat (+ h 1)
                            do (when (< n size) (setf (aref out n) (aref bytes i)))
                               (incf n) (incf i)))
                     ((> h 128)
                      (let ((b (aref bytes i)))
                        (incf i)
                        (loop repeat (- 257 h)
                              do (when (< n size) (setf (aref out n) b))
                                 (incf n)))))))
    (values out i)))
