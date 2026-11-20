;;;; test/packbits-test.lisp -- lib/packbits.lisp: TN1023's own example, round
;;;; trips over runs, literals and their boundaries.  Portable CL (sbcl --script,
;;;; or Modus).  The last line is "PACKBITS-TEST PASS" or "PACKBITS-TEST FAIL".

(load (merge-pathnames "../lib/package.lisp" *load-truename*))
(load (merge-pathnames "../lib/packbits.lisp" *load-truename*))

(defvar *checks* 0)
(defvar *fails* 0)
(defun check (ok label)
  (setq *checks* (+ *checks* 1))
  (unless ok (setq *fails* (+ *fails* 1)) (format t "FAIL: ~a~%" label)))

(defun same (a b)
  (and (= (length a) (length b)) (every #'= a b)))

;;; TN1023's example: AA AA AA 80 00 2A AA AA AA AA 80 00 2A 22 AA AA AA AA AA
;;; AA AA AA AA AA unpacks from FE AA 02 80 00 2A FD AA 03 80 00 2A 22 F7 AA
(let ((unpacked #(#xAA #xAA #xAA #x80 #x00 #x2A #xAA #xAA #xAA #xAA #x80 #x00 #x2A #x22
                  #xAA #xAA #xAA #xAA #xAA #xAA #xAA #xAA #xAA #xAA))
      (packed #(#xFE #xAA #x02 #x80 #x00 #x2A #xFD #xAA #x03 #x80 #x00 #x2A #x22 #xF7 #xAA)))
  (check (same (fs-apple:unpackbits packed (length unpacked)) unpacked) "TN1023 unpacks")
  (check (same (fs-apple:packbits unpacked) packed) "TN1023 packs to Apple's bytes"))

;;; round trips: every length 0..300 of a pattern with runs and literals
(let ((ok t))
  (dotimes (n 301)
    (let ((v (make-array n)))
      (dotimes (i n) (setf (aref v i) (if (< (mod i 37) 20) 7 (logand (* i 13) 255))))
      (unless (same (fs-apple:unpackbits (fs-apple:packbits v) n) v) (setq ok nil))))
  (check ok "round trips, lengths 0..300"))

;;; a run of 300 (over 128: split), a literal of 300 (over 128: split)
(let ((run (make-array 300 :initial-element 9))
      (lit (make-array 300)))
  (dotimes (i 300) (setf (aref lit i) (logand i 255)))
  (check (same (fs-apple:unpackbits (fs-apple:packbits run) 300) run) "a long run")
  (check (same (fs-apple:unpackbits (fs-apple:packbits lit) 300) lit) "a long literal")
  (check (= (length (fs-apple:packbits run)) 6) "300 equal bytes pack to 3 pairs"))

;;; 128 (no-op) headers are skipped when unpacking
(check (same (fs-apple:unpackbits #(128 0 5) 1) #(5)) "a 128 header is skipped")

(format t "~D checks, ~D failed~%PACKBITS-TEST ~A~%" *checks* *fails* (if (zerop *fails*) "PASS" "FAIL"))
