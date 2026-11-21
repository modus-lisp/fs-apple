;;;; lib/appledouble.lisp -- AppleDouble (RFC 1740's AppleSingle / AppleDouble
;;;; format, version 2): a Mac file's resource fork and Finder information in a
;;;; separate "._NAME" file beside the data, as unar -k hidden, macOS and most
;;;; archivers write it.
;;;;
;;;; Header: magic #x00051607, version #x00020000, 16 filler bytes, the entry
;;;; count; then per entry its ID, offset and length (u32 each).  Entry 2 is the
;;;; resource fork, 9 the Finder information (type, creator, Finder flags, ...).
;;;; Others (3 real name, 8 dates, ...) are kept as they are.

(in-package #:fs-apple)

(defconstant +appledouble-magic+ #x00051607)

(defun appledouble-read (v)
  "An AppleDouble file's plist: :RSRC (octets, empty when none), :TYPE and
   :CREATOR (4-character strings, \"????\" when there is no Finder info), :FLAGS
   (the Finder flags), :ENTRIES (every entry, (ID . octets))."
  (unless (= (u32-ref v 0) +appledouble-magic+)
    (error "fs-apple: not AppleDouble (magic #x~8,'0X)" (u32-ref v 0)))
  (let ((rsrc (make-array 0 :element-type '(unsigned-byte 8)))
        (type "????") (creator "????") (flags 0) (entries nil))
    (dotimes (i (u16-ref v 24))
      (let* ((e (+ 26 (* 12 i))) (id (u32-ref v e)) (off (u32-ref v (+ e 4))) (len (u32-ref v (+ e 8)))
             (bytes (subseq v off (+ off len))))
        (push (cons id bytes) entries)
        (case id
          (2 (setq rsrc bytes))
          (9 (setq type (map 'string #'code-char (subseq v off (+ off 4)))
                   creator (map 'string #'code-char (subseq v (+ off 4) (+ off 8)))
                   flags (u16-ref v (+ off 8)))))))
    (list :rsrc rsrc :type type :creator creator :flags flags :entries (nreverse entries))))

(defun appledouble-write (&key (rsrc #()) (type "????") (creator "????") (flags 0))
  "An AppleDouble file holding Finder information (TYPE, CREATOR, FLAGS; 32
   bytes, the rest zero) and RSRC, the resource fork."
  (let* ((n 2) (finfo-off (+ 26 (* 12 n))) (rsrc-off (+ finfo-off 32))
         (out (make-array (+ rsrc-off (length rsrc)) :element-type '(unsigned-byte 8) :initial-element 0)))
    (set-u32 out 0 +appledouble-magic+)
    (set-u32 out 4 #x00020000)
    (set-u16 out 24 n)
    (set-u32 out 26 9) (set-u32 out 30 finfo-off) (set-u32 out 34 32)
    (set-u32 out 38 2) (set-u32 out 42 rsrc-off) (set-u32 out 46 (length rsrc))
    (set-ascii out finfo-off type 4)
    (set-ascii out (+ finfo-off 4) creator 4)
    (set-u16 out (+ finfo-off 8) flags)
    (replace out rsrc :start1 rsrc-off)
    out))
