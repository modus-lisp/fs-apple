;;;; lib/resource-fork.lisp -- a Mac file's resource fork, written and read.
;;;;
;;;; Inside Macintosh: More Macintosh Toolbox, "Resource Manager", "Format of a
;;;; Resource Fork": a 16-byte header (data offset, map offset, data length, map
;;;; length), 240 bytes for the system, the resource data (each a 4-byte length,
;;;; then the bytes), then the map: a copy of the header, 6 reserved bytes,
;;;; attributes, the type list's and name list's offsets, the type list (count -
;;;; 1, then per type its code, count - 1 and reference list offset), the
;;;; reference lists (per resource its ID, name offset -1 for none, attributes
;;;; and 24-bit data offset, 4 reserved bytes) and the name list.
;;;;
;;;; A RESOURCE is a list (TYPE ID DATA &optional NAME ATTRIBUTES): TYPE a
;;;; 4-character string, ID a signed 16-bit integer, DATA octets, NAME a string
;;;; (Mac Roman) or NIL, ATTRIBUTES the attribute byte (default 0).

(in-package #:fs-apple)

(defun %rf-octets (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

(defun %rf-type-code (type)
  (unless (and (stringp type) (= (length type) 4))
    (error "fs-apple: a resource type is 4 characters, not ~S" type))
  type)

(defun resource-fork-write (resources &key (map-header t))
  "The resource fork (octets) holding RESOURCES, in their order; types in the
   order they first appear.  MAP-HEADER NIL leaves the map's copy of the file
   header zero (the Resource Manager does not read it; macresources writes it so)."
  (let* ((datas (mapcar (lambda (r) (coerce (third r) '(vector (unsigned-byte 8)))) resources))
         (data-len (reduce #'+ datas :key (lambda (d) (+ 4 (length d)))))
         (types (remove-duplicates (mapcar (lambda (r) (%rf-type-code (first r))) resources)
                                   :test #'string= :from-end t))
         (names (loop for r in resources when (fourth r) collect (mac-roman-encode (fourth r))))
         (names-len (reduce #'+ names :key (lambda (n) (1+ (length n)))))
         (tl-len (+ 2 (* 8 (length types))))
         (refs-len (* 12 (length resources)))
         (map-len (+ 28 tl-len refs-len names-len))
         (data-off 256) (map-off (+ data-off data-len))
         (out (%rf-octets (+ map-off map-len))))
    (flet ((header (at)
             (set-u32 out at data-off) (set-u32 out (+ at 4) map-off)
             (set-u32 out (+ at 8) data-len) (set-u32 out (+ at 12) map-len)))
      (header 0)
      ;; the data, and each resource's offset into it
      (let ((offs (loop with at = 0
                        for d in datas
                        collect at
                        do (set-u32 out (+ data-off at) (length d))
                           (replace out d :start1 (+ data-off at 4))
                           (incf at (+ 4 (length d))))))
        ;; the map: header copy, reserved, attributes 0, type list at 28, names after the refs
        (when map-header (header map-off))
        (set-u16 out (+ map-off 24) 28)
        (set-u16 out (+ map-off 26) (+ 28 tl-len refs-len))
        (let ((tl (+ map-off 28)) (ref-at tl-len) (name-at 0))
          (set-u16 out tl (logand (1- (length types)) #xFFFF))
          (loop for type in types for k from 0
                for mine = (loop for r in resources for o in offs
                                 when (string= (first r) type) collect (cons r o))
                do (let ((e (+ tl 2 (* 8 k))))
                     (set-ascii out e type 4)
                     (set-u16 out (+ e 4) (1- (length mine)))
                     (set-u16 out (+ e 6) ref-at)
                     (loop for (r . o) in mine
                           do (let ((ref (+ tl ref-at)))
                                (set-u16 out ref (logand (second r) #xFFFF))
                                (if (fourth r)
                                    (let ((n (mac-roman-encode (fourth r))) (na (+ tl tl-len refs-len name-at)))
                                      (set-u16 out (+ ref 2) name-at)
                                      (setf (aref out na) (length n))
                                      (replace out n :start1 (1+ na))
                                      (incf name-at (1+ (length n))))
                                    (set-u16 out (+ ref 2) #xFFFF))
                                (set-u32 out (+ ref 4) (logior (ash (or (fifth r) 0) 24) (logand o #xFFFFFF)))
                                (incf ref-at 12))))))))
    out))

(defun resource-fork-read (fork)
  "The resources in FORK, as RESOURCE-FORK-WRITE takes them (TYPE ID DATA NAME
   ATTRIBUTES), in type-list order.  An empty fork has none."
  (when (zerop (length fork)) (return-from resource-fork-read nil))
  (let* ((data-off (u32-ref fork 0)) (map-off (u32-ref fork 4))
         (tl (+ map-off (u16-ref fork (+ map-off 24))))
         (nl (+ map-off (u16-ref fork (+ map-off 26))))
         (ntypes (logand (1+ (u16-ref fork tl)) #xFFFF)))
    (loop for k below ntypes
          for e = (+ tl 2 (* 8 k))
          for type = (map 'string #'code-char (subseq fork e (+ e 4)))
          append (loop for j to (u16-ref fork (+ e 4))
                       for ref = (+ tl (u16-ref fork (+ e 6)) (* 12 j))
                       collect (let* ((id (let ((v (u16-ref fork ref))) (if (>= v #x8000) (- v #x10000) v)))
                                      (no (u16-ref fork (+ ref 2)))
                                      (at (+ data-off (logand (u32-ref fork (+ ref 4)) #xFFFFFF)))
                                      (data (subseq fork (+ at 4) (+ at 4 (u32-ref fork at)))))
                                 (list type id data
                                       (unless (= no #xFFFF)
                                         (mac-roman-decode fork :start (+ nl no 1) :end (+ nl no 1 (aref fork (+ nl no)))))
                                       (ldb (byte 8 24) (u32-ref fork (+ ref 4)))))))))
