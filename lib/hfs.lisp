;;;; lib/hfs.lisp -- HFS volumes (Inside Macintosh: Files, "Data Organization on
;;;; Volumes"): read one into folders and files, write folders and files as one.
;;;;
;;;; A volume is boot blocks (0-1), the master directory block (2), the volume
;;;; bitmap (from 3), then allocation blocks holding the extents overflow file,
;;;; the catalog file and every fork; a copy of the MDB two blocks from the end.
;;;; The catalog and extents files are B*-trees of 512-byte nodes.
;;;;
;;;; The writer is machfs's (Elliot Nunn, MIT licence; the Python library this
;;;; replaced in modus's tools), decision for decision, so that the same tree
;;;; makes the same bytes -- its volumes booted System 6.0.8 and 7.5.3: one block
;;;; per fork run, files in path order from CNID 16, aliases after everything
;;;; else (and rewritten for this volume), the catalog in HFS's name order,
;;;; index nodes of 8, an empty extents file, folders' Finder info zero, a System
;;;; 6 "Desktop" file (and, from 2 MB, an empty Desktop DB / DF), boot blocks
;;;; from the System file's 'boot' 1 naming it, its Finder and the startup
;;;; application.
;;;;
;;;; A folder is an HFS-FOLDER: its entries, (NAME . object) in order, NAME
;;;; looked up without regard to case; a volume is the root folder with the
;;;; volume's name and dates.  A file is an HFS-FILE.  Names are strings, Mac
;;;; Roman on the disk.

(in-package #:fs-apple)

(defstruct (hfs-folder (:constructor make-hfs-folder (&key name entries (flags 0) (x 0) (y 0)
                                                           (crdate 0) (mddate 0) (bkdate 0))))
  name entries (flags 0) (x 0) (y 0) (crdate 0) (mddate 0) (bkdate 0))

(defstruct (hfs-file (:constructor make-hfs-file (&key (type "????") (creator "????") (flags 0) (x 0) (y 0)
                                                       (crdate 0) (mddate 0) (bkdate 0) aliastarget
                                                       (data (make-array 0 :element-type '(unsigned-byte 8)))
                                                       (rsrc (make-array 0 :element-type '(unsigned-byte 8))))))
  (type "????") (creator "????") (flags 0) (x 0) (y 0) (crdate 0) (mddate 0) (bkdate 0) aliastarget data rsrc)

(defun hfs-entry (folder name)
  (assoc name (hfs-folder-entries folder) :test #'string-equal))

(defun hfs-ref (folder &rest path)
  "The object at PATH (names) under FOLDER, or NIL."
  (if (null path)
      folder
      (let ((e (and (hfs-folder-p folder) (hfs-entry folder (first path)))))
        (and e (apply #'hfs-ref (cdr e) (rest path))))))

(defun hfs-set (folder name object)
  "FOLDER's NAME is OBJECT: in its old place when NAME (in any case) was there,
   under the new spelling; else last.  OBJECT."
  (let ((e (hfs-entry folder name)))
    (if e
        (setf (car e) name (cdr e) object)
        (setf (hfs-folder-entries folder) (append (hfs-folder-entries folder) (list (cons name object)))))
    object))

(defun hfs-remove (folder name)
  (setf (hfs-folder-entries folder)
        (remove name (hfs-folder-entries folder) :key #'car :test #'string-equal)))

(defun hfs-paths (folder)
  "Every (PATH . object) under FOLDER, depth first, a folder before its contents."
  (loop for (name . obj) in (hfs-folder-entries folder)
        collect (cons (list name) obj)
        when (hfs-folder-p obj)
          append (mapcar (lambda (p) (cons (cons name (car p)) (cdr p))) (hfs-paths obj))))

;;; ---- bytes ------------------------------------------------------------------

(defun %hfs-octets (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))
(defun %pad-up (n factor) (* factor (ceiling n factor)))
(defun %cat (vs)
  (let ((out (%hfs-octets (reduce #'+ vs :key #'length))) (at 0))
    (dolist (v vs out) (replace out v :start1 at) (incf at (length v)))))
(defun %pstring (bytes) (%cat (list (vector (length bytes)) bytes)))
(defun %ostr (s) (map '(vector (unsigned-byte 8)) #'char-code s))   ; a type or creator

(defun %pack (&rest fields)
  "FIELDS: (:u8 x) (:u16 x) (:u32 x) (:bytes v) (:pad n) (:fixed v n), big-endian."
  (%cat (loop for (kind x n) in fields
              collect (ecase kind
                        (:u8 (vector (logand x #xFF)))
                        (:u16 (let ((v (%hfs-octets 2))) (set-u16 v 0 (logand x #xFFFF)) v))
                        (:u32 (let ((v (%hfs-octets 4))) (set-u32 v 0 (logand x #xFFFFFFFF)) v))
                        (:bytes x)
                        (:pad (%hfs-octets x))
                        (:fixed (let ((v (%hfs-octets n))) (replace v x) v))))))

;;; ---- B*-trees ----------------------------------------------------------------

(defun %node-records (buf start)
  "(values flink type records) of the node at START."
  (let* ((nrecs (u16-ref buf (+ start 10)))
         (offs (loop for i from 0 to nrecs collect (u16-ref buf (+ start 510 (* -2 i))))))
    (values (u32-ref buf start) (aref buf (+ start 8))
            (loop for (a b) on offs while b collect (subseq buf (+ start a) (+ start b))))))

(defun %btree-leaf-records (buf)
  "Every record in the leaves of the B*-tree BUF, in order."
  (multiple-value-bind (fl ty recs) (%node-records buf 0)
    (declare (ignore fl ty))
    (let* ((h (first recs)) (fnode (u32-ref h 10)) (lnode (u32-ref h 14)) (out nil))
      (when (zerop fnode) (return-from %btree-leaf-records nil))
      (loop for node = fnode then flink
            for flink = (multiple-value-bind (f ty rs) (%node-records buf (* 512 node))
                          (declare (ignore ty))
                          (dolist (r rs) (push r out))
                          f)
            until (= node lnode))
      (nreverse out))))

(defun %node-bytes (flink blink type height records)
  (let ((buf (%hfs-octets 512)) (left 14) (right 510))
    (dolist (r records)
      (when (> (+ left (length r)) (- right 2)) (return-from %node-bytes nil))
      (replace buf r :start1 left)
      (set-u16 buf right left)
      (incf left (length r)) (decf right 2))
    (set-u16 buf right left)
    (set-u32 buf 0 flink) (set-u32 buf 4 blink)
    (setf (aref buf 8) (logand type #xFF) (aref buf 9) (logand height #xFF))
    (set-u16 buf 10 (length records))
    buf))

(defun %bits (ntotal nset)
  "NTOTAL bits, the first NSET set."
  (let ((v (%hfs-octets (ceiling ntotal 8))) (nset (max 0 (min nset ntotal))))
    (dotimes (i nset v) (setf (aref v (floor i 8)) (logior (aref v (floor i 8)) (ash #x80 (- (mod i 8))))))))

(defun %pack-leaf (key value)
  (let* ((value (if (oddp (length value)) (%cat (list value #(0))) value))
         (b (%cat (list (vector (1+ (length key)) 0) key))))
    (%cat (list (if (oddp (length b)) (%cat (list b #(0))) b) value))))

(defun %index-record (rec pointer)
  (let* ((k (subseq rec 0 (1+ (aref rec 0))))
         (k (%cat (list #(#x25) (subseq k 1))))
         (k (%cat (list k (%hfs-octets (max 0 (- (1+ (aref k 0)) (length k))))))))
    (%cat (list k (%pack (list :u32 pointer))))))

(defun %make-btree (records key-len blksize)
  "machfs's make_btree: a header node, the leaves in order, index nodes of 8
   up to one root, map nodes when the header's map is not enough."
  (let* ((nodemult (floor blksize 512))
         (nodes (list (list :type 1 :height 0 :records nil)))   ; the header, filled in last
         (nrecs 0) (root 0) (depth 0))
    (flet ((last-node () (car (last nodes)))
           (add (n) (setf nodes (append nodes (list n)))))
      (dolist (kv records)
        (incf nrecs) (setq root 1 depth 1)
        (let ((packed (%pack-leaf (car kv) (cdr kv))))
          (when (= nrecs 1) (add (list :type #xFF :height 1 :records nil)))
          (let ((n (last-node)))
            (setf (getf n :records) (append (getf n :records) (list packed)))
            (unless (%node-bytes 0 0 0 0 (getf n :records))
              (setf (getf n :records) (butlast (getf n :records)))
              (add (list :type #xFF :height 1 :records (list packed)))))))
      (loop while (> (- (length nodes) root) 1)
            do (let* ((nums (loop for i from root below (length nodes) collect i))
                      (groups (loop for g on nums by (lambda (l) (nthcdr 8 l)) collect (subseq g 0 (min 8 (length g))))))
                 (setq root (length nodes) depth (1+ (getf (last-node) :height)))
                 (dolist (g groups)
                   (add (list :type 0 :height depth
                              :records (loop for p in g
                                             collect (%index-record (first (getf (nth p nodes) :records)) p)))))))
      (let ((bits-covered 2048) (mapnodes nil))
        (loop while (< bits-covered (%pad-up (length nodes) nodemult))
              do (let ((m (list :type 2 :height 1 :records (list (%hfs-octets (/ 3952 8))))))
                   (add m) (push m mapnodes) (incf bits-covered 3952)))
        (setq mapnodes (nreverse mapnodes))
        (let ((head (first nodes)))
          (loop for m in mapnodes for i from 0
                do (setf (getf m :records) (list (%bits 3952 (- (length nodes) 2048 (* i 3952))))))
          ;; one linked list per node type
          (let ((links (make-array (length nodes) :initial-element nil)) (recent (make-hash-table)))
            (loop for n in nodes for i from 0
                  do (setf (aref links i) (list (gethash (getf n :type) recent 0)))
                     (setf (gethash (getf n :type) recent) i))
            (let ((lnode (gethash #xFF recent 0)))
              (clrhash recent)
              (loop for i from (1- (length nodes)) downto 0
                    for n = (nth i nodes)
                    do (push (gethash (getf n :type) recent 0) (aref links i))
                       (setf (gethash (getf n :type) recent) i))
              (let* ((fnode (gethash #xFF recent 0))
                     (free (- (%pad-up (length nodes) nodemult) (length nodes)))
                     (nnodes (+ (length nodes) free)))
                (setf (getf head :records)
                      (list (%pack (list :u16 depth) (list :u32 root) (list :u32 nrecs) (list :u32 fnode)
                                   (list :u32 lnode) (list :u16 512) (list :u16 key-len)
                                   (list :u32 nnodes) (list :u32 free) (list :pad 76))
                            (%hfs-octets 128)
                            (%bits 2048 (length nodes))))
                (%cat (append (loop for n in nodes for i from 0
                                    collect (destructuring-bind (flink blink) (aref links i)
                                              (or (%node-bytes flink blink (getf n :type) (getf n :height) (getf n :records))
                                                  (error "fs-apple: a B*-tree node overflows"))))
                              (list (%hfs-octets (* 512 free)))))))))))))

;;; ---- reading -----------------------------------------------------------------

(defun %extents (rec &optional (at 0))
  (loop for k below 3
        for start = (u16-ref rec (+ at (* 4 k))) for count = (u16-ref rec (+ at (* 4 k) 2))
        unless (zerop count) collect (cons start count)))

(defun hfs-read (disk)
  "The volume in DISK (octets; the first 'BD' signature at a 512-byte boundary
   + 1024 is it): the root HFS-FOLDER, named as the volume, its entries the
   volume's top level.  The Desktop files go; an alias whose 'alis' names this
   volume points at its target (HFS-FILE-ALIASTARGET)."
  (let ((base (loop for i from 0 below (- (length disk) 1025) by 512
                    when (and (= (aref disk (+ i 1024)) #x42) (= (aref disk (+ i 1025)) #x44)) return i)))
    (unless base (error "fs-apple: no HFS volume (no 'BD' signature)"))
    (let* ((m (+ base 1024))
           (crdate (u32-ref disk (+ m 2))) (mddate (u32-ref disk (+ m 6))) (bkdate (u32-ref disk (+ m 64)))
           (alblksiz (u32-ref disk (+ m 20))) (alblst (u16-ref disk (+ m 28)))
           (vname (mac-roman-decode disk :start (+ m 37) :end (+ m 37 (aref disk (+ m 36)))))
           (overflow (make-hash-table :test 'equal)))
      (labels ((block-offset (b) (+ base (* 512 alblst) (* alblksiz b)))
               (fork (size extrec cnid kind)
                 (let ((nblocks (ceiling size alblksiz)) (exts (%extents extrec)) (accum 0))
                   (dolist (e exts) (incf accum (cdr e)))
                   (loop while (< accum nblocks)
                         do (let ((more (%extents (or (gethash (list cnid kind accum) overflow)
                                                      (error "fs-apple: a fork's extents are missing")))))
                              (dolist (e more) (incf accum (cdr e)))
                              (setq exts (append exts more))))
                   (let ((out (%cat (loop for (s . c) in exts
                                          collect (subseq disk (block-offset s) (block-offset (+ s c)))))))
                     (subseq out 0 size)))))
        (dolist (rec (%btree-leaf-records (fork (u32-ref disk (+ m 130)) (subseq disk (+ m 134) (+ m 146)) 3 :data)))
          (when (= (aref rec 0) 7)
            (setf (gethash (list (u32-ref rec 2) (if (= (aref rec 1) #xFF) :rsrc :data) (u16-ref rec 6)) overflow)
                  (subseq rec 8 20))))
        (let ((cnids (make-hash-table)) (children nil))
          (dolist (rec (%btree-leaf-records (fork (u32-ref disk (+ m 146)) (subseq disk (+ m 150) (+ m 162)) 4 :data)))
            (let ((klen (aref rec 0)))
              (unless (zerop klen)
                (let* ((par (u32-ref rec 2)) (nlen (aref rec 6))
                       (name (mac-roman-decode rec :start 7 :end (+ 7 nlen)))
                       (v (%pad-up (1+ klen) 2)) (d (+ v 2)))
                  (case (aref rec v)
                    (1 (let ((f (make-hfs-folder :flags (u16-ref rec (+ d 28)) :x (u16-ref rec (+ d 30))
                                                 :y (u16-ref rec (+ d 32))
                                                 :crdate (u32-ref rec (+ d 8)) :mddate (u32-ref rec (+ d 12))
                                                 :bkdate (u32-ref rec (+ d 16)))))
                         (setf (gethash (u32-ref rec (+ d 4)) cnids) f)
                         (push (list par name f) children)))
                    (2 (let* ((cnid (u32-ref rec (+ d 18)))
                              (f (make-hfs-file :type (map 'string #'code-char (subseq rec (+ d 2) (+ d 6)))
                                                :creator (map 'string #'code-char (subseq rec (+ d 6) (+ d 10)))
                                                :flags (u16-ref rec (+ d 10)) :x (u16-ref rec (+ d 12))
                                                :y (u16-ref rec (+ d 14))
                                                :crdate (u32-ref rec (+ d 42)) :mddate (u32-ref rec (+ d 46))
                                                :bkdate (u32-ref rec (+ d 50))
                                                :data (fork (u32-ref rec (+ d 24)) (subseq rec (+ d 72) (+ d 84)) cnid :data)
                                                :rsrc (fork (u32-ref rec (+ d 34)) (subseq rec (+ d 84) (+ d 96)) cnid :rsrc))))
                         (setf (gethash cnid cnids) f)
                         (push (list par name f) children))))))))
          (loop for (par name obj) in (nreverse children)
                unless (= par 1) do (hfs-set (or (gethash par cnids) (error "fs-apple: a catalog entry's folder is missing"))
                                             name obj))
          (let ((root (gethash 2 cnids)))
            (unless root (error "fs-apple: no root folder"))
            (let ((vol (make-hfs-folder :name vname :entries (hfs-folder-entries root)
                                        :crdate crdate :mddate mddate :bkdate bkdate)))
              (dolist (n '("Desktop" "Desktop DB" "Desktop DF")) (hfs-remove vol n))
              ;; aliases to this volume: their 'alis' volume creation date is ours
              (maphash (lambda (cnid obj)
                         (declare (ignore cnid))
                         (when (and (hfs-file-p obj) (logtest (hfs-file-flags obj) #x8000))
                           (ignore-errors
                            (let ((alis (third (find "alis" (resource-fork-read (hfs-file-rsrc obj))
                                                     :key #'first :test #'string=))))
                              (when (and alis (= (u32-ref alis 38) crdate))
                                (let ((target (gethash (u32-ref alis 114) cnids)))
                                  (when target (setf (hfs-file-aliastarget obj) target))))))))
                       cnids)
              vol)))))))

;;; ---- writing -----------------------------------------------------------------

(defparameter +hfs-order+
  ;; HFS's catalog name order (machfs's table): case folded, accented letters
  ;; beside their base, for Mac Roman bytes 0..255
  #(#x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0a #x0b #x0c #x0d #x0e #x0f
    #x10 #x11 #x12 #x13 #x14 #x15 #x16 #x17 #x18 #x19 #x1a #x1b #x1c #x1d #x1e #x1f
    #x20 #x22 #x23 #x28 #x29 #x2a #x2b #x2c #x2f #x30 #x31 #x32 #x33 #x34 #x35 #x36
    #x37 #x38 #x39 #x3a #x3b #x3c #x3d #x3e #x3f #x40 #x41 #x42 #x43 #x44 #x45 #x46
    #x47 #x48 #x58 #x5a #x5e #x60 #x67 #x69 #x6b #x6d #x73 #x75 #x77 #x79 #x7b #x7f
    #x8d #x8f #x91 #x93 #x96 #x98 #x9f #xa1 #xa3 #xa5 #xa8 #xaa #xab #xac #xad #xae
    #x54 #x48 #x58 #x5a #x5e #x60 #x67 #x69 #x6b #x6d #x73 #x75 #x77 #x79 #x7b #x7f
    #x8d #x8f #x91 #x93 #x96 #x98 #x9f #xa1 #xa3 #xa5 #xa8 #xaf #xb0 #xb1 #xb2 #xb3
    #x4c #x50 #x5c #x62 #x7d #x81 #x9a #x55 #x4a #x56 #x4c #x4e #x50 #x5c #x62 #x64
    #x65 #x66 #x6f #x70 #x71 #x72 #x7d #x89 #x8a #x8b #x81 #x83 #x9c #x9d #x9e #x9a
    #xb4 #xb5 #xb6 #xb7 #xb8 #xb9 #xba #x95 #xbb #xbc #xbd #xbe #xbf #xc0 #x52 #x85
    #xc1 #xc2 #xc3 #xc4 #xc5 #xc6 #xc7 #xc8 #xc9 #xca #xcb #x57 #x8c #xcc #x52 #x85
    #xcd #xce #xcf #xd0 #xd1 #xd2 #xd3 #x26 #x27 #xd4 #x20 #x4a #x4e #x83 #x87 #x87
    #xd5 #xd6 #x24 #x25 #x2d #x2e #xd7 #xd8 #xa7 #xd9 #xda #xdb #xdc #xdd #xde #xdf
    #xe0 #xe1 #xe2 #xe3 #xe4 #xe5 #xe6 #xe7 #xe8 #xe9 #xea #xeb #xec #xed #xee #xef
    #xf0 #xf1 #xf2 #xf3 #xf4 #xf5 #xf6 #xf7 #xf8 #xf9 #xfa #xfb #xfc #xfd #xfe #xff))

(defun %catalog-key< (a b)
  "A catalog key (parent ID, then the name as a Pascal string) before another:
   the parent ID's bytes, then the name's bytes through +HFS-ORDER+."
  (flet ((sortable (k) (concatenate 'list (subseq k 0 4) (map 'list (lambda (c) (aref +hfs-order+ c)) (subseq k 5)))))
    (let ((x (sortable a)) (y (sortable b)))
      (loop (cond ((null y) (return nil)) ((null x) (return t))
                  ((< (first x) (first y)) (return t)) ((> (first x) (first y)) (return nil)))
            (pop x) (pop y)))))

(defun %encode-name (name max)
  (let ((v (mac-roman-encode name)))
    (when (or (< (length v) 1) (> (length v) max) (find (char-code #\:) v))
      (error "fs-apple: a bad HFS name: ~S" name))
    v))

(defun %bb-name (name)
  (let ((p (%pstring (%encode-name name 31))))
    ;; padded with spaces: machfs's bytes.ljust(16)
    (if (< (length p) 16) (%cat (list p (make-array (- 16 (length p)) :element-type '(unsigned-byte 8) :initial-element 32))) p)))

(defun %common-prefix (a b)
  "machfs's _common_prefix: where A and B first differ, or 0 when they do not."
  (loop for x in a for y in b for i from 0 unless (string= x y) do (return-from %common-prefix i))
  0)

(defun hfs-write (volume &key (size (* 800 1024)) (align 512) (desktopdb t) startapp)
  "VOLUME (an HFS-FOLDER, its name the volume's) as an HFS volume of SIZE bytes
   (a multiple of 512, at least 400 KB), allocation blocks a multiple of ALIGN.
   DESKTOPDB: the Desktop file(s) System 6's Finder wants (no Desktop DB under
   System 7's).  STARTAPP: the path (names) of the application a System's boot
   blocks launch.  An error when it does not fit."
  (when (or (< align 512) (plusp (mod align 512))) (error "fs-apple: ALIGN must be a multiple of 512"))
  (when (or (< size (* 400 1024)) (plusp (mod size 512))) (error "fs-apple: SIZE must be a multiple of 512, at least 400 KB"))
  (let* ((vname (hfs-folder-name volume))
         (drvn (%encode-name vname 27))
         (drcrdate (hfs-folder-crdate volume))
         (alblksiz (let ((r align)) (loop while (> (- size (* 6 512)) (* r 65536)) do (incf r align)) r))
         (bitmap-blks (let ((n 0))
                        (loop while (> (floor (- size (* (+ 5 n) 512)) alblksiz) (* n 512 8)) do (incf n))
                        (loop while (plusp (mod (* (+ 3 n) 512) align)) do (incf n))
                        n))
         (nmalblks (floor (- size (* (+ 5 bitmap-blks) 512)) alblksiz))
         (blocks nil) (nblocks 0))
    (labels ((accumulate (bytes)
               (let ((start nblocks))
                 (loop for i from 0 below (length bytes) by alblksiz
                       do (let ((b (%hfs-octets alblksiz)))
                            (replace b bytes :start2 i :end2 (min (length bytes) (+ i alblksiz)))
                            (push b blocks) (incf nblocks)))
                 (when (> nblocks nmalblks) (error "fs-apple: the volume is full"))
                 (cons start (- nblocks start)))))
      (let* ((xt (%make-btree nil 7 alblksiz))
             (xt-ext (accumulate xt))
             (desktop (when desktopdb
                        (append (list (cons "Desktop"
                                            (make-hfs-file :type "FNDR" :creator "ERIK" :flags #x4000
                                                           :rsrc (resource-fork-write
                                                                  (list (list "STR " 0 (%cat (list #(10) (%ostr "Finder 1.0")))))
                                                                  :map-header nil))))
                                (when (>= size (* 2 1024 1024))
                                  (list (cons "Desktop DB" (make-hfs-file :type "BTFL" :creator "DMGR" :flags #x4000
                                                                          :data (%make-btree nil 37 alblksiz)))
                                        (cons "Desktop DF" (make-hfs-file :type "DTFL" :creator "DMGR" :flags #x4000)))))))
             ;; the root as written: its entries, the Desktop files last (replacing any)
             (root-entries (let ((es (copy-list (hfs-folder-entries volume))))
                             (dolist (d desktop es)
                               (let ((e (assoc (car d) es :test #'string-equal)))
                                 (if e (setf es (substitute d e es)) (setf es (append es (list d))))))))
             (root (make-hfs-folder :name vname :entries root-entries))
             (paths (hfs-paths root))
             (ordered (append (remove-if (lambda (p) (and (hfs-file-p (cdr p)) (hfs-file-aliastarget (cdr p)))) paths)))
             (aliases (remove-if-not (lambda (p) (and (hfs-file-p (cdr p)) (hfs-file-aliastarget (cdr p)))) paths))
             (wraps nil)                                 ; (path cnid obj type creator data rsrc dext rext), in order
             (by-path (make-hash-table :test 'equal))
             (by-obj (make-hash-table :test 'eq))
             (next-cnid 16) (bootblocks (%hfs-octets 1024)) (system-cnid 0) (startapp-cnid 0))
        (setf (gethash nil by-path) (list nil 1 nil) (gethash (list vname) by-path) (list (list vname) 2 volume))
        ;; aliases after everything else, in machfs's order (each pass from the end)
        (let ((approved (make-hash-table :test 'eq)) (pending (reverse aliases)) (later nil))
          (dolist (p ordered) (setf (gethash (cdr p) approved) (car p)))
          (loop while pending
                do (let ((progress nil) (left nil))
                     (dolist (p pending)
                       (let ((target (gethash (hfs-file-aliastarget (cdr p)) approved)))
                         (if target
                             (progn (push (list p target) later) (setf (gethash (cdr p) approved) (car p)) (setq progress t))
                             (push p left))))
                     (setq pending (nreverse left))
                     (unless progress (return))))
          (setq ordered (append (mapcar (lambda (p) (list p nil)) ordered) (nreverse later))))
        (dolist (item ordered)
          (destructuring-bind ((rel . obj) aliastarget) item
            (let* ((path (cons vname rel)) (cnid next-cnid)
                   (w (list :path path :cnid cnid :obj obj)))
              (incf next-cnid)
              (setf (gethash path by-path) (list path cnid obj) (gethash obj by-obj) cnid)
              (when (and (hfs-file-p obj) (string-equal (hfs-file-type obj) "ZSYS"))
                (ignore-errors
                 (let* ((parent (third (gethash (butlast path) by-path)))
                        (fndr (car (find-if (lambda (e) (and (hfs-file-p (cdr e)) (string= (hfs-file-type (cdr e)) "FNDR")))
                                            (hfs-folder-entries (if (eq parent volume) root parent)))))
                        (boot1 (third (find-if (lambda (r) (and (string= (first r) "boot") (= (second r) 1)))
                                               (resource-fork-read (hfs-file-rsrc obj))))))
                   (when (and fndr boot1 (= (length boot1) 1024))
                     (let ((bb (copy-seq boot1)) (sn (%bb-name (car (last path)))) (fn (%bb-name fndr)))
                       (when (and (= (length sn) 16) (= (length fn) 16))
                         (replace bb sn :start1 #x0A) (replace bb fn :start1 #x1A)
                         (setq bootblocks bb system-cnid (second (gethash (butlast path) by-path)))))))))
              (when (and (hfs-file-p obj) startapp (equal rel startapp))
                (setq startapp-cnid (second (gethash (butlast path) by-path))))
              (when (hfs-file-p obj)
                (setf (getf w :type) (hfs-file-type obj) (getf w :creator) (hfs-file-creator obj)
                      (getf w :data) (hfs-file-data obj) (getf w :rsrc) (hfs-file-rsrc obj)))
              (when aliastarget
                (let* ((tpath (cons vname aliastarget))
                       (target (third (gethash tpath by-path)))
                       (ttype (and (hfs-file-p target) (hfs-file-type target))))
                  (cond ((hfs-folder-p target) (setf (getf w :creator) "MACS" (getf w :type) "fdrp"))
                        (t (setf (getf w :creator) (hfs-file-creator target)
                                 (getf w :type) (if (string= ttype "APPL") "adrp" ttype))))
                  (setf (getf w :data) (%hfs-octets 0))
                  (let* ((from (%common-prefix path tpath))
                         (alis (%pack (list :fixed #() 4) (list :u16 9999) (list :u16 2)
                                      (list :u16 (if (hfs-folder-p target) 1 0))
                                      (list :fixed (%pstring drvn) 28) (list :u32 drcrdate) (list :bytes (%ostr "BD"))
                                      (list :u16 5) (list :u32 (second (gethash (butlast tpath) by-path)))
                                      (list :fixed (%pstring (%encode-name (car (last tpath)) 31)) 64)
                                      (list :u32 (second (gethash tpath by-path)))
                                      (list :u32 (if (hfs-folder-p target) (hfs-folder-crdate target) (hfs-file-crdate target)))
                                      (list :fixed (if (hfs-file-p target) (%ostr (hfs-file-type target)) #()) 4)
                                      (list :fixed (if (hfs-file-p target) (%ostr (hfs-file-creator target)) #()) 4)
                                      (list :u16 (- (length path) from)) (list :u16 (- (length tpath) from))
                                      (list :u32 0) (list :u16 0) (list :pad 10))))
                    (flet ((app (kind bytes)
                             (when (oddp (length alis)) (setq alis (%cat (list alis #(0)))))
                             (setq alis (%cat (list alis (%pack (list :u16 kind) (list :u16 (length bytes))) bytes)))
                             (when (oddp (length alis)) (setq alis (%cat (list alis #(0)))))))
                      (app 0 (mac-roman-encode (car (last (butlast tpath)))))
                      (app 2 (mac-roman-encode (format nil "~{~A~^:~}" tpath)))
                      (app -1 #()))
                    (set-u16 alis 4 (length alis))
                    (setf (getf w :rsrc) (resource-fork-write (list (list "alis" 0 alis (car (last path)))) :map-header nil)))))
              (when (hfs-file-p obj)
                (setf (getf w :dext) (if (plusp (length (getf w :data))) (accumulate (getf w :data)) (cons 0 0))
                      (getf w :rext) (if (plusp (length (getf w :rsrc))) (accumulate (getf w :rsrc)) (cons 0 0))))
              (push w wraps))))
        (setq wraps (nreverse wraps))
        ;; the catalog
        (let ((catalog nil) (filcnt 0) (dircnt -1))
          (flet ((add-object (path cnid obj w)
                   (let* ((pname (%pstring (%encode-name (car (last path)) 31)))
                          (parent (second (gethash (butlast path) by-path)))
                          (key (%cat (list (%pack (list :u32 parent)) pname))))
                     (if (hfs-file-p obj)
                         (destructuring-bind (ds . dc) (getf w :dext)
                           (destructuring-bind (rs . rc) (getf w :rext)
                             (incf filcnt)
                             (push (cons key (%pack (list :u8 2) (list :pad 1) (list :u8 2) (list :u8 0)
                                                    (list :fixed (%ostr (getf w :type)) 4) (list :fixed (%ostr (getf w :creator)) 4)
                                                    (list :u16 (hfs-file-flags obj)) (list :u16 (hfs-file-x obj))
                                                    (list :u16 (hfs-file-y obj)) (list :pad 2)   ; FInfo: 16 bytes
                                                    (list :u32 cnid)
                                                    (list :u16 ds) (list :u32 (length (getf w :data)))
                                                    (list :u32 (%pad-up (length (getf w :data)) alblksiz))
                                                    (list :u16 rs) (list :u32 (length (getf w :rsrc)))
                                                    (list :u32 (%pad-up (length (getf w :rsrc)) alblksiz))
                                                    (list :u32 (hfs-file-crdate obj)) (list :u32 (hfs-file-mddate obj))
                                                    (list :u32 (hfs-file-bkdate obj)) (list :pad 16) (list :u16 0)
                                                    (list :u16 ds) (list :u16 dc) (list :pad 8)
                                                    (list :u16 rs) (list :u16 rc) (list :pad 8) (list :pad 4)))
                                   catalog)))
                         (progn
                           (incf dircnt)
                           (push (cons key (%pack (list :u8 1) (list :pad 1) (list :u16 0)
                                                  (list :u16 (length (hfs-folder-entries (if (eq obj volume) root obj))))
                                                  (list :u32 cnid) (list :u32 (hfs-folder-crdate obj))
                                                  (list :u32 (hfs-folder-mddate obj)) (list :u32 (hfs-folder-bkdate obj))
                                                  (list :pad 32) (list :pad 16)))
                                 catalog)))
                     (push (cons (%pack (list :u32 cnid) (list :u8 0))
                                 (%cat (list (%pack (list :u8 (if (hfs-file-p obj) 4 3)) (list :pad 9) (list :u32 parent)) pname)))
                           catalog))))
            (add-object (list vname) 2 volume nil)
            (dolist (w wraps) (add-object (getf w :path) (getf w :cnid) (getf w :obj) w)))
          (let* ((catalog (stable-sort (nreverse catalog) #'%catalog-key< :key #'car))
                 (ct (%make-btree catalog 37 alblksiz))
                 (ct-ext (accumulate ct)))
            ;; the startup application, in boot blocks a System wrote
            (when (and (plusp system-cnid) (plusp startapp-cnid))
              (let ((n (ignore-errors (%bb-name (car (last startapp))))))
                (if (and n (= (length n) 16)) (replace bootblocks n :start1 #x5A) (setq startapp-cnid 0))))
            (let* ((bitmap (%bits (* bitmap-blks 512 8) nblocks))
                   (nmfls (count-if (lambda (e) (hfs-file-p (cdr e))) (hfs-folder-entries volume)))
                   (nmrtdirs (count-if (lambda (e) (hfs-folder-p (cdr e))) (hfs-folder-entries volume)))
                   (fndr (%pack (list :u32 system-cnid) (list :u32 startapp-cnid) (list :u32 startapp-cnid) (list :pad 20)))
                   (vib (%pack (list :bytes (%ostr "BD")) (list :u32 drcrdate) (list :u32 (hfs-folder-mddate volume))
                               (list :u16 256) (list :u16 nmfls) (list :u16 3) (list :u16 0) (list :u16 nmalblks)
                               (list :u32 alblksiz) (list :u32 alblksiz) (list :u16 (+ 3 bitmap-blks))
                               (list :u32 next-cnid) (list :u16 (- nmalblks nblocks))
                               (list :fixed (%pstring drvn) 28) (list :u32 0) (list :u16 0) (list :u32 0)
                               (list :u32 alblksiz) (list :u32 alblksiz) (list :u16 nmrtdirs)
                               (list :u32 filcnt) (list :u32 dircnt) (list :bytes fndr)
                               (list :u16 0) (list :u16 0) (list :u16 0)
                               (list :u32 (length xt)) (list :u16 (car xt-ext)) (list :u16 (cdr xt-ext)) (list :pad 8)
                               (list :u32 (length ct)) (list :u16 (car ct-ext)) (list :u16 (cdr ct-ext)) (list :pad 8)))
                   (vib (%cat (list vib (%hfs-octets (- 512 (length vib))))))
                   (left (%cat (list* bootblocks vib bitmap (reverse blocks))))
                   (unused (- size (length left) 1024)))
              (%cat (list left (%hfs-octets unused) vib (%hfs-octets 512))))))))))
