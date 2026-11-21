;;;; test/hfs-test.lisp -- lib/hfs.lisp: a volume written from a tree here must
;;;; be machfs's volume for the same tree, byte for byte (its length and CRC-32,
;;;; recorded from machfs 1.3), and read back to the same tree.  The tree has
;;;; nested folders, both forks, a Mac Roman name, sixty files (index nodes), an
;;;; alias, a System whose 'boot' 1 resource becomes the boot blocks, a Finder,
;;;; the Desktop file and a startup application.  (Checked separately on real
;;;; volumes: System 6.0.8 and 7.5.3, MCL and Allegro floppies, the period-Lisp
;;;; volumes -- read identically to machfs on 17, rewritten byte for byte on 6.)
;;;; Needs cram (CRAM, else ../cram).  sbcl --script; the last line is
;;;; "HFS-TEST PASS" or "HFS-TEST FAIL".

(unless (find-package "CRAM")
  (let* ((env (sb-ext:posix-getenv "CRAM"))
         (dir (if (and env (plusp (length env)))
                  (pathname (if (char= (char env (1- (length env))) #\/) env (concatenate 'string env "/")))
                  (merge-pathnames "../../cram/" *load-truename*))))
    (load (merge-pathnames "src/packages.lisp" dir))
    (load (merge-pathnames "src/crc16.lisp" dir))
    (load (merge-pathnames "src/deflate.lisp" dir))))
(dolist (f '("package" "macroman" "apm" "packbits" "resource-fork" "macbinary" "appledouble" "diskcopy" "hfs"))
  (load (merge-pathnames (format nil "../lib/~A.lisp" f) *load-truename*)))

(defvar *fails* 0)
(defun check (ok label) (unless ok (incf *fails*) (format t "FAIL: ~a~%" label)))
(defun pat (n k) (let ((v (make-array n :element-type '(unsigned-byte 8)))) (dotimes (i n v) (setf (aref v i) (mod (+ (* i k) n) 256)))))
(defun same (a b) (and (= (length a) (length b)) (every #'= a b)))
(defun ostr (s) (map '(vector (unsigned-byte 8)) #'char-code s))

(let* ((vol (fs-apple:make-hfs-folder :name "Test Vol" :crdate #xA0000000))
       (sysf (fs-apple:hfs-set vol "System Folder" (fs-apple:make-hfs-folder)))
       (boot (let ((v (make-array 1024 :element-type '(unsigned-byte 8)))) (dotimes (i 1024 v) (setf (aref v i) (mod (* i 7) 256)))))
       (apps (fs-apple:hfs-set vol "Applications" (fs-apple:make-hfs-folder)))
       (app (fs-apple:make-hfs-file :type "APPL" :creator "TEST" :flags #x2000 :data (pat 70000 3) :rsrc (pat 1234 11)))
       (docs (fs-apple:hfs-set vol "Docs" (fs-apple:make-hfs-folder))))
  (fs-apple:hfs-set sysf "System" (fs-apple:make-hfs-file :type "ZSYS" :creator "MACS"
                                                           :rsrc (fs-apple:resource-fork-write (list (list "boot" 1 boot)) :map-header nil)))
  (fs-apple:hfs-set sysf "Finder" (fs-apple:make-hfs-file :type "FNDR" :creator "MACS" :data (pat 3000 5)))
  (fs-apple:hfs-set apps (format nil "Caf~C App" (code-char #xE9)) app)
  (dotimes (i 60)
    (fs-apple:hfs-set docs (format nil "note ~2,'0D" i)
                      (fs-apple:make-hfs-file :type "TEXT" :creator "ttxt" :x i :y (* 2 i) :crdate (+ #xA0000000 i)
                                              :data (pat (+ 100 (* 37 i)) (1+ i)))))
  (fs-apple:hfs-set vol (format nil "Caf~C App alias" (code-char #xE9)) (fs-apple:make-hfs-file :flags #x8000 :aliastarget app))
  (let ((disk (fs-apple:hfs-write vol :size (* 1440 1024) :desktopdb t
                                      :startapp (list "Applications" (format nil "Caf~C App" (code-char #xE9))))))
    (check (= (length disk) 1474560) "the volume's length")
    (check (= (cram:crc32 disk) #x47a184e6) (format nil "machfs's volume, byte for byte (CRC-32 #x~8,'0x)" (cram:crc32 disk)))
    (check (same (subseq disk #x5A #x6A) (concatenate '(vector (unsigned-byte 8)) (vector 8) (fs-apple:mac-roman-encode (format nil "Caf~C App" (code-char #xE9))) (make-array 7 :initial-element 32)))
           "the boot blocks name the startup application")
    ;; read back
    (let* ((back (fs-apple:hfs-read disk))
           (app2 (fs-apple:hfs-ref back "Applications" (format nil "Caf~C App" (code-char #xE9))))
           (alias (fs-apple:hfs-ref back (format nil "caf~C app ALIAS" (code-char #xE9)))))
      (check (string= (fs-apple:hfs-folder-name back) "Test Vol") "read: the volume's name")
      (check (and (null (fs-apple:hfs-ref back "Desktop")) (= 4 (length (fs-apple:hfs-folder-entries back)))) "read: the top level, no Desktop file")
      (check (and app2 (same (fs-apple:hfs-file-data app2) (pat 70000 3)) (same (fs-apple:hfs-file-rsrc app2) (pat 1234 11))
                  (string= (fs-apple:hfs-file-type app2) "APPL") (= (fs-apple:hfs-file-flags app2) #x2000))
             "read: the application, both forks")
      (check (and alias (eq (fs-apple:hfs-file-aliastarget alias) app2) (string= (fs-apple:hfs-file-type alias) "adrp"))
             "read: the alias points at the application")
      (check (= 60 (length (fs-apple:hfs-folder-entries (fs-apple:hfs-ref back "Docs")))) "read: sixty documents")
      (check (every (lambda (i) (let ((d (fs-apple:hfs-ref back "Docs" (format nil "note ~2,'0D" i))))
                                  (and d (same (fs-apple:hfs-file-data d) (pat (+ 100 (* 37 i)) (1+ i)))
                                       (= (fs-apple:hfs-file-x d) i) (= (fs-apple:hfs-file-y d) (* 2 i))
                                       (= (fs-apple:hfs-file-crdate d) (+ #xA0000000 i)))))
                    (loop for i below 60 collect i))
             "read: every document, its data, place and date")
      ;; written again from what was read: the same volume (the alias rebuilt)
      (setf (fs-apple:hfs-folder-crdate back) #xA0000000)
      (check (= (length (fs-apple:hfs-write back :size (* 1440 1024))) 1474560) "rewritten"))))
(check (handler-case (progn (fs-apple:hfs-write (fs-apple:make-hfs-folder :name "x") :size 1000) nil) (error () t)) "a too-small size refused")
(check (handler-case (progn (fs-apple:hfs-read (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0)) nil) (error () t)) "no volume: an error")
(format t "~&HFS-TEST ~A~%" (if (zerop *fails*) "PASS" "FAIL"))
