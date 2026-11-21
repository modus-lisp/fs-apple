;;;; lib/macroman.lisp -- Mac OS Roman, the classic Mac's character set: file,
;;;; folder, volume and resource names on HFS are Mac Roman bytes.  Bytes below
;;;; #x80 are ASCII; the high half is Apple's table (the Unicode mapping Python's
;;;; `mac_roman' codec and Apple's ROMAN.TXT give; #xDB is the euro, #xF0 Apple's
;;;; logo in the private use area).

(in-package #:fs-apple)

(defparameter +mac-roman-high+
  #(#x00C4 #x00C5 #x00C7 #x00C9 #x00D1 #x00D6 #x00DC #x00E1
    #x00E0 #x00E2 #x00E4 #x00E3 #x00E5 #x00E7 #x00E9 #x00E8
    #x00EA #x00EB #x00ED #x00EC #x00EE #x00EF #x00F1 #x00F3
    #x00F2 #x00F4 #x00F6 #x00F5 #x00FA #x00F9 #x00FB #x00FC
    #x2020 #x00B0 #x00A2 #x00A3 #x00A7 #x2022 #x00B6 #x00DF
    #x00AE #x00A9 #x2122 #x00B4 #x00A8 #x2260 #x00C6 #x00D8
    #x221E #x00B1 #x2264 #x2265 #x00A5 #x00B5 #x2202 #x2211
    #x220F #x03C0 #x222B #x00AA #x00BA #x03A9 #x00E6 #x00F8
    #x00BF #x00A1 #x00AC #x221A #x0192 #x2248 #x2206 #x00AB
    #x00BB #x2026 #x00A0 #x00C0 #x00C3 #x00D5 #x0152 #x0153
    #x2013 #x2014 #x201C #x201D #x2018 #x2019 #x00F7 #x25CA
    #x00FF #x0178 #x2044 #x20AC #x2039 #x203A #xFB01 #xFB02
    #x2021 #x00B7 #x201A #x201E #x2030 #x00C2 #x00CA #x00C1
    #x00CB #x00C8 #x00CD #x00CE #x00CF #x00CC #x00D3 #x00D4
    #xF8FF #x00D2 #x00DA #x00DB #x00D9 #x0131 #x02C6 #x02DC
    #x00AF #x02D8 #x02D9 #x02DA #x00B8 #x02DD #x02DB #x02C7)
  "Unicode code points of Mac Roman bytes #x80..#xFF.")

(defvar *mac-roman-encode* nil)

(defun mac-roman-decode (v &key (start 0) (end (length v)))
  "Mac Roman bytes V[START,END) as a string."
  (let ((s (make-string (- end start))))
    (loop for i from start below end for k from 0
          do (let ((b (aref v i)))
               (setf (char s k) (code-char (if (< b #x80) b (aref +mac-roman-high+ (- b #x80)))))))
    s))

(defun mac-roman-encode (s)
  "S as Mac Roman bytes, an octet vector; an error for a character it lacks."
  (unless *mac-roman-encode*
    (let ((h (make-hash-table)))
      (dotimes (i 128) (setf (gethash (aref +mac-roman-high+ i) h) (+ #x80 i)))
      (setq *mac-roman-encode* h)))
  (map '(simple-array (unsigned-byte 8) (*))
       (lambda (c)
         (let ((code (char-code c)))
           (cond ((< code #x80) code)
                 ((gethash code *mac-roman-encode*))
                 (t (error "fs-apple: ~S is not in Mac Roman" c)))))
       s))
