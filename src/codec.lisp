(in-package #:cl-ipp)

;;; Data model
;;;
;;;   message   = struct: version (major minor), code, request-id, groups, document
;;;   group     = (delimiter-keyword . attributes)         e.g. (:printer ...)
;;;   attribute = (name . values)                          name is a string
;;;   value     = (tag-keyword . lisp-value)
;;;
;;; Lisp values by tag: integer/enum -> integer; boolean -> T / NIL;
;;; text, name, keyword, uri, charset, ... -> string; octet-string -> octet
;;; vector; date-time -> (y mo d h mi s deci dir utc-h utc-m); resolution ->
;;; (x y :dpi|:dpcm); range-of-integer -> (lo . hi); *-with-language ->
;;; (language . string); begin-collection -> list of attributes (members);
;;; out-of-band tags (:unsupported :unknown :no-value) -> NIL.

(defstruct (message (:copier nil))
  (version '(2 0))
  (code 0)
  (request-id 1)
  (groups '())
  (document nil))

(define-condition ipp-decode-error (error)
  ((reason :initarg :reason :reader ipp-decode-error-reason)
   (position :initarg :position :reader ipp-decode-error-position))
  (:report (lambda (c s)
             (format s "IPP decode error at byte ~D: ~A"
                     (ipp-decode-error-position c) (ipp-decode-error-reason c)))))

(defun decode-fail (pos fmt &rest args)
  (error 'ipp-decode-error :position pos :reason (apply #'format nil fmt args)))

;;; Constructors -------------------------------------------------------------

(defun attr (name tag value &rest more-values)
  "Build an attribute NAME with one or more values all of TAG."
  (cons name (mapcar (lambda (v) (cons tag v)) (cons value more-values))))

(defun collection (&rest members)
  "A collection value: list of member attributes (use inside ATTR with :begin-collection)."
  members)

(defun attribute-values (attribute)
  "Plain lisp values of ATTRIBUTE (dropping tags)."
  (mapcar #'cdr (cdr attribute)))

(defun group-attributes (message group)
  "All attributes of the first GROUP in MESSAGE."
  (cdr (assoc group (message-groups message))))

(defun find-attribute (name attributes)
  (assoc name attributes :test #'string=))

;;; Octet helpers ---------------------------------------------------------------

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun octets (seq) (coerce seq 'octets))

(defun string->octets (s) (sb-ext:string-to-octets s :external-format :utf-8))
(defun octets->string (o) (sb-ext:octets-to-string (octets o) :external-format :utf-8))

(defun u16 (n) (list (ldb (byte 8 8) n) (ldb (byte 8 0) n)))
(defun u32 (n) (list (ldb (byte 8 24) n) (ldb (byte 8 16) n)
                     (ldb (byte 8 8) n) (ldb (byte 8 0) n)))

(defun rd-u16 (buf pos)
  (when (> (+ pos 2) (length buf)) (decode-fail pos "truncated 16-bit field"))
  (logior (ash (aref buf pos) 8) (aref buf (1+ pos))))

(defun rd-u32 (buf pos)
  (when (> (+ pos 4) (length buf)) (decode-fail pos "truncated 32-bit field"))
  (logior (ash (aref buf pos) 24) (ash (aref buf (+ pos 1)) 16)
          (ash (aref buf (+ pos 2)) 8) (aref buf (+ pos 3))))

(defun rd-s32 (buf pos)
  (let ((n (rd-u32 buf pos)))
    (if (logbitp 31 n) (- n (ash 1 32)) n)))

(defun s32 (n) (u32 (ldb (byte 32 0) n)))

;;; Encoding ---------------------------------------------------------------------

(defun lisp-value->octets (tag value)
  (ecase tag
    ((:integer :enum) (s32 value))
    (:boolean (list (if value 1 0)))
    ((:text :name :keyword :uri :uri-scheme :charset :natural-language
      :mime-media-type :member-attr-name)
     (coerce (string->octets value) 'list))
    (:octet-string (coerce value 'list))
    (:date-time (destructuring-bind (y mo d h mi s ds dir uh um) value
                  (append (u16 y) (list mo d h mi s ds (char-code dir) uh um))))
    (:resolution (destructuring-bind (x y units) value
                   (append (s32 x) (s32 y)
                           (list (ecase units (:dpi 3) (:dpcm 4))))))
    (:range-of-integer (append (s32 (car value)) (s32 (cdr value))))
    ((:text-with-language :name-with-language)
     (let ((lang (string->octets (car value)))
           (text (string->octets (cdr value))))
       (append (u16 (length lang)) (coerce lang 'list)
               (u16 (length text)) (coerce text 'list))))
    ((:unsupported :unknown :no-value) '())))

(defun encode-entry (tag name value)
  "Octets (as a list) for one tag/name/value triple, expanding collections."
  (let* ((name-octets (coerce (string->octets name) 'list)))
    (if (eq tag :begin-collection)
        (append (list (tag-code tag *value-tags*))
                (u16 (length name-octets)) name-octets
                (u16 0)
                (loop for (member-name . member-values) in value
                      append (encode-entry :member-attr-name "" member-name)
                      append (loop for (mtag . mval) in member-values
                                   append (encode-entry mtag "" mval)))
                (list (tag-code :end-collection *value-tags*)
                      0 0 0 0))
        (let ((payload (lisp-value->octets tag value)))
          (append (list (tag-code tag *value-tags*))
                  (u16 (length name-octets)) name-octets
                  (u16 (length payload)) payload)))))

(defun encode-attribute (attribute)
  (destructuring-bind (name . values) attribute
    (loop for (tag . value) in values
          for first = t then nil
          append (encode-entry tag (if first name "") value))))

(defun encode-message (message)
  "Encode MESSAGE to a vector of octets. A non-NIL document (octets) is appended
after the end-of-attributes tag, as for Print-Job."
  (let ((head (append (list (first (message-version message))
                            (second (message-version message)))
                      (u16 (tag-code (message-code message) (append *operations* *status-codes*)))
                      (u32 (message-request-id message))
                      (loop for (group . attrs) in (message-groups message)
                            append (list (tag-code group *delimiter-tags*))
                            append (mapcan #'encode-attribute attrs))
                      (list (tag-code :end *delimiter-tags*)))))
    (let ((doc (message-document message)))
      (octets (if doc (append head (coerce doc 'list)) head)))))

;;; Decoding ---------------------------------------------------------------------

(defun decode-value (tag raw pos)
  (case tag
    ((:integer :enum) (if (= (length raw) 4) (rd-s32 raw 0)
                          (decode-fail pos "integer of ~D bytes" (length raw))))
    (:boolean (/= 0 (if (plusp (length raw)) (aref raw 0) 0)))
    ((:text :name :keyword :uri :uri-scheme :charset :natural-language
      :mime-media-type :member-attr-name)
     (octets->string raw))
    (:date-time
     (if (= (length raw) 11)
         (list (rd-u16 raw 0) (aref raw 2) (aref raw 3) (aref raw 4) (aref raw 5)
               (aref raw 6) (aref raw 7) (code-char (aref raw 8)) (aref raw 9)
               (aref raw 10))
         raw))
    (:resolution
     (if (= (length raw) 9)
         (list (rd-s32 raw 0) (rd-s32 raw 4)
               (case (aref raw 8) (3 :dpi) (4 :dpcm) (t (aref raw 8))))
         raw))
    (:range-of-integer
     (if (= (length raw) 8) (cons (rd-s32 raw 0) (rd-s32 raw 4)) raw))
    ((:text-with-language :name-with-language)
     (let* ((ll (rd-u16 raw 0))
            (lang (octets->string (subseq raw 2 (+ 2 ll))))
            (tl (rd-u16 raw (+ 2 ll)))
            (text (octets->string (subseq raw (+ 4 ll) (+ 4 ll tl)))))
       (cons lang text)))
    ((:unsupported :unknown :no-value) nil)
    (t raw)))

(defun read-entry (buf pos)
  "Read one tag/name/value entry at POS. Returns (values tag name value new-pos).
Begin-collection values are decoded recursively into member attribute lists."
  (let* ((code (aref buf pos))
         (tag (code-tag code *value-tags*))
         (nl (rd-u16 buf (+ pos 1)))
         (name-end (+ pos 3 nl)))
    (when (> (+ name-end 2) (length buf)) (decode-fail pos "truncated entry"))
    (let* ((name (octets->string (subseq buf (+ pos 3) name-end)))
           (vl (rd-u16 buf name-end))
           (value-start (+ name-end 2))
           (value-end (+ value-start vl)))
      (when (> value-end (length buf)) (decode-fail pos "value runs past end of message"))
      (if (eq tag :begin-collection)
          (multiple-value-bind (members np) (read-collection buf value-end)
            (values tag name members np))
          (values tag name
                  (decode-value tag (octets (subseq buf value-start value-end)) pos)
                  value-end)))))

(defun read-collection (buf pos)
  "Read collection members after a begin-collection entry, through end-collection.
Returns (values member-attributes new-pos)."
  (let ((members '()) (current nil))
    (flet ((flush () (when current
                       (push (cons (car current) (reverse (cdr current))) members)
                       (setf current nil))))
      (loop
        (when (>= pos (length buf)) (decode-fail pos "unterminated collection"))
        (multiple-value-bind (tag name value np) (read-entry buf pos)
          (declare (ignorable name))
          (setf pos np)
          (case tag
            (:end-collection (flush) (return (values (nreverse members) pos)))
            (:member-attr-name (flush) (setf current (list value)))
            (t (unless current (decode-fail pos "collection value without member name"))
               (push (cons tag value) (cdr current)))))))))

(defun read-attributes (buf pos)
  "Read attributes until a delimiter tag. Returns (values attributes new-pos)."
  (let ((attrs '()))
    (loop
      (when (>= pos (length buf)) (decode-fail pos "missing end-of-attributes tag"))
      (when (delimiter-code-p (aref buf pos))
        (return (values (nreverse attrs) pos)))
      (multiple-value-bind (tag name value np) (read-entry buf pos)
        (setf pos np)
        (if (string= name "")
            (if attrs
                (setf (car attrs)
                      (append (car attrs) (list (cons tag value))))
                (decode-fail pos "additional value with no preceding attribute"))
            (push (list name (cons tag value)) attrs))))))

(defun decode-message (bytes &key (start 0))
  "Decode IPP BYTES into a MESSAGE. Any bytes after the end-of-attributes tag
are returned as the message document (NIL when empty)."
  (let ((buf (octets bytes)) (pos start))
    (when (< (- (length buf) start) 9) (decode-fail start "message shorter than 9 bytes"))
    (let ((version (list (aref buf pos) (aref buf (1+ pos))))
          (code (rd-u16 buf (+ pos 2)))
          (request-id (rd-u32 buf (+ pos 4)))
          (groups '()))
      (setf pos (+ pos 8))
      (loop
        (when (>= pos (length buf)) (decode-fail pos "missing end-of-attributes tag"))
        (let ((d (aref buf pos)))
          (cond ((= d (tag-code :end *delimiter-tags*))
                 (incf pos)
                 (return))
                ((delimiter-code-p d)
                 (multiple-value-bind (attrs np) (read-attributes buf (1+ pos))
                   (push (cons (code-tag d *delimiter-tags*) attrs) groups)
                   (setf pos np)))
                (t (decode-fail pos "expected delimiter tag, got #x~2,'0X" d)))))
      (make-message :version version :code code :request-id request-id
                    :groups (nreverse groups)
                    :document (when (< pos (length buf)) (subseq buf pos))))))
