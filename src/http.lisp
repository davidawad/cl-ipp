(in-package #:cl-ipp)

;;; Minimal HTTP/1.1 POST over usocket. One request per connection
;;; (Connection: close); handles Content-Length, chunked and close-delimited
;;; bodies. Plain HTTP only: ipps:// (TLS) is not supported.

(define-condition ipp-http-error (error)
  ((status :initarg :status :reader ipp-http-error-status)
   (detail :initarg :detail :initform nil :reader ipp-http-error-detail))
  (:report (lambda (c s)
             (format s "IPP HTTP error ~A~@[: ~A~]"
                     (ipp-http-error-status c) (ipp-http-error-detail c)))))

(defun parse-ipp-uri (uri)
  "ipp://host[:port]/path (or http://) -> (values host port path http-url).
Default port 631 for ipp, 80 for http."
  (let* ((scheme-end (or (search "://" uri) (error "Not a URI: ~A" uri)))
         (scheme (string-downcase (subseq uri 0 scheme-end)))
         (rest (subseq uri (+ scheme-end 3)))
         (slash (position #\/ rest))
         (authority (subseq rest 0 slash))
         (path (if slash (subseq rest slash) "/"))
         (colon (position #\: authority :from-end t))
         (host (if colon (subseq authority 0 colon) authority))
         (port (if colon
                   (parse-integer authority :start (1+ colon))
                   (if (string= scheme "http") 80 631))))
    (when (member scheme '("ipps" "https") :test #'string=)
      (error "TLS (~A) is not supported by cl-ipp" scheme))
    (values host port path (format nil "http://~A:~D~A" host port path))))

(defun ascii-octets (s) (map 'octets #'char-code s))

(defun read-all-octets (stream sock timeout)
  "Read STREAM to EOF, waiting at most TIMEOUT seconds for each wait."
  (let ((out (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop
      (unless (listen stream)
        (unless (usocket:wait-for-input sock :timeout timeout :ready-only t)
          (error 'ipp-http-error :status :timeout :detail "no data from printer")))
      (let ((b (read-byte stream nil nil)))
        (if b (vector-push-extend b out) (return))))
    (octets out)))

(defun find-crlfcrlf (buf)
  (loop for i from 0 to (- (length buf) 4)
        when (and (= (aref buf i) 13) (= (aref buf (+ i 1)) 10)
                  (= (aref buf (+ i 2)) 13) (= (aref buf (+ i 3)) 10))
          return i))

(defun split-lines (string)
  (let ((lines '()) (start 0))
    (loop for i = (search (coerce '(#\Return #\Newline) 'string) string :start2 start)
          while i do (push (subseq string start i) lines) (setf start (+ i 2)))
    (push (subseq string start) lines)
    (nreverse lines)))

(defun parse-head (head-octets)
  "-> (values status-code headers) with lowercase header names."
  (let* ((lines (split-lines (map 'string #'code-char head-octets)))
         (status (parse-integer (second (remove "" (split-on-space (first lines)) :test #'string=))))
         (headers (loop for l in (rest lines)
                        for c = (position #\: l)
                        when c collect (cons (string-downcase (subseq l 0 c))
                                             (string-trim " " (subseq l (1+ c)))))))
    (values status headers)))

(defun split-on-space (s)
  (loop with start = 0
        for i = (position #\Space s :start start)
        collect (subseq s start i)
        while i do (setf start (1+ i))))

(defun dechunk (buf)
  "Decode a chunked transfer-encoded body."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (pos 0))
    (loop
      (let* ((eol (or (loop for i from pos below (1- (length buf))
                            when (and (= (aref buf i) 13) (= (aref buf (1+ i)) 10)) return i)
                      (return)))
             (size-line (map 'string #'code-char (subseq buf pos eol)))
             (size (parse-integer size-line :radix 16
                                            :end (or (position #\; size-line) (length size-line)))))
        (setf pos (+ eol 2))
        (when (zerop size) (return))
        (loop for i from pos below (min (length buf) (+ pos size))
              do (vector-push-extend (aref buf i) out))
        (setf pos (+ pos size 2))))
    (octets out)))

(defun http-post (host port path body &key (content-type "application/ipp") (timeout 15))
  "POST BODY (octets). Returns (values status headers body-octets)."
  (let ((sock (usocket:socket-connect host port :element-type '(unsigned-byte 8)
                                                :timeout timeout)))
    (unwind-protect
         (let ((stream (usocket:socket-stream sock))
               (head (ascii-octets
                      (format nil "POST ~A HTTP/1.1~C~CHost: ~A:~D~C~CContent-Type: ~A~C~CContent-Length: ~D~C~CConnection: close~C~CUser-Agent: cl-ipp/0.1~C~C~C~C"
                              path #\Return #\Newline host port #\Return #\Newline
                              content-type #\Return #\Newline (length body)
                              #\Return #\Newline #\Return #\Newline #\Return #\Newline
                              #\Return #\Newline))))
           (write-sequence head stream)
           (write-sequence body stream)
           (finish-output stream)
           (let* ((raw (read-all-octets stream sock timeout))
                  (split (or (find-crlfcrlf raw)
                             (error 'ipp-http-error :status :malformed
                                                    :detail "no HTTP header terminator"))))
             (multiple-value-bind (status headers) (parse-head (subseq raw 0 split))
               (let* ((payload (subseq raw (+ split 4)))
                      (te (cdr (assoc "transfer-encoding" headers :test #'string=)))
                      (cl (cdr (assoc "content-length" headers :test #'string=))))
                 (values status headers
                         (cond ((and te (search "chunked" te)) (dechunk payload))
                               (cl (subseq payload 0 (min (length payload) (parse-integer cl))))
                               (t payload))))))
           )
      (usocket:socket-close sock))))
