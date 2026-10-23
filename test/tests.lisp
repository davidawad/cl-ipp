(in-package #:cl-ipp-test)

(def-suite :cl-ipp-tests)
(in-suite :cl-ipp-tests)

(defparameter *fixture-dir*
  (merge-pathnames "fixtures/" (asdf:system-relative-pathname "cl-ipp" "test/")))

(defun read-fixture (name)
  (with-open-file (s (merge-pathnames name *fixture-dir*) :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun bytes (&rest xs) (coerce xs '(simple-array (unsigned-byte 8) (*))))

(defparameter *uri* "ipp://BRW000000000000.local:631/ipp/print")

;;; Codec: hand-computed bytes ---------------------------------------------------

(test encode-matches-hand-written-bytes
  "Get-Printer-Attributes for a short URI, bytes written out by hand from RFC 8010."
  (let* ((msg (get-printer-attributes-request "ipp://h/p"))
         (expected (concatenate
                    '(vector (unsigned-byte 8))
                    (bytes 2 0 0 #x0B 0 0 0 1 #x01)
                    (bytes #x47 0 18) (map 'vector #'char-code "attributes-charset")
                    (bytes 0 5) (map 'vector #'char-code "utf-8")
                    (bytes #x48 0 27) (map 'vector #'char-code "attributes-natural-language")
                    (bytes 0 2) (map 'vector #'char-code "en")
                    (bytes #x45 0 11) (map 'vector #'char-code "printer-uri")
                    (bytes 0 9) (map 'vector #'char-code "ipp://h/p")
                    (bytes #x03))))
    (is (equalp expected (encode-message msg)))))

(test integer-boolean-enum-encoding
  (let* ((msg (make-message :code :get-jobs :request-id #x01020304
                            :groups (list (cons :operation
                                                (list (attr "limit" :integer -2)
                                                      (attr "my-jobs" :boolean t)
                                                      (attr "x" :enum 4))))))
         (b (encode-message msg)))
    (is (equalp (bytes 2 0 0 10 1 2 3 4 1
                       #x21 0 5 108 105 109 105 116 0 4 #xFF #xFF #xFF #xFE
                       #x22 0 7 109 121 45 106 111 98 115 0 1 1
                       #x23 0 1 120 0 4 0 0 0 4
                       3)
                b))))

(test roundtrip-all-value-types
  (let* ((attrs (list (attr "i" :integer 5 -7 100000)
                      (attr "b" :boolean t)
                      (attr "b2" :boolean nil)
                      (attr "e" :enum 3)
                      (attr "k" :keyword "a" "b")
                      (attr "t" :text "héllo")
                      (attr "n" :name "name")
                      (attr "u" :uri "ipp://x/y")
                      (attr "o" :octet-string (bytes 1 2 3))
                      (attr "d" :date-time '(2026 10 22 12 0 0 0 #\+ 5 0))
                      (attr "r" :resolution '(600 600 :dpi))
                      (attr "g" :range-of-integer '(1 . 10))
                      (attr "tl" :text-with-language '("en" . "hi"))
                      (attr "nv" :no-value nil)
                      (attr "col" :begin-collection
                            (list (attr "x-dimension" :integer 21000)
                                  (attr "inner" :begin-collection
                                        (list (attr "m" :keyword "p" "q")))
                                  (attr "y-dimension" :integer 29700))
                            (list (attr "x-dimension" :integer 1)))))
         (msg (make-message :code :successful-ok :request-id 9
                            :groups (list (cons :operation attrs)
                                          (cons :job (list (attr "job-id" :integer 1)))
                                          (cons :job (list (attr "job-id" :integer 2))))))
         (back (decode-message (encode-message msg))))
    (is (equalp (message-groups msg) (message-groups back)))
    (is (= 9 (message-request-id back)))
    (is (null (message-document back)))))

(test print-job-document-is-appended-and-recovered
  (let* ((doc (bytes 37 33 80 83))
         (msg (print-job-request *uri* doc :document-format "application/postscript"
                                           :job-name "t"))
         (wire (encode-message msg))
         (back (decode-message wire)))
    (is (equalp doc (message-document back)))
    (is (= 2 (message-code back)))
    (is (equalp doc (subseq wire (- (length wire) 4))))
    (is (equal '("application/postscript")
               (attribute-values (find-attribute "document-format"
                                                 (group-attributes back :operation)))))))

(test get-jobs-request-attributes
  (let* ((back (decode-message (encode-message
                                (get-jobs-request *uri* :which-jobs "completed" :limit 5
                                                        :requested-attributes '("job-id" "job-state")))))
         (ops (group-attributes back :operation)))
    (is (= 10 (message-code back)))
    (is (equal '("completed") (attribute-values (find-attribute "which-jobs" ops))))
    (is (equal '(5) (attribute-values (find-attribute "limit" ops))))
    (is (equal '("job-id" "job-state")
               (attribute-values (find-attribute "requested-attributes" ops))))))

(test decode-errors-are-signalled
  (signals ipp-decode-error (decode-message (bytes 2 0 0)))
  (signals ipp-decode-error (decode-message (bytes 2 0 0 0 0 0 0 1 1)))
  ;; value length points past the end
  (signals ipp-decode-error (decode-message (bytes 2 0 0 0 0 0 0 1 1 #x21 0 1 97 0 40 0 0))))

;;; Real fixture -------------------------------------------------------------------

(defun fixture-response () (decode-message (read-fixture "get-printer-attributes-response.bin")))

(test fixture-request-is-byte-identical-to-ours
  "The request sent to the real printer (via curl) equals what we generate."
  (is (equalp (read-fixture "get-printer-attributes-request.bin")
              (encode-message (get-printer-attributes-request *uri*)))))

(test fixture-response-status-and-groups
  (let ((r (fixture-response)))
    (is (eq :successful-ok (status-keyword (message-code r))))
    (is (response-ok-p r))
    (is (= 1 (message-request-id r)))
    (is (eq :operation (car (first (message-groups r)))))
    (is (assoc :printer (message-groups r)))))

(test fixture-printer-state-and-markers
  (let ((s (printer-summary (fixture-response))))
    (is (eq :idle (getf s :state)))
    (is (equal '(24 22 16 19)
               (mapcar (lambda (m) (getf m :level)) (getf s :markers))))
    (is (= 4 (length (getf s :markers))))
    (is (every (lambda (m) (and (getf m :name) (getf m :color) (getf m :type)))
               (getf s :markers)))
    (is (equal "ink-cartridge" (getf (first (getf s :markers)) :type)))))

(test fixture-reencodes-losslessly-at-model-level
  (let* ((r (fixture-response))
         (again (decode-message (encode-message r))))
    (is (equalp (message-groups r) (message-groups again)))))

(test fixture-simplified-plist
  (let* ((p (simplify-message (fixture-response)))
         (printer (cdr (assoc :printer (getf p :groups)))))
    (is (eq :successful-ok (getf p :status)))
    (is (eql 3 (getf printer :printer-state)))
    (is (stringp (getf printer :printer-make-and-model)))))

;;; URI + HTTP (mock server on loopback; no printer needed) ---------------------------

(test parse-uri
  (multiple-value-bind (h p path) (parse-ipp-uri "ipp://host.local:631/ipp/print")
    (is (equal '("host.local" 631 "/ipp/print") (list h p path))))
  (multiple-value-bind (h p path) (parse-ipp-uri "ipp://host")
    (is (equal '("host" 631 "/") (list h p path))))
  (signals error (parse-ipp-uri "ipps://host/x")))

(defun serve-once (listener response-body &key chunked)
  "Accept one connection, read the request, reply. Returns the request body octets via thread result."
  (sb-thread:make-thread
   (lambda ()
     (let* ((sock (usocket:socket-accept listener :element-type '(unsigned-byte 8)))
            (s (usocket:socket-stream sock)))
       (unwind-protect
            (let ((acc (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
                  (content-length nil) (head-end nil))
              (loop
                (vector-push-extend (read-byte s) acc)
                (unless head-end
                  (let ((n (length acc)))
                    (when (and (>= n 4) (equalp (subseq acc (- n 4)) (bytes 13 10 13 10)))
                      (setf head-end n)
                      (let* ((txt (map 'string #'code-char acc))
                             (p (search "Content-Length: " txt)))
                        (setf content-length (parse-integer txt :start (+ p 16)
                                                                :junk-allowed t))))))
                (when (and head-end (>= (length acc) (+ head-end content-length))) (return)))
              (let ((hdr (format nil "HTTP/1.1 200 OK~C~CContent-Type: application/ipp~C~C~A~C~C"
                                 #\Return #\Newline #\Return #\Newline
                                 (if chunked
                                     (format nil "Transfer-Encoding: chunked~C~C" #\Return #\Newline)
                                     (format nil "Content-Length: ~D~C~C" (length response-body)
                                             #\Return #\Newline))
                                 #\Return #\Newline)))
                (write-sequence (map '(vector (unsigned-byte 8)) #'char-code hdr) s)
                (if chunked
                    (let* ((mid (floor (length response-body) 2))
                           (a (subseq response-body 0 mid))
                           (b (subseq response-body mid)))
                      (flet ((chunk (c)
                               (write-sequence (map 'vector #'char-code
                                                    (format nil "~X~C~C" (length c) #\Return #\Newline)) s)
                               (write-sequence c s)
                               (write-sequence (bytes 13 10) s)))
                        (chunk a) (chunk b)
                        (write-sequence (map 'vector #'char-code
                                             (format nil "0~C~C~C~C" #\Return #\Newline #\Return #\Newline)) s)))
                    (write-sequence response-body s))
                (finish-output s))
              (subseq acc head-end))
         (usocket:socket-close sock))))))

(defun mock-roundtrip (&key chunked)
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t))
         (port (usocket:get-local-port listener))
         (fixture (read-fixture "get-printer-attributes-response.bin"))
         (thread (serve-once listener fixture :chunked chunked)))
    (unwind-protect
         (let ((resp (get-printer-attributes (format nil "ipp://127.0.0.1:~D/ipp/print" port)
                                             :timeout 5)))
           (values resp (sb-thread:join-thread thread)))
      (usocket:socket-close listener))))

(test http-content-length-roundtrip
  (multiple-value-bind (resp sent) (mock-roundtrip)
    (is (eq :idle (getf (printer-summary resp) :state)))
    (is (= 2 (aref sent 0)))
    (is (= #x0B (aref sent 3)))))

(test http-chunked-roundtrip
  (multiple-value-bind (resp) (mock-roundtrip :chunked t)
    (is (equal '(24 22 16 19)
               (mapcar (lambda (m) (getf m :level)) (getf (printer-summary resp) :markers))))))
