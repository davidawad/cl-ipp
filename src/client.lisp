(in-package #:cl-ipp)

(defun send-request (printer-uri message &key (timeout 15))
  "POST MESSAGE to PRINTER-URI (ipp://host:631/ipp/print) and return the decoded response MESSAGE."
  (multiple-value-bind (host port path) (parse-ipp-uri printer-uri)
    (multiple-value-bind (status headers body)
        (http-post host port path (encode-message message) :timeout timeout)
      (declare (ignore headers))
      (unless (= status 200)
        (error 'ipp-http-error :status status))
      (decode-message body))))

(defun get-printer-attributes (printer-uri &key requested-attributes (timeout 15))
  "Read-only. Returns the response MESSAGE."
  (send-request printer-uri
                (get-printer-attributes-request printer-uri
                                               :requested-attributes requested-attributes)
                :timeout timeout))

(defun printer-info (printer-uri &key (timeout 15))
  "Read-only. Get-Printer-Attributes digested with PRINTER-SUMMARY."
  (printer-summary (get-printer-attributes printer-uri :timeout timeout)))

(defun get-jobs (printer-uri &rest keys &key (timeout 15) &allow-other-keys)
  "Read-only. Returns the response MESSAGE; digest with JOB-SUMMARIES."
  (let ((req-keys (loop for (k v) on keys by #'cddr
                        unless (eq k :timeout) append (list k v))))
    (send-request printer-uri (apply #'get-jobs-request printer-uri req-keys)
                  :timeout timeout)))

(defun print-job (printer-uri document &rest keys &key (timeout 15) &allow-other-keys)
  "SENDS A PRINT JOB. Returns the response MESSAGE. Never called by tests or examples."
  (let ((req-keys (loop for (k v) on keys by #'cddr
                        unless (eq k :timeout) append (list k v))))
    (send-request printer-uri (apply #'print-job-request printer-uri document req-keys)
                  :timeout timeout)))
