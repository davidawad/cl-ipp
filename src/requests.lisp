(in-package #:cl-ipp)

;;; Request builders. Each returns a MESSAGE; pass it to ENCODE-MESSAGE or SEND-REQUEST.

(defun operation-attributes (printer-uri &rest extra)
  "Mandatory operation attributes (RFC 8011 4.1.4) followed by EXTRA attributes (nil entries dropped)."
  (list* (attr "attributes-charset" :charset "utf-8")
         (attr "attributes-natural-language" :natural-language "en")
         (attr "printer-uri" :uri printer-uri)
         (remove nil extra)))

(defun requested-attributes-attr (names)
  (when names (apply #'attr "requested-attributes" :keyword names)))

(defun get-printer-attributes-request (printer-uri &key requested-attributes (request-id 1))
  "Get-Printer-Attributes (0x000B). REQUESTED-ATTRIBUTES is a list of names or NIL for all."
  (make-message :code :get-printer-attributes :request-id request-id
                :groups (list (cons :operation
                                    (operation-attributes
                                     printer-uri
                                     (requested-attributes-attr requested-attributes))))))

(defun get-jobs-request (printer-uri &key (which-jobs "not-completed") limit my-jobs
                                       requesting-user-name requested-attributes
                                       (request-id 1))
  "Get-Jobs (0x000A). WHICH-JOBS is \"completed\" or \"not-completed\"."
  (make-message :code :get-jobs :request-id request-id
                :groups (list (cons :operation
                                    (operation-attributes
                                     printer-uri
                                     (when requesting-user-name
                                       (attr "requesting-user-name" :name requesting-user-name))
                                     (when limit (attr "limit" :integer limit))
                                     (when which-jobs (attr "which-jobs" :keyword which-jobs))
                                     (when my-jobs (attr "my-jobs" :boolean t))
                                     (requested-attributes-attr requested-attributes))))))

(defun print-job-request (printer-uri document &key (document-format "application/octet-stream")
                                                 job-name (requesting-user-name "cl-ipp")
                                                 (request-id 1))
  "Print-Job (0x0002) carrying DOCUMENT (octet vector) after the attributes.
Building this request does not send anything; only SEND-REQUEST / PRINT-JOB do."
  (make-message :code :print-job :request-id request-id
                :document (octets document)
                :groups (list (cons :operation
                                    (operation-attributes
                                     printer-uri
                                     (attr "requesting-user-name" :name requesting-user-name)
                                     (when job-name (attr "job-name" :name job-name))
                                     (attr "document-format" :mime-media-type document-format))))))
