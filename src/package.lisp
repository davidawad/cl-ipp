(defpackage #:cl-ipp
  (:use #:cl)
  (:export
   ;; codec
   #:encode-message #:decode-message #:ipp-decode-error
   #:message-version #:message-code #:message-request-id #:message-groups #:message-document
   #:make-message #:group-attributes #:attribute-values #:find-attribute
   #:attr #:collection
   ;; requests
   #:get-printer-attributes-request #:get-jobs-request #:print-job-request
   ;; response helpers
   #:status-keyword #:operation-keyword #:response-ok-p
   #:simplify-message #:simplify-attributes #:printer-summary #:job-summaries
   ;; transport
   #:parse-ipp-uri #:http-post #:ipp-http-error #:send-request
   ;; client
   #:get-printer-attributes #:get-jobs #:print-job #:printer-info))
