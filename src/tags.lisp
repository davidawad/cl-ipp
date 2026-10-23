(in-package #:cl-ipp)

;;; Tag, operation and status tables (RFC 8010 section 3.5, RFC 8011 section 5.4).

(defparameter *delimiter-tags*
  '((:operation . #x01) (:job . #x02) (:end . #x03) (:printer . #x04)
    (:unsupported-attributes . #x05) (:subscription . #x06)
    (:event-notification . #x07) (:resource . #x08)
    (:document . #x09) (:system . #x0A)))

(defparameter *value-tags*
  '((:unsupported . #x10) (:unknown . #x12) (:no-value . #x13)
    (:integer . #x21) (:boolean . #x22) (:enum . #x23)
    (:octet-string . #x30) (:date-time . #x31) (:resolution . #x32)
    (:range-of-integer . #x33) (:begin-collection . #x34)
    (:text-with-language . #x35) (:name-with-language . #x36)
    (:end-collection . #x37)
    (:text . #x41) (:name . #x42) (:keyword . #x44) (:uri . #x45)
    (:uri-scheme . #x46) (:charset . #x47) (:natural-language . #x48)
    (:mime-media-type . #x49) (:member-attr-name . #x4A)))

(defparameter *operations*
  '((:print-job . #x0002) (:validate-job . #x0004) (:create-job . #x0005)
    (:send-document . #x0006) (:cancel-job . #x0008)
    (:get-job-attributes . #x0009) (:get-jobs . #x000A)
    (:get-printer-attributes . #x000B)))

(defparameter *status-codes*
  '((:successful-ok . #x0000)
    (:successful-ok-ignored-or-substituted-attributes . #x0001)
    (:successful-ok-conflicting-attributes . #x0002)
    (:client-error-bad-request . #x0400)
    (:client-error-forbidden . #x0401)
    (:client-error-not-authenticated . #x0402)
    (:client-error-not-authorized . #x0403)
    (:client-error-not-possible . #x0404)
    (:client-error-timeout . #x0405)
    (:client-error-not-found . #x0406)
    (:client-error-gone . #x0407)
    (:client-error-request-entity-too-large . #x0408)
    (:client-error-document-format-not-supported . #x040A)
    (:client-error-attributes-or-values-not-supported . #x040B)
    (:server-error-internal-error . #x0500)
    (:server-error-operation-not-supported . #x0501)
    (:server-error-service-unavailable . #x0502)
    (:server-error-version-not-supported . #x0503)
    (:server-error-device-error . #x0504)
    (:server-error-temporary-error . #x0505)
    (:server-error-busy . #x0507)))

(defparameter *printer-states* '((3 . :idle) (4 . :processing) (5 . :stopped)))

(defparameter *job-states*
  '((3 . :pending) (4 . :pending-held) (5 . :processing)
    (6 . :processing-stopped) (7 . :canceled) (8 . :aborted) (9 . :completed)))

(defun tag-code (tag table)
  "Numeric code for keyword TAG in TABLE; integers pass through."
  (etypecase tag
    (integer tag)
    (keyword (or (cdr (assoc tag table))
                 (error "Unknown IPP tag ~S" tag)))))

(defun code-tag (code table)
  "Keyword for numeric CODE in TABLE, or CODE itself when unknown."
  (or (car (rassoc code table)) code))

(defun delimiter-code-p (code) (<= code #x0F))

(defun status-keyword (code)
  (code-tag code *status-codes*))

(defun operation-keyword (code)
  (code-tag code *operations*))
