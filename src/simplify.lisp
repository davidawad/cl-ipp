(in-package #:cl-ipp)

;;; Turn decoded messages into plain plists. Attribute names become keywords
;;; (upcased: "printer-state" -> :PRINTER-STATE). A single value is unwrapped; a
;;; multi-valued attribute is a list. Collections become plists.

(defun name->keyword (name)
  (intern (string-upcase name) :keyword))

(defun simplify-value (value)
  (destructuring-bind (tag . v) value
    (if (eq tag :begin-collection) (simplify-attributes v) v)))

(defun simplify-attributes (attributes)
  "Attributes -> plist."
  (loop for (name . values) in attributes
        append (list (name->keyword name)
                     (let ((vs (mapcar #'simplify-value values)))
                       (if (cdr vs) vs (car vs))))))

(defun simplify-message (message)
  "MESSAGE -> plist (:version :status :request-id :groups ((group . plist) ...)).
:status is the status keyword (or integer if unknown); :code keeps the raw number."
  (list :version (message-version message)
        :code (message-code message)
        :status (status-keyword (message-code message))
        :request-id (message-request-id message)
        :groups (loop for (g . attrs) in (message-groups message)
                      collect (cons g (simplify-attributes attrs)))))

(defun response-ok-p (message)
  (< (message-code message) #x0100))

(defun ensure-list (x) (if (listp x) x (list x)))

(defun printer-summary (message)
  "Printer group of a Get-Printer-Attributes response as a plist with the common
fields pre-digested:
  :state          :idle / :processing / :stopped
  :state-reasons  list of strings
  :markers        list of (:name :color :type :level :low-level :high-level)
plus every other printer attribute under :attributes (a plist)."
  (let* ((attrs (simplify-attributes (group-attributes message :printer)))
         (names (ensure-list (getf attrs :marker-names)))
         (colors (ensure-list (getf attrs :marker-colors)))
         (types (ensure-list (getf attrs :marker-types)))
         (levels (ensure-list (getf attrs :marker-levels)))
         (lows (ensure-list (getf attrs :marker-low-levels)))
         (highs (ensure-list (getf attrs :marker-high-levels))))
    (list :state (let ((s (getf attrs :printer-state)))
                   (or (cdr (assoc s *printer-states*)) s))
          :state-reasons (ensure-list (getf attrs :printer-state-reasons))
          :markers (loop for n in names
                         for i from 0
                         collect (list :name n
                                       :color (nth i colors)
                                       :type (nth i types)
                                       :level (nth i levels)
                                       :low-level (nth i lows)
                                       :high-level (nth i highs)))
          :attributes attrs)))

(defun job-summaries (message)
  "One plist per :job group of a Get-Jobs response, with :state keyworded."
  (loop for (g . attrs) in (message-groups message)
        when (eq g :job)
          collect (let* ((p (simplify-attributes attrs))
                         (s (getf p :job-state)))
                    (list* :state (or (cdr (assoc s *job-states*)) s) p))))
