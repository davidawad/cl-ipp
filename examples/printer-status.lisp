;;; Live, READ-ONLY query (Get-Printer-Attributes + Get-Jobs). Sends no print job.
;;; Run:  sbcl --load ~/quicklisp/setup.lisp --load examples/printer-status.lisp   (set IPP_URI to override)

(push (truename (merge-pathnames "../" (directory-namestring *load-truename*)))
      asdf:*central-registry*)
(ql:quickload :cl-ipp :silent t)

(let* ((uri (or (uiop:getenv "IPP_URI")
                "ipp://BRW000000000000.local:631/ipp/print"))
       (info (cl-ipp:printer-info uri))
       (attrs (getf info :attributes)))
  (format t "~A~%state: ~(~A~) ~A~%"
          (getf attrs :printer-make-and-model)
          (getf info :state) (getf info :state-reasons))
  (dolist (m (getf info :markers))
    (format t "  ~A (~A): ~A%~%" (getf m :name) (getf m :color) (getf m :level)))
  (let ((jobs (cl-ipp:job-summaries
               (cl-ipp:get-jobs uri :which-jobs "not-completed" :limit 10
                                    :requested-attributes '("job-id" "job-state" "job-name")))))
    (format t "active jobs: ~D~%" (length jobs))
    (dolist (j jobs) (format t "  ~S~%" j))))
