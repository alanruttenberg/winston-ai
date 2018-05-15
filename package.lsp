(intern "?" 'cl-user)

(defpackage winston-ai
  (:use :cl)
  (:export #:remember-assertion #:remember-rule #:forward-chain #:make-empty-stream #:*rules* #:*assertions*)
  (:import-from :cl-user cl-user::?))
