(in-package :cl-user)


(defparameter *tests* ())
(defparameter *expected-failures* ())

(defun do-tests (&key compiled)
  (let ((fails ()))
    (format t "~&~%Running ~d test~:p, ~a~%"
            (length *tests*)
            (if compiled
                "compiled"
                "evaluated"))
    (dolist (tst *tests*)
      (destructuring-bind (name fn form expected) tst
        (destructuring-bind (err . vals)
            (if compiled
                (funcall fn)
                (eval form))
          (cond
            ;; No errors allowed
            (err
             (push name fails)
             (format t "ERROR ~a: ~a~%" name err))
            ;; XFAIL
            ((find name *expected-failures*)
             (cond
               ((equalp vals expected)
                (push name fails)
                (format t "XOK   ~a~%" name))
               (t
                (format t "XFAIL ~a:~%  got ~s,~%  expected ~s~%"
                        name vals expected))))
            ;; expected OK
            (t
             (cond
               ((equalp vals expected)
                (format t "OKAY  ~a~%" name))
               (t
                (push name fails)
                (format t "NACK  ~a:~%  got ~s,~%  expected ~s~%"
                        name vals expected))))))))
    (format t "~%~d mismatches: ~s.~%~%" 
            (length fails)
            fails)
    fails))

(defmacro deftest (name form &rest values)
  (let ((form `(block ,name
                 (handler-case ,form
                   (t (e)
                     (list e))
                   (:no-error (&rest vals)
                     (list* nil vals))))))
    `(progn
       (setf *tests*
             (cons (list ',name
                         (lambda () ,form)
                         ',form
                         ',values)
                   (remove ',name *tests*
                           :key #'first)))
       ',name)))

(export '(deftest do-tests
          *tests* *expected-failures*))
