(in-package #:reserpt)

(declaim (ftype (function (t) t) expanded-eval %do-tests))
(declaim (type list *tests*))
(declaim (ftype (function (t &rest t) t) report-error))
(declaim (ftype (function (t &optional t) t) %do-test))

(defvar *test-number* 0)
(defvar *test* nil "Current test name")
(defvar *tests* nil
  "Test database.")
(defvar *items* nil
  "Note database")
(defvar *in-test* nil "Used by TEST")
(defvar *debug* nil "For debugging")
(defvar *abort-on-error* t "When true signaled ERROR in a test becomes a test abort.")
(defvar *print-circle-on-failure* nil
  "Failure reports are printed with *PRINT-CIRCLE* bound to this value.")

(defvar *test-execution* :eval)
(defvar *compile-declarations* '((optimize (safety 3))))
(defvar *compile-batch-size*
  #+ecl 512
  #-ecl 16
  "Number of tests to compile at the same time.")

(defvar *failed-tests* nil "After DO-TESTS, becomes the list of names of tests that have failed")
(defvar *passed-tests* nil "After DO-TESTS, becomes the list of names of tests that have passed")

(defvar *unknown-expected-failures* nil
  "A list of test names that are expected to fail but are not valid names.")

(defvar *unexpected-successes* nil
  "A list of tests that passed but were expected to fail.")

(defvar *unexpected-failures* nil
  "A list of tests that failed but were not expected to fail.")

;;; Note objects are used to attach information to tests.
;;; A typical use is to mark tests that depend on a particular
;;; part of a set of requirements, or a particular interpretation
;;; of the requirements.

(defstruct note
  name
  documentation
  properties
  notes)

(defun note-property (note indicator &optional default-value)
  (prog ((value (getf (note-properties note) indicator note))
         (notes (note-notes note)))
     (unless (eq value note)
       (return value))
   next
     (when notes
       (setf value (note-property (gethash (pop notes) *items*) indicator note))
       (if (eq value note)
           (go next)
           (return value)))
     (return default-value)))

(defun (setf note-property) (new-value note indicator &optional default-value)
  (setf (getf (note-properties note) indicator default-value) new-value))

(defun parse-properties (body)
  (prog (documentation properties initials)
     (when (stringp (car body))
       (setf documentation (pop body)))
   next
     (unless (cdr body)
       (go end))
     (when (eq (car body) :notes)
       (setf (getf initials (pop body)) `',(pop body))
       (go next))
     (when (keywordp (car body))
       (setf (getf properties (pop body)) (pop body))
       (go next))
   end
     (return (values documentation properties initials body))))

;;; Note handling functions and macros

(defun %defnote (note)
  (let ((previous (get (note-name note) :reserpt)))
    (typecase previous
      (note
       (warn "Redefining note ~:@(~S~)" (note-name note)))
      (null)
      (t
       (error "Name conflict for note ~:@(~S~)" (note-name note)))))
  (setf (get (note-name note) :reserpt) note))

(defmacro defnote (name &rest body)
  (multiple-value-bind (documentation properties initials rest)
      (parse-properties body)
    (declare (ignore initials))
    (when rest
      (error "Non-empty body in note"))
    `(%defnote (make-note :name ',name
                          :documentation ',documentation
                          :properties ',properties))))

(defstruct test
  name
  documentation
  properties
  (number (incf *test-number*))
  pending
  notes
  form
  compiled-form
  vals)

(defun %deftest (test)
  (let ((previous (get (test-name test) :reserpt)))
    (typecase previous
      (test
       (warn "Redefining test ~:@(~S~)" (test-name test)))
      (null)
      (t
       (error "Name conflict for test ~:@(~S~)" (test-name test)))))
  (setf (get (test-name test) :reserpt) test)
  (setq *test* (test-name test)))

(defmacro deftest (name &rest body)
  (multiple-value-bind (documentation properties initials rest)
      (parse-properties body)
    `(%deftest (make-test :name ',name
                          :documentation ',documentation
                          :properties ',properties
                          :form ',(pop rest)
                          :vals ',rest
                          ,@initials))))

(defun test-property (test indicator &optional default-value)
  (prog ((value (getf (test-properties test) indicator test))
         (notes (test-notes test)))
     (unless (eq value test)
       (return value))
   next
     (when notes
       (setf value (note-property (gethash (pop notes) *items*) indicator test))
       (if (eq value test)
           (go next)
           (return value)))
     (return default-value)))

(defun (setf test-property) (new-value test indicator &optional default-value)
  (setf (getf (test-properties test) indicator default-value) new-value))

(defun get-tests (package names)
  (with-package-iterator (next-symbol package :internal :external)
    (prog ((tests nil)
           (items (make-hash-table :test #'eq))
           (item nil))
     next
       (multiple-value-bind (presentp symbol)
           (next-symbol)
         (when presentp
           (typecase (setf item (get symbol :reserpt))
             (test
              (setf item (copy-test item)
                    (test-properties item) (copy-list (test-properties item))
                    (gethash (test-name item) items) item
                    tests (merge 'list tests (list (copy-test item)) #'< :key #'test-number))
              (when (and names (member (test-name item) names))
                (setf (test-property item :skip) nil)))
             (note
              (setf item (copy-note item)
                    (note-properties item) (copy-list (note-properties item))
                    (gethash (note-name item) items) item)
              (when (and names (member (note-name item) names))
                (setf (note-property item :skip) nil)))
             (null)
             (t
              (error "Unknown reserpt object")))
           (go next)))
       (return (values tests items)))))

(defun do-test (name
                &key ((:abort-on-error *abort-on-error*) *abort-on-error*)
                     ((:execution *test-execution*) *test-execution*)
                &aux (test (get name :reserpt)))
  (if (test-p test)
      (%do-test test)
      (error "~%No test with name ~:@(~S~)." name)))

(defun equalp-with-case (x y)
  "Like EQUALP, but doesn't do case conversion of characters."
  (or (eq x y)
      (typecase x
        (cons
         (and (consp y)
              (equalp-with-case (car x) (car y))
              (equalp-with-case (cdr x) (cdr y))))
        (vector
         (and (vectorp y)
              (eql (length x) (length y))
              (every #'equalp-with-case x y)))
        (array
         (and (arrayp y)
              (= (array-rank x) (array-rank y))
              (if (zerop (array-rank x))
                  (equalp-with-case (aref x) (aref y))
                  (and (equal (array-dimensions x) (array-dimensions y))
                       (loop for i below (array-total-size x)
                             always (equalp-with-case (row-major-aref x i)
                                                      (row-major-aref y i)))))))
        (pathname
         (equal x y))
        (t
         (eql x y)))))

(defun compile* (lambda-expr &optional muffle-warnings)
  (if muffle-warnings
      (handler-bind
          ((style-warning #'(lambda (c) (muffle-warning c))))
        ;; redirecting *error-output* is the best way to get rid of
        ;; annoying output from the compiler
        (let ((*error-output* (make-broadcast-stream)))
          (compile nil lambda-expr)))
      (compile nil lambda-expr)))

(defun test-thunk (test)
  (let ((declarations (test-property test :compile-declarations *compile-declarations*)))
    `(lambda ()
       ,@(when declarations
           `((declare ,@declarations)))
       ,(test-form test))))

(defun compile-test (test)
  (or (test-compiled-form test)
      (setf (test-compiled-form test)
            (compile* (test-thunk test)
                      (test-property test :muffle-warnings t)))))

(defun %do-test/actual (test)
  (ecase (test-property test :execution *test-execution*)
    (:compile
     (multiple-value-list (funcall (compile-test test))))
    (:expanded-eval
     (multiple-value-list (expanded-eval (test-form test))))
    (:eval
     (multiple-value-list (eval (test-form test))))))

(defun %do-test/default (test stream)
  (flet ((%do ()
           (handler-bind
               #-sbcl nil
             #+sbcl ((sb-ext:code-deletion-note #'(lambda (c)
                                                    (when (test-property test :muffle-warnings t)
                                                      (muffle-warning c)))))
             (%do-test/actual test)))
         (print-failure (&optional (value nil valuep))
           (let ((*print-circle* *print-circle-on-failure*))
             (format stream
                     "~&Test ~:@(~S~) ~:[aborted~;failed~]~%Form: ~S~%Expected value~P:~%"
                     (test-name test) valuep (test-form test) (length (test-vals test)))
             (dolist (v (test-vals test)) (format stream "~10t~S~%" v))
             (when valuep
               (handler-case
                   (progn
                     (format stream "Actual value~P:~%" (length value))
                     (dolist (v value)
                       (format stream "~10t~S~:[~; [~2:*~A]~]~%"
                               v (typep v 'condition))))
                 (error ()
                   (format stream "Actual value: #<error during printing>~%"))))
             (finish-output stream))))
    (let (r)
      (if (test-property test :abort-on-error *abort-on-error*)
          (handler-bind
              ((style-warning #'(lambda (c)
                                  (if (test-property test :muffle-warnings t)
                                      (muffle-warning c)
                                      c)))
               (error #'(lambda (c)
                          (declare (ignore c))
                          (print-failure)
                          (return-from %do-test/default nil))))
            (setf r (%do)))
          (setf r (%do)))
      (cond ((equalp-with-case r (test-vals test))
             t)
            (t
             (print-failure r)
             nil)))))

(defun %do-test/error (test stream)
  (handler-case
      (%do-test/actual test)
    (error (condition)
      (cond ((typep condition (first (test-vals test)))
             t)
            (t
             (let ((*print-circle* *print-circle-on-failure*))
               (format stream
                       "~&Test ~:@(~S~) failed~%Form: ~S~%Expected ~S error~%"
                       (test-name test) (test-form test) (first (test-vals test)))
               (finish-output stream))
             nil)))
    (:no-error (value)
      (let ((*print-circle* *print-circle-on-failure*))
        (format stream
                "~&Test ~:@(~S~) failed~%Form: ~S~%Expected ~S error:~%"
                     (test-name test) (test-form test) (first (test-vals test)))
        (handler-case
            (progn
              (format stream "Actual value~P:~%" (length value))
              (dolist (v value)
                (format stream "~10t~S~:[~; [~2:*~A]~]~%"
                        v (typep v 'condition))))
          (error ()
            (format stream "Actual value: #<error during printing>~%"))))
      nil)))

(defun %do-test (test &optional (s *standard-output*))
  (catch '*in-test*
    (let ((*test* (test-name test))
          (*in-test* t))
      (prog2
          (setf (test-pending test) t)
          (case (test-property test :result)
            (:error
             (%do-test/error test s))
            (otherwise
             (%do-test/default test s)))
        (setf (test-pending test) nil)))))

(defun expanded-eval (form)
  "Split off top level of a form and eval separately.  This reduces the chance that
   compiler optimizations will fold away runtime computation."
  (if (not (consp form))
      (eval form)
      (let ((op (car form)))
        (cond
          ((eq op 'let)
           (let* ((bindings (loop for b in (cadr form)
                                  collect (if (consp b) b (list b nil))))
                  (vars (mapcar #'car bindings))
                  (binding-forms (mapcar #'cadr bindings)))
             (apply
              (the function
                   (eval `(lambda ,vars ,@(cddr form))))
              (mapcar #'eval binding-forms))))
          ((and (eq op 'let*) (cadr form))
           (let* ((bindings (loop for b in (cadr form)
                                  collect (if (consp b) b (list b nil))))
                  (vars (mapcar #'car bindings))
                  (binding-forms (mapcar #'cadr bindings)))
             (funcall
              (the function
                   (eval `(lambda (,(car vars) &aux ,@(cdr bindings)) ,@(cddr form))))
              (eval (car binding-forms)))))
          ((eq op 'progn)
           (loop for e on (cdr form)
                 do (if (null (cdr e)) (return (eval (car e)))
                        (eval (car e)))))
          ((and (symbolp op) (fboundp op)
                (not (macro-function op))
                (not (special-operator-p op)))
           (apply (symbol-function op)
                  (mapcar #'eval (cdr form))))
          (t (eval form))))))

(defun continue-testing ()
  (if *in-test*
      (throw '*in-test* nil)
      (%do-tests *standard-output*)))

(defun exit (successp &aux (code (if successp 0 1)))
  #+abcl (ext:quit :status code)
  #+acl (excl:exit code :no-unwind t :quiet t)
  #+ccl (ccl:quit code)
  #+cmucl (handler-case (ext:quit nil code)
            ;; Only the most recent versions of cmucl support an exit code.
            ;; If it doesn't, we get a program error (wrong number of args),
            ;; so catch that and just call quit without the arg.
            (program-error ()
              (ext:quit)))
  #+(or clasp clisp ecl) (ext:quit code)
  #+gcl (lisp:quit code)
  #+lispworks (lispworks:quit :status code :ignore-errors-p t)
  #+sbcl (sb-ext:exit :code code))

(defun load-expected-failures (expected-failures skip-failing-tests skip-failing-notes)
  "Initialize *expected-failures* and disabled notes from expected-failures.
If expected-failures is a list then just iterate through the list. If the
item is a keyword then disable the note by that name. Otherwise add the test
name to *expected-failures*. If expected-failures is a string or a pathname
then repeatedly READ each symbol from the file and use the same logic as
above. if-does-not-exist is passed to OPEN so it behaves as it does there."
  (flet ((add-expected-failure (name &aux (item (gethash name *items*)))
           (typecase item
             (note
              (if skip-failing-notes
                  (setf (note-property item :skip) t)
                  (setf (note-property item :expected-failure) t)))
             (test
              (if skip-failing-tests
                  (setf (test-property item :skip) t)
                  (setf (test-property item :expected-failure) t)))
             (t
              (push name *unknown-expected-failures*)))))
    (if (or (stringp expected-failures)
            (pathnamep expected-failures))
        (with-open-file (stream expected-failures :if-does-not-exist :error)
          (format t "Loading expected failures from ~s~%" expected-failures)
          (do ((name (read stream nil stream) (read stream nil stream)))
              ((eq name stream))
            (add-expected-failure name)))
        (dolist (name expected-failures)
          (add-expected-failure name))))
  (setq *unknown-expected-failures* (nreverse *unknown-expected-failures*)))

(defparameter *sandbox-path* (ignore-errors (truename #P"sandbox/")))

(defun %do-tests (s)
  (let ((count (count t (the list *tests*) :key #'test-pending)))
    (format s "~&Doing ~A pending test~:P of ~A test~:P total.~%"
            count (length *tests*))
    (finish-output s)
    ;; Make two passes to account for muffle-warnings
    (compile-tests s (remove-if (lambda (test)
                                  (or (test-property test :muffle-warnings t)
                                      (not (eq (test-property test :execution *test-execution*)
                                               :compile))
                                      (not (test-pending test))))
                                *tests*)
                   :muffle-warnings nil)
    (compile-tests s (remove-if (lambda (test)
                                  (or (not (test-property test :muffle-warnings t))
                                      (not (eq (test-property test :execution *test-execution*)
                                               :compile))
                                      (not (test-pending test))))
                                *tests*)
                   :muffle-warnings t)
    (dolist (test *tests*)
      (when (test-pending test)
        (let ((success? (%do-test test s)))
          (cond (success?
                 (push (test-name test) *passed-tests*)
                 (when (test-property test :expected-failure)
                   (push (test-name test) *unexpected-successes*)))
                (t
                 (push (test-name test) *failed-tests*)
                 (unless (test-property test :expected-failure)
                   (push (test-name test) *unexpected-failures*))))
          (format s "~@[~<~%~:; ~:@(~S~)~>~]" (test-name test)))
        (finish-output s)))
    (setq *passed-tests* (nreverse *passed-tests*)
          *failed-tests* (nreverse *failed-tests*)
          *unexpected-failures* (nreverse *unexpected-failures*)
          *unexpected-successes* (nreverse *unexpected-successes*))
    (format s
            "~&~@[Found unknown test or note names in expected failure list:~
                  ~%  ~<~@{~S~^, ~:_~}~:>~%~]~
             ~A failure~:P with ~A unexpected failure~:P and ~A unexpected ~
             success~:*~[es~;~:;es~] out of ~A test~:P.~%~
             ~:[No failures~;~:*Failures: ~{~%  ~S~}~]~%~
             ~:[No unexpected failures~;~:*Unexpected failures: ~{~%  ~S~}~]~%~
             ~:[No unexpected successes~;~:*Unexpected successes: ~{~%  ~S~}~]~%"
            *unknown-expected-failures*
            (length *failed-tests*) (length *unexpected-failures*)
            (length *unexpected-successes*) count
            *failed-tests* *unexpected-failures*
            *unexpected-successes*)
    (terpri)
    (finish-output s)
    (values (null *unexpected-failures*)
            *passed-tests*
            *failed-tests*
            *unexpected-failures*
            *unexpected-successes*)))

(defun do-tests (package
                 &key tests
                      (stream *standard-output*)
                      ((:abort-on-error *abort-on-error*) *abort-on-error*)
                      ((:execution *test-execution*) *test-execution*)
                      (expected-failures nil expected-failures-p)
                      skip-failing-tests (skip-failing-notes t)
                      exit)
  (let ((*package* (find-package package))
        (*failed-tests* nil)
        (*passed-tests* nil)
        (*unexpected-failures* nil)
        (*unexpected-successes* nil)
        (default-skip (and tests t)))
    (multiple-value-bind (*tests* *items*)
        (get-tests *package* tests)
      (when expected-failures-p
        (load-expected-failures expected-failures skip-failing-tests skip-failing-notes))
      (dolist (test *tests*)
        (setf (test-pending test) (not (test-property test :skip default-skip))))
      (let* ((*default-pathname-defaults* *sandbox-path*)
             (successp (if (streamp stream)
                           (%do-tests stream)
                           (with-open-file
                               (stream stream :direction :output)
                             (%do-tests stream)))))
        (when exit
          (exit successp))
        successp))))

(defun compile-tests
    (stream tests
     &key (number-of-tests (length tests))
          (batch-size (min number-of-tests *compile-batch-size*))
          (muffle-warnings t)
          silent)
  "Compile all test functions in batches"
  (do ((remaining-tests tests (nthcdr batch-size remaining-tests))
       (remaining-number-of-tests number-of-tests (- remaining-number-of-tests batch-size))
       (body nil nil))
      ((or (null remaining-tests) (<= remaining-number-of-tests 0)))
    (unless (or silent
                (let ((processed-number-of-tests
                        (- number-of-tests remaining-number-of-tests)))
                  ;; only output the message every 1024 tests
                  (= (ceiling processed-number-of-tests 1024)
                     (ceiling (+ processed-number-of-tests batch-size)
                              1024))))
      (format stream "~&~A of ~A tests remaining to be compiled.~%"
              remaining-number-of-tests number-of-tests))
    (do ((n 0 (1+ n))
         (current-tests remaining-tests (rest current-tests)))
        ((or (null current-tests) (>= n batch-size)
             (>= n remaining-number-of-tests)))
      (let ((test (first current-tests)))
        (push `(setf (test-compiled-form (gethash ',(test-name test) *items*))
                     ,(test-thunk test))
              body)))
    (multiple-value-bind (function warnings-p failure-p)
        (handler-case
            (compile* `(lambda () ,@body) muffle-warnings)
          (warning () (values nil t t))
          (error () (values nil t t)))
      (declare (ignore warnings-p))
      (if (not failure-p)
          (funcall function)
          (if (= batch-size 1)
              (format stream "~&Cannot compile test function for test ~A.~%"
                      (test-name (first remaining-tests)))
              ;; Something went wrong, try to narrow down where
              (compile-tests stream remaining-tests
                             :number-of-tests batch-size
                             :batch-size (floor batch-size 2)
                             :silent t
                             :muffle-warnings muffle-warnings))))))

;;; Extended random regression

(defun do-extended-tests (package
                          &key tests (count nil)
                               ((:abort-on-error *abort-on-error*) *abort-on-error*)
                               ((:execution *test-execution*) *test-execution*))
  "Execute randomly chosen tests from TESTS until one fails or until
   COUNT is an integer and that many tests have been executed."
  (let ((*package* (find-package package))
        (*failed-tests* nil)
        (*passed-tests* nil)
        (*unexpected-failures* nil)
        (*unexpected-successes* nil)
         (*default-pathname-defaults* (make-pathname :directory (append (pathname-directory (or *compile-file-pathname*
                                                                                                *load-pathname*
                                                                                                *default-pathname-defaults*))
                                                                        '("sandbox")))))
    (multiple-value-bind (*tests* *items*)
        (get-tests *package* tests)
      (loop with test-vector = (coerce (loop for test in *tests*
                                       unless (test-property test :skip)
                                         collect test)
                                       'simple-vector)
            with n = (length test-vector)
            initially (when (= n 0)
                        (error "Must provide at least one test."))
            for name = (svref test-vector (random n))
            until (eql i count)
            do (print name)
            unless (do-test name) return (values name (1+ i))))))
