;-*-syntax:COMMON-LISP;Package:(RT :use "COMMON-LISP" :colon-mode :external)-*-

#|----------------------------------------------------------------------------|
 | Copyright 1990 by the Massachusetts Institute of Technology, Cambridge MA. |
 |                                                                            |
 | Permission  to  use,  copy, modify, and distribute this software  and  its |
 | documentation for any purpose  and without fee is hereby granted, provided |
 | that this copyright  and  permission  notice  appear  in  all  copies  and |
 | supporting  documentation,  and  that  the  name  of M.I.T. not be used in |
 | advertising or  publicity  pertaining  to  distribution  of  the  software |
 | without   specific,   written   prior   permission.      M.I.T.  makes  no |
 | representations  about  the  suitability of this software for any purpose. |
 | It is provided "as is" without express or implied warranty.                |
 |                                                                            |
 |  M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,  INCLUDING  |
 |  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL  |
 |  M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL  DAMAGES  OR  |
 |  ANY  DAMAGES  WHATSOEVER  RESULTING  FROM  LOSS OF USE, DATA OR PROFITS,  |
 |  WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER  TORTIOUS  ACTION,  |
 |  ARISING  OUT  OF  OR  IN  CONNECTION WITH THE USE OR PERFORMANCE OF THIS  |
 |  SOFTWARE.                                                                 |
 |----------------------------------------------------------------------------|#

;This was the December 19, 1990 version of the regression tester, but
;has since been modified.

(in-package #:reserpt)

(declaim (ftype (function (t) t) expanded-eval %do-tests))
(declaim (type list *tests*))
(declaim (ftype (function (t &rest t) t) report-error))
(declaim (ftype (function (t &optional t) t) %do-test))

(defvar *test-number* 0)
(defvar *test* nil "Current test name")
(defvar *do-tests-when-defined* nil)
(defvar *tests* nil
  "Test database.")
(defvar *items* nil
  "Note database")
(defvar *in-test* nil "Used by TEST")
(defvar *debug* nil "For debugging")
(defvar *catch-errors* t "When true, causes errors in a test to be caught.")
(defvar *print-circle-on-failure* nil
  "Failure reports are printed with *PRINT-CIRCLE* bound to this value.")

(defvar *compile-tests* nil "When true, compile the tests before running them.")
(defvar *expanded-eval* nil "When true, convert the tests into a form that is less likely to have compiler optimizations.")
(defvar *optimization-settings* '((safety 3)))
(defvar *compile-batch-size*
  #+ecl 512
  #-ecl 16
  "Number of tests to compile at the same time.")

(defvar *failed-tests* nil "After DO-TESTS, becomes the list of names of tests that have failed")
(defvar *passed-tests* nil "After DO-TESTS, becomes the list of names of tests that have passed")

(defvar *expected-failures* nil
  "A list of test names that are expected to fail.")

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
  properties)

(defun note-property (note indicator &optional default-value)
  (getf (note-properties note) indicator default-value))

(defun (setf note-property) (new-value note indicator &optional default-value)
  (setf (getf (note-properties note) indicator default-value) new-value))

(defun parse-properties (body &rest indicators)
  (prog (documentation properties initials)
     (when (stringp (car body))
       (setf documentation (pop body)))
   next
     (unless (cdr body)
       (go end))
     (when (member (car body) indicators)
       (setf (getf initials (pop body)) `',(pop body))
       (go next))
     (when (keywordp (car body))
       (setf (getf properties (pop body)) (pop body))
       (go next))
   end
     (return (values documentation properties initials body))))

;;; Note handling functions and macros

(defun add-note (note)
  ;(setq note (copy-note note))
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
    `(add-note (make-note :name ',name
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
  function
  vals)

(defun add-test (test)
  ;(setq test (copy-test test))
  (let ((previous (get (test-name test) :reserpt)))
    (typecase previous
      (test
       (warn "Redefining test ~:@(~S~)" (test-name test)))
      (null)
      (t
       (error "Name conflict for test ~:@(~S~)" (test-name test)))))
  (setf (get (test-name test) :reserpt) test)
  (when *do-tests-when-defined*
    (%do-test test))
  (setq *test* (test-name test)))

(defmacro deftest (name &rest body)
  (multiple-value-bind (documentation properties initials rest)
      (parse-properties body :notes)
    `(add-test (make-test :name ',name
                            :documentation ',documentation
                            :properties ',properties
                            :form ',(pop rest)
                            :vals ',rest
                            ,@initials))))

(defun has-note (test note)
  (unless (note-p note)
    (let ((new-note (get note :reserpt)))
      (setf note new-note)))
  (and note (not (not (member note (test-notes test))))))

(defun test-property (test indicator &optional default-value)
  (getf (test-properties test) indicator default-value))

(defun (setf test-property) (new-value test indicator &optional default-value)
  (setf (getf (test-properties test) indicator default-value) new-value))

#|(defun rem-all-tests (package)
  (with-package-iterator (next-symbol package :internal :external)
    (tagbody
     next
       (multiple-value-bind (presentp symbol)
           (next-symbol)
         (when presentp
           (remprop symbol :reserpt)
           (go next))))))

(defun rem-test (&optional (name *test*))
  (remprop name :reserpt))
|#
#|(defun get-test (&optional (name *test*))
  (defn (get-test name)))|#

(defun get-tests (package)
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
                    tests (merge 'list tests (list (copy-test item)) #'< :key #'test-number)))
             (note
              (setf item (copy-note item)
                    (note-properties item) (copy-list (note-properties item))
                    (gethash (note-name item) items) item))
             (null)
             (t
              (error "Unknown reserpt object")))
           (go next)))
       (return (values tests items)))))

(defun report-error (error? &rest args)
  (cond (*debug*
         (apply #'format t args)
         (if error? (throw '*debug* nil)))
        (error? (apply #'error args))
        (t (apply #'warn args)))
  nil)

(defun do-test (name
                &key ((:catch-errors *catch-errors*) *catch-errors*)
                     ((:compile *compile-tests*) *compile-tests*)
                &aux (test (get name :reserpt)))
  (if (test-p test)
      (%do-test test)
      (error "~%No test with name ~:@(~S~)." name)))

(defun my-aref (a &rest args)
  (apply #'aref a args))

(defun my-row-major-aref (a index)
  (row-major-aref a index))

(defun equalp-with-case (x y)
  "Like EQUALP, but doesn't do case conversion of characters.
   Currently doesn't work on arrays of dimension > 2."
  (cond
   ((eq x y) t)
   ((consp x)
    (and (consp y)
         (equalp-with-case (car x) (car y))
         (equalp-with-case (cdr x) (cdr y))))
   ((and (typep x 'array)
         (= (array-rank x) 0))
    (equalp-with-case (my-aref x) (my-aref y)))
   ((typep x 'vector)
    (and (typep y 'vector)
         (let ((x-len (length x))
               (y-len (length y)))
           (and (eql x-len y-len)
                (loop
                 for i from 0 below x-len
                 for e1 = (my-aref x i)
                 for e2 = (my-aref y i)
                 always (equalp-with-case e1 e2))))))
   ((and (typep x 'array)
         (typep y 'array)
         (not (equal (array-dimensions x)
                     (array-dimensions y))))
    nil)

   ((typep x 'array)
    (and (typep y 'array)
         (let ((size (array-total-size x)))
           (loop for i from 0 below size
                 always (equalp-with-case (my-row-major-aref x i)
                                          (my-row-major-aref y i))))))
   ((typep x 'pathname)
    (equal x y))
   (t (eql x y))))

(defun compile* (lambda-expr &optional muffle-warnings)
  (if muffle-warnings
      (handler-bind
          ((style-warning #'(lambda (c) (muffle-warning c))))
        ;; redirecting *error-output* is the best way to get rid of
        ;; annoying output from the compiler
        (let ((*error-output* (make-broadcast-stream)))
          (compile nil lambda-expr)))
      (compile nil lambda-expr)))

(defun compile-test-function (test)
  (or (test-function test)
      (setf (test-function test)
            (compile* `(lambda ()
                         (declare (optimize ,@*optimization-settings*))
                         ,(test-form test))
                      (test-property test :muffle-warnings t)))))

(defun %do-test (test &optional
                       (s *standard-output*))
  (catch '*in-test*
    (setq *test* (test-name test))
    (setf (test-pending test) t)
    (let* ((*in-test* t)
           ;; (*break-on-warnings* t)
           (aborted nil)
           r)
      ;; (declare (special *break-on-warnings*))

      (block aborted
        (setf r
              (flet ((%do ()
                          (handler-bind
                           #-sbcl nil
                           #+sbcl ((sb-ext:code-deletion-note #'(lambda (c)
                                                                  (when (test-property test :muffle-warnings t)
                                                                    (muffle-warning c)))))
                           (cond
                            (*compile-tests*
                             (multiple-value-list
                              (funcall (compile-test-function test))))
                            (*expanded-eval*
                             (multiple-value-list
                              (expanded-eval (test-form test))))
                            (t
                             (multiple-value-list
                              (eval (test-form test))))))))
                (if *catch-errors*
                    (handler-bind
                     ((style-warning #'(lambda (c) (if (test-property test :muffle-warnings t)
                                                       (muffle-warning c)
                                                       c)))
                      (error #'(lambda (c)
                                 (setf aborted t)
                                 (setf r (list c))
                                 (return-from aborted nil))))
                      (%do))
                    (%do)))))

      (setf (test-pending test)
            (or aborted
                (not (equalp-with-case r (test-vals test)))))

      (when (test-pending test)
        (let ((*print-circle* *print-circle-on-failure*))
          (format s "~&Test ~:@(~S~) failed~%Form: ~S~%Expected value~P:~%"
                  *test* (test-form test) (length (test-vals test)))
          (dolist (v (test-vals test)) (format s "~10t~S~%" v))
          (handler-case
              (progn
                (format s "Actual value~P:~%" (length r))
                (dolist (v r)
                  (format s "~10t~S~:[~; [~2:*~A]~]~%"
                          v (typep v 'condition))))
            (error () (format s "Actual value: #<error during printing>~%")))
          (finish-output s)))))
  (when (not (test-pending test)) *test*))

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

(defun load-expected-failures (expected-failures &key (if-does-not-exist :error))
  "Initialize *expected-failures* and disabled notes from expected-failures.
If expected-failures is a list then just iterate through the list. If the
item is a keyword then disable the note by that name. Otherwise add the test
name to *expected-failures*. If expected-failures is a string or a pathname
then repeatedly READ each symbol from the file and use the same logic as
above. if-does-not-exist is passed to OPEN so it behaves as it does there."
  (flet ((add-expected-failure (name &aux (item (gethash name *items*)))
           (typecase item
             (note
              (setf (note-property item :enabled) (not (note-property item :enabled t))))
             (test
              (push name *expected-failures*))
             (t
              (push name *unknown-expected-failures*)))))
    (setf *expected-failures* nil)
    (if (or (stringp expected-failures)
            (pathnamep expected-failures))
        (with-open-file (stream expected-failures :if-does-not-exist if-does-not-exist)
          (cond (stream
                 (format t "Loading expected failures from ~s~%" expected-failures)
                 (do ((name (read stream nil stream) (read stream nil stream)))
                     ((eq name stream))
                   (add-expected-failure name)))
                (t
                 (format t "Expected failures file ~s not found~%" expected-failures))))
        (dolist (name expected-failures)
          (add-expected-failure name))))
  (setq *unknown-expected-failures* (nreverse *unknown-expected-failures*)
        *expected-failures* (nreverse *expected-failures*)))

(defparameter *sandbox-path* (ignore-errors (truename #P"sandbox/")))

(defun do-tests (package
                 &key (out *standard-output*)
                      ((:catch-errors *catch-errors*) *catch-errors*)
                      ((:compile *compile-tests*) *compile-tests*)
                      (expected-failures nil expected-failures-p)
                      exit)
  (let ((*package* (find-package package))
        (*failed-tests* nil)
        (*passed-tests* nil)
        (*unexpected-failures* nil)
        (*unexpected-successes* nil))
    (multiple-value-bind (*tests* *items*)
        (get-tests *package*)
      (when expected-failures-p
        (load-expected-failures expected-failures)
        (loop for test in *tests*
              do (loop for note-name in (test-notes test)
                       do (loop for (indicator value) on
                                    (note-properties (gethash note-name *items*)) by #'cddr
                                do (setf (test-property test indicator) value)))))
      (dolist (test *tests*)
        (setf (test-pending test) (test-property test :enabled t)))
      (let* ((*default-pathname-defaults* *sandbox-path*)
             (successp (if (streamp out)
                           (%do-tests out)
                           (with-open-file
                               (stream out :direction :output)
                             (%do-tests stream)))))
        (when exit
          (exit successp))
        successp))))

(defun compile-tests
    (stream tests
     &optional (number-of-tests (length tests))
               (batch-size (min number-of-tests *compile-batch-size*))
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
        (when (test-property test :muffle-warnings t)
          (push `(setf (test-function ,test)
                       (lambda ()
                         (declare (optimize ,@*optimization-settings*))
                         ,(test-form test)))
                body))))
    (multiple-value-bind (function warnings-p failure-p)
        (handler-case
            (compile* `(lambda () ,@body))
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
                               batch-size (floor batch-size 2)
                               t))))))

(defun %do-tests (s)
  (let ((count (count t (the list *tests*) :key #'test-pending)))
    (format s "~&Doing ~A pending test~:P of ~A test~:P total.~%"
            count (length *tests*))
    (finish-output s)
    (when *compile-tests*
      (compile-tests s *tests*))
    (dolist (test *tests*)
      (when (test-pending test)
        (let ((success? (%do-test test s)))
          (cond (success?
                 (push (test-name test) *passed-tests*)
                 (when (member (test-name test) *expected-failures*)
                   (push (test-name test) *unexpected-successes*)))
                (t
                 (push (test-name test) *failed-tests*)
                 (unless (member (test-name test) *expected-failures*)
                   (push (test-name test) *unexpected-failures*))))
          (format s "~@[~<~%~:; ~:@(~S~)~>~]" success?))
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

;;; Extended random regression

(defun do-extended-tests (package
                          &key (tests *passed-tests*) (count nil)
                               ((:catch-errors *catch-errors*) *catch-errors*)
                               ((:compile *compile-tests*) *compile-tests*))
  "Execute randomly chosen tests from TESTS until one fails or until
   COUNT is an integer and that many tests have been executed."
  (let* ((*package* (find-package package))
         (*tests* (get-tests *package*))
         (*default-pathname-defaults* (make-pathname :directory (append (pathname-directory (or *compile-file-pathname*
                                                                                                *load-pathname*
                                                                                                *default-pathname-defaults*))
                                                                        '("sandbox"))))
        (test-vector (coerce tests 'simple-vector)))
    (let ((n (length test-vector)))
      (when (= n 0) (error "Must provide at least one test."))
      (loop for i from 0
            for name = (svref test-vector (random n))
            until (eql i count)
            do (print name)
            unless (do-test name) return (values name (1+ i))))))
