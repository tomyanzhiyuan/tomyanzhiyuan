(load "tomyanzhiyuan/173-proj-3/project3.lisp")

; sbcl
; (load "tomyanzhiyuan/173-proj-3/testP3.lisp)
; (run-all-tests)

;;; Test Helper Function
(defun run-test (fn args expected)
  (let ((result (apply fn args)))
    (format t "~A ~A => ~A : ~A~%"
            fn args result (if (equalp result expected) "PASS" "FAIL"))))

;;; List Function Tests
(defun test-list-functions ()
  (format t "~%Testing List Functions:~%")
  (run-test #'.length '(()) 0)
  (run-test #'.length '((1 2 3)) 3)
  (run-test #'.length '((1 2 3 4 5 6 7 8 9 10)) 10)

  (run-test #'.remove-all '(a (a b a c a d)) '(b c d))
  (run-test #'.remove-all '(a ()) ())
  (run-test #'.remove-all '(a (b c d)) '(b c d))

  (run-test #'.map '(#'1+ (1 2 3)) '(2 3 4))
  (run-test #'.map '(#'1+ ()) ())
  (run-test #'.map '(#'(lambda (x) (* x x)) (1 2 3 4 5)) '(1 4 9 16 25))

  (run-test #'.merge '((1 3 5) (2 4 6)) '(1 2 3 4 5 6))
  (run-test #'.merge '(() (1 2 3)) '(1 2 3))
  (run-test #'.merge '((1 2 3) ()) '(1 2 3))
  (run-test #'.merge '((1 1 2) (1 2 3)) '(1 1 1 2 2 3)))

;;; Set Function Tests
(defun test-set-functions ()
  (format t "~%Testing Set Functions:~%")
  (run-test #'.element-of '(a (a b c)) t)
  (run-test #'.element-of '(d (a b c)) nil)
  (run-test #'.element-of '(a ()) nil)

  (run-test #'.insert '((a b c) d) '(d a b c))
  (run-test #'.insert '((a b c) a) '(a b c))
  (run-test #'.insert '(() a) '(a))

  (run-test #'.difference '((a b c) (b c d)) '(a))
  (run-test #'.difference '((a b c) (d e f)) '(a b c))
  (run-test #'.difference '(() (a b c)) ())

  (run-test #'.superseteq '((a b c d) (b c)) t)
  (run-test #'.superseteq '((a b c) (b c d)) nil)
  (run-test #'.superseteq '(() ()) t)
  (run-test #'.superseteq '((a b c) ()) t))

;;; Math Function Tests
(defun test-math-functions ()
  (format t "~%Testing Math Functions:~%")
  (run-test #'.factorial '(0) 1)
  (run-test #'.factorial '(1) 1)
  (run-test #'.factorial '(5) 120)

  (run-test #'.right-tri '(3 4 5) t)
  (run-test #'.right-tri '(5 12 13) t)
  (run-test #'.right-tri '(1 1 1) nil)
  (run-test #'.right-tri '(0 0 0) nil)

  (run-test #'.nth-fibo '(0) 0)
  (run-test #'.nth-fibo '(1) 1)
  (run-test #'.nth-fibo '(6) 8)
  (run-test #'.nth-fibo '(10) 55)

  (run-test #'.pow '(2 3) 8)
  (run-test #'.pow '(2 0) 1)
  (run-test #'.pow '(2 -2) 1/4)
  (run-test #'.pow '(5 3) 125))

;;; Run all tests
(defun run-all-tests ()
  (test-list-functions)
  (test-set-functions)
  (test-math-functions))

;; To run all tests, call (run-all-tests)

(run-all-tests)