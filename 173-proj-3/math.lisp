;;; Test Function
(defun test-function (name fn cases)
  (format t "~%Testing ~A:~%" name)
  (let ((all-passed t))
    (dolist (case cases)
      (let* ((input (first case))
             (expected (second case))
             (result (apply fn input))
             (passed (equalp result expected)))
        (unless passed (setf all-passed nil))
        (format t "Input: ~A, Expected: ~A, Got: ~A -> ~A~%" 
                input expected result (if passed "PASS" "FAIL"))))
    all-passed))

(format t "~%=== EXTENDED TEST SUITE WITH EDGE CASES ===~%")

;;; Extended Factorial Tests
(format t "~%1. Extended Factorial Tests:")
(test-function ".FACTORIAL" #'.factorial 
  '(((0) 1)          ; base case
    ((1) 1)          ; base case
    ((5) 120)        ; normal case
    ((3) 6)          ; small number
    ((7) 5040)       ; larger number
    ((10) 3628800)   ; big number
    ((2) 2)          ; small number
    ((4) 24)         ; medium number
    ((6) 720)))      ; medium number

;;; Extended Right Triangle Tests
(format t "~%2. Extended Right Triangle Tests:")
(test-function ".RIGHT-TRI" #'.right-tri
  '(((3 4 5) t)          ; basic right triangle
    ((5 12 13) t)        ; larger right triangle
    ((1 1 1) nil)        ; equal sides, not right triangle
    ((0 0 0) nil)        ; zero case
    ((5 4 3) nil)        ; wrong order
    ((8 15 17) t)        ; larger right triangle
    ((7 24 25) t)        ; larger right triangle
    ((6 8 10) t)         ; another valid right triangle
    ((1 2 3) nil)        ; not a right triangle
    ((9 12 15) t)        ; multiple of 3-4-5
    ((2 3 4) nil)        ; close but not right triangle
    ((5 5 7) nil)        ; isosceles but not right
    ((1 1 2) nil)))      ; degenerate case

;;; Extended Fibonacci Tests
(format t "~%3. Extended Fibonacci Tests:")
(test-function ".NTH-FIBO" #'.nth-fibo
  '(((0) 0)           ; base case
    ((1) 1)           ; base case
    ((2) 1)           ; early number
    ((3) 2)           ; early number
    ((4) 3)           ; early number
    ((5) 5)           ; medium number
    ((6) 8)           ; medium number
    ((7) 13)          ; medium number
    ((8) 21)          ; larger number
    ((9) 34)          ; larger number
    ((10) 55)         ; larger number
    ((11) 89)         ; larger number
    ((12) 144)        ; larger number
    ((15) 610)        ; much larger number
    ((20) 6765)))     ; very large number

;;; Extended Power Function Tests
(format t "~%4. Extended Power Function Tests:")
(test-function ".POW" #'.pow
  '(((2 3) 8)         ; basic positive case
    ((2 0) 1)         ; zero exponent
    ((2 -2) 1/4)      ; negative exponent
    ((5 3) 125)       ; larger base
    ((3 3) 27)        ; same base and exponent
    ((2 8) 256)       ; larger exponent
    ((2 -1) 1/2)      ; negative 1 exponent
    ((10 2) 100)      ; larger base
    ((1 5) 1)         ; base of 1
    ((0 3) 0)         ; base of 0
    ((3 0) 1)         ; any number to 0
    ((4 -2) 1/16)     ; larger negative exponent
    ((2 -3) 1/8)      ; odd negative exponent
    ((5 2) 25)        ; square
    ((3 4) 81)))      ; higher power

(format t "~%=== Extended Test Suite Complete ===~%")