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
        (format t "Case: ~A~%Expected: ~A~%Got: ~A -> ~A~%~%" 
                input expected result (if passed "PASS" "FAIL"))))
    (format t "~%Overall: ~A~%~%" (if all-passed "ALL TESTS PASSED" "SOME TESTS FAILED"))
    all-passed))

(format t "~%=== EXTENDED LIST FUNCTION TESTS ===~%")

;;; Length Tests
(test-function ".LENGTH" #'.length
  '((((1 3 X A)) 4)                ; Basic length test
    ((() ) 0)                      ; Empty list
    (((A B C D E)) 5)              ; All symbols
    (((1 2 3 4 5)) 5)              ; All numbers
    (((1 A 2 B 3 C)) 6)            ; Mixed content
    (((nil)) 1)                    ; List with nil
    (((1 2 (3 4) 5)) 4)            ; Nested structures
    (((T NIL T NIL)) 4)))          ; Boolean values

;;; Remove-all Tests
(test-function ".REMOVE-ALL" #'.remove-all
  '(((A (B A C A A D A)) (B C D))      ; Multiple occurrences
    ((A (X Y Z)) (X Y Z))              ; No occurrences
    ((A ()) NIL)                       ; Empty list
    ((1 (1 2 1 3 1 4)) (2 3 4))        ; Remove numbers
    ((NIL (A NIL B NIL)) (A B))        ; Remove NIL
    ((X (X X X)) ())                   ; All elements same
    ((A (A B A C A)) (B C))            ; Alternating pattern
    ((Z (A B C)) (A B C))))            ; No matching elements

;;; Map Tests
(test-function ".MAP" #'.map
  '(((#'1+ (1 2 3)) (2 3 4))                   ; Basic increment
    ((#'1+ ()) ())                             ; Empty list
    ((#'(lambda (x) (* x x)) (1 2 3 4)) (1 4 9 16))   ; Square function
    ((#'(lambda (x) (+ x 10)) (0 5 10)) (10 15 20))   ; Add constant
    ((#'numberp (1 A 2 B 3 C)) (T NIL T NIL T NIL))   ; Type checking
    ((#'null (NIL NIL NIL)) (T T T))                  ; All NIL
    ((#'(lambda (x) x) (A B C)) (A B C))))            ; Identity function

;;; Merge Tests
(test-function ".MERGE" #'.merge
  '((((1 3 4 7) (2 3 6)) (1 2 3 3 4 6 7))     ; Basic merge
    ((() (1 2 3)) (1 2 3))                     ; First empty
    (((1 2 3) ()) (1 2 3))                     ; Second empty
    ((() ()) ())                               ; Both empty
    (((1 1 2) (1 2 3)) (1 1 1 2 2 3))         ; Duplicates
    (((1 2 3) (4 5 6)) (1 2 3 4 5 6))         ; No overlaps
    (((1 3 5) (2 4 6)) (1 2 3 4 5 6))         ; Alternating
    (((1 2 3) (1 2 3)) (1 1 2 2 3 3))         ; Identical lists
    (((1) (2)) (1 2))                          ; Single elements
    (((1 2 3 4 5) (6 7 8 9 10)) (1 2 3 4 5 6 7 8 9 10))))   ; Long lists

(format t "~%=== Extended Test Suite Complete ===~%")