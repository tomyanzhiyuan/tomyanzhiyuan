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

;;; Extended Set Function Tests
(format t "~%=== EXTENDED SET FUNCTION TESTS ===~%")

;;; Element-of Tests
(test-function ".ELEMENT-OF" #'.element-of 
  '(((3 (1 3 X A)) t)              ; Basic membership
    ((2 (1 3 X A)) nil)            ; Basic non-membership
    ((2 ()) nil)                   ; Empty set
    ((A (A B C)) t)                ; Symbol membership
    ((D (A B C)) nil)              ; Symbol non-membership
    ((nil (nil A B)) t)            ; nil as element
    ((() ()) nil)                  ; Empty list as element
    ((X (1 2 X 4)) t)              ; Symbol in number list
    ((1 (1 1 1)) t)                ; Repeated elements
    ((A ()) nil)))                 ; Empty set test

;;; Insert Tests
(test-function ".INSERT" #'.insert
  '((((B C D) A) (A B C D))        ; Basic insert
    (((A B C D) A) (A B C D))      ; Insert existing element
    ((() A) (A))                   ; Insert into empty set
    (((1 2 3) 1) (1 2 3))          ; Number already exists
    (((1 2 3) 4) (4 1 2 3))        ; Insert new number
    (((A) A) (A))                  ; Single element, already exists
    (((nil) t) (t nil))            ; Insert with nil in set
    ((() nil) (nil))               ; Insert nil into empty set
    (((A B C) D) (D A B C))        ; Normal insert
    (((1 2 3) X) (X 1 2 3))))      ; Mix numbers and symbols

;;; Difference Tests
(test-function ".DIFFERENCE" #'.difference
  '(((((A B C) (A C D)) (B))       ; Basic difference
    (((A C D) (A B C)) (D))        ; Reverse difference
    ((() (A B C)) ())              ; Empty first set
    (((A B C) ()) (A B C))         ; Empty second set
    ((() ()) ())                   ; Both empty
    (((1 2 3) (2 3 4)) (1))        ; Numbers
    (((A A B) (A C)) (B))          ; Duplicate in first set
    (((X Y Z) (A B C)) (X Y Z))    ; No common elements
    (((A B C) (A B C)) ())         ; Identical sets
    (((A B C D) (B D)) (A C))))    ; Multiple elements difference

;;; Superseteq Tests
(test-function ".SUPERSETEQ" #'.superseteq
  '((((A B C D) (A B)) t)          ; Basic superset
    (((A B) (A B)) t)              ; Equal sets
    (() (A B)) nil)                ; Empty set vs non-empty
    (((A B) ()) t)                 ; Any set is superset of empty
    ((() ()) t)                    ; Empty set equality
    (((1 2 3 4) (1 2)) t)          ; Numbers superset
    (((1 2) (1 2 3)) nil)          ; Not a superset
    (((A) (B)) nil)                ; Disjoint sets
    (((A B C) (X Y Z)) nil)        ; No common elements
    (((A A B) (A B)) t)))          ; Duplicate elements in superset

(format t "~%=== Extended Test Suite Complete ===~%")