;;; List Functions
;;; Length: Count number of elements in a list
(defun .length (l)
  (if (null l)
    0
    (+ 1 (.length (cdr l)))))

;;; Remove-all: Remove all occurrences of X from list L
(defun .remove-all (x l)
  (cond 
    ((null l) nil)
    ((equalp x (car l)) 
     (.remove-all x (cdr l)))
    (t (cons (car l) 
             (.remove-all x (cdr l))))))

;;; Map: Apply function F to each element of list L
(defun .map (f l)
  (if (null l)
    nil
    (cons (funcall f (car l)) 
          (.map f (cdr l)))))

;;; Merge: Merge two sorted lists maintaining order
(defun .merge (l1 l2)
  (cond ((null l1) l2)
    ((null l2) l1)
    ((<= (car l1) (car l2))
     (cons (car l1) (.merge (cdr l1) l2)))
    (t (cons (car l2) (.merge l1 (cdr l2))))))

;;; Set Functions
;;; Element-of: Check if X is an element of set S
(defun .element-of (x s)
  (cond ((null s) nil)
    ((equalp x (car s)) t)
    (t (.element-of x (cdr s)))))

;;; Insert: Add X to set S if not already present
(defun .insert (s x)
  (if (.element-of x s)
    s
    (cons x s)))

;;; Difference: Return elements in S1 that are not in S2
(defun .difference (s1 s2)
  (cond ((null s1) nil)
    ((.element-of (car s1) s2) 
     (.difference (cdr s1) s2))
    (t (cons (car s1) 
             (.difference (cdr s1) s2)))))

;;; Superseteq: Check if S1 is a superset or equal to S2
(defun .superseteq (s1 s2)
  (cond ((null s2) t)
    ((.element-of (car s2) s1) 
     (.superseteq s1 (cdr s2)))
    (t nil)))

;;; math functions
(defun .factorial (n)
  (cond ((< n 0) nil)        ; Handle negative numbers
    ((= n 0) 1)          ; Base case: 0! = 1
    (t (* n (.factorial (- n 1))))))

;;; Right triangle checker - validates if sides form a right triangle
(defun .right-tri (a b c)
  (and (> a 0) (> b 0) (> c 0)                ; Ensure all are positive
       (= (* c c) (+ (* a a) (* b b)))))      ; Check Pythagorean theorem

;;; Fibonacci helper
(defun .fibo-helper (n a b)              
  (cond ((< n 0) nil)                     ; Handle negative
        ((= n 0) a)                       ; Base case
        (t (.fibo-helper (- n 1) b (+ a b)))))
        
;;; main Fibonacci func
(defun .nth-fibo (n)
  (.fibo-helper n 0 1))

;;; Power function - computes X raised to power Y
(defun .pow (x y)
  (cond ((= y 0) 1)                          ; Anything^0 = 1
    ((> y 0) (* x (.pow x (- y 1))))     ; Positive exponents
    (t (/ 1 (.pow x (- y))))))           ; Negative exponents
