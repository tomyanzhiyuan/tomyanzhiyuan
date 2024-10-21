;;; List Functions

(defun .length (l)
  (if (null l)
      0
      (+ 1 (.length (cdr l)))))

(defun .remove-all (x l)
  (cond ((null l) nil)
        ((equalp x (car l)) (.remove-all x (cdr l)))
        (t (cons (car l) (.remove-all x (cdr l))))))

(defun .map (f l)
  (if (null l)
      nil
      (cons (funcall f (car l)) (.map f (cdr l)))))

(defun .merge (l1 l2)
  (cond ((null l1) l2)
        ((null l2) l1)
        ((<= (car l1) (car l2))
         (cons (car l1) (.merge (cdr l1) l2)))
        (t
         (cons (car l2) (.merge l1 (cdr l2))))))

;;; Set Functions

(defun .element-of (x s)
  (cond ((null s) nil)
        ((equalp x (car s)) t)
        (t (.element-of x (cdr s)))))

(defun .insert (s x)
  (if (.element-of x s)
      s
      (cons x s)))

(defun .difference (s1 s2)
  (cond ((null s1) nil)
        ((.element-of (car s1) s2) (.difference (cdr s1) s2))
        (t (cons (car s1) (.difference (cdr s1) s2)))))

(defun .superseteq (s1 s2)
  (cond ((null s2) t)
        ((.element-of (car s2) s1) (.superseteq s1 (cdr s2)))
        (t nil)))

;;; Math Functions

(defun .factorial (n)
  (if (<= n 1)
      1
      (* n (.factorial (- n 1)))))

(defun .right-tri (a b c)
  (and (> a 0) (> b 0) (> c 0)
       (= (+ (* a a) (* b b)) (* c c))))

(defun .nth-fibo (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (.nth-fibo (- n 1))
              (.nth-fibo (- n 2))))))

(defun .pow (x y)
  (cond ((= y 0) 1)
        ((> y 0) (* x (.pow x (- y 1))))
        (t (/ 1 (.pow x (- y))))))