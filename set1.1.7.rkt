#lang sicp

(define (sqrt x)
  (define (good? g)
    (define (abs x)
      (if (> 0 x) (- x) x))
    (define (square x) (* x x))
    (< (abs (- (square g) x)) 0.001))
  (define (improve g)
    (define (average x y) (/ (+ x y) 2))
    (average g (/ x g)))
  (define (try g)
   (if (good? g)
       g
       (try (improve g))))
  (try 1.0))

    
;    Exercise 1.7: The good-enough? test used in computing square roots will not be very effective for finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost always performed with limited precision. This makes our test inadequate for very large numbers. Explain these statements, with examples showing how the test fails for small and large numbers. An alternative strategy for implementing good-enough? is to watch how guess changes from one iteration to the next and to stop when the change is a very small fraction of the guess. Design a square-root procedure that uses this kind of end test. Does this work better for small and large numbers? 
(sqrt 2)
(sqrt 1e-4)
(sqrt 1e-32)
(sqrt 1e-10)
;this one locks up because the computer can't represent a number that is closer that .0001 to this number
;(sqrt 1.1e32)

(define (goodroot x)
  (define (good? g1 g2)
    (define (abs x)
      (if (> 0 x) (- x) x))
    (< (abs (- g1 g2)) (* g2 1e-64)))
  (define (improve g)
    (define (average x y) (/ (+ x y) 2))
    (average g (/ x g)))
  (define (try g-last g-next)
   (if (good? g-last g-next)
       g-next
       (try g-next (improve g-next))))
  (try x 1.0))

(goodroot 2)
(goodroot 1e-4)
(goodroot 1e-32)
(goodroot 1e-10)
(goodroot 1.1e32)
 
;    Exercise 1.8: Newton’s method for cube roots is based on the fact that if y is an approximation to the cube root of x , then a better approximation is given by the value
;    (x/y^2 + 2y)/3 .
;    Use this formula to implement a cube-root procedure analogous to the square-root procedure. (In 1.3.4 we will see how to implement Newton’s method in general as an abstraction of these square-root and cube-root procedures.) 
(define (cuberoot x)
  (define (good? g1 g2)
    (define (abs x)
      (if (> 0 x) (- x) x))
    (< (abs (- g1 g2)) (* g2 1e-64)))
  (define (improve g)
    (/ (+ (/ x g g) (* 2 g)) 3))
  (define (try g-last g-next)
   (if (good? g-last g-next)
       g-next
       (try g-next (improve g-next))))
  (try x 1.0))

(cuberoot 27)
