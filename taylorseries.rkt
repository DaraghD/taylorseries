#lang racket

#|

Project : Taylor series 

Background info
The taylor series is a series that can be used for any function to approximate f(x), in my case i will be using it to approximate e^x,
as the taylor series approaches infinity it approaches e^x  ,as we cannot (unfortunately) count to infinity,
 we will be taking steps as an argument which is how many steps in the series we will go to and thus an approximation of e^x

Taylor series given by:

x^0/0! + x^1/1! + x^2/2! + x^3/3! ... , or sum from n = 0 to infinity of x^n/n!

I have made two functions to assist me in this - factorial and a taylor series function suited to e^x 
Factorial will be used as apart of the taylor series function, both functions are recursive and have been suited to deal 
with malformed data such as strings and negative numbers 
|#
(define factorial
  (lambda (x)
    (if (false?(integer? x))
        #f 
    (if (< x 0) ; cannot get the factorial of a negative this will clear out "malformed" data 
        #f
    (if (= x 1)
        1
        (* x
           (factorial
            (- x 1))))))))

(define taylorseries
  (lambda (num steps)
    (if (false?(integer? num)) ;these two if statements make sure that there is no string or other malformed data being passed in
         #f
    (if (false?(number? steps))
        #f
    (if (or (< num 0) (< steps 0)) ;makes sure that we are given a positive number 
        #f
    (if(= steps 1)
       (+ num 1)
       (+ (/ (expt num steps)
             (factorial steps))
          (taylorseries
           num(- steps 1)))))))))

#|

For testing purposes of my taylor series function i will be testing how many steps of the series to acheive a certain level of accuracy,
for this i will be pulling e^10 from wolfram alpha and defining it as a variable to subtract from my own calculations 
123 decimal places 
|# 
(define e123 22026.465794806716516957900645284244366353512618556781074235426355225202818570792575199120968164525895451555501092457836652423291)

(- e123 (taylorseries 10 5))
(- e123 (taylorseries 10 10)) ; from 5 to 10 steps was the most notable change with getting over 11,000 closer 
(- e123 (taylorseries 10 20))
(- e123 (taylorseries 10 50)) ;after 50 steps of taylor series we are up to 123 decimal places of accuracy 
(- e123 (taylorseries 10 100))

;all of the below should return false

(factorial 5) ;returns 120 which shows its correct
(factorial -5) ;returns false as it has malformed data
(factorial "malformed string") ;returns false, cant factorial a string
(taylorseries "malformed string" "string")
(taylorseries -1 -1) 
(taylorseries 0.5 0.5) ;only supports integers

#|

Statement                                 Output

(- e123 (taylorseries 10 5))              20548.79912814005
(- e123 (taylorseries 10 10))             9184.16068016827
(- e123 (taylorseries 10 20))             34.98376914165419      
(- e123 (taylorseries 10 50))             0.0
(- e123 (taylorseries 10 100))            0.0 

Therefore as we can conclude that after 50 iterations we acheive e^10 at 123 decimal places which in my opinion is fairly accurate 

(time (taylorseries 10 10000)) ;cpu time: 2652281 real time: 3343680 gc time: 32718
this took around 55 minutes
commented it out so this programm dosent eat up memory on run remove comment if you want to check
unfortunately this function is really inefficient, as it has to re-do multiplication all the way down even though it has
just done it, e.g 9999! is just one multiplication off from 10,000! but since we arent storing 9999! in memory we have to re-calculate it for 10,000!
|#

(time (taylorseries 10 1000)) ; this only takes 2.7 seconds --- cpu time: 1859 real time: 2115 gc time: 31 