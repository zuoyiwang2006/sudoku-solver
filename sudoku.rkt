;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; Wyn Wang (21186308)
;; CS 135 Fall 2025
;; Assignment 10 problem 01 sudoku
;;

;;Question 01 part a

(define (all-satisfy? pred? mx)
  (local [(define (satisfy? pred? l)
            (cond [(empty? l) true]
                  [(pred? (first l)) (satisfy? pred? (rest l))]
                  [else false]))]
    (cond [(empty? mx) true]
          [(satisfy? pred? (first mx)) (all-satisfy? pred? (rest mx))]
          [else false])))

;(check-expect (all-satisfy? integer? '((2 3 4) (5 6 7))) true)
;(check-expect (all-satisfy? integer? '((2 3 4) (5 six 7))) false)

;;Question 01 part b
(define (any-satisfy? pred? mx)
  (not (all-satisfy? (lambda (x) (not (pred? x))) mx)))
;(check-expect (any-satisfy? symbol? '((2 3 4) (5 6 7))) false)
;(check-expect (any-satisfy? symbol? '((2 3 4) (5 six 7))) true)

;;Question 01 part c
;; Requires: clause that some value in the Matrix satisfies the predicate.
(define (find-where pred? m)
  (local [(define (one-satisfy? pred? l)
            (cond [(empty? l) false]
                  [(pred? (first l)) true]
                  [else (one-satisfy? pred? (rest l))]))
          (define (findp pred? m c r)
            (cond [(one-satisfy? pred? (first m))
                   (cond [(pred? (first (first m))) (list c r)]
                         [else (findp pred? (cons (rest (first m)) (rest m)) (add1 c) r)])]
                  [else (findp pred? (rest m) c (add1 r))]))]
    (findp pred? m 0 0)))
(define wherematrix '(( 1    2    3    4 )
                      ( 4    5  (3 6)  (1 2) )
                      ((7)  8     9    () )))
;(check-expect (find-where list? wherematrix) '(2 1))
;(check-expect (find-where empty? wherematrix) '(3 2))
;(check-expect (find-where integer? wherematrix) '(0 0))

;;Question 02
;; A Cell is a (anyof (listof Nat) Nat)
;; Requires: Nat values are positive.
;; List values contain no duplicates, and are in increasing order.

;; A Puzzle is a (matrixof Cell)

;; A Single is a (list Nat). That is, a list of length exactly 1.

;; A Solution is a (matrixof Nat).
(define (strings->puzzle ls)
  (local [(define (trans-char ls)
            (cond [(empty? ls) empty]
                  [else (cons (string->list (first ls)) (strings->puzzle (rest ls)))]))
          (define (add-up list-of-char)
            (build-list (length list-of-char) (lambda (x) (add1 x))))
          (define (replace-cell list-of-char l)
            (cond [(empty? list-of-char) empty]
                  [(char=? (first list-of-char) #\1) (cons '(1) (replace-cell (rest list-of-char) l))]
                  [(char=? (first list-of-char) #\2) (cons '(2) (replace-cell (rest list-of-char) l))]
                  [(char=? (first list-of-char) #\3) (cons '(3) (replace-cell (rest list-of-char) l))]
                  [(char=? (first list-of-char) #\4) (cons '(4) (replace-cell (rest list-of-char) l))]
                  [(char=? (first list-of-char) #\5) (cons '(5) (replace-cell (rest list-of-char) l))]
                  [(char=? (first list-of-char) #\6) (cons '(6) (replace-cell (rest list-of-char) l))]
                  [(char=? (first list-of-char) #\7) (cons '(7) (replace-cell (rest list-of-char) l))]
                  [(char=? (first list-of-char) #\8) (cons '(8) (replace-cell (rest list-of-char) l))]
                  [(char=? (first list-of-char) #\9) (cons '(9) (replace-cell (rest list-of-char) l))]
                  [else (cons (add-up l) (replace-cell (rest list-of-char) l))]))]
    (cond [(empty? ls) empty]
          [else (cons (replace-cell (first (trans-char ls)) (first (trans-char ls)))
                      (strings->puzzle (rest ls)))])))
;(check-expect (strings->puzzle '("???"
;                                 "?3?"
;                                 "??2"))
;              '(( (1 2 3) (1 2 3) (1 2 3) )
;                ( (1 2 3) (3) (1 2 3) )
;                ( (1 2 3) (1 2 3) (2) )))
;(check-expect (strings->puzzle '("??3?"
;                                 "??2?"
;                                 "?4??"
;                                 "????"))
;              '(( (1 2 3 4) (1 2 3 4) (3) (1 2 3 4) )
;                ( (1 2 3 4) (1 2 3 4) (2) (1 2 3 4) )
;                ( (1 2 3 4) (4) (1 2 3 4) (1 2 3 4) )
;                ( (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) )))

;;Question 3
(define (index n lst)
  (first (foldl (lambda (x y) (rest y)) lst
                (build-list n +))))
(define (single? A)
  (local [(define (singlel? lst)
            (cond [(empty? lst) false]
                  [(not (list? (first lst))) (singlel? (rest lst))]
                  [(= 1 (length (first lst))) true]
                  [else (singlel? (rest lst))]))]
    (cond [(empty? A) false]
          [(singlel? (first A)) true]
          [else (single? (rest A))])))
(define (replace-single lst)
  (cond [(empty? lst) empty]
        [(not (list? (first lst))) (cons (first lst) (replace-single (rest lst)))]
        [(= 1 (length (first lst))) (cons (first (first lst)) (rest lst))]
        [else (cons (first lst) (replace-single (rest lst)))]))
(define (get-rid num l)
  (cond [(list? l) (filter (lambda (x) (not (= num x))) l)]
        [else l]))
(define (loc lnm A)
  (local [(define n (first lnm))
                 (define m (second lnm))]
    (cond [(zero? (second lnm))
           (cond [(zero? n) (first (first A))]
                 [else (loc (cons (sub1 n) (rest lnm)) (cons (rest (first A)) (rest A)))])]
          [else (loc (list n (sub1 m)) (rest A))])))
(define (filter-row num lst)
  (build-list (length lst) (lambda (x) (get-rid num (index x lst)))))
(define (filter-col n num lst)
  (build-list (length lst) (lambda (x)
                             (cond [(= n x) (get-rid num (index x lst))]
                                   [else (index x lst)]))))
;;;替换坐标
(define (loc-replace A)
  (find-where (lambda (lst)
                (cond [(list? lst) (= 1 (length lst))]
                      [else false])) A))
(define (replace-what A)
  (first (loc (loc-replace A) A)))

(define (remove-singlef A)
  (local [(define (remove-single A A1 m)
            (cond [(empty? A) empty]
                  [(= m 0) (cons (filter-row (replace-what A1) (replace-single (first A)))
                                 (remove-single (rest A) A1 (sub1 m)))]
                  [else (cons (filter-col (first (loc-replace A1)) (replace-what A1) (first A))
                              (remove-single (rest A) A1 (sub1 m)))]))]
    (remove-single A A (second (loc-replace A)))))

(define (remove-singles A)
  (cond [(single? A) (remove-singles (remove-singlef A))]
        [else A]))
;(check-expect (remove-singles '(( (1 2 3) (1 2 3) (1 2 3) )
;                                ( (1 2 3) (3) (1 2 3) )
;                                ( (1 2 3) (1 2 3) (2) )))
;              (list (list 1 2 3)
;                    (list 2 3 1)
;                    (list 3 1 2)))
(check-expect (remove-singles (strings->puzzle '("??3?"
                                                 "??2?"
                                                 "?4??"
                                                 "????")))
              '(( (1 2 4) (1 2) 3 (1 2 4) )
                ( (1 3 4) (1 3) 2 (1 3 4) )
                ( (2 3) 4 1 (2 3) )
                ( (1 2 3) (1 2 3) 4 (1 2 3) )))

;;Question 4
;; Ignore the parameter; always produce true.
;; yes: Any -> Bool
(define (yes x) true)
;; Ignore the parameter; always produce false.
;; no: Any -> Bool
(define (no x) false)
;; Determine if the diagonal of p has a 2 in it.
;; diagonal-has-2?: Solution -> Bool
(define (diagonal-has-2? p)
  (and (not (empty? p))
       (or (= 2 (first (first p)))
           (diagonal-has-2? (map rest (rest p))))))


(define (cell puzzle)
    (cond [(empty? puzzle) empty]
          [else (local [(define row (first puzzle))]
                  (cond [(empty? row) (cell (rest puzzle))]
                        [(cons? (first row)) (first row)]
                        [else (cell (cons (rest row) (rest puzzle)))]))]))
(define (replace-cell n A)
  (local [(define (replace-cell-l n lst)
            (cond [(empty? lst) empty]
                  [(cons? (first lst))  (cons (list (index n (first lst))) (rest lst))]
                  [else (cons (first lst) (replace-cell-l n (rest lst)))]))
          (define (list=? l1 l2)
            (cond [(empty? l1) (empty? l2)]
                  [(empty? l2) false]
                  [(list? (first l1))
                   (cond [(list? (first l2)) (and (list=? (first l1) (first l2))
                                                  (list=? (rest l1) (rest l2)))]
                         [else false])]
                  [(list? (first l2)) false]
                  [(= (first l1) (first l2)) (list=? (rest l1) (rest l2))]
                  [else false]))]
    (cond [(empty? A) empty]
          [else (local [(define row (first A))]
             (cond [(list=? (replace-cell-l n row) row) (cons row (replace-cell n (rest A)))]
                   [else (cons (replace-cell-l n row) (rest A))]))])))

(define (solve-latin requirement puzzle)
  (local [(define (no-list? puzzle)
                   (cond [(empty? puzzle) true]
                         [(empty? (first puzzle)) (no-list? (rest puzzle))]
                         [(list? (first (first puzzle))) false]
                         [else (no-list? (cons (rest (first puzzle)) (rest puzzle)))]))
          (define spuzzle (remove-singles puzzle))]
    (cond [(no-list? spuzzle)
           (cond [(requirement spuzzle) spuzzle]
                 [else empty])]
          [else (local [(define fs (find-solution (cell spuzzle) requirement spuzzle 0))
                        (define (any-empty? A)
                          (local [(define (any-emptyl? lst)
                                    (cond [(empty? lst) false]
                                          [(empty? (first lst)) true]
                                          [else (any-emptyl? (rest lst))]))]
                            (cond [(empty? A) false]
                                  [(any-emptyl? (first A)) true]
                                  [else (any-empty? (rest A))])))]
                  (cond [(any-empty? spuzzle) empty]
                        [(empty? fs) empty]
                        [else fs]))])))
(define (find-solution cell requirement puzzle a)
    (cond [(empty? cell) empty]
          [else (local [(define (ns n) (solve-latin requirement (replace-cell n puzzle)))]
                  (cond [(>= a (length cell)) empty]
                        [(empty? (ns a)) (find-solution cell requirement puzzle (add1 a))]
                        [else (ns a)]))]))
;(define 23puzzle (strings->puzzle '("???"
;                                    "?3?"
;                                    "??2")))
;(check-expect (solve-latin yes 23puzzle)
;              '((1 2 3)
;                (2 3 1)
;                (3 1 2)))
;(define 324puzzle (strings->puzzle '("??3?"
;                                     "??2?"
;                                     "?4??"
;                                     "????")))
;(check-expect (solve-latin yes 324puzzle)
;              '((1 2 3 4)
;                (4 1 2 3)
;                (3 4 1 2)
;                (2 3 4 1)))
;(check-expect (solve-latin diagonal-has-2? 324puzzle)
;              '((1 2 3 4)
;                (4 3 2 1)
;                (2 4 1 3)
;                (3 1 4 2)))
;(check-expect (solve-latin no 324puzzle) empty)

;;Question 5
;;Requires 1<=n<=9
(define (get-sub-square n solution)
  (cond [(= n 1) (build-list 3 (lambda (y) (build-list 3 (lambda (x) (loc (list y x) solution)))))]
        [(= n 2) (build-list 3 (lambda (y) (build-list 3 (lambda (x) (loc (list (+ 3 y)
                                                                                x) solution)))))]
        [(= n 3) (build-list 3 (lambda (y) (build-list 3 (lambda (x) (loc (list (+ 6 y)
                                                                                x) solution)))))]
        [(= n 4) (build-list 3 (lambda (y) (build-list 3 (lambda (x) (loc (list y
                                                                               (+ 3 x)) solution)))))]
        [(= n 5) (build-list 3 (lambda (y) (build-list 3 (lambda (x) (loc (list (+ 3 y)
                                                                               (+ 3 x)) solution)))))]
        [(= n 6) (build-list 3 (lambda (y) (build-list 3 (lambda (x) (loc (list (+ 6 y)
                                                                               (+ 3 x)) solution)))))]
        [(= n 7) (build-list 3 (lambda (y) (build-list 3 (lambda (x) (loc (list y
                                                                               (+ 6 x)) solution)))))]
        [(= n 8) (build-list 3 (lambda (y) (build-list 3 (lambda (x) (loc (list (+ 3 y)
                                                                               (+ 6 x)) solution)))))]
        [(= n 9) (build-list 3 (lambda (y) (build-list 3 (lambda (x) (loc (list (+ 6 y)
                                                                             (+ 6 x)) solution)))))]))
(define (sub-square n solution)
  (append (first (get-sub-square n solution)) (second (get-sub-square n solution))
          (third (get-sub-square n solution))))
(define (1? lst)
  (cond [(empty? lst) false]
        [(= 1 (first lst)) true]
        [else (1? (rest lst))]))
(define (2? lst)
  (cond [(empty? lst) false]
        [(= 2 (first lst)) true]
        [else (2? (rest lst))]))
(define (3? lst)
  (cond [(empty? lst) false]
        [(= 3 (first lst)) true]
        [else (3? (rest lst))]))
(define (4? lst)
  (cond [(empty? lst) false]
        [(= 4 (first lst)) true]
        [else (4? (rest lst))]))
(define (5? lst)
  (cond [(empty? lst) false]
        [(= 5 (first lst)) true]
        [else (5? (rest lst))]))
(define (6? lst)
  (cond [(empty? lst) false]
        [(= 6 (first lst)) true]
        [else (6? (rest lst))]))
(define (7? lst)
  (cond [(empty? lst) false]
        [(= 7 (first lst)) true]
        [else (7? (rest lst))]))
(define (8? lst)
  (cond [(empty? lst) false]
        [(= 8 (first lst)) true]
        [else (8? (rest lst))]))
(define (9? lst)
  (cond [(empty? lst) false]
        [(= 9 (first lst)) true]
        [else (9? (rest lst))]))
(define (sudoku? solution)
  (local [(define (l n) (sub-square n solution))
          (define (all-true lst)
            (cond [(empty? lst) true]
                  [else (and (first lst) (all-true (rest lst)))]))]
    (cond [(all-true (build-list 9 (lambda (x)
                                     (and (1? (l (add1 x))) (2? (l (add1 x)))
                                          (3? (l (add1 x))) (4? (l (add1 x)))
                                          (5? (l (add1 x))) (6? (l (add1 x))) (7? (l (add1 x)))
                                          (8? (l (add1 x))) (9? (l (add1 x))))))) true]
          [else false])))
;(check-expect (sudoku? '((1 2 3 4 5 6 7 8 9)
;                         (4 6 5 7 8 9 1 2 3)
;                         (7 8 9 1 2 3 4 5 6)
;                         (4 5 6 7 8 9 1 2 3)
;                         (1 2 3 4 5 6 7 8 9)
;                         (7 8 9 1 2 3 4 5 6)
;                         (7 8 9 1 2 3 6 5 4)
;                         (4 5 6 7 8 9 1 2 3)
;                         (3 2 1 4 5 6 7 8 9))) true)
;(check-expect (sudoku? '((1 2 3 4 5 6 7 8 9)
;                         (4 5 6 7 8 9 1 2 3)
;                         (7 8 9 1 2 3 4 5 6)
;                         (4 5 6 7 8 9 1 2 3)
;                         (1 2 3 4 5 6 7 8 9)
;                         (7 8 9 1 2 3 4 5 6)
;                         (7 8 9 1 2 3 4 5 7)
;                         (4 5 6 7 8 9 1 2 3)
;                         (1 2 3 4 5 6 7 8 9))) false)


(solve-latin sudoku? (strings->puzzle '("48?9?2???"
                                        "?93??5?86"
                                        "6??3???19"
                                        "???531?9?"
                                        "????9????"
                                        "?3?2?6???"
                                        "?7??23??5"
                                        "24?1??36?"
                                        "3??6?4?27")))

