;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |cs135search(1)|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;; ***************************************************
;; A05
;; userid: 20990507
;; username: Jerry Zeng
;; ***************************************************

;;2(a)
;;(both DL#X DL#Y)consumes two DLs and produces a doc-list (DL)
;;that occur in both DLs

;;example
(check-expect (both (list "sd.txt") (list "sd.txt" "dfg.txt")) (list "sd.txt"))
;;both: DL DL -> DL
(define (both DL#X DL#Y)
  (cond
    [(or (empty? DL#X) (empty? DL#Y)) empty]
    [(string=? (first DL#X) (first DL#Y))
     (cons (first DL#X) (both (rest DL#X) (rest DL#Y)))]
    [(string<? (first DL#X) (first DL#Y)) (both (rest DL#X) DL#Y)]
    [(string>? (first DL#X) (first DL#Y)) (both  DL#X (rest DL#Y))]))
;;test
(check-expect (both (list "cs135.txt") (list "cs115.txt" "cs135.txt")) (list "cs135.txt"))
(check-expect (both (list "mat135.txt" "mat137.txt") (list "mat137.txt")) (list "mat137.txt"))

;;2(b)
;;(exclude DL#X DL#Y) consumes two DLs and produces a doc-list (DL)
;;that occur in the first DL but not the second one.

;;example
;;(check-expect (exclude (list "sd.txt") (list "sd.txt" "dfg.txt")) (list "dfg.txt")) 
;;exclude: DL DL -> DL
(define (exclude DL#X DL#Y)
  (cond
    [(empty? DL#X) empty]
    [(empty? DL#Y) DL#X]
    [(string>? (first DL#X) (first DL#Y)) (exclude DL#X (rest DL#Y))]
    [(string<? (first DL#X) (first DL#Y))
     (cons (first DL#X) (exclude (rest DL#X) DL#Y))]
     [(string=? (first DL#X) (first DL#Y))
     (exclude (rest DL#X) (rest DL#Y))]))

;;test
;;(check-expect (exclude (list "sd.txt" "xtp.txt") (list "sd.txt" "dfg.txt")) (list "sd.txt" "dfg.txt"))
;;(check-expect (exclude (list "qwe.txt") (list "qwe.txt" )) empty)
;;(check-expect (exclude (list "qwe.txt") empty) empty)

;;2(c)
;;(keys-retrieve doc an-il) onsumes a Str and an IL and producesa(listof Str)
;;with lexicographic ordering. Also, The values inthe produced list are the keys
;;from an-il whose doc-lists contain doc
;;example
(check-expect (key-retrieve "x.txt" (list
                                     (list "I" (list "z.txt"))
                                     (list "love" (list "x.txt" "z.txt"))
                                     (list "study" (list "y.txt"))
                                     (list "ing" (list "x.txt"))
                                     (list "CS" (list "y.txt" "z.txt"))))
              (list "love" "ing"))
;;keys-retrieve: Str IL -> (listof Str)
(define (key-retrieve doc an-il)
  (cond
    [(empty? an-il) empty]
    [(empty? (first an-il)) empty]
    [(empty? (both (second (first an-il)) (list doc))) (key-retrieve doc (rest an-il))]
    [(cons? (both (second (first an-il)) (list doc)))
     (cons (first (first an-il)) (key-retrieve doc (rest an-il)))]))
;;test
(check-expect (key-retrieve "w.txt" (list
                                     (list "he" (list "w.txt"))
                                     (list "pulls" (list "q.txt" "e.txt"))
                                     (list "out" (list "e.txt"))
                                     (list "the" (list "q.txt"))
                                     (list "glock" (list "e.txt" "w.txt"))))
              (list "he" "glock"))

;;2(d)
;;(search sym strX strY an-il) consumes a symbol, a string "x", a string "Y"
;;and a inverted list and produces a doc-list (DL).
;;example
(check-expect (search 'both "orange" "banana" (list
                                              (list "bird" (list "r.txt"))
                                              (list "orange" (list "j.txt" "t.txt"))
                                              (list "apple" (list "r.txt" "s.txt"))
                                              (list "banana" (list "j.txt" "k.txt"))
                                              (list "pineapple" (list "l.txt"))
                                              (list "melon" (list "j.txt" "v.txt"))
                                              (list "juice" (list "r.txt" "t.txt"))))
              (list "j.txt"))
;;search: Sym Str Str IL -> DL
(define (search sym strX strY an-il)
  (cond
    [(symbol=? sym 'exclude) (exclude (source-tracking strX an-il) (source-tracking strY an-il))]
    [(symbol=? sym 'both) (both (source-tracking strX an-il) (source-tracking strX an-il))]))
;;test
(check-expect (search 'exclude "orange" "juice" (list (list "bird" (list "r.txt"))
                                              (list "orange" (list "t.txt" "w.txt"))
                                              (list "apple" (list "f.txt" "s.txt"))
                                              (list "banana" (list "j.txt"))
                                              (list "pineapple" (list "l.txt"))
                                              (list "melon" (list "j.txt" "v.txt"))
                                              (list "juice" (list "q.txt" "t.txt"))))
              (list "w.txt"))


;;source-tracking: str IL -> DL
(define (source-tracking string an-il)
  (cond
    [(empty? an-il) empty]
    [(empty? (first an-il)) empty]
    [(string=? (first (first an-il)) string) (first (rest (first an-il)))]
    [(false? (string=? (first (first an-il)) string))  (source-tracking string (rest an-il))]))
  
    
