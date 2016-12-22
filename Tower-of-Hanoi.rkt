#lang racket

;; Tower of Hanoi
;; made by Julian

(require 2htdp/universe 2htdp/image rackunit)

;; A Tower-of-Hanoi (TOH) is a (make toh Peglist Number String Number)
(struct toh (pegs selected text moves) #:transparent)
;; - pegs contain the three pegs and the disks on each peg
;; - selected represents the selected peg, if none is selected it is set to -1
;; - text is the current instructional message
;; - moves counts the number of moves a player makes

;; Peglist is a [List-of [List-of Number] [List-of Number] [List-of Number]])
;; Imagelist is a [List-of [List-of Image] [List-of Image] [List-of Image]])

;; A Posn is a (posn Number Number)
(struct posn (x y) #:transparent)
;; x is the x-coordinate
;; y is the y-coordinate

;; Disks for render function
(define disk-imgs (list (rectangle 85 12 "solid" "pink")
                        (rectangle 100 12 "solid" "orange")
                        (rectangle 115 12 "solid" "blue")
                        (rectangle 130 12 "solid" "green")
                        (rectangle 145 12 "solid" "red")
                        (rectangle 160 12 "solid" "gold")))

;; Background for rendering the disks on
(define base (place-image/align
              (rectangle 12 180 "solid" "black")
              90
              180
              "center"
              "bottom"
              (place-image/align
               (rectangle 180 12 "solid" "black")
               90
               180
               "center"
               "bottom"
               (rectangle 180 180 "outline" "white"))))
(define window (foldr (lambda (pos img)
                        (place-image base pos 100 img))
                      (empty-scene 600 250)
                      (list 100 300 500)))

;; Set how many disks you want
(define n 3)
(define gamestate (toh (list (build-list n (lambda (x) (add1 x)))
                             '()
                             '())
                       -1 "Select disk to move" 0))

;; Number -> TOH
;; Starts the game with n amount of disks
(define (play)
  (big-bang gamestate
            [on-mouse mouse-handler]
            [on-key key-handler]
            [stop-when win? win-screen]
            [to-draw render]))

;; TOH -> Image
;; renders the game
(define (render w)
  (define pegs (diskify (toh-pegs w))) ; TODO: Revisit this
  (define p1 (first pegs))
  (define p2 (second pegs))
  (define p3 (third pegs))
  (define (peg-image lop)
    (foldr above empty-image (reverse lop)))
  (define p1-img (peg-image p1))
  (define p2-img (peg-image p2))
  (define p3-img (peg-image p3))
  (place-image (text (number->string (toh-moves w)) 30 "black") 580 220
               (place-image (text (toh-text w) 30 "black") 300 220
                            (place-image/align p1-img
                                               100 178 "center" "bottom"
                                               (place-image/align p2-img
                                                                  300 178 "center" "bottom"
                                                                  (place-image/align p3-img
                                                                                     500 178 "center" "bottom"
                                                                                     window))))))
#;(check-equal? (render (toh '((1 2 3) () ()) -1 "Select disk to move" 0))
                ...)

(struct step (from to))
;; make different big bang for showing how its done

(define (solver n)
  (define (towers-of-hanoi n source dest temp)
    (cond [(= n 1)
           (printf "Move the disk from ~a to ~a~n" source dest)]
          [else
           (towers-of-hanoi (sub1 n) source temp dest)
           (printf "Move the disk from ~a to ~a~n" source dest)
           (towers-of-hanoi (sub1 n) temp dest source)]))
  (towers-of-hanoi n 1 3 2))
              

;; Peglist -> Imagelist
;; makes three lists which contains the images the disks for each peg
(define (diskify pegs)
  (define disk-n (+ (length (first pegs))
                    (length (second pegs))
                    (length (third pegs))))
  (define disk-images (reverse (extract (reverse disk-imgs) disk-n))) ; causes an error if not enough images
  (define (peg-help lop acc)
    (cond [(empty? lop) acc]
          [else (define f-num (first lop))
                (peg-help (rest lop) (cons (list-ref disk-images (sub1 f-num)) acc))]))
  (list (peg-help (first pegs) '())
        (peg-help (second pegs) '())
        (peg-help (third pegs) '())))

;; TOH KeyEvent -> TOH
;; handles key events
(define (key-handler w a-key)
  (cond [(key=? a-key "r") gamestate]
        [else w]))
(check-equal? (key-handler (toh '(() (1 2) (3)) -1 "Select disk to move" 14) "r")
              gamestate)

;; [List-of X] Number -> [List-of X]
;; gets the first n items from lox
(define (extract lox n)
  (cond [(zero? n) '()]
        [else (cons (first lox) (extract (rest lox) (sub1 n)))]))
(check-equal? (extract '(a b c d e f) 4) '(a b c d))

;; TOH -> TOH
;; handles mouse actions
(define (mouse-handler w x y action)
  (define selection (toh-selected w))
  (define pegs (toh-pegs w))
  (define moves (toh-moves w))
  (if (string=? action "button-down")
      (cond [(<= x 200) (if (or (= selection -1) (= selection 1))
                            (toh pegs 1 "Select where you want to place it" moves)
                            (move-disk pegs selection 1 moves))]
            [(<= x 400) (if (or (= selection -1) (= selection 2))
                            (toh pegs 2 "Select where you want to place it" moves)
                            (move-disk pegs selection 2 moves))]
            [else (if (or (= selection -1) (= selection 3))
                      (toh pegs 3 "Select where you want to place it" moves)
                      (move-disk pegs selection 3 moves))])
      w))

;; Peglist Number Number -> TOH
;; moves a disk
(define (move-disk pegs from to moves)
  (define wanted-from (list-ref pegs (- from 1)))
  (define wanted-to (list-ref pegs (- to 1)))
  (define valid? (valid-move? wanted-from wanted-to))
  (list from to)
  (if valid?
      (toh (list (cond [(= from 1) (rest (first pegs))]
                       [(= to 1) (cons (first wanted-from) (first pegs))]
                       [else (first pegs)])
                 (cond [(= from 2) (rest (second pegs))]
                       [(= to 2) (cons (first wanted-from) (second pegs))]
                       [else (second pegs)])
                 (cond [(= from 3) (rest (third pegs))]
                       [(= to 3) (cons (first wanted-from) (third pegs))]
                       [else (third pegs)]))
           -1 "Select disk to move" (add1 moves))
      (toh pegs -1 "Invalid Move" moves)))

;; [List-of Number] [List-of Number] -> Boolean
;; can you move the first item from p1 to p2?
(define (valid-move? p1 p2)
  (cond [(empty? p1) #f]
        [(empty? p2) #t]
        [(< (first p1) (first p2)) #t]
        [else #f]))

;; TOH -> Boolean
;; has the player won?
(define (win? w)
  (define pegs (toh-pegs w))
  (define third-peg (third pegs))
  (define other-peg-count (+ (length (first pegs))
                             (length (second pegs))))
  (= other-peg-count 0))

;; TOH -> Image
;; display the end of game screen
(define (win-screen w)
  (define disk-n (length (third (toh-pegs w))))
  (define moves (toh-moves w))
  (define text-img (above (text "You win!" 50 "red")
                          (if (= (sub1 (expt 2 disk-n)) moves)
                              (text "You managed to complete the puzzle efficiently." 25 "purple")
                              (text "Maybe try to improve a bit?" 25 "purple"))))
  (place-image text-img 300 100 (empty-scene 600 400)))


#| Test suite |#

(check-equal? (mouse-handler (toh '((1 2 3) () ()) -1 "Select disk to move" 0) 100 0 "button-down")
              (toh '((1 2 3) () ()) 1 "Select where you want to place it" 0))
(check-equal? (mouse-handler (toh '((1 2 3) () ()) 1 "Select where you want to place it" 1) 100 0 "button-down")
              (toh '((1 2 3) () ()) 1 "Select where you want to place it" 1))
(check-equal? (mouse-handler (toh '((1 2 3) () ()) 1 "Select where you want to place it" 0) 250 0 "button-down")
              (toh '((2 3) (1) ()) -1 "Select disk to move" 1))

(check-equal? (move-disk '((1 2 3) () ()) 1 2 3)
              (toh '((2 3) (1) ()) -1 "Select disk to move" 4))
(check-equal? (move-disk '((1 2 3) () ()) 2 3 2)
              (toh '((1 2 3) () ()) -1 "Invalid Move" 2))

(check-true (valid-move? '(1 2 3) '()))
(check-false (valid-move? '(2 3) '(1)))
(check-false (valid-move? '() '(1 2 3)))

(check-true (win? (toh '(() () (1 2 3)) -1 "" 7)))
(check-false (win? (toh '(() (1) (2 3)) -1 "" 3)))

(check-equal? (win-screen (toh '(() () (1 2 3 4)) -1 "Select disk to move" 15))
              (place-image (above (text "You win!" 50 "red")
                                  (text "You managed to complete the puzzle efficiently." 25 "purple"))
                           300 100 (empty-scene 600 400)))
(check-equal? (win-screen (toh '(() () (1 2 3 4 5)) -1 "Select disk to move" 34))
              (place-image (above (text "You win!" 50 "red")
                                  (text "Maybe try to improve a bit?" 25 "purple"))
                           300 100 (empty-scene 600 400)))

#|
TODO:
- Implement solving
- integrate step instruction to the move-disk function (and its calls)
- find a way to show it on the fly
|#