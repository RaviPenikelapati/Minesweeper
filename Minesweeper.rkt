;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Minesweeper) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A Content is one of:
;; - Nat
;; - #t
;; and represents either a count of the number of mines surrounding
;; a cell or a mine itself

(define CONTENT-BLANK 0)
(define CONTENT-MINE #t)

;; content-template : Content -> ???
(define (content-template c)
  (cond [(number? c) (... c ...)]
        [(boolean? c) ...]))

(define-struct hidden [con])
(define-struct visible [con])
(define-struct flagged [con])

;; A Cell is one of:
;; - (make-hidden Content)
;; - (make-visible Content)
;; - (make-flagged Content)
;; and represents either a hidden cell, a visible cell, or a flagged cell

(define CELL-H0 (make-hidden 0))
(define CELL-V0 (make-visible 0))
(define CELL-F0 (make-flagged 0))

(define CELL-H1 (make-hidden 1))
(define CELL-V1 (make-visible 1))
(define CELL-F1 (make-flagged 1))

(define CELL-HM (make-hidden #t))
(define CELL-VM (make-visible #t))
(define CELL-FM (make-flagged #t))

;; cell-template : Cell -> ???
(define (cell-template cell)
  (cond [(hidden? cell) (... (hidden-con cell) ...)]
        [(visible? cell) (... (visible-con cell) ...)]
        [(flagged? cell) (... (flagged-con cell) ...)]))

;-----------------------------------------------------------------------------------------------------

;; A Board is a [List-of [List-of Cell]
;; and represents a grid of cells that make up a game board

(define BOARD-EMPTY '())
(define BOARD-SEMI-EMPTY '(() () () ()))
(define BOARD-2X2-BLANK (make-list 2 (make-list 2 CELL-H0)))
(define BOARD-3X3-MID (list (make-list 3 CELL-H1)
                            (list CELL-H1 CELL-HM CELL-H1)
                            (make-list 3 CELL-H1)))
(define BOARD-LOSE (list (list CELL-VM)))

;; board-template : Board -> ???
(define (board-template b)
  (cond [(empty? b) ...]
        [(cons? b) (... (row-template (first b))
                        (board-template (rest b)) ...)]))

;; row-template : [List-of Cell] -> ???
(define (row-template loc)
  (cond [(empty? loc) ...]
        [(cons? loc) (... (cell-template (first loc))
                          (row-template (rest loc)) ...)]))

;-----------------------------------------------------------------------------------------------------

(define-struct game [board rev?])
;; A Game is a (make-game Board Boolean)
;; and represents a game of Minesweeper with a board of cells and a flag that is
;; #t if the mouse is revealing cells and #f if it is flagging them

(define GAME-EMPTY (make-game BOARD-EMPTY #t))
(define GAME-2X2-T (make-game BOARD-2X2-BLANK #t))
(define GAME-2X2-F (make-game BOARD-2X2-BLANK #f))
(define GAME-3X3-T (make-game BOARD-3X3-MID #t))
(define GAME-3X3-F (make-game BOARD-3X3-MID #f))
(define GAME-LOSE (make-game BOARD-LOSE #t))

;; game-template : Game -> ???
(define (game-template g)
  (... (board-template (game-board g))
       (game-rev? g) ...))

;-----------------------------------------------------------------------------------------------------

(require 2htdp/universe)
(require 2htdp/image)

;; mine-sweeper : Nat Nat -> Game
;; Play the minesweeper game with a square board of the given size and the
;; given number of mines
(define (mine-sweeper size mines)
  (mine-sweeper-from (make-game (generate-mine-board size mines) #t)))

;; mine-sweeper-from : Game -> Game
;; Play the minesweeper game with the given initial game state
(define (mine-sweeper-from g)
  (big-bang g
    [to-draw draw-game]
    [on-mouse change-if-click]
    [on-key change-mouse-state]
    [stop-when game-over? draw-game]))


;; add-counts : Board -> Board
;; Add the correct count for each item on the given board
(check-expect (add-counts '()) '())
(check-expect
 (add-counts (list (make-list 3 CELL-H0)
                   (list CELL-H0 CELL-HM CELL-H0)
                   (make-list 3 CELL-H0)))
 BOARD-3X3-MID)
(define (add-counts b)
  (build-list (length b)
              (λ (row) (build-list (length b)
                                   (λ (col) (add-count-to-cell b row col))))))

;; add-count-to-cell : Board Nat Nat -> Cell
;; If the cell at the given location is a mine, leave it alone
;; If it is not a mine, count the mines around it and make that the count for the cell
(check-expect (add-count-to-cell BOARD-3X3-MID 0 0) CELL-H1)
(check-expect
 (add-count-to-cell
  (list (list CELL-HM CELL-HM) (list CELL-HM CELL-H0)) 1 1)
 (make-hidden 3))
(define (add-count-to-cell board row col)
  (local [(define cell (list-ref (list-ref board row) col))
          (define neighbor-posns (get-neighbor-indices row col (length board)))
          (define neighbor-cells
            (map (λ (p) (list-ref (list-ref board (posn-x p)) (posn-y p))) neighbor-posns))]
    (update-cell-count cell (length (filter mine-cell? neighbor-cells)))))

;; get-neighbor-indices : Nat Nat Nat -> [List-of Posn]
;; Get a list of the row/column indices of the neighbors of the cell with the given indices
(check-expect (get-neighbor-indices 0 0 0) '())
(check-expect
 (get-neighbor-indices 0 0 2)
 (list (make-posn 0 1) (make-posn 1 0) (make-posn 1 1)))
(check-expect
 (get-neighbor-indices 2 2 3)
 (list (make-posn 1 1) (make-posn 1 2) (make-posn 2 1)))
(check-expect
 (get-neighbor-indices 1 2 4)
 (list (make-posn 0 1) (make-posn 0 2) (make-posn 0 3)
       (make-posn 1 1) (make-posn 1 3)
       (make-posn 2 1) (make-posn 2 2) (make-posn 2 3)))
(define (get-neighbor-indices row col size)
  (local [(define can-add-left? (> size col 0))
          (define can-add-right? (< col (sub1 size)))
          (define can-add-up? (> size row 0))
          (define can-add-down? (< row (sub1 size)))]
    (append (if (and can-add-left? can-add-up?) (list (make-posn (sub1 row) (sub1 col))) '())
            (if can-add-up? (list (make-posn (sub1 row) col)) '())
            (if (and can-add-right? can-add-up?) (list (make-posn (sub1 row) (add1 col))) '())
            (if can-add-left? (list (make-posn row (sub1 col))) '())
            (if can-add-right? (list (make-posn row (add1 col))) '())
            (if (and can-add-left? can-add-down?) (list (make-posn (add1 row) (sub1 col))) '())
            (if can-add-down? (list (make-posn (add1 row) col)) '())
            (if (and can-add-right? can-add-down?) (list (make-posn (add1 row) (add1 col))) '()))))

;; update-cell-count : Cell Nat -> Cell
;; If this cell is a mine, leave it, otherwise update to the given count
(check-expect (update-cell-count CELL-HM 5) CELL-HM)
(check-expect (update-cell-count (make-visible 7) 2) (make-visible 2))
(check-expect (update-cell-count (make-flagged 0) 3) (make-flagged 3))
(define (update-cell-count cell new-count)
  (local [;; update-contents : Content -> Content
          ;; Update the contents if it is not a mine to be new-count
          (define (update-contents c)
            (if (boolean? c) c new-count))]
    (cond [(hidden? cell)
           (make-hidden (update-contents (hidden-con cell)))]
          [(visible? cell)
           (make-visible (update-contents (visible-con cell)))]
          [(flagged? cell)
           (make-flagged (update-contents (flagged-con cell)))])))

;; mine-cell? : Cell -> Boolean
;; Is this a mine cell?
(check-expect (mine-cell? CELL-HM) #t)
(check-expect (mine-cell? (make-visible 3)) #f)
(check-expect (mine-cell? (make-flagged 0)) #f)
(define (mine-cell? cell)
  (cond [(hidden? cell) (boolean? (hidden-con cell))]
        [(visible? cell) (boolean? (visible-con cell))]
        [(flagged? cell) (boolean? (flagged-con cell))]))

;-----------------------------------------------------------------------------------------------------

;; change-mouse-state : Game KeyEvent -> Game
;; Change the state of the mouse if the user pressed the space bar
(check-expect (change-mouse-state GAME-EMPTY "x") GAME-EMPTY)
(check-expect (change-mouse-state GAME-2X2-F " ") GAME-2X2-T)
(define (change-mouse-state g key)
  (if (key=? key " ") (make-game (game-board g) (not (game-rev? g))) g))

;-----------------------------------------------------------------------------------------------------

;; game-over? : Game -> Boolean
;; Did the user either win or lose?
(check-expect (game-over? GAME-LOSE) #t)
(check-expect (game-over? GAME-EMPTY) #t)
(check-expect (game-over? GAME-3X3-T) #f)
(define (game-over? g)
  (or (board-win? (game-board g))
      (board-lose? (game-board g))))

;; board-win? : Board -> Boolean
;; Did the user reveal all the non-mine squares?
(check-expect (board-win? BOARD-EMPTY) #t)
(check-expect (board-win? BOARD-3X3-MID) #f)
(check-expect (board-win? (make-list 2 (make-list 2 CELL-FM))) #t)
(define (board-win? board)
  (local [;; mine-or-visible? : Cell -> Boolean
          ;; Is the given cell either a mine or visible?
          (define (mine-or-visible? cell)
            (cond [(visible? cell) #t]
                  [(hidden?  cell) (boolean? (hidden-con cell))]
                  [(flagged? cell) (boolean? (flagged-con cell))]))]
    (andmap (λ (row) (andmap mine-or-visible? row)) board)))

;; board-lose? : Board -> Boolean
;; Is there any visible mine square on the board?
(check-expect (board-lose? BOARD-3X3-MID) #f)
(check-expect
 (board-lose?
  (list (list (make-hidden #t) (make-visible 0))
        (list (make-flagged #t) (make-visible #t))))
 #t)
(define (board-lose? board)
  (local [;; visible-mine? : Cell -> Boolean
          ;; Is the given cell a mine that is visible?
          (define (visible-mine? cell)
            (and (visible? cell) (boolean? (visible-con cell))))]
    (ormap (λ (row) (ormap visible-mine? row)) board)))

(define CELL-SIZE 60) ; for pixel->grid conversion

;-----------------------------------------------------------------------------------------------------

;; change-if-click : Game Number Number MouseEvent -> Game
;; Change the board if the user clicked on a space that needs to be revealed/flagged/unflagged
(check-expect (change-if-click GAME-3X3-T 10 5 "drag") GAME-3X3-T)
(check-expect
 (change-if-click GAME-2X2-T 5 5 "button-down")
 (make-game (make-list 2 (make-list 2 CELL-V0)) #t))
(check-expect
 (change-if-click GAME-2X2-F 5 5 "button-down")
 (make-game (list (list CELL-F0 CELL-H0) (make-list 2 CELL-H0)) #f))
(define (change-if-click g x y me)
  (if (mouse=? me "button-down")
      (if (game-rev? g)
          (make-game (reveal-cell (game-board g) (pixel->grid y) (pixel->grid x)) #t)
          (make-game (flag-cell (game-board g) (pixel->grid y) (pixel->grid x)) #f))
      g))

;; reveal-cell : Board Nat Nat -> Board
;; Reveal the cell at the given row and column
(check-expect
 (reveal-cell BOARD-3X3-MID 1 1)
 (list (make-list 3 (make-hidden 1))
       (list (make-hidden 1) (make-visible #t) (make-hidden 1))
       (make-list 3 (make-hidden 1))))
(check-expect
 (reveal-cell BOARD-2X2-BLANK 0 1)
 (make-list 2 (make-list 2 (make-visible 0))))
(define (reveal-cell board row col)
  (local [(define cell (board-ref board row col))
          (define clickable? (hidden? cell))
          ;; is-blank? : Content -> Boolean
          (define (is-blank? c)
            (and (number? c) (zero? c)))]
    (if (and clickable? (is-blank? (hidden-con cell)))
        (reveal-all-cells
         (reveal-single-cell board row col)
         (generate-neighbor-coordinates row col (length board)))
        (reveal-single-cell board row col))))

;; reveal-all-cells : Board [List-of Posn] -> Board
;; Reveal all the cells in the given list (x=row, y=col)
(check-expect (reveal-all-cells BOARD-3X3-MID '()) BOARD-3X3-MID)
(check-expect
 (reveal-all-cells BOARD-2X2-BLANK (list (make-posn 0 0) (make-posn 1 1)))
 (make-list 2 (make-list 2 (make-visible 0))))
(define (reveal-all-cells board all-coordinates)
  (foldr (λ (coord sofar) (reveal-cell sofar (posn-x coord) (posn-y coord))) board all-coordinates))

;; generate-neighbor-coordinates : Nat Nat Nat -> [List-of Posn]
;; Produces a list of coordinates of neighboring positions on a board of the given size
(check-expect
 (generate-neighbor-coordinates 0 0 2)
 (list (make-posn 0 1) (make-posn 1 0) (make-posn 1 1)))
(check-expect
 (generate-neighbor-coordinates 1 1 3)
 (list (make-posn 0 0) (make-posn 0 1) (make-posn 0 2)
       (make-posn 1 0) (make-posn 1 2)
       (make-posn 2 0) (make-posn 2 1) (make-posn 2 2)))
(define (generate-neighbor-coordinates row col boardsize)
  (local [(define can-sub1-row? (> row 0))
          (define can-add1-row? (< row (sub1 boardsize)))
          (define can-sub1-col? (> col 0))
          (define can-add1-col? (< col (sub1 boardsize)))]
    (append (if (and can-sub1-row? can-sub1-col?) (list (make-posn (sub1 row) (sub1 col))) '())
            (if can-sub1-row? (list (make-posn (sub1 row) col)) '())
            (if (and can-sub1-row? can-add1-col?) (list (make-posn (sub1 row) (add1 col))) '())
            (if can-sub1-col? (list (make-posn row (sub1 col))) '())
            (if can-add1-col? (list (make-posn row (add1 col))) '())
            (if (and can-add1-row? can-sub1-col?) (list (make-posn (add1 row) (sub1 col))) '())
            (if can-add1-row? (list (make-posn (add1 row) col)) '())
            (if (and can-add1-row? can-add1-col?) (list (make-posn (add1 row) (add1 col))) '()))))

;; reveal-single-cell : Board Nat Nat -> Board
;; Reveal the cell at the given location (no flooding)
(check-expect
 (reveal-single-cell BOARD-3X3-MID 1 1)
 (list (make-list 3 (make-hidden 1))
       (list (make-hidden 1) (make-visible #t) (make-hidden 1))
       (make-list 3 (make-hidden 1))))
(check-expect
 (reveal-single-cell BOARD-2X2-BLANK 0 1)
 (list (list (make-hidden 0) (make-visible 0))
       (make-list 2 (make-hidden 0))))
(define (reveal-single-cell board row col)
  (local [(define size (length board))

          ;; update-cell : Nat Nat -> Cell
          ;; Get the cell at the given location and make it visible
          ;; if it matches the desired location
          (define (update-cell r c)
            (if (and (= r row) (= c col))
                (make-cell-visible (board-ref board r c))
                (board-ref board r c)))]
    (build-list size (λ (r) (build-list size (λ (c) (update-cell r c)))))))

;; make-cell-visible : Cell -> Cell
;; Make the given cell visible if it isn't flagged
(check-expect (make-cell-visible (make-visible 7)) (make-visible 7))
(check-expect (make-cell-visible (make-hidden #t)) (make-visible #t))
(check-expect (make-cell-visible (make-flagged 0)) (make-flagged 0))
(define (make-cell-visible cell)
  (cond [(visible? cell) cell]
        [(hidden? cell) (make-visible (hidden-con cell))]
        [(flagged? cell) cell]))

;; pixel->grid : Number -> Nat
;; Convert from pixel coordinates to grid coordinates
(check-expect (pixel->grid (add1 CELL-SIZE)) 1)
(check-expect (pixel->grid (sub1 (* 3 CELL-SIZE))) 2)
(define (pixel->grid n)
  (floor (/ n CELL-SIZE)))

;; flag-cell : Board Nat Nat -> Board
;; Flag the cell at the given row and column
(check-expect (flag-cell '() 4 5) '())
(check-expect
 (flag-cell BOARD-2X2-BLANK 1 0)
 (list (make-list 2 (make-hidden 0))
       (list (make-flagged 0) (make-hidden 0))))
(define (flag-cell board row col)
  (local [(define size (length board))

          ;; update-cell : Nat Nat -> Cell
          ;; Get the cell at the given position and flag it if necessary
          (define (update-cell r c)
            (if (and (= r row) (= c col))
                (make-cell-flagged (board-ref board r c))
                (board-ref board r c)))]
    (build-list size (λ (r) (build-list size (λ (c) (update-cell r c)))))))

;; make-cell-flagged : Cell -> Cell
;; Flag the cell if it is hidden, unflag if flagged, otherwise leave alone
(check-expect (make-cell-flagged (make-visible 7)) (make-visible 7))
(check-expect (make-cell-flagged (make-hidden #t)) (make-flagged #t))
(check-expect (make-cell-flagged (make-flagged 3)) (make-hidden 3))
(define (make-cell-flagged cell)
  (cond [(visible? cell) cell]
        [(hidden?  cell) (make-flagged (hidden-con  cell))]
        [(flagged? cell) (make-hidden (flagged-con cell))]))

;-----------------------------------------------------------------------------------------------------

;; generate-mine-board : Nat Nat -> Board
;; Generate a board of the given size with the given number of mines
(check-expect (generate-mine-board 0 0) '())
(check-expect (generate-mine-board 3 0) (make-list 3 (make-list 3 (make-hidden 0))))
(check-expect (board-count (generate-mine-board 4 5) cell-mine?) 5)
(check-error (generate-mine-board 2 7))
(define (generate-mine-board size mines)
  (if (> mines (* size size))
      (error (string-append "Cannot fit " (number->string mines) " mines in a "
                            (number->string size) "x" (number->string size) " board."))
      (add-mines (make-list size (make-list size (make-hidden 0))) mines)))

;; add-mines : Board Nat -> Board
;; Add the given number of mines to the given board
;; ASSUMPTION: There is space for the given number of mines on the given board
(check-expect
 (add-mines (list (list (make-hidden 0))) 1)
 (list (list (make-hidden #t))))
(check-expect (board-count (add-mines BOARD-2X2-BLANK 2) cell-mine?) 2)
(define (add-mines board to-add)
  (if (zero? to-add) board
      (local [(define size (length board))
              (define row (random size))
              (define col (random size))]
        (if (cell-mine? (board-ref board row col))
            (add-mines board to-add)
            (add-mines (add-mine-at board row col) (sub1 to-add))))))

;; board-ref : Board Nat Nat -> Cell
;; Get the cell at the given row and column
;; ASSUMPTION: This is a valid location on the given board
(check-expect (board-ref BOARD-2X2-BLANK (random 2) (random 2)) (make-hidden 0))
(check-expect (board-ref BOARD-3X3-MID 1 1) (make-hidden #t))
(define (board-ref board row col)
  (list-ref (list-ref board row) col))

;; add-mine-at : Board Nat Nat -> Board
;; Add a mine at the given row and column and update the counts around it
;; ASSUMPTION: The cell at this location is not a mine
(check-expect
 (add-mine-at BOARD-2X2-BLANK 0 1)
 (list (list (make-hidden 1) (make-hidden #t))
       (list (make-hidden 1) (make-hidden 1))))
(check-expect (board-count (add-mine-at BOARD-3X3-MID 0 2) cell-mine?) 2)
(define (add-mine-at board row col)
  (local [(define size (length board))

          ;; update-cell : Nat Nat -> Cell
          ;; Get the cell at the given location and update by either adding a mine
          ;; or updating the count if it is neighboring the new mine
          (define (update-cell r c)
            (cond [(and (= row r) (= col c)) (make-hidden #t)]
                  [(and (<= (sub1 row) r (add1 row))
                        (<= (sub1 col) c (add1 col)))
                   (add-to-count (board-ref board r c))]
                  [else (board-ref board r c)]))]
    (build-list size (λ (r) (build-list size (λ (c) (update-cell r c)))))))

;; add-to-count : Cell -> Cell
;; Add 1 to the given cell's count if it is not a mine
(check-expect (add-to-count (make-visible 0)) (make-visible 1))
(check-expect (add-to-count (make-hidden #t)) (make-hidden #t))
(check-expect (add-to-count (make-flagged 7)) (make-flagged 8))
(define (add-to-count cell)
  (local [;; add-to-content : Content -> Content
          ;; Add 1 to the given content if it is not a mine
          (define (add-to-content c)
            (if (number? c) (add1 c) c))]
    (cond [(visible? cell) (make-visible (add-to-content (visible-con cell)))]
          [(hidden?  cell) (make-hidden (add-to-content (hidden-con cell)))]
          [(flagged? cell) (make-flagged (add-to-content (flagged-con cell)))])))

;; board-count : Board [Cell -> Boolean] -> Nat
;; Count the number of cells on the given board that pass the given predicate
(check-expect (board-count BOARD-2X2-BLANK hidden?) 4)
(check-expect (board-count BOARD-3X3-MID cell-mine?) 1)
(define (board-count board check?)
  (foldr (λ (row sofar) (foldr (λ (cell sofar) (if (check? cell) (add1 sofar) sofar)) sofar row))
         0
         board))

;; cell-mine? : Cell -> Boolean
;; Is this cell a mine?
(check-expect (cell-mine? (make-visible 0)) #f)
(check-expect (cell-mine? (make-hidden #t)) #t)
(check-expect (cell-mine? (make-flagged 5)) #f)
(define (cell-mine? cell)
  (boolean?
   (cond [(visible? cell) (visible-con cell)]
         [(hidden?  cell) (hidden-con cell)]
         [(flagged? cell) (flagged-con cell)])))

                                         
;; draw-game : Game -> Image
;; Draw the current state of the game (WARNING: THIS IS CURRENTLY A STUB)
(check-expect (draw-game GAME-3X3-T)
                (above
                 (beside (cell->image (make-hidden 1))
                         (cell->image (make-hidden 1))
                         (cell->image (make-hidden 1)))

                 (beside (cell->image (make-hidden 1))
                         (cell->image (make-hidden #true))
                         (cell->image (make-hidden 1)))

                 (beside (cell->image (make-hidden 1))
                         (cell->image (make-hidden 1))
                         (cell->image (make-hidden 1)))
                 (text "mines:1" 20 "black")
                 (text "flagged:0" 20 "black")
                 (text "reveal" 20 "black")))
               
             
(define (draw-game g)
  (local [(define board (game-board g))
          ;; draw-board : Board -> Image
          ;; draws the whole board
          (define (draw-board b)
            (cond
              [(empty? b) empty-image]
              [(cons? b) (above (draw-row (first b)) (draw-board (rest b)))]))
          ;; draw-row : [List-of Cell] -> Image
          ;; draws a row of cells
          (define (draw-row loc) 
            (cond
              [(empty? loc) empty-image]
              [(cons? loc) (beside (cell->image (first loc)) (draw-row (rest loc)))]))
          ;; game-visualize : Board String -> Image
          ;; returns a game with a given string color
          (define (game-visualize b color)
            (above
             (draw-board board)
             (text (string-append "mines:" (number->string (board-count board cell-mine?)))
                   20 color)
             (text (string-append "flagged:" (number->string (board-count board flagged?)))
                   20 color)
             (if (game-rev? g) (text "reveal" 20 color) (text "flag" 20 color))))]
            
    (cond
      [(board-win? board) (game-visualize board "green")]
      [(board-lose? board) (game-visualize board "red")]
      [else (game-visualize board "black")])))

;; cell->image : Cell -> Image
;; visualizes a cell
(check-expect (cell->image CELL-H0) (square CELL-SIZE "outline" "black"))
(check-expect (cell->image CELL-V0) (overlay
                                     (text "0" (/ CELL-SIZE 2) "black")
                                     (square CELL-SIZE "solid" "grey")))
(check-expect (cell->image CELL-VM) (overlay
                                     (circle (/ CELL-SIZE 3) "solid" "red")
                                     (square CELL-SIZE "solid" "grey")))
(check-expect (cell->image CELL-F0) (square CELL-SIZE "solid" "red"))
                                 

(define (cell->image c)
  (cond [(visible? c) (overlay
                       (if (boolean? (visible-con c))
                           (circle (/ CELL-SIZE 3) "solid" "red")
                           (text (number->string (visible-con c)) (/ CELL-SIZE 2) "black"))
                       (square CELL-SIZE "solid" "grey"))]
        [(hidden?  c) (square CELL-SIZE "outline" "black")]
        [(flagged? c) (square CELL-SIZE "solid" "red")]))

(mine-sweeper 10 8)




           

  

 
 

    




