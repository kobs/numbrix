#| 

 _________ 
< NUMBRIX >
 --------- 
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||

  by David Ethier and Kenneth Sanders

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *filename* nil)
(defvar *board* nil
  "Multidimensional array representing the board state.")
(defvar *original-board* nil
  "Original state")
(defvar *locked* nil
  "Initial board configuration -- the locked cells")
(defvar *board-size* nil)
(defvar *max-digits* nil
  "The number of digits in the largest number for this board size
  i.e.: If the board size is 12, the largest number is 144, with 3 digits.
  Used for output formatting.")
(defvar *val-open* 
  "The number of open cells")
(defparameter *moves* nil
  "The players list of moves")
(defvar *lowest* nil
  "The initial lowest value on the board without a succesor")
(defvar *time-start* nil)
(defvar *time-end* nil)
(defvar *time-total* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miscellaneous functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun num-digits (n)
  (floor (1+ (log n 10))))

(defun split-string-by-space (string)
  (loop for i = 0 then (1+ j) as j = (position #\Space string :start i)
	collect (subseq string i j)
	while j))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I/O
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prompt-read (prompt)
  "Standard input prompt"
  (format *query-io* "~&~a" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-filename ()
  (format t "~&")
  (let ((input (prompt-read "Board file (default: 1.board): ")))
    (cond
      ((or (equal "" input) (null input)) (setf *filename* "1.board"))
      (t (progn (setf *filename* input) input)))))



(defun format-string (digits char)
  (format nil " ~~~ad ~a" digits char))

(defun row-string-format (digits)
  (format nil "~~~ad" digits))

(defun print-dashes ()
  (loop repeat (+ 2 *max-digits*) do
       (format t "-")))

(defun print-border ()
  "Prints border in the format +---+---+"
  (format t "~&")
  (dotimes (idx (1+ (* 2 *board-size*)))
    (if (evenp idx)
        (format t "+")
        (print-dashes))))

(defun print-row (row)
  "Prints a row in the format of '| 42 | 16 | ..'"
  (print-border)
  (format t "~&|")
  (dotimes (idx *board-size*)
    (let ((entry (aref *board* row idx)))
          (if (zerop entry)
              (progn (loop repeat (+ 2 *max-digits*) do (format t " ")) (format t "|"))
              (format t (format-string *max-digits* "|") entry))))
  (when (equal row (- *board-size* 1)) (print-border)))

(defun print-board ()
  (format t "~%Numbrix-~a~%~%" *board-size*)
  (dotimes (row *board-size*)
    (print-row row)))


(defun prompt-move ()
  (format t "~&")
  (let ((complete nil))
  (cond 
     ((equal *val-open* 0) 
      (cond
       ((check-for-solution) (progn (format t "Congratulations! You solved the puzzle") (setf complete t)))
       (t (progn (format t "You have filled the board, but failed to find a solution, BOARD RESET.") (print-moves) (clear-board) (print-board))))))
    (cond
     ((equal complete nil)
       (let ((input (prompt-read "move: ")))
       (cond
        ((equal "h" input) (print-instructions))
        ((equal "p" input) (print-board))
        ((equal "q" input) (progn (format t "Goodbye!") 'q))
        ((equal "a" input) (progn 
                             (setf *time-start* (get-internal-run-time))

                             ;; solver
                             (find-givens) (auto-solve) 

                             (setf *time-end* (get-internal-run-time)) 
                             (setf *time-total* (/ (- *time-end* *time-start*) internal-time-units-per-second)) 
                             (calc-moves-list)
                             (print-board)
                             (print-moves)
                             (format t "~%total time: ~d seconds~%" (float *time-total*))
                             (numbrix)))
        (t (progn (make-move (split-string-by-space input)) (print-board))))))
     (t 'c))))

(defun prompt-repeat ()
  (format t "~&")
  (let ((input (prompt-read "Would you like to play again? (y/n): ")))
    (cond
     ((equal "y" input) t)
     ((equal "Y" input) t)
     ((equal "n" input) nil)
     ((equal "N" input) nil)
     (t (progn (format t "Unreadable input, please try again") (prompt-repeat))))))


(defun prompt-overwrite ()
  (format t "~&")
  (let ((input (prompt-read "Enter y/n to overwrite: ")))
    (cond
     ((equal "y" input) t)
     ((equal "Y" input) t)
     ((equal "n" input) nil)
     ((equal "N" input) nil)
     (t (progn (format t "Unreadable input, please try again") (prompt-overwrite))))))

(defun print-instructions ()
  (format t "~%INSTRUCTIONS:~%")
  (format t "  Starting anywhere, fill in the blank squares with the missing numbers so they make a path in numerical order, 1 through ~d. You can work horizontally or vertically in any direction. Diagonal paths are not allowed."
          (expt *board-size* 2))
  (format t "~%~%CONTROLS:~%")
  (format t "  Moves are entered in the format: ROW COLUMN X , where X is the number you'd like to place in the cell located in row ROW and column COLUMN.~%")
  (format t "~%  Example: \"4 5 28\"~%")
  (format t "   ...in which 28 would be placed in the cell located at row 4, column 5~%")
  (format t "~%~%NOTE: To clear an entry, enter ROW COLUMN 0")
  (format t "~%~%  'h': help")
  (format t "~%  'a': auto-solve the rest of the puzzle")
  (format t "~%  'p': print board")
  (format t "~%  'q': quit")
  (format t "~%"))


(defun print-moves ()
  (format t "~%Moves:~%")
  (dolist (move *moves*)
    (format t "(~d, ~d): ~d~%" (elt move 0) (elt move 1) (elt move 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Board creation and manipulation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-move (row col value)
  "add move to global moves list"
  (setf *moves* (append *moves* (list (list (+ 1 row) (+ 1 col) value)))))

(defun clear-moves ()
  (setf *moves* '()))

(defun compute-open-cells ()
  "Finds the number of unsolved cells in the initial state"
  (let ((count 0))
    (dotimes (row *board-size*)
      (dotimes (col *board-size*)
        (unless (zerop (aref *board* row col)) 
          (progn (setf count (+ 1 count))
                 (setf (aref *locked* row col) 1)))))
    (setf *val-open* (- (expt *board-size* 2) count))))

(defun load-board (file)
  "Returns a 2-D array with 'files' as the contents"
  (let ((cells '()))
    (with-open-file (stream file)
      (setf *board-size* (parse-integer (read-line stream)))
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (setf cells (append cells (list (mapcar 'parse-integer (split-string-by-space line)))))))
    (make-array (list *board-size* *board-size*) :initial-contents cells)))
      
(defun create-board (file)
  "Setup/preparation for board creation"
  (setf *board* (load-board file))
  (setf *original-board* (load-board file))
  (setf *max-digits* (num-digits (expt *board-size* 2)))
  (setf *board-size* (car (array-dimensions *board*)))
  (setf *locked* (make-array (list *board-size* *board-size*) :initial-element 0))
  (compute-open-cells)
  (clear-moves))

(defun copy-board (board)
  (let ((new-board (make-array (list *board-size* *board-size*))))
    (dotimes (row *board-size*)
      (dotimes (col *board-size*)
        (setf (aref new-board row col) (aref board row col))))
    new-board))

(defun clear-board ()
  (create-board *filename*)
  (compute-open-cells)
  (clear-moves))

(defun update-cell (row col value)
  "Places the value on the global *board*"
  (cond
   ((equal (aref *locked* row col) 1) (format t "Not a legal move, square is part of given start data"))
   ((zerop value) (progn (setf (aref *board* row col) 0) (setf *val-open* (+ *val-open* 1))))
   ((value-in-array value 0 0) (format t "Not a legal move, value already found in array"))
   ((not (equal (aref *board* row col) 0)) 
         (progn (format t "Square already has a value, would you like to overwrite?") 
           (cond 
            ((prompt-overwrite) (progn (setf (aref *board* row col) value) (add-move row col value)))
            (t (format t "Value not overwritten, no changes made")))))                                        
   (T (progn (setf *val-open* (- *val-open* 1)) (setf (aref *board* row col) value) (add-move row col value)))))

(defun update-cell-auto (row col value)
  "Used by the find-givens function to modify the global *board*"
  (cond
   ((equal (aref *locked* row col) 1) (format t "Not a legal move, square is part of given start data"))
   ((zerop value) (progn (setf (aref *board* row col) 0) (setf *val-open* (+ *val-open* 1))))
   ((value-in-array value 0 0) (format t "Not a legal move, value already found in array"))
   ((not (equal (aref *board* row col) 0)) 
         (progn (format t "Square already has a value, would you like to overwrite?") 
           (cond 
            ((prompt-overwrite) (progn (setf (aref *board* row col) value)))
            (t (format t "Value not overwritten, no changes made")))))                                        
   (T (progn (setf *val-open* (- *val-open* 1)) (setf (aref *board* row col) value)))))

(defun check-for-solution ()
  (cond 
   ((equal (aref *board* 0 0) 1) (solution-ascending 0 0))
   ((equal (aref *board* 0 0) (* *board-size* *board-size*)) (solution-descending 0 0))
   (t (and (solution-ascending 0 0) (cond ((equal (- (aref *board* 0 0) 1) (aref *board* 0 1)) (solution-descending 0 1))
                                          ((equal (- (aref *board* 0 0) 1) (aref *board* 1 0)) (solution-descending 1 0))
                                          (t nil))))))

(defun solution-ascending (row col)
  (let ((cur-val (aref *board* row col)))
   (cond
   ((equal cur-val (* *board-size* *board-size*)) t)
   ((and (< (+ 1 row) *board-size*) (equal (+ cur-val 1) (aref *board* (+ 1 row) col))) (solution-ascending (+ 1 row) col))
   ((and (>= (- row 1) 0) (equal (+ cur-val 1) (aref *board* (- row 1) col))) (solution-ascending (- row 1) col))
   ((and (< (+ 1 col) *board-size*) (equal (+ cur-val 1) (aref *board* row (+ 1 col)))) (solution-ascending row (+ col 1)))
   ((and (>= (- col 1) 0) (equal (+ cur-val 1) (aref *board* row (- col 1)))) (solution-ascending row (- col 1)))
   (t nil))))

(defun solution-descending (row col)
  (let ((cur-val (aref *board* row col)))
    (cond
     ((equal cur-val 1) t)
     ((and (< (+ 1 row) *board-size*) (equal (- cur-val 1) (aref *board* (+ 1 row) col))) (solution-descending (+ 1 row) col))
     ((and (>= (- row 1) 0) (equal (- cur-val 1) (aref *board* (- row 1) col))) (solution-descending (- row 1) col))
     ((and (< (+ 1 col) *board-size*) (equal (- cur-val 1) (aref *board* row (+ 1 col)))) (solution-descending row (+ col 1)))
     ((and (>= (- col 1) 0) (equal (- cur-val 1) (aref *board* row (- col 1)))) (solution-descending row (- col 1)))
     (t nil))))

(defun value-in-array (value row col)
  (cond ((equal row *board-size*) nil)
        ((equal col *board-size*) (value-in-array value (+ 1 row) 0))
        ((equal value (aref *board* row col)) T)
        (T (value-in-array value row (+ col 1)))))

(defun make-move (input)
  "Parse input and make the specified move"
  (let ((row (- (parse-integer (pop input) :radix 10) 1)) 
        (col (- (parse-integer (pop input) :radix 10) 1)) 
        (value (parse-integer (pop input))))
    (cond ((or (>= row *board-size*) (< row 0)) (progn (format t "Error: row must be between 1 and ~d (inclusive)" *board-size*) (prompt-move)))
	  ((or (>= col *board-size*) (< col 0)) (progn (format t "Error: column must be between 1 and ~d (inclusive)" *board-size*) (prompt-move)))
	  ((or (< value 0) (> value (expt *board-size* 2))) (progn (format t "Error: value must be between 1 and ~d (inclusive)" (expt *board-size* 2)) (prompt-move)))
          (t (update-cell row col value)))))

(defun make-move-auto (row col value)
  "take input from auto-solving functions to make move"
      (cond ((or (>= row *board-size*) (< row 0)) (progn (format t "Error: row must be between 1 and ~d (inclusive)" *board-size*) (prompt-move)))
	  ((or (>= col *board-size*) (< col 0)) (progn (format t "Error: column must be between 1 and ~d (inclusive)" *board-size*) (prompt-move)))
	  ((or (< value 0) (> value (expt *board-size* 2))) (progn (format t "Error: value must be between 1 and ~d (inclusive)" (expt *board-size* 2)) (prompt-move)))
	  (t (update-cell-auto row col value))))

(defun calc-moves-list ()
  (let ((value-list-orig nil) (value-list-finished nil))
    (loop for value from 1 to (* *board-size* *board-size*) do
          (setf value-list-orig (find-value-orig value))
          (if (equal value-list-orig (list -1 -1 -1))
              (progn
                (setf value-list-finished (find-value-finished value))
                (add-move (cadr value-list-finished) (caddr value-list-finished) (car value-list-finished)))))))

(defun find-value-orig (value)
  (dotimes (row *board-size*)
    (dotimes (col *board-size*)
      (if (equal (aref *original-board* row col) value)
          (return-from find-value-orig (list value row col)))))
  (return-from find-value-orig (list -1 -1 -1)))

(defun find-value-finished (value)
  (dotimes (row *board-size*)
    (dotimes (col *board-size*)
      (if (equal (aref *board* row col) value)
          (return-from find-value-finished (list value row col))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Artificial Intelligence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;
;Definition of given
;1) checking square WITH value: if not doubly connected and only ONE open adjacent then must be
;the number 1 (no connections), or singly connected and the open will be next number(+ or - depending on connection)
;
;2)checking square WITHOUT value: if 2 neighbors are 2 values apart (get top value, check if left, right, bottom are 2 away, then get left, check only right bottom ect) not neccesarily
;
; AND if 
;3)Row/colum manhattan distance givens(if a number is X distance away from another in its same row/colum where X is the manhanttan distance then path is given) 
;
;4)Only path manhattan distance givens (if a number is X distance away from another anywhere on the board, and there is only one path of empty squares between them path is given)
;
;So, find-givens will go thru every square, first check for in bounds , then check for type of square (blank or number) then check for given
;if change is made, call find-isolated-given on 4 surrounding squares as these might have new givens found, continue this recursively
;;;;;
(defun find-givens ()
  (let ((givens-found T))
  (loop while givens-found do
        
    (setf givens-found nil)
    (dotimes (row *board-size*)
    (dotimes (col *board-size*)
      (let ((top T) (left T) (bot T) (right T) (top-zero T) (left-zero T) (bot-zero T) (right-zero T) (count 0) (connection-count 0) (connection-value 0) (not-changed t) (i 1) (j 1) (num-found nil))
        (progn
          (if (< (- row 1) 0) (setf top nil))
          (if (>= (+ row 1) *board-size*) (setf bot nil))
          (if (< (- col 1) 0) (setf left nil))
          (if (>= (+ col 1) *board-size*) (setf right nil)))
          (cond 
           ((zerop (aref *board* row col))
            (progn
              (if top
                  (if (not (zerop (aref *board* (- row 1) col)))
                      (progn
                        (if left
                            (if (not (zerop (aref *board* row (- col 1))))
                                (if (= 2 (abs (- (aref *board* (- row 1) col) (aref *board* row (- col 1)))))
                                    (if (and (not (equal (aref *board* (- row 1) (- col 1)) (- (aref *board* (- row 1) col) 1)))
                                             (not (equal (aref *board* (- row 1) (- col 1)) (+ (aref *board* (- row 1) col) 1)))
                                             (not (equal (aref *board* (- row 1) (- col 1)) 0)))
                                        (progn
                                          (make-move-auto row col (/ (+ (aref *board* (- row 1) col) (aref *board* row (- col 1))) 2))
                                          (setf givens-found T)
                                          (setf not-changed nil))))))
                        
                        (if (and right not-changed)
                            (if (not (zerop (aref *board* row (+ col 1))))
                                (if (= 2 (abs (- (aref *board* (- row 1) col) (aref *board* row (+ col 1)))))
                                    (if (and (not (equal (aref *board* (- row 1) (+ col 1)) (- (aref *board* (- row 1) col) 1)))
                                             (not (equal (aref *board* (- row 1) (+ col 1)) (+ (aref *board* (- row 1) col) 1)))
                                             (not (equal (aref *board* (- row 1) (+ col 1)) 0)))
                                        (progn
                                          (make-move-auto row col (/ (+ (aref *board* (- row 1) col) (aref *board* row (+ col 1))) 2))
                                          (setf givens-found T)
                                          (setf not-changed nil)))))))))
              
              (if (and left not-changed)
                  (if (not (zerop (aref *board* row (- col 1))))
                      (if bot
                          (if (not (zerop (aref *board* (+ row 1) col)))
                              (if (= 2 (abs (- (aref *board* (+ row 1) col) (aref *board* row (- col 1)))))
                                    (if (and (not (equal (aref *board* (+ row 1) (- col 1)) (- (aref *board* (+ row 1) col) 1)))
                                             (not (equal (aref *board* (+ row 1) (- col 1)) (+ (aref *board* (+ row 1) col) 1)))
                                             (not (equal (aref *board* (+ row 1) (- col 1)) 0)))
                                        (progn
                                          (make-move-auto row col (/ (+ (aref *board* (+ row 1) col) (aref *board* row (- col 1))) 2))
                                          (setf givens-found T)
                                          (setf not-changed nil))))))))
              
              (if (and right not-changed)
                  (if (not (zerop (aref *board* row (+ col 1))))
                      (if bot
                          (if (not (zerop (aref *board* (+ row 1) col)))
                              (if (= 2 (abs (- (aref *board* (+ row 1) col) (aref *board* row (+ col 1)))))
                                    (if (and (not (equal (aref *board* (+ row 1) (+ col 1)) (- (aref *board* (+ row 1) col) 1)))
                                             (not (equal (aref *board* (+ row 1) (+ col 1)) (+ (aref *board* (+ row 1) col) 1)))
                                             (not (equal (aref *board* (+ row 1) (+ col 1)) 0)))
                                        (progn
                                          (make-move-auto row col (/ (+ (aref *board* (+ row 1) col) (aref *board* row (+ col 1))) 2))
                                          (setf givens-found T)
                                          (setf not-changed nil))))))))))
           
           (t
            (progn
              
              (if top
                  (loop while (and (not num-found) (> (- row i) -1)) do
                        (if (not (zerop (aref *board* (- row i) col)))
                            (progn
                              (setf num-found t)
                              (if (and (not (equal i 1)) (equal (abs (- (aref *board* row col) (aref *board* (- row i) col))) i))
                                  (if (< (aref *board* row col) (aref *board* (- row i) col))
                                      (loop while (< j i) do
                                            (progn
                                              (make-move-auto (- row j) col (+ (aref *board* row col) j))
                                              (setf givens-found T)
                                              (setf j (+ j 1))))
                                    (loop while (< j i) do
                                          (progn
                                            (make-move-auto (- row j) col (- (aref *board* row col) j))
                                            (setf givens-found T)
                                            (setf j (+ j 1))))))
                              ))
                        (setf i (+ i 1))
                        ))
              (setf num-found nil)
              (setf i 1)
              (setf j 1)
              
              (if left
                  (loop while (and (not num-found) (> (- col i) -1)) do
                        (if (not (zerop (aref *board* row (- col i))))
                            (progn
                              (setf num-found t)
                              (if (and (not (equal i 1)) (equal (abs (- (aref *board* row col) (aref *board* row (- col i)))) i))
                                  (if (< (aref *board* row col) (aref *board* row (- col i)))
                                      (loop while (< j i) do
                                            (progn
                                              (make-move-auto row (- col j) (+ (aref *board* row col) j))
                                              (setf givens-found T)
                                              (setf j (+ j 1))))
                                    (loop while (< j i) do
                                          (progn
                                            (make-move-auto row (- col j) (- (aref *board* row col) j))
                                            (setf givens-found T)
                                            (setf j (+ j 1))))))
                              ))
                        (setf i (+ i 1))
                        ))
              (setf num-found nil)
              (setf i 1)
              (setf j 1)

              
              (if bot
                  (loop while (and (not num-found) (< (+ row i) *board-size*)) do
                        (if (not (zerop (aref *board* (+ row i) col)))
                            (progn
                              (setf num-found t)
                              (if (and (not (equal i 1)) (equal (abs (- (aref *board* row col) (aref *board* (+ row i) col))) i))
                                  (if (< (aref *board* row col) (aref *board* (+ row i) col))
                                      (loop while (< j i) do
                                            (progn
                                              (make-move-auto (+ row j) col (+ (aref *board* row col) j))
                                              (setf givens-found T)
                                              (setf j (+ j 1))))
                                    (loop while (< j i) do
                                          (progn
                                            (make-move-auto (+ row j) col (- (aref *board* row col) j))
                                            (setf givens-found T)
                                            (setf j (+ j 1))))))
                              ))
                        (setf i (+ i 1))
                        ))
              
              (setf num-found nil)
              (setf i 1)
              (setf j 1)

              
              (if right
                  (loop while (and (not num-found) (< (+ col i) *board-size*)) do
                        (if (not (zerop (aref *board* row (+ col i))))
                            (progn
                              (setf num-found t)
                              (if (and (not (equal i 1)) (equal (abs (- (aref *board* row col) (aref *board* row (+ col i)))) i))
                                  (if (< (aref *board* row col) (aref *board* row (+ col i)))
                                      (loop while (< j i) do
                                            (progn
                                              (make-move-auto row (+ col j) (+ (aref *board* row col) j))
                                              (setf givens-found T)
                                              (setf j (+ j 1))))
                                    (loop while (< j i) do
                                          (progn
                                            (make-move-auto row (+ col j) (- (aref *board* row col) j))
                                            (setf givens-found T)
                                            (setf j (+ j 1))))))
                              ))
                        (setf i (+ i 1))
                        ))
              
              (setf num-found nil)

          
              
              ;beggining of "dead-end" testing
              (if top
                  (if (not (zerop (aref *board* (- row 1) col))) (progn (setf count (+ count 1)) (setf top-zero nil)))
                (setf count (+ count 1)))
              
              (if left
                  (if (not (zerop (aref *board* row (- col 1)))) (progn (setf count (+ count 1)) (setf left-zero nil)))
                (setf count (+ count 1)))
              
              (if bot
                  (if (not (zerop (aref *board* (+ row 1) col))) (progn (setf count (+ count 1)) (setf bot-zero nil)))
                (setf count (+ count 1)))
              
              (if right
                  (if (not (zerop (aref *board* row (+ col 1)))) (progn (setf count (+ count 1))) (setf right-zero nil))
                (setf count (+ count 1)))
              
              (if (equal count 3)
                  (progn
                    (if top
                        (if (equal (abs (- (aref *board* row col) (aref *board* (- row 1) col))) 1)
                            (progn
                              (setf connection-count (+ connection-count 1))
                              (setf connection-value (aref *board* (- row 1) col)))))
                    
                    (if left
                        (if (equal (abs (- (aref *board* row col) (aref *board* row (- col 1)))) 1)
                            (progn
                              (setf connection-count (+ connection-count 1))
                              (setf connection-value (aref *board* row (- col 1))))))
                    
                    (if bot
                        (if (equal (abs (- (aref *board* row col) (aref *board* (+ row 1) col))) 1)
                            (progn
                              (setf connection-count (+ connection-count 1))
                              (setf connection-value (aref *board* (+ row 1) col)))))
                    
                    (if right
                        (if (equal (abs (- (aref *board* row col) (aref *board* row (+ col 1)))) 1)
                            (progn
                              (setf connection-count (+ connection-count 1))
                              (setf connection-value (aref *board* row (+ col 1))))))
                      
                      (if (equal connection-count 1)
                          (if (and top top-zero)
                              (if (< connection-value (aref *board* row col))
                                  (if (not (equal (aref *board* row col) (* *board-size* *board-size*))) (progn (setf givens-found T) (make-move-auto (- row 1) col (+ connection-value 2))))
                                (if (not (equal (aref *board* row col) 1)) (progn (setf givens-found T) (make-move-auto (- row 1) col (- connection-value 2)))))
                            
                            (if (and left left-zero)
                                (if (< connection-value (aref *board* row col))
                                    (if (not (equal (aref *board* row col) (* *board-size* *board-size*))) (progn (setf givens-found T) (make-move-auto row (- col 1) (+ connection-value 2))))
                                  (if (not (equal (aref *board* row col) 1)) (progn (setf givens-found T) (make-move-auto row (- col 1) (- connection-value 2)))))
                              
                              (if (and bot bot-zero)
                                  (if (< connection-value (aref *board* row col))
                                    (if (not (equal (aref *board* row col) (* *board-size* *board-size*))) (progn (setf givens-found T) (make-move-auto (+ row 1) col (+ connection-value 2))))
                                    (if (not (equal (aref *board* row col) 1)) (progn (setf givens-found T) (make-move-auto (+ row 1) col (- connection-value 2)))))
                                
                                (if (< connection-value (aref *board* row col))
                                    (if (not (equal (aref *board* row col) (* *board-size* *board-size*))) (progn (setf givens-found T) (make-move-auto row (+ col 1) (+ connection-value 2))))
                                  (if (not (equal (aref *board* row col) 1)) (progn (setf givens-found T) (make-move-auto row (+ col 1) (- connection-value 2))))))))))))))))))))


(defun test-for-path (lowest next)
  "Tests whether the numbers located in lowest and next are adjacent"
  (or (and (equal (cadr lowest) (cadr next)) (equal (abs (- (caddr lowest) (caddr next))) 1)) 
      (and (equal (caddr lowest) (caddr next)) (equal (abs (- (cadr lowest) (cadr next))) 1))))


(defun check-for-end-case (board lowest)
  (finish-end board lowest))

(defun finish-end (board lowest)
  "Fills in the rest of the board"
  (if (>= (car lowest) (* *board-size* *board-size*))
      (if (finish-start board *lowest*)
          (return-from finish-end T)
        (return-from finish-end nil)))
  (let* ((lowest-val (car lowest)) (row (cadr lowest)) (col (caddr lowest)) (new-board nil))
    
    ;left
    (if (and (> (- col 1) -1) (zerop (aref board row (- col 1))))
        (progn
          (setf new-board (copy-board board))
          (setf (aref new-board row (- col 1)) (+ lowest-val 1))
          (if (finish-end new-board (list (+ lowest-val 1) row (- col 1)))
              (return-from finish-end T))))
    ; bottom 
    (if (and (< (+ row 1) *board-size*) (zerop (aref board (+ row 1) col)))
        (progn
          (setf new-board (copy-board board))
          (setf (aref new-board (+ row 1) col) (+ lowest-val 1))
          (if (finish-end new-board (list (+ lowest-val 1) (+ row 1) col))
              (return-from finish-end T))))
    
    ; right
    (if (and (< (+ col 1) *board-size*) (zerop (aref board row (+ col 1))))
        (progn
          (setf new-board (copy-board board))
          (setf (aref new-board row (+ col 1)) (+ lowest-val 1))
          (if (finish-end new-board (list (+ lowest-val 1) row (+ col 1)))
              (return-from finish-end T))))
    ; top
    (if (and (> (- row 1) -1) (zerop (aref board (- row 1) col)))
        (progn
          (setf new-board (copy-board board))
          (setf (aref new-board (- row 1) col) (+ lowest-val 1))
          (if (finish-end new-board (list (+ lowest-val 1) (- row 1) col))
              (return-from finish-end T))))

    (return-from finish-end nil)))


(defun finish-start (board lowest)
  (if (equal (car lowest) 1)
      (progn
        (setf *board* board)
        (return-from finish-start T)))
  
  (let* ((lowest-val (car lowest)) (row (cadr lowest)) (col (caddr lowest)) (new-board nil))
    
    (if (and (> (- col 1) -1) (zerop (aref board row (- col 1))))
        (progn
          (setf new-board (copy-board board))
          (setf (aref new-board row (- col 1)) (- lowest-val 1))
          (if (finish-start new-board (list (- lowest-val 1) row (- col 1)))
              (return-from finish-start T))))
    
    (if (and (< (+ row 1) *board-size*) (zerop (aref board (+ row 1) col)))
        (progn
          (setf new-board (copy-board board))
          (setf (aref new-board (+ row 1) col) (- lowest-val 1))
          (if (finish-start new-board (list (- lowest-val 1) (+ row 1) col))
              (return-from finish-start T))))
    
    (if (and (< (+ col 1) *board-size*) (zerop (aref board row (+ col 1))))
        (progn
          (setf new-board (copy-board board))
          (setf (aref new-board row (+ col 1)) (- lowest-val 1))
          (if (finish-start new-board (list (- lowest-val 1) row (+ col 1)))
              (return-from finish-start T))))
    
    (if (and (> (- row 1) -1) (zerop (aref board (- row 1) col)))
        (progn
          (setf new-board (copy-board board))
          (setf (aref new-board (- row 1) col) (- lowest-val 1))
          (if (finish-start new-board (list (- lowest-val 1) (- row 1) col))
              (return-from finish-start T))))
    
    (return-from finish-start nil)))

(defun find-ultimate-low (board)
  (let ((min (list (aref board 0 0) 0 0)))
    (dotimes (row *board-size*)
      (dotimes (col *board-size*)
        (if (and (not (zerop (aref board row col))) (< (aref board row col) (car min)))
            (setf min (list (aref board row col) row col)))))
    min))

      

(defun find-path (board lowest next ineff)
  (if (< ineff 0)                                                    ; Impossible Path (no way to get back to next in enough moves) FAIL
      (return-from find-path nil)
    (if (equal (- (car next) (car lowest)) 1)                              ;Possible solution (the low value is only 1 away from next)
      (if (not (test-for-path lowest next))                          ;If lowest and next are not touching FAIL
          (return-from find-path nil)
        (progn
          (setf lowest (find-lowest board))
          (setf next (find-next board (car lowest)))
          (setf ineff (calc-ineff lowest next))
          (if (equal lowest (list (* *board-size* *board-size*) *board-size* *board-size*))
              (if (finish-start board (find-ultimate-low board))
                  (return-from find-path T)
                (return-from find-path nil)))
          (if (equal next (list (+ (* *board-size* *board-size*) 1) *board-size* *board-size*))
              (if (check-for-end-case board lowest)
                  (return-from find-path t)
                  (return-from find-path nil))
            (if (find-path board lowest next ineff)
                (return-from find-path T)
              (return-from find-path nil)))))
      (progn
      
    
    
  
  (let* ((lowest-val (car lowest)) (row (cadr lowest)) (col (caddr lowest))
         (successor (+ lowest-val 1))
         (new-board nil))



    ; top
      (if (and (> (- row 1) -1) (zerop (aref board (- row 1) col)))
       (progn
         (setf new-board (copy-board board))
         (setf (aref new-board (- row 1) col) successor)
         (if (find-path new-board (list successor (- row 1) col) next (calc-ineff (list successor (- row 1) col) next))
             (return-from find-path T))))

    ; bottom
      (if (and (< (+ col 1) *board-size*) (zerop (aref board row (+ col 1))))
        (progn
          (setf new-board (copy-board board))
          (setf (aref new-board row (+ col 1)) successor)
          (if (find-path new-board (list successor row (+ col 1)) next (calc-ineff (list successor row (+ col 1)) next))
              (return-from find-path T))))

    ; right
      (if (and (< (+ row 1) *board-size*) (zerop (aref board (+ row 1) col)))
       (progn
         (setf new-board (copy-board board))
         (setf (aref new-board (+ row 1) col) successor)
         (if (find-path new-board (list successor (+ row 1) col) next (calc-ineff (list successor (+ row 1) col) next))
             (return-from find-path T))))

    ; left
      (if (and (> (- col 1) -1) (zerop (aref board row (- col 1))))
        (progn
          (setf new-board (copy-board board))
          (setf (aref new-board row (- col 1)) successor)
          (if (find-path new-board (list successor row (- col 1)) next (calc-ineff (list successor row (- col 1)) next))
              (return-from find-path T))))

    (return-from find-path nil))))))


(defun find-next (board cur-lowest)
  (let ((next (list (+ (* *board-size* *board-size*) 1) *board-size* *board-size*)))
    (dotimes (row *board-size*)
      (dotimes (col *board-size*)
        (if (and (< (aref board row col) (car next)) (> (aref board row col) cur-lowest) (not (zerop (aref board row col))))
            (setf next (list (aref board row col) row col)))))
    next))

(defun find-lowest (board)
  "Finds the smallest value on the board without a successor"
  (let ((lowest (list (* *board-size* *board-size*) *board-size* *board-size*)))
    (dotimes (row *board-size*)
      (dotimes (col *board-size*)
        (if (and (< (aref board row col) (car lowest)) (not (zerop (aref board row col))) (not (touch-successor board row col)))                           
            (setf lowest (list (aref board row col) row col)))))
    lowest))



(defun touch-successor (board row col)
  "Returns t or nil depending on whether the value located at row col is adjacent to its successor"
  (let ((cur (aref board row col)))
    (cond
     ((and (> (- row 1) -1) 
           (if (equal (aref board (- row 1) col) (+ cur 1)) t nil))
      (return-from touch-successor t))
     
     ((and (< (+ row 1) *board-size*) 
           (if (equal (aref board (+ row 1) col) (+ cur 1)) t nil))
      (return-from touch-successor t))

     ((and (> (- col 1) -1)
           (if (equal (aref board row (- col 1)) (+ cur 1)) t nil))
      (return-from touch-successor t))

     ((and (< (+ col 1) *board-size*)
           (if (equal (aref board row (+ col 1)) (+ cur 1)) t nil))
      (return-from touch-successor t))
     
     (t (return-from touch-successor nil)))))                       
                            

                    
(defun calc-ineff (first next)
  "Calculates the ineffective move value. Used when finding paths.
   Defined as the (numerical difference - manhattant distance) / 2"
  (let ((num-diff (- (car next) (car first)))
        (manhattan-diff (+
                         (abs (- (cadr next) (cadr first)))
                         (abs (- (caddr next) (caddr first))))))
    (/ (- num-diff manhattan-diff) 2)))



(defun auto-solve ()
  "Where the magic happens."
  (let* ((lowest (setf *lowest* (find-lowest *board*))) 
         (next (find-next *board* (car lowest))) 
         (ineff (calc-ineff lowest next)) 
         (new-board nil))

    (setf new-board (copy-board *board*))
    (find-path new-board lowest next ineff)))


    
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gameplay
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun numbrix ()
  (loop
    (let ((ret-val nil))
      (create-board (prompt-filename))
      (print-instructions)
      (print-board)
      (loop
        (setf ret-val (prompt-move))
        (cond 
         ((equal 'q ret-val) (progn (print-moves) (return)))
         ((equal 'c ret-val) (progn (print-moves) (return)))))
      (cond
       ((equal ret-val 'q) (return))
       ((equal (prompt-repeat) nil) (return))))))