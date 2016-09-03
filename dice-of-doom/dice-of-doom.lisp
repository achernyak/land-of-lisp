(defparameter *num-players* 4)
(defparameter *max-dice* 3)
(defparameter *board-size* 5)
(defparameter *board-hexnum* (* *board-size* *board-size*))

(defparameter *dice-odds* #(#(0.84 0.97 1.0 1.0)
			    #(0.44 0.78 0.94 0.99)
			    #(0.15 0.45 0.74 0.91)
			    #(0.04 0.19 0.46 0.72)
			    #(0.01 0.06 0.22 0.46)))

(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
		  collect (list (random *num-players*)
				(1+ (random *max-dice*))))))

(defun player-letter (n)
  (code-char (+ 97 n)))

(defun draw-board (board)
  (loop for y below *board-size*
     do (progn (fresh-line)
	       (loop repeat (- *board-size* y)
		  do (princ "  "))
	       (loop for x below *board-size*
		  for hex = (aref board (+ x (* *board-size* y)))
		  do (format t "~a-~a " (player-letter (first hex))
			     (second hex))))))

(defun game-tree (board player spare-dice first-move)
  (list player
	board
	(add-passing-move board
			  player
			  spare-dice
			  first-move
			  (attacking-moves board player spare-dice))))

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (lazy-cons (list nil
		  (game-tree (add-new-dice board player (1- spare-dice))
			     (mod (1+ player) *num-players*)
			     0
			     t))
	    moves)))

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
	     (car (aref board pos)))
	   (dice (pos)
	     (cadr (aref board pos))))
    (lazy-mapcan (lambda (src)
	      (if (eq (player src) cur-player)
		(lazy-mapcan (lambda (dst)
			  (if (and (not (eq (player dst) cur-player))
				     (> (dice src) 1))
			    (make-lazy
			     (list (list (list src dst)
					 (game-tree (board-attack board
								  cur-player
								  src
								  dst
								  (dice src))
						    cur-player
						    (+ spare-dice (dice dst))
						    nil)
					 (game-tree (board-attack-fail board
								       cur-player
								       src
								       dst
								       (dice src))
						    cur-player
						    (+ spare-dice (dice dst))
						    nil))))
			    (lazy-nil)))
			     (make-lazy (neighbors src)))
		(lazy-nil)))
		 (make-lazy (loop for n below *board-hexnum*
				  collect n)))))

(defun neighbors (pos)
  (let ((up (- pos *board-size*))
	(down (+ pos *board-size*)))
    (loop for p in (append (list up down)
			   (unless (zerop (mod pos *board-size*))
			     (list (1- up) (1- pos)))
			   (unless (zerop (mod (1+ pos) *board-size*))
			     (list (1+ pos) (1+ down))))
       when (and (>= p 0) (< p *board-hexnum*))
       collect p)))

(defun board-attack (board player src dst dice)
  (board-array (loop for pos
		  for hex across board
		  collect (cond ((eq pos src) (list player 1))
				((eq pos dst) (list player (1- dice)))
				(t hex)))))

(defun board-attack-fail (board player src dst dice)
  (board-array (loop for pos from 0
		  for hex across board
		  collect (if (eq pos src)
			      (list player 1)
			      hex))))

(defun roll-dice (dice-num)
  (let ((total (loop repeat dice-num
		  sum (1+ (random 6)))))
    (fresh-line)
    (format t "On ~a dice rolled ~a. " dice-num total)
    total))

(defun roll-against (src-dice dst-dice)
  (> (roll-dice src-dice) (roll-dice dst-dice)))

(defun pick-chance-branch (board move)
  (labels ((dice (pos)
	     (cadr (aref board pos))))
    (let ((path (car move)))
      (if (or (null path) (roll-against (dice (car path))
				    (dice (cadr path))))
	  (cadr move)
	  (caddr move)))))

(defun add-new-dice (board player spare-dice)
       (labels ((f (lst n acc)
		  (cond ((zerop n) (append (reverse acc) lst))
			((null lst) (reverse acc))
			(t (let ((cur-player (caar lst))
				 (cur-dice (cadar lst)))
			     (if (and (eq cur-player player)
				      (< cur-dice *max-dice*))
				 (f (cdr lst)
				    (1- n)
				    (cons (list cur-player (1+ cur-dice)) acc))
				 (f (cdr lst) n (cons (car lst) acc))))))))
	 (board-array (f (coerce board 'list)
			 (largest-cluster-size board player) ()))))

(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
	       (unless (lazy-null moves)
		 (let* ((move (lazy-car moves))
			(action (car move)))
		   (fresh-line)
		   (format t "~a. " n)
		   (if action
		       (format t "~a -> ~a" (car action) (cadr action))
		       (princ "end turn")))
		 (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (pick-chance-branch (cadr tree) (lazy-nth (1- (read)) moves))))

(defun winners (board)
  (let* ((tally (loop for hex across board
		   collect (car hex)))
	 (totals (mapcar (lambda (player)
			  (cons player (count player tally)))
			(remove-duplicates tally)))
	 (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
	    (remove-if (lambda (x)
			 (not (eq (cdr x) best)))
		       totals))))

(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
	(format t "The game is a tie between ~a" (mapcar #'player-letter w))
	(format t "The winner is ~a" (player-letter (car w))))))

;; AI

(defun rate-position (tree player)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
	(apply (if (eq (car tree) player)
		   #'max
		   #'min)
	       (get-ratings tree player))
	(score-board (cadr tree) player))))

(defun get-ratings (tree player)
  (let ((board (cadr tree)))
    (labels ((dice (pos)
	       (cadr (aref board pos))))
      (take-all (lazy-mapcar
		 (lambda (move)
		   (let ((path (car move)))
		     (if path
			 (let* ((src (car path))
				(dst (cadr path))
				(odds (aref (aref *dice-odds*
						  (1- (dice dst)))
					    (- (dice src) 2))))
			   (+ (* odds (rate-position (cadr move) player))
			      (* (- 1 odds) (rate-position (caddr move)
							   player))))
			 (rate-position (cadr move) player))))
		 (caddr tree))))))

(defparameter *ai-level* 4)

(defun handle-computer (tree)
  (let ((ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*)
				     (car tree)
				     most-positive-fixnum
				     most-negative-fixnum)))
    (pick-chance-branch
     (cadr tree)
     (lazy-nth (position (apply #'max ratings) ratings)
		      (caddr tree)))))

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree)))
	((zerop (car tree)) (play-vs-computer (handle-human tree)))
	(t (play-vs-computer (handle-computer tree)))))

(defun score-board (board player)
  (loop for hex across board
	for pos from 0
	sum (if (eq (car hex) player)
		(if (threatened pos board)
		    1
		    2)
		-1)))

(defun threatened (pos board)
  (let* ((hex (aref board pos))
	 (player (car hex))
	 (dice (cadr hex)))
    (loop for n in (neighbors pos)
	  do (let* ((nhex (aref board n))
		    (nplayer (car nhex))
		    (ndice (cadr nhex)))
	       (when (and (not (eq player nplayer)) (> ndice dice))
		 (return t))))))

(defun ab-get-ratings-max (tree player upper-limit lower-limit)
  (labels ((f (moves lower-limit)
	     (unless (lazy-null moves)
	       (let ((x (ab-rate-position (cadr (lazy-car moves))
					  player
					  upper-limit
					  lower-limit)))
		 (if (>= x upper-limit)
		     (list x)
		     (cons x (f (lazy-cdr moves) (max x lower-limit))))))))
    (f (caddr tree) lower-limit)))

(defun ab-get-ratings-min (tree player upper-limit lower-limit)
  (labels ((f (moves upper-limit)
	     (unless (lazy-null moves)
	       (let ((x (ab-rate-position (cadr (lazy-car moves))
					  player
					  upper-limit
					  lower-limit)))
		 (if (<= x lower-limit)
		     (list x)
		     (cons x (f (lazy-cdr moves) (min x upper-limit))))))))
    (f (caddr tree) upper-limit)))

(defun ab-rate-position (tree player upper-limit lower-limit)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
	(if (eq (car tree) player)
	    (apply #'max (ab-get-ratings-max tree
					     player
					     upper-limit
					     lower-limit))
	    (apply #'min (ab-get-ratings-min tree
					     player
					     upper-limit
					     lower-limit)))
	(score-board (cadr tree) player))))

;; Memoizing

(let ((old-neighbors (symbol-function 'neighbors))
      (previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
	(setf (gethash pos previous) (funcall old-neighbors pos)))))

(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
	(setf (gethash rest previous) (apply old-game-tree rest)))))

(let ((old-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab
	(setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
	  (setf (gethash tree tab)
		(funcall old-rate-position tree player))))))

(defun limit-tree-depth (tree depth)
  (list (car tree)
	(cadr tree)
	(if (zerop depth)
	    (lazy-nil)
	    (lazy-mapcar (lambda (move)
			   (cons (car move)
				 (mapcar (lambda (x)
					   (limit-tree-depth x (1- depth)))
					 (cadr move))))
			 (caddr tree)))))

(defun get-connected (board player pos)
  (labels ((check-pos (pos visited)
	     (if (and (eq (car (aref board pos)) player)
		      (not (member pos visited)))
		 (check-neighbors (neighbors pos) (cons pos visited))
		 visited))
	   (check-neighbors (lst visited)
	     (if lst
		 (check-neighbors (cdr lst) (check-pos (car lst) visited))
		 visited)))
    (check-pos pos '())))

(defun largest-cluster-size (board player)
  (labels ((f (pos visited best)
	     (if (< pos *board-hexnum*)
		 (if (and (eq (car (aref board pos)) player)
			  (not (member pos visited)))
		     (let* ((cluster (get-connected board player pos))
			    (size (length cluster)))
		       (if (> size best)
			   (f (1+ pos) (append cluster visited) size)
			   (f (1+ pos) (append cluster visited) best)))
		     (f (1+ pos) visited best))
		 best)))
    (f 0 '() 0)))
