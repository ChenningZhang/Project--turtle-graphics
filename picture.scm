;; Code for CS61A project 2 -- picture language

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))
;;Define the below operation for painters. Below takes two painters as arguments.
;; The resulting painter, given a frame, draws with the first painter in the bottom
;; of the frame and with the second painter in the top. Define below in two different
;; ways -- first by writing a procedure that is analogous to the beside procedure given
;; above, and again in terms of beside and suitable rotation operations (from the exercise above).
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point))
	   (paint-top
	    (transform-painter painter2
			       split-point
			       (make-vect 1.0 0.5)
			       (make-vect 0.0 1.0))))
      (lambda (frame)
	(paint-bottom frame)
	(paint-top frame)))))
;;second way-in terms of beside and rotation
;;(define (bolow painter1 painter2)
;;  (rotate90 (beside painter1 painter2)))

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

;;Define the procedure up-split used by corner-split. It is similar
;; to right-split, except that it switches the roles of below and beside.
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

;;Right-split and up-split can be expressed as instances of a general splitting operation.
;;Define a procedure split with the property that evaluating

;;(define right-split (split beside below))
;;(define up-split (split below beside))
;;produces procedures right-split and up-split with the same behaviors as the ones already defined.
(define (split proc1 proc2)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split proc1 proc2) painter (- n 1))))
	  (proc1 painter (proc2 smaller smaller))))))
(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (identity x) x)

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))

;; or

; (define flipped-pairs
;   (square-of-four identity flip-vert identity flip-vert))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

;;A two-dimensional vector v running from the origin to a point can be represented as a
;; pair consisting of an x-coordinate and a y-coordinate. Implement a data abstraction for
;; vectors by giving a constructor make-vect and corresponding selectors xcor-vect and
;; ycor-vect. In terms of your selectors and constructor, implement procedures add-vect,
;; sub-vect, and scale-vect that perform the operations vector addition, vector subtraction,
;; and multiplying a vector by a scalar.
;;Notice that scale-vect takes two different arguments.  To be compatible with the given code,
;;you should define scale-vect so that the first argument is the scalar, and the second argument is the vector.
(define (make-vect xcor-vect ycor-vect)
  (cons xcor-vect ycor-vect))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
	     (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
	     (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

;;Here are two possible constructors for frames:

;;(define (make-frame origin edge1 edge2)
;;  (list origin edge1 edge2))

;;(define (make-frame origin edge1 edge2)
;;  (cons origin (cons edge1 edge2)))
;;For each constructor supply the appropriate selectors to produce an implementation for frames.
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

;;alternative frame constructor and selectors
;;(define (make-frame origin edge1 edge2)
;;  (cons origin (cons edge1 edge2)))
;;(define (origin-frame frame) (car frame))
;;(define (edge1-frame frame) (cadr frame))
;;(define (edge2-frame frame) (cddr frame))

;;A directed line segment in the plane can be represented as a pair of vectors -- the
;; vector running from the origin to the start-point of the segment, and the vector
;; running from the origin to the end-point of the segment. Use your vector representation
;; from the previous page (Data Abstraction Part 1) to define a representation for segments
;; with a constructor make-segment and selectors start-segment and end-segment.
(define (make-segment a b c d)
  (cons (make-vect a b) (make-vect c d)))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;;Use segments->painter to define the following primitive painters:

;;a. The painter that draws the outline of the designated frame.

;;b. The painter that draws an X'' by connecting opposite corners of the frame.

;;c. The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.

;;d. The wave painter.
(define outline
  (segments->painter (list (make-segment 0.0 0.0 1.0 0.0)
			   (make-segment 1.0 0.0 1.0 1.0)
			   (make-segment 1.0 1.0 0.0 1.0)
			   (make-segment 0.0 1.0 0.0 0.0))
		     ))
(define X
  (segments->painter (list (make-segment 0.0 0.0 1.0 1.0)
			   (make-segment 0.0 1.0 1.0 0.0))
		      ))
(define diamond
  (segments->painter (list (make-segment 0.5 0.0 1.0 0.5)
			   (make-segment 1.0 0.5 0.5 1.0)
			   (make-segment 0.5 1.0 0.0 0.5)
			   (make-segment 0.0 0.5 0.5 0.0))
		     ))

(define wave
  (segments->painter (list (make-segment 0.0 0.625 0.13 0.4)
			   (make-segment 0.13 0.4 0.3125 0.6)
			   (make-segment 0.3125 0.6 0.375 0.5)
			   (make-segment 0.375 0.5 0.25 0.0)
			   (make-segment 0.38 0.0 0.5 0.25)
			   (make-segment 0.5 0.25 0.5625 0.0)
			   (make-segment 0.68 0.0 0.5625 0.49)
			   (make-segment 0.5625 0.49 1.0 0.125)
			   (make-segment 0.0 0.8 0.135 0.6)
			   (make-segment 0.135 0.6 0.3125 0.63)
			   (make-segment 0.3125 0.63 0.38 0.63)
			   (make-segment 0.38 0.63 0.32 0.8)
			   (make-segment 0.32 0.8 0.385 1.0)
			   (make-segment 0.5625 1.0 0.625 0.8)
			   (make-segment 0.625 0.8 0.5625 0.6)
			   (make-segment 0.5625 0.6 0.75 0.6)
			   (make-segment 0.75 0.6 1.0 0.25))
		     ))

(define (draw-line v1 v2)
  (penup)
  (setxy (- (* (xcor-vect v1) 200) 100)
	 (- (* (ycor-vect v1) 200) 100))
  (pendown)
  (setxy (- (* (xcor-vect v2) 200) 100)
	 (- (* (ycor-vect v2) 200) 100)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
		    (make-vect 0.5 0.5)
		    (make-vect 1.0 0.5)
		    (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))

;;Define the transformation flip-horiz, which flips painters horizontally,
;; and transformations that rotate painters counterclockwise by 180 degrees and 270 degrees.
(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))
(define (rotate180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)))
(define (rotate270 painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(define full-frame (make-frame (make-vect -0.5 -0.5)
			       (make-vect 2 0)
			       (make-vect 0 2)))

;;Make changes to the square limit of wave shown in the figure below by working at each of the levels described above. In particular: 



;;a. Add some segments to the primitive wave painter of exercise 6, Primitive Painters (to add a smile, for example).

;;b. Change the pattern constructed by corner-split (for example, by using only one copy of the up-split and right-split images instead of two).

;;c. Modify the version of square-limit that uses square-of-four so as to assemble the corners in a different pattern. (For example, you might make the big Mr. Rogers look outward from each corner of the square.)
(define wave-with-smile
  (segments->painter (list (make-segment 0.0 0.625 0.13 0.4)
			   (make-segment 0.13 0.4 0.3125 0.6)
			   (make-segment 0.3125 0.6 0.375 0.5)
			   (make-segment 0.375 0.5 0.25 0.0)
			   (make-segment 0.38 0.0 0.5 0.25)
			   (make-segment 0.5 0.25 0.5625 0.0)
			   (make-segment 0.68 0.0 0.5625 0.49)
			   (make-segment 0.5625 0.49 1.0 0.125)
			   (make-segment 0.0 0.8 0.135 0.6)
			   (make-segment 0.135 0.6 0.3125 0.63)
			   (make-segment 0.3125 0.63 0.38 0.63)
			   (make-segment 0.38 0.63 0.32 0.8)
			   (make-segment 0.32 0.8 0.385 1.0)
			   (make-segment 0.5625 1.0 0.625 0.8)
			   (make-segment 0.625 0.8 0.5625 0.6)
			   (make-segment 0.5625 0.6 0.75 0.6)
			   (make-segment 0.75 0.6 1.0 0.25)
			   (make-segment 0.4 0.875 0.42 0.875)
			   (make-segment 0.42 0.875 0.42 0.88)
			   (make-segment 0.42 0.88 0.4 0.88)
			   (make-segment 0.4 0.88 0.4 0.875)
			   (make-segment 0.53125 0.875 0.55125 0.875)
			   (make-segment 0.55125 0.875 0.55125 0.88)
			   (make-segment 0.55125 0.88 0.53125 0.88)
			   (make-segment 0.53125 0.88 0.53125 0.875)
			   (make-segment 0.42 0.78 0.44 0.74)
			   (make-segment 0.44 0.74 0.52 0.74)
			   (make-segment 0.52 0.74 0.54 0.78))
		     ))
;;b.change the pattern of corner-split
(define (corner-split-change painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1)))
	    (corner (corner-split painter (- n 1))))
	  (beside (below painter up)
		  (below right corner)))))
;;c.MODIFICATION VERSION OF SQUARE-LIMIT
(define (square-limit-waving-out painter n)
  (let ((combine4 (square-of-four identity flip-horiz
				  flip-vert rotate180 )))
    (combine4 (corner-split painter n))))
