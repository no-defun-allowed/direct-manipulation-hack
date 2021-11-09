(defstruct (point (:conc-name p-)) 
  (pos (vec 0.0 0.0 0.0))
  (vel (vec 0.0 0.0 0.0))
  (weight 100))
(defstruct (spring
            (:constructor
                make-spring
                (point1 point2 k
                 &aux (length
                       (vlength (v- (p-pos point1) (p-pos point2)))))))
  point1 point2
  length k)
(defstruct gravity point)

(defvar *points* (collection))
(defvar *forces* (collection))
(defmacro define-thing (name collection constructor)
  `(defvar ,name (in-collection ,collection ,constructor)))

;;; Points
(define-thing *base* *points*
  (make-point :pos (vec 0.0 0.0 0.0)
              :weight 1e9))
(define-thing *pendulum* *points*
  (make-point :pos (vec -3.5 -4.0 3.0)))
(define-thing *pendulum2* *points*
  (make-point :pos (vec -5.5 -4.0 3.0)))

;;; Forces
(define-thing *spring* *forces*
  (make-spring *pendulum* *base* 0.5))
(define-thing *gravity* *forces*
  (make-gravity :point *pendulum*))
(define-thing *spring2* *forces*
  (make-spring *pendulum* *pendulum2* 0.5))
(define-thing *gravity2* *forces*
  (make-gravity :point *pendulum2*))

(defgeneric apply-force (force)
  (:method ((spring spring))
    (let* ((point1 (spring-point1 spring))
           (p1 (p-pos point1))
           (point2 (spring-point2 spring))
           (p2 (p-pos point2))
           (midpoint (v/ (v+ p1 p2) 2))
           (delta-length (- (spring-length spring) (vlength (v- p1 p2))))
           (k (spring-k spring)))
      (setf (p-vel point1)
            (v+ (p-vel point1)
                (v* (v- p1 midpoint) delta-length *delta-t* k
                    (/ (p-weight point1))))
            (p-vel point2)
            (v+ (p-vel point2)
                (v* (v- p2 midpoint) delta-length *delta-t* k
                    (/ (p-weight point2)))))))
  (:method ((gravity gravity))
    (let ((point (gravity-point gravity)))
      (setf (p-vel point)
            (v- (p-vel point) (vec 0 (* *delta-t* 0.005) 0))))))

(defvar *iterations-per-step* 1000)

(defun tick ()
  ;; Iterate over points
  (let ((*delta-t* (/ *delta-t* *iterations-per-step*)))
    (dotimes (n *iterations-per-step*)
      (map-collection #'apply-force *forces*)
      (do-collection (p *points*)
        (setf (p-pos p)
              (v+ (p-pos p) (p-vel p))))))
  (clear-scene)
  (draw-line (p-pos *base*) (p-pos *pendulum*)
             17)
  (do-collection (p *points*)
    (draw-block (p-pos p) 49)))
