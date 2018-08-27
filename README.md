# lisp

(defvar *start* '(0 0))

 (defun first-jug (state) (car state))

 (defun second-jug (state) (cadr state))

(defun mk-state (f s) (list f s))

(defun goalp (state g)
  (equal  state g))

(defun new-states (state jug1 jug2) 
(remove-null
    (list
   (fill-first state jug1 jug2) 
(fill-second state jug1 jug2) 
(pour-first-second state jug1 jug2) 
(pour-second-first state jug1 jug2) 
(empty-first state) 
(empty-second state))))

(defun remove-null (x)
 (cond
  ((null x) nil)
    ((null (car x)) (remove-null (cdr x)))
    ((cons (car x) (remove-null (cdr x))))))

 (defun fill-first (state jug1 jug2)
  (cond
((< (first-jug state) jug1) 
(mk-state jug1 (second-jug state)))))

(defun fill-second (state jug1 jug2)
 (cond
  ((< (second-jug state) jug2) (mk-state (first-jug state) jug2))))

(defun pour-first-second (state jug1 jug2) 
(let ( (f (first-jug state))
(s (second-jug state))) 
(cond
((zerop f) nil)
 ((= s jug2) nil) 
((<= (+ f s) jug2)
(mk-state 0 (+ f s)))
(t (mk-state (- (+ f s) jug2) jug2)))))

(defun pour-second-first (state jug1 jug2)
(let ( (f (first-jug state))
(s (second-jug state)))
 (cond
  ((zerop s) nil)
 ((= f jug1) nil)
 ((<= (+ f s) jug1)
(mk-state (+ f s) 0))
(t (mk-state jug1 (- (+ f s) jug1))))))

(defun empty-first (state)  
(cond
((> (first-jug state) 0) (mk-state 0 (second-jug state)))))

(defun empty-second (state)
  (cond
  ((> (second-jug state) 0) (mk-state (first-jug state) 0))))



 (defun bfs1 (queue jug1 jug2 g)
(setf *node* (+ 1 *node*))
  (cond
   ((null queue) nil)
((goalp (caar queue) g)(car queue))
((> *node* *limit*) nil)
((let ((children (new-states (caar queue) jug1 jug2)))
  (setf *expanded* (+ 1 *expanded*))
(setf *branches* (+ (length children) *branches*)) 
  (bfs1
     (append (cdr queue)  
(mapcar  #'(lambda (state) (cons state (car queue)))
    children)) jug1 jug2 g ) ) ) ) )


(defun bfs (jug1 jug2 g )
 (setf state '(0 0))
       
           (setf *node* 0)
(setf *expanded* 0)
(setf *branches* 0)
(setf *limit* 100000)
 (setf *result* (bfs1 (list (list state)) jug1 jug2 g))
  (print (list *node* *expanded* *branches*))( reverse  *result*)) 

(defun dfs1 (state depth jug1 jug2 g )
(setf *node* (+ 1 *node*))                                                       
  (cond
     ((goalp state g) (list state))
((zerop depth) nil)
((> *node* *limit*) nil)
((let ((children (new-states state jug1 jug2)))
  (setf *expanded* (+ 1 *expanded*))
(setf *branches* (+ (length children) *branches*)) 
(let ((result (dfs2 children (- depth 1)jug1 jug2 g)))
   (and result (cons state result)))))))

(defun dfs2 (states depth jug1 jug2 g)
 (cond
((null states) nil)
((dfs1 (car states) depth jug1 jug2 g))
 ((dfs2 (cdr states) depth jug1 jug2 g))))

(defun dfs (jug1 jug2 g)
(setf depth 7)
(setf state '(0 0))
     (setf *node* 0)
(setf *expanded* 0)
(setf *branches* 0)
(setf *limit* 100000)
(setf *result* (dfs1 state depth jug1 jug2 g))
 (print (list *node* *expanded* *branches*)) *result*)
