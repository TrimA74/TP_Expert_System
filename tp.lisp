(defun longueur (l)
	(cond
			((null l) 0 ) ; si l est null on renvoie 0
			((atom l) nil) ; si l est un atom on renvoie nil
			( t ( + (longueur ( cdr l)) 1 )) ; sinon on renvoie longueur du cdr de la liste + 1 
	)
)

(defun longueurPlus (l)
	(cond
			((null l) 0 )
			((atom l) nil)
			( (listp ( car l)) ( + (longueurPlus (car l) (longueurPlus (cdr l) )) ) ) ; si car l est une liste on renvoie la longueur de car + cdr de l 
			( t ( + (longueur ( cdr l)) 1 ))
	)
)

(defun isIn (l e) 
	(cond 
		((atom l ) nil)
		((equal e (car l)) t) ; si e = car l 
		(t (isIn (cdr l) e)) ; sinon isIn 
	)
)

(defun munion ( l1 l2 )
	(cond
		((null l2) l1)
		((null l1) l2)
		((atom l2) (cond 
						( (isIn l1 l2) l1)
						(t append l1 l2)
					))
		((atom l1) (cond 
						( (isIn l2 l1) l2)
						(t append l2 l1
					)))
		((isIn l1 (car l2)) (munion l1 (cdr l2)) )
		(t (cons (car l2) (munion (cdr l2) l1 )) )
	)
)
; marche pas 
(defun inter ( l1 l2 )
	(cond
		( (not (and  (listp l1) (listp l2) ) ) nil ) 
		( (null l1 ) nil)
		( ( member l1 (car l2) ) ( cons (car l2) (inter l1 (cdr l2) ) ) )
		( t (inter l1 (cdr l2) ) ) 
	)
)
(defun my-intersection ( liste1 liste2 )
  (cond
       ( (not (and (listp liste1) (listp liste2))) nil)
       ( (null liste1) nil)
       ( (member (car liste1) liste2) ( cons (car liste1) (my-intersection (cdr liste1) liste2) ))
       (t (my-intersection (cdr liste1) liste2))
  )
)
(defun fin (l)
	(cond
		((null l) nil )
		( (null (cdr l) ) (car l) )
		( t (fin (cdr l)) )
	) 
)

(defun inverse (l)
	(cond
		( () () )
		(t () )


	)


)

(defun renverse (liste)
	(cond 
		( (atom liste) liste)
 	( t (append (renverse (cdr liste))
 		(list (renverse (car liste)))

 	))



 	)


) 
