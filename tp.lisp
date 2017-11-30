;fact base


(defstruct waste
  material
  volume
  dirt_type
)

(defstruct selectiveTrash 
  container_list
    
)

(defstruct container
	color 
  max_capacity
  waste_amount
 )

;rule base

;init variables
(defvar green_container)
( setq green_container (make-container
  	:color '"green"
  	:max_capacity 100
    :waste_amount 0
  )
)
(defvar yellow_container)
( setq yellow_container (make-container
  	:color '"yellow"
  	:max_capacity 100
    :waste_amount 0
  )
)
(defvar blue_container)
( setq blue_container (make-container
  	:color '"blue"
  	:max_capacity 100
    :waste_amount 0

  )
)
(defvar waste1)
( setq waste1 (make-waste 
  	:material 'paper
    :volume 1
    :dirt_type 'clean
  )
) 

(defvar containerChosen)
(setq containerChosen nil)
(defvar stopCondition)
(defvar rulesList)


;Rule list
(defvar LR)
(setq LR nil)



; Définition des structures
(defstruct rule name condition action weight applied)

; Fonctions de manipulations
(defun newrule(myname mycondition myaction myweight)
    (push (make-rule ; comme le new en POO
               :name myname
               :condition mycondition
               :action myaction
               :weight myweight
               :applied nil
          )
    LR)
)

;(defun put (symbol prop value)     ;marche pas
;    (setf (get symbol prop) value))    

(defun myApply (x) ;@x une règle
    (eval (rule-action x)) ; on évalue l'action de x
    (setf (rule-applied x) t) ;(put x 'applied t) ) ; mets true dans la propriété appliquee de @x
)

; Tester si une règle est applicable 
(defun applicable (x)
    (print (rule-condition x))
    (eval (rule-condition x))                                   ;(get x 'condition ) )) ; on évalue la condition  retourn bool
)    

;Savoir si une règle a déjà été appliquée
(defun applied (x)
    (rule-applied x)
    ;(get x 'applied) 
) ; récupère la propriété appliquée

; findActivable
(defun findActivable ()
        (setq rulesList nil)
        (mapc 
            (lambda (x) 
                (print x)
                (print (applicable x))
                (cond
                    ((and (not (null (applied x))) (applicable x))
                    (setq rulesList (cons x rulesList)))
                )
                
            )
        LR)
        (print rulesList)
        rulesList
)
; adding rules

; r1 
;; condition -> si waste.material == glass 
;; action -> null
;; weight 1
(newrule 'r1 '(eq  (waste-material waste1) 'paper) '(print "ok") 1)

; r2
;; condition -> si waste.dirt_type == clean && r1.applied==true
;; action -> mettre dans la glassTrash
;; weight 13
(newrule 'r2 '(and (eq (waste-dirt_type waste1) 'clean ) (applied r1)) '(setq containerChosen 'blue_container) 1)






;stop condition
(setq stopCondition
	(cond
		
		((or (<= (container-max_capacity green_container) (container-waste_amount green_container))
			(<= (container-max_capacity blue_container) (container-waste_amount blue_container))
			(<= (container-max_capacity yellow_container) (container-waste_amount yellow_container))
            (not (null containerChosen))  ;on s'arrete quand la poubelle choisi est défini
                                            ;poubelle peut etre blue, yellow, green...ou other(aucune de nos poubelles correspondent)
		)t)
		

	)
)


; Moteur d'inférences
; -----------------------
(defun run ()
    (cond 
        ( (eval stopCondition) (print 'motor_stop) (print containerChosen ))
        ( 
            (null (findActivable)) (print 'problem_not_solve_or_nothing_applicable) nil 
        )
        ( t 
            (mapc 'myApply (findActivable)) ; on applique toutes les règles
            (run)
        )
    )
)

