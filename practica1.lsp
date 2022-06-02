(defun conversorGR (x)
    (/ (* x pi ) 180)
)

(defun inicia-patrons()
    (inicia-patronsCub)
    (inicia-patronsOctaedre)
    (inicia-patronsPrisma)
    (iniciaEscena)
)

;Crea les figura del Cub
(defun inicia-patronsCub()
    (putprop 'cub '((-50 -50 -50)   ;Punto 1 
                    (50 -50 -50)  ;Punto 2
                    (50 50 -50)  ;Punto 3
                    (-50 50 -50) ;Punto 4
                    ; Mateixos valors amb la diferencia que estan alçats
                    (-50 -50 50)   ;Punto 5 
                    (50 -50 50)  ;Punto 6
                    (50 50 50)  ;Punto 7
                    (-50 50 50) ;Punto 8
                  ) 'punts  
    )
    (putprop 'cub '(
                        (1 2)
                        (2 3)
                        (3 4)
                        (4 1)
                        (5 6)
                        (6 7)

                        (8 7)
                        (5 8)
                        (6 2)
                        (7 3)
                        (8 3)
                        (1 5)
                    ) 'arestes
    )
    (putprop 'cub '(
                        (1 2 3 4)
                        (1 2 6 5)
                        (1 3 5 7)
                        (2 4 6 8)
                        (3 4 7 8)
                        (5 6 7 8)

                  ) 'cares
    )
)


(defun inicia-patronsPrisma()
    (putprop 'prisma '((0 28 50)   ;Punto 1 
                    (25 -14 100)  ;Punto 2
                    (-25 -14 100)  ;Punto 3
                    (0 28 -50) ;Punto 4
                    ; Mateixos valors amb la diferencia que estan alçats
                    (25 -14 -50)   ;Punto 5 
                    (-25 -14 -50)  ;Punto 6
                  ) 'punts  
    )
    (putprop 'prisma '(
                        (1 2)
                        (1 3)
                        (2 3)
                        (4 5)
                        (4 6)
                        (5 6)

                        (3 6)
                        (1 4)
                        (2 5)
                    ) 'arestes
    )
    (putprop 'prisma '(
                        (1 2 3)
                        (9 8 7)
                        (3 6 4 9)
                        (2 5 6 8)
                        (1 4 5 7)

                  ) 'cares
    )
)

;Crea la figura de l'octaedre
(defun inicia-patronsOctaedre ()
    (putprop 'octaedre '(
                    (-50 0 -50)   ;Punto 1 
                    (50 0 -50)  ;Punto 2
                    (50 0 50)  ;Punto 3
                    (-50 0 50) ;Punto 4
                    ; Mateixos valors amb la diferencia que estan alçats
                    (0 100 0)   ;Punto 5 
                    (0 -100 0)  ;Punto 6
                  ) 'punts
    )
    (putprop 'octaedre '(
                        (1 2)
                        (2 3)
                        (3 4)
                        (4 1)
                        (1 5)
                        (2 5)

                        (3 5)
                        (4 5)
                        (1 6)
                        (2 6)
                        (3 6)
                        (4 6)
                    ) 'arestes
    )
    (putprop 'octaedre '(
                        (1 2 3 4)
                        (1 5 6)
                        (2 6 7)
                        (3 7 8)
                        (4 8 5)

                        (1 9 10)
                        (2 10 11)
                        (3 11 12)
                        (4 12 9)
                    ) 'cares
    )
)

(defun crea-figura (nom patro color)
    (putprop nom color 'colorF)
    (putprop nom patro 'patroFigura)
    (putprop nom '(
                    (1 0 0 0)
                    (0 1 0 0)
                    (0 0 1 0)
                    (0 0 0 1)
                )
                'matriu
    )
    (afegeixFigura nom)
)

(defun afegeixFigura(f)
    (putprop 'escena (cons f (get 'escena 'figures) ) 'figures)
)

(defun iniciaEscena() 
    (putprop 'escena '() 'figura)
)


; elimina de la llista de figures posicio f
(defun borra-figura (f)
    (putprop 'escena  (delete f (get 'escena 'figures)) 'figures)
)

; repintam la figura f, amb el color del fons (blanc)
(defun cls-figura (f)
    (crea-figura (f (get f 'puntos) (255 255 255)))                   ; cream la figura a base dels punts passats per f
    (pinta-figura f)                                                  ; pintam la figura aux      
    (putprop 'escena  (delete f (get 'escena 'figures)) 'figures)     ; eliminam la figura aux de l'escena
)

(defun borra-figures ()
    (cls)   ;Borramos pantalla
    (putprop 'escena nil 'figures) ;-> borra la lista figures de escena
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pinta-figura (f)
    ;cridam a la funcio que pintara les cares recursivament de f (passem llista de cares per parametre i la seva posicio)
    (eval (cons 'color (get f 'colorF))) ;define el color
    (pinta-cares (get (get f 'patroFigura) 'cares) f)
)

(defun pinta-cares (caresL f)
    ;mentres hi hagi cares, cridar a pinta-cares
    (cond ((null caresL) t)
        (t  
            (pinta-arestes (car caresL) f)
            (pinta-cares (cdr caresL) f)
        )
    )
)

(defun pinta-arestes (arestesL f)
    (cond ((null arestesL) t)
        (t (pinta-punts (agafa-n (get (get f 'patroFigura) 'arestes) (- (car arestesL) 1)) f) 
        (pinta-arestes (cdr arestesL) f))
    )
)

(defun pinta-punts (La f) 
    ;Multiplicamos los vertices por la matriz de transformación
    (moveCursor (mult-fila (add1 (agafa-n (get (get f 'patroFigura) 'punts) (- (car La) 1))) (transposta (get f 'matriu))))      
    (drawCursor (mult-fila (add1 (agafa-n (get (get f 'patroFigura) 'punts) (- (cadr La) 1))) (transposta (get f 'matriu))))
)

;Añade un 1 al final de la lista para poder multiplicar
(defun add1 (L)
    (reverse (cons 1 (reverse L)))
)

;Funciones para pintar una lista de puntos desde el centro de la pantalla
(defun moveCursor (L) 
    (move (toInt (+ 320 (car L))) (toInt (+ 187 (cadr L))))    
)

(defun drawCursor (L)
    (draw (toInt(+ 320 (car L))) (toInt(+ 187 (cadr L))))    
)

(defun toInt (x) (realpart (round x)))

(defun retornP (n puntsL)
    (cond ((null puntsL) nil) 
        ((= n 0) (car puntsL))
        (t (retornP (- n 1) (cdyr puntsL)))    
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun translacio (dx dy dz) 
    (list '(1 0 0 0) 
        '(0 1 0 0) 
        '(0 0 1 0) 
        (list dx dy dz 1)
    )
)

(defun escalat (ex ey ez) 
    (list (ex 0 0 0) 
        (list 0 ey 0 0) 
        (list 0 0 ez 0) 
        '(0 0 0 1)
    )
)

(defun rotax (a) 
    (list '(1 0 0 0)
        (list 0 (cos a) (- 0 (sin a)) 0) 
        (list 0 (sin a) (cos a) 0) 
        '(0 0 0 1)
    )
)

(defun rotay (a) 
    (list (list (cos a) 0 (- 0 (sin a)) 0) 
        '(0 1 0 0) 
        (list (sin a) 0 (cos a) 0) 
        '(0 0 0 1)
    )
)

(defun rotaz (a) 
    (list (list (cos a) (- 0 (sin a)) 0 0) 
        (list (sin a) (cos a) 0 0) 
        '(0 0 1 0) 
        '(0 0 0 1)
    )
)

(defun trasllada-figura (f x y z) 
    (putprop f (mult-mat (get f 'matriu) (translacio x y z)) 'matriu)
)

(defun rota-figura (f x y z) 
    (putprop f (mult-mat (get f 'matriu) (rotax x)) 'matriu)
    (putprop f (mult-mat (get f 'matriu) (rotay y)) 'matriu)
    (putprop f (mult-mat (get f 'matriu) (rotaz z)) 'matriu)
)

(defun escala-figura (f x y z)
    (putprop f (mult-mat (get f 'matriu) (escalat x y z)) 'matriu)
)

(defun inicia-figura (f)



)

;---------------------------------------------
;-------------      PART 2      --------------
;---------------------------------------------

(defun animacio (f) )

(defun anima-rotacio (f) )

(defun anima-translacio (f) )

(defun anima-escalat (f) )

;---------------------------------------------
;-------       FUNCIONS AUXILIARS      -------
;---------------------------------------------

;transposta de una matriu
(defun transposta (M) (transposta2 M 0))

(defun transposta2 (M n)
	(cond ((= n (contafiles M)) nil)
		(t (cons (fila-n M n) (transposta2 M (+ n 1))))
    )
)

;Devuelve el n-esimo elemento de una lista
(defun agafa-n (L n)
    (cond ((= n 0) (car L))
		(t(agafa-n (cdr L) (- n 1)))
    )
)
		
;Monta una lista con los n-esimos elementos de cada fila de la matriz
(defun fila-n (M n)
    (cond ((null M) nil)
	    (t(cons (agafa-n (car M) n) (fila-n (cdr M) n)))
    )
)
		
;Cuenta las filas de una matriz
(defun contafiles (M)
	(cond ((null M) 0)
		(t(+ 1 (contafiles (cdr M))))
    )
)

;Multiplicació de matrius
(defun mult-mat (M1 M2) (mult-mat2 M1 (transposta M2)))

(defun mult-mat2 (M1 M2)
	(cond ((null M1) nil)
		(t (cons (mult-fila (car M1) M2) (mult-mat2 (cdr M1) M2)))
    )
)

;Realiza las operaciones recursivas para obtener la fila de la matriz resultado
(defun mult-fila (L M)
	(cond ((null M) nil)
		(t (cons (opera-filas L (car M)) (mult-fila L (cdr M))))
    )
)

;Opera las filas para obtener el elemento Eij de la matriz resultado
(defun opera-filas (L1 L2)
	(cond ((null (cdr L1)) (* (car L1) (car L2)))
		(t (+ (* (car L1) (car L2)) (opera-filas (cdr L1) (cdr L2))))
    )
)
