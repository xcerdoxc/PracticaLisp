(defun conversorGR (x)
    (/ (* x pi ) 180)
)

(defun inicia-patrons()
    (inicia-patronsCub)
    (iniciaEscena)
)

;Crea les figura del Cub
(defun inicia-patronsCub()
    (putprop 'cub '(
                    (0 0 0)   ;Punto 1 
                    (100 0 0)  ;Punto 2
                    (100 100 0)  ;Punto 3
                    (0 100 0) ;Punto 4
                    ; Mateixos valors amb la diferencia que estan alçats
                    (0 0 100)   ;Punto 5 
                    (100 0 100)  ;Punto 6
                    (100 100 100)  ;Punto 7
                    (0 100 100) ;Punto 8
                  ) 'puntsC  
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
                    ) 'arestesC
    )
    (putprop 'cub '(
                        (1 2 3 4)
                        (1 2 6 5)
                        (1 3 5 7)
                        (2 4 6 8)
                        (3 4 7 8)
                        (5 6 7 8)

                  ) 'caresC
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
                'transformacions
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
; sobreescribim el valor de figures a l'escena
(defun borra-figura (f)
    (putprop 'escena  (delete f (get 'escena 'figures)) 'figures)
    (cls-figura f)
)

; repintam la figura f, amb el color del fons (blanc)
(defun cls-figura (f)
    (crea-figura (aux (get f 'puntos) (255 255 255)))                   ; cream la figura a base dels punts passats per f
    (pinta-figura aux)                                                  ; pintam la figura aux      
    (putprop 'escena  (delete aux (get 'escena 'figures)) 'figures)     ; eliminam la figura aux de l'escena
)

(defun borra-figures ()
    (cls)   ;Borramos pantalla
    (putprop 'escena nil 'figures) ;-> borra la lista figures de escena
)

(defun pinta-figura (f)
    ;cridam a la funcio que pintara les cares recursivament de f (passem llista de cares per parametre i la seva posicio)
    (eval (cons 'color (get f 'colorF))) ;define el color
    (pinta-cares (get (get f 'patroFigura) 'caresC) f)
)

;(get (get 'nomCub50 'patroFigura) 'arestesC)
(defun pinta-cares (caresL f)
    ;mentres hi hagi cares, cridar a pinta-cares
    (cond ((null (cdr caresL)) (pinta-arestes-marto (car caresL) f))
        (t  (moveCursor (agafa-n (get (get f 'patroFigura) 'puntsC) (+ 1 (car (agafa-n (get (get f 'patroFigura) 'arestesC) (+ 1 (car (car caresL))))))))
            (pinta-arestes-martoXF (car caresL) f)
            (pinta-cares (cdr caresL) f)
        )
    )
)


(defun pinta-arestes-no-funciona (grupArestes f)
    (cond ((null grupArestes) nil)
        (t  (pinta-arestes-marto (car grupArestes) f) 
            (pinta-arestes-marto (cadr grupArestes) f)
            (pinta-arestes-marto (caddr grupArestes) f)
            (pinta-arestes-marto (cadddr grupArestes) f)
        )
    )
)

(defun pinta-punts-marto (La f)
    ;(moveCursor (agafa-n (get (get f 'patroFigura) 'puntsC) (+ 1 (car La))));se ha de ejecutar una vex por cada cara, y luego
    (drawCursor  (agafa-n (get (get f 'patroFigura) 'puntsC) (+ 1 (cadr La))))
)



(defun pinta-arestes-martoXF (Lc f)
    (cond ((null (cdr Lc)) (pinta-punts-marto (agafa-n (get (get f 'patroFigura) 'arestesC) (+ 1 (car Lc))) f))
        (t(pinta-punts-marto (agafa-n (get (get f 'patroFigura) 'arestesC) (+ 1 (car Lc))) f) 
        (pinta-arestes-marto (cdr Lc) f))
    )
)



;tiene un error ;crear un comentario para entenderlo bien
(defun pinta-arestes-marto (Lc f)
    (cond ((null (cdr Lc)) (moveCursor (agafa-n (get (get f 'patroFigura) 'puntsC)  (car (agafa-n (get (get f 'patroFigura) 'arestesC (+ 1 (car Lc)))))))(pinta-punts-marto (agafa-n (get (get f 'patroFigura) 'arestesC) (+ 1 (car Lc))) f))
        (t (moveCursor (agafa-n (get (get f 'patroFigura) 'puntsC) (+ 1 (car (agafa-n (get (get f 'patroFigura) 'arestesC) (+ 1 (car Lc)))))))(pinta-punts-marto (agafa-n (get (get f 'patroFigura) 'arestesC) (+ 1 (car Lc))) f) 
        (pinta-arestes-marto (cdr Lc) f))
    )
)




(defun pinta-aresta (arestesL f)  ;(1 2)  ;como accedo a dicha lista a la posicion que quiero 
    (cond ((null arestesL) nil)
        (t                                  ;(cdr todoEso)         car(cdr todoEso)
            ;(move x y)  ;punt 1  retonrP => (-0.5          0 -0.5)             
            (moveCursor (retornP (car arestesL) (get (get f 'patroFigura) 'puntsC)))
            ;(draw x y)  ;punt 2
            (drawCursor  (retornP (cadr arestesL) (get (get f 'patroFigura) 'puntsC)))            ;car(cdr l) = cadr l
        )
    )
)

;(pinta-aresta ((get (get 'nom3 'patroFigura) 'arestesC) 'nom3) nom3) 

;Función que retorna el n-esimo elemento de una lista
(defun retornP (n puntsL)
    (cond ((null puntsL) nil) 
        ((= n 0) (car puntsL))
        (t (retornP (- n 1) (cdr puntsL)))    
    )
)

;Funciones para pintar una lista de puntos desde el centro de la pantalla
(defun moveCursor (L) 
    (move (toInt (+ 320 (car L))) (toInt (+ 187 (cadr L))))    
)

(defun drawCursor (L) 
    (draw (toInt(+ 320 (car L))) (toInt(+ 187 (cadr L))))    
)

(defun toInt (x) (realpart (round x)))

(defun translacio (dx dy dz) 
    '((1 0 0 0) (0 1 0 0) (0 0 1 0) (dx dy dz 1))
)

(defun escalat (ex ey ez) 
    '((ex 0 0 0) (0 ey 0 0) (0 0 ez 0) (0 0 0 1))
)

(defun rotax (a) 
    '((1 0 0 0)
     (0 (cos a) (- 0 (sin a)) 0) 
     (0 (sin a) (cos a) 0) 
     (0 0 0 1))
)

(defun rotay (a) 
    '(((cos a) 0 (- 0 (sin a)) 0) 
    (0 1 0 0) 
    ((sin a) 0 (cos a) 0) 
    (0 0 0 1))
)

(defun rotaz (a) 
    '(((cos a) (- 0 (sin a)) 0 0) 
    ((sin a) (cos a) 0 0) 
    (0 0 1 0) 
    (0 0 0 1))
)

(defun trasllada-figura (f x y z) )

(defun rota-figura (f x y z) )

(defun escala-figura (f x y z) )

(defun inicia-figura (f) )

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
		
;Monta una lista con los n-esimos elementos de cadafila de la matriz
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
		(t (cons (opera-fila L (car M)) (mult-fila L (cdr M))))
    )
)

;Opera las filas para obtener el elemento Eij de la matriz resultado
(defun opera-filas (L1 L2)
	(cond ((null (cdr L1)) (* (car L1) (car L2)))
		(t (+ (* (car L1) (car L2)) (opera-filas (cdr L1) (cdr L2))))
    )
)