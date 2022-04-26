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
                    (0.5 0 0.5)   ;Punto 1 
                    (-0.5 0 0.5)  ;Punto 2
                    (0.5 0 -0.5)  ;Punto 3
                    (-0.5 0 -0.5) ;Punto 4
                    ; Mateixos valors amb la diferencia que estan alçats
                    (0.5 1 0.5)   ;Punto 5 
                    (-0.5 1 0.5)  ;Punto 6
                    (0.5 1 -0.5)  ;Punto 7
                    (-0.5 1 -0.5) ;Punto 8
                  ) 'puntsC  
    )
    (putprop 'cub '(
                        (1 2)
                        (1 5)
                        (1 3)
                        (2 4)
                        (2 6)
                        (3 4)
                        (3 7)
                        (4 8)
                        (5 6)
                        (5 7)
                        (6 8)
                        (7 8)
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
    (afegeixFigura 'nom)
)

(defun afegeixFigura(f)
    (cons (get 'escena 'figura) f)
)

(defun iniciaEscena() 
    (putprop 'escena '() 'figura)
)


;pau
; elimina de la llista de figures posicio f
; sobreescribim el valor de figures a l'escena
(defun borra-figura (f)
    (putprop 'escena  (delete f (get 'escena 'figures)) 'figures)
)

;pau
; repintam la figura f, amb el color del fons (blanc)
(defun cls-figura (f)
    (crea-figura (aux (get f 'puntos) (255 255 255)))                   ; cream la figura a base dels punts passats per f
    (pinta-figura aux)                                                  ; pintam la figura aux      
    (putprop 'escena  (delete aux (get 'escena 'figures)) 'figures)     ; eliminam la figura aux de l'escena
)

;pau
;borra tot el contingut de l'escena (i de la pantalla)
(defun borra-figures
    (cond ((null l) nil)
        (t  (borra_figura (car (get 'escena 'figures)))
            (borra-figures)
        )
    )
)

(defun pinta-figura (f) 


)



(defun pinta-figures 

)



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

(defun pescalar (l1 l2)
    (cond ((null l1) nil)
        (t (+ (* (car l1) (car l2)) (pescalar (cdr l1) (cdr l2))))
    )
)
        
(defun transposta (m)
    (cond ((null (car m)) m)
        ()
    )
)
