; --------------------------------------------------------------------------------------------------------------------
; -----------------------------------------------  CLASSES  ----------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Clases definidas en la ontología (exportar de CLIPS)

; Fri Nov 30 09:52:54 GMT+01:00 2018
; 
;+ (version "3.5")
;+ (build "Build 663")


; Mon Dec 03 17:37:13 CET 2018
; 
;+ (version "3.5")
;+ (build "Build 663")


(defclass %3ACLIPS_TOP_LEVEL_SLOT_CLASS "Fake class to save top-level slot information"
	(is-a USER)
	(role abstract)
	(single-slot Dia
		(type INTEGER)
		(range 1 7)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Intensidad
		(type INTEGER)
		(range 0 2)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot Num_repeticiones
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Duracion
		(type INTEGER)
		(default 0)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Beneficios
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot num_Repeticiones
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot IAPractica2_Class7
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot IAPractica2_Class1
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Sesiones
		(type INSTANCE)
;+		(allowed-classes Session)
		(cardinality 3 7)
		(create-accessor read-write))
	(multislot Partes_ejercitadas
		(type STRING)
		(create-accessor read-write))
	(single-slot Repeticiones
		(type INTEGER)
		(range 0 10)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot num_repeticiones
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot partes_Ejercitadas
		(type STRING)
		(create-accessor read-write))
	(single-slot Repeticiones_Ejercicio
		(type INTEGER)
		(range 0 100)
		(default 1)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot partes_ejercitadas
		(type STRING)
		(create-accessor read-write))
	(single-slot Nombre_Deporte
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(multislot Ejercicios
		(type INSTANCE)
;+		(allowed-classes Ejercicio)
		(create-accessor read-write))
	(multislot Partes_Ejercitadas
		(type STRING)
		(create-accessor read-write))
	(multislot Deportes
		(type INSTANCE)
;+		(allowed-classes Deporte)
		(create-accessor read-write))
	(multislot IAPractica2_Class10004
		(type STRING)
		(create-accessor read-write))
	(single-slot IAPractica2_Class10005
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Nombre_Ejercicio
;+		(comment "Nombre del ejercicio")
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot nombre
		(type STRING)
;+		(cardinality 0 1)
		(create-accessor read-write)))

(defclass Session
	(is-a USER)
	(role concrete)
	(single-slot Dia
		(type INTEGER)
		(range 1 7)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot Ejercicios
		(type INSTANCE)
;+		(allowed-classes Ejercicio)
		(create-accessor read-write)))

(defclass Ejercicio
	(is-a USER)
	(role concrete)
	(single-slot Intensidad
		(type INTEGER)
		(range 0 2)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot Num_repeticiones
		(type INTEGER)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(single-slot Nombre_Ejercicio
;+		(comment "Nombre del ejercicio")
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot Duracion
		(type INTEGER)
		(default 0)
;+		(cardinality 0 1)
		(create-accessor read-write))
	(multislot partes_Ejercitadas
		(type STRING)
		(create-accessor read-write)))

(defclass Aerobico "Aerobico o resistencia"
	(is-a Ejercicio)
	(role concrete))

(defclass Fuerza "Fuerza o musculacion"
	(is-a Ejercicio)
	(role concrete))

(defclass Equilibrio
	(is-a Ejercicio)
	(role concrete))

(defclass Flexibilidad
	(is-a Ejercicio)
	(role concrete))

(defclass Deporte
	(is-a USER)
	(role concrete)
	(single-slot Nombre_Deporte
		(type STRING)
;+		(cardinality 1 1)
		(create-accessor read-write))
	(single-slot Intensidad
		(type INTEGER)
		(range 0 2)
;+		(cardinality 1 1)
		(create-accessor read-write)))

; --------------------------------------------------------------------------------------------------------------------
; -----------------------------------------------  INSTANCES  --------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Instancias (de CLIPS también)

(definstances Instancias



([IAPractica2_Class10] of  Aerobico

	(Duracion 30)
	(Intensidad 0)
	(Nombre_Ejercicio "caminar")
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class10007] of  Flexibilidad

	(Duracion 7)
	(Intensidad 1)
	(Nombre_Ejercicio "estiramiento de los tendones del muslo")
	(Num_repeticiones 0)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class10008] of  Aerobico

	(Duracion 30)
	(Intensidad 1)
	(Nombre_Ejercicio "pedalear")
	(partes_Ejercitadas
		"brazos"
		"piernas"))

([IAPractica2_Class10010] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "levantar los brazos")
	(Num_repeticiones 0)
	(partes_Ejercitadas "brazos"))

([IAPractica2_Class10011] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "flexion de hombros")
	(Num_repeticiones 1)
	(partes_Ejercitadas "brazos"))

([IAPractica2_Class10013] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "ejercicios de biceps")
	(Num_repeticiones 2)
	(partes_Ejercitadas "brazos"))

([IAPractica2_Class10014] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "extension de triceps")
	(Num_repeticiones 3)
	(partes_Ejercitadas "brazos"))

([IAPractica2_Class10015] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "levantarse de una silla")
	(Num_repeticiones 4)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class10017] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "flexion plantar")
	(Num_repeticiones 5)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class10018] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "flexion de rodilla")
	(Num_repeticiones 6)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class10019] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "extension de rodilla")
	(Num_repeticiones 7)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class10020] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "flexion de cadera")
	(Num_repeticiones 8)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class10021] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "extension de cadera")
	(Num_repeticiones 9)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class10022] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "elevar piernas a los lados")
	(Num_repeticiones 10)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class10026] of  Equilibrio

	(Duracion 15)
	(Intensidad 0)
	(Nombre_Ejercicio "flexion plantar")
	(Num_repeticiones 0))

([IAPractica2_Class10027] of  Equilibrio

	(Duracion 15)
	(Intensidad 0)
	(Nombre_Ejercicio "flexion de rodilla")
	(Num_repeticiones 1))

([IAPractica2_Class10028] of  Equilibrio

	(Duracion 15)
	(Intensidad 0)
	(Nombre_Ejercicio "flexion de cadera")
	(Num_repeticiones 2))

([IAPractica2_Class10029] of  Equilibrio

	(Duracion 15)
	(Intensidad 0)
	(Nombre_Ejercicio "extension de cadera")
	(Num_repeticiones 3))

([IAPractica2_Class13] of  Aerobico

	(Duracion 3)
	(Intensidad 0)
	(Nombre_Ejercicio "subir escaleras")
	(Num_repeticiones 3)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class16] of  Aerobico

	(Duracion 30)
	(Intensidad 1)
	(Nombre_Ejercicio "natacion"))

([IAPractica2_Class17] of  Aerobico

	(Duracion 20)
	(Intensidad 2)
	(Nombre_Ejercicio "remar")
	(partes_Ejercitadas "brazos"))

([IAPractica2_Class19] of  Equilibrio

	(Duracion 15)
	(Intensidad 0)
	(Nombre_Ejercicio "levantar pierna")
	(Num_repeticiones 4))

([IAPractica2_Class20009] of  Deporte

	(Intensidad 0)
	(Nombre_Deporte "paseo marcha o senderismo"))

([IAPractica2_Class20010] of  Deporte

	(Intensidad 2)
	(Nombre_Deporte "footing"))

([IAPractica2_Class20011] of  Deporte

	(Intensidad 1)
	(Nombre_Deporte "aquagym"))

([IAPractica2_Class20012] of  Deporte

	(Intensidad 1)
	(Nombre_Deporte "golf"))

([IAPractica2_Class20013] of  Deporte

	(Intensidad 1)
	(Nombre_Deporte "vela"))

([IAPractica2_Class20014] of  Deporte

	(Intensidad 1)
	(Nombre_Deporte "ciclismo"))

([IAPractica2_Class20015] of  Aerobico

	(Duracion 30)
	(Intensidad 1)
	(Nombre_Ejercicio "danza")
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class20016] of  Flexibilidad

	(Duracion 7)
	(Intensidad 1)
	(Nombre_Ejercicio "estiramiento de la pantorrilla")
	(Num_repeticiones 1)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class20017] of  Deporte

	(Intensidad 2)
	(Nombre_Deporte "baloncesto"))

([IAPractica2_Class20018] of  Deporte

	(Intensidad 1)
	(Nombre_Deporte "tenis"))

([IAPractica2_Class20019] of  Deporte

	(Intensidad 0)
	(Nombre_Deporte "taichi"))

([IAPractica2_Class22] of  Equilibrio

	(Duracion 15)
	(Intensidad 0)
	(Nombre_Ejercicio "equilibrio, caminar recto")
	(Num_repeticiones 5))

([IAPractica2_Class25] of  Flexibilidad

	(Duracion 7)
	(Intensidad 1)
	(Nombre_Ejercicio "estiramiento de tobillo")
	(Num_repeticiones 2)
	(partes_Ejercitadas
		"brazos"
		"piernas"))

([IAPractica2_Class26] of  Flexibilidad

	(Duracion 7)
	(Intensidad 1)
	(Nombre_Ejercicio "estiramiento de triceps")
	(Num_repeticiones 3)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class27] of  Flexibilidad

	(Duracion 7)
	(Intensidad 1)
	(Nombre_Ejercicio "estiramiento de muñeca")
	(Num_repeticiones 4)
	(partes_Ejercitadas "brazos"))

([IAPractica2_Class3] of  Flexibilidad

	(Duracion 7)
	(Intensidad 1)
	(Nombre_Ejercicio "rotacion de hombros")
	(Num_repeticiones 5)
	(partes_Ejercitadas "brazos"))

([IAPractica2_Class4] of  Flexibilidad

	(Duracion 7)
	(Intensidad 1)
	(Nombre_Ejercicio "estiramiento de quadriceps")
	(Num_repeticiones 6)
	(partes_Ejercitadas
		"brazos"
		"piernas"))

([IAPractica2_Class5] of  Flexibilidad

	(Duracion 7)
	(Intensidad 1)
	(Nombre_Ejercicio "rotacion doble de cadera")
	(Num_repeticiones 7)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class6] of  Flexibilidad

	(Duracion 2)
	(Intensidad 0)
	(Nombre_Ejercicio "rotacion simple de cadera")
	(Num_repeticiones 8)
	(partes_Ejercitadas "piernas"))



)

; --------------------------------------------------------------------------------------------------------------------
; ------------------------------------------------  FUNCTIONS  -------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Funciones generales


; --------------------------------------------------------------------------------------------------------------------
; ----------------------------------------------  TEMPLATES ----------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------


; --------------------------------------------------------------------------------------------------------------------
; --------------------------------------------------  MAIN  ----------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------

(defmodule MAIN (export ?ALL))


	

(defrule MAIN::initial_rule "Regla inicial"
	?f <-(initial-fact)
	?ej <- (object (is-a Ejercicio)(Intensidad ?valor))
	=>
	(printout t "SCB de rutinas" crlf)
	(printout t "por Sergi Aragall, Alberto Camacho y Albert Teira" crlf)
	(assert (newRutine))
	(retract ?f) 
	(focus question_module)
)

(defrule MAIN::cal_reset "Estan carregades les instancies?"
	?f<-(initial-fact)
	=>
	(printout t "ERROR! falta cargar las instancias, haga (reset) antes de ejecutar el programa." crlf)
	(retract ?f)
)



; --------------------------------------------------------------------------------------------------------------------
; ---------------------------------------------  QUESTION MODULE  ----------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Definir preguntas para más adelante poder inferir


; Definimos el módulo para las preguntas
(defmodule question_module
	(import MAIN ?ALL)
	(export ?ALL)
)

;------------------------ FUNCIONES PREGUNTA --------------------

;pregunta y comprueba que el valor devuelto sea uno entre los puestos
(deffunction question_module::ask-question (?question $?allowed-values)
   (printout t ?question crlf)
   (bind ?answer (read))
   (if (lexemep ?answer)
       then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t "ERROR, INTRODUZCA UN VALOR CORRECTO:  " ?question crlf)
      (bind ?answer (read)))
      (if (lexemep ?answer)
          then (bind ?answer (lowcase ?answer)))
   ?answer)

;pregunta y comprueba si el resultado es si o no
(deffunction question_module::yes-or-no-p (?question)
   (bind ?response (ask-question ?question si no s n SI NO S N Y y yes YES))
   (if (or (eq ?response no) (eq ?response n) (eq ?response N) (eq ?response NO))
       then FALSE
       else TRUE))

;pregunta y comprueba que el valor devuelto este entre el rango
(deffunction question_module::pregunta-numerica (?pregunta ?rangini ?rangfi)
	(printout t ?pregunta "[" ?rangini "," ?rangfi "]" crlf)
	(bind ?respuesta (read))
	
	(while (or (not(numberp ?respuesta))(not(and(>= ?respuesta ?rangini)(<= ?respuesta ?rangfi)))) do		
		(printout t "ERROR, INTRODUZCA UN VALOR CONTENIDO EN EL INTERVALO: " ?pregunta "[" ?rangini "," ?rangfi "]" crlf)
		(bind ?respuesta (read))
	)
	?respuesta
)


;------------------------ FUNCIONES AUXILIARES --------------------


		
		
;-------------------- DEFTEMPLATE ----------------------
		
(deftemplate question_module::edad (slot numero (type INTEGER)))

(deftemplate question_module::capacidad_fisica (slot valor (type INTEGER) (range 0 10)))

(deftemplate question_module::autocapacidad (slot valor (type INTEGER) (range 0 10)))

(deftemplate question_module::enfermedad (slot numero (type INTEGER) (range 0 9)))

(deftemplate question_module::fumador (slot frequencia (type INTEGER) (range 1 10)))


(deftemplate question_module::depresion (slot nivel (type INTEGER) (range 0 2)))


(deftemplate question_module::coeficiente (slot coef (type FLOAT) (range 0.0 1.0)))

(deftemplate question_module::nivel_depresion (slot nivel (type FLOAT)))


(deftemplate question_module::res (slot nivel (type INTEGER) (range 0 3)))
(deftemplate question_module::fuer (slot nivel (type INTEGER) (range 0 3)))
(deftemplate question_module::eq (slot nivel (type INTEGER) (range 0 2)))
(deftemplate question_module::cal (slot nivel (type INTEGER) (range 0 3)))



;------------ RULES ------------------------------
	
	; EDAD
	(defrule question_module::pregunta_edad
		(declare (salience 10))
		(newRutine)
		=>
    	(bind ?f (pregunta-numerica "Indique cual es su edad" 0 100) )
		(assert (edad (numero ?f)))
		(assert (depresion (nivel 0)))
        )
		
	(defrule question_module::es_menor
		(declare (salience 10))
		(edad (numero ?f))
		=>
		(if (< ?f 65) then
                        (printout t "Vaya, parece que no esta dentro de la franja de edad necesaria para usar la aplicacion, \ 
				pero no tema, ya tendra tiempo para ser viejo; por ahora disfrute de los resquizos de juventud que le quedan" crlf)
			(pop-focus)
        ))
		
    ; ESTADO CIVIL
	(defrule question_module::estado_civil
                (declare (salience 10))
                (newRutine)
                =>
                (bind ?f (pregunta-numerica "Indique su estado civil:\
												    1-> Soltero/a\
												    2-> Casado/a\
												    3-> Viudo/a\
												    4-> Divorciado/a" 1 4))
                (if (= ?f 2) then 
                    (assert (casado))
                 else
                    (assert (pot-dep)))
        )	

    ;VALORACION CAPACIDAD FISICA
    (defrule question_module::valoracion_fisica
        (declare (salience 10))
        (newRutine)
         =>
        (bind ?f (pregunta-numerica "Como valoraria su capacidad fisica actual, respecto a la de unos anos atras?" 0 10 ))
        (assert (autocapacidad (valor ?f)))
    )
	
	(defrule question_module::tiene_dep
        (declare (salience 10))
        (newRutine)
        (autocapacidad(valor ?v))
		(test (<= ?v 5))
		(not (pot-dep))
        => 
        (assert (pot-dep))
    )
	
	(defrule question_module::tiene_dep_aux
        (declare (salience 10))
        (newRutine)
        (autocapacidad(valor ?v))
		(test (<= ?v 5))
        => 
         (assert (pregunta-caida))             
    )
	
	
	(defrule question_module::caidas
		(declare (salience 11))
		(newRutine)
        ?f<-(pregunta-caida)
		=>
		(if (yes-or-no-p "Ha sufrido alguna caida recientemente? [SI/NO]") then
			(assert (caida))
		)
        (retract ?f)
	)
		
	;REALIZA EJERCICIO
	(defrule question_module::realiza_ejercicio
		(declare (salience 10))
		(newRutine)
		=>
	    (bind ?f (pregunta-numerica "Con que frecuencia realiza ejercicio? " 0 10))
		(assert (capacidad_fisica (valor ?f)))

	)
        (defrule question_module::tipo-ejercicio
            (declare (salience 11))
            (newRutine)
            (capacidad_fisica (valor ?f))
            =>
            (if (< 0 ?f) then
                (if (yes-or-no-p "Realiza ejercicios de resistencia? [SI/NO]") then
                    (assert(fa-resistencia))
                )
                (if (yes-or-no-p "Realiza ejercicios de fuerza? [SI/NO]") then
                    (assert (fa-fuerza))))
        )
	
        ;MEDICAMENTOS
        (defrule question_module::toma_medicamento
                (declare (salience 10))
                (newRutine)
                =>
                (if (yes-or-no-p "Toma algun tipo de medicamento (pastillas para dormir, antihistaminicos, analgesicos...)  [SI/NO]") then
                    (assert (medicamento)))
        )

        ;FUMADOR
        (defrule question_module::es_fumador
            (declare (salience 10))
            (newRutine)
            =>
            (if (yes-or-no-p "Es usted fumador? [SI/NO]") then
                (assert (Fuma))
        ))
        (defrule question_module::frequencia_fuma
                (declare (salience 11))
                (newRutine)
                ?f<-(Fuma)
                => 
                (bind ?p (pregunta-numerica "Con que frecuencia fuma?" 1 10))
                (assert (fumador (frequencia ?p)))
                (retract ?f)
        )
        
       
        ;CONDICION FISICA A DESTACAR   
        (defrule question_module::condicion_fisica
            (declare (salience 10))
            (newRutine)
            =>
            (if (yes-or-no-p "Padece alguna condicion fisica a destacar?") then
                (bind ?f (pregunta-numerica "Tiene problemas al ejercer los musculos relacionados con la pierna o el brazo? \
                            0 -> No\
                            1 -> Brazo\
                            2 -> Pierna
                            3 -> Brazo y pierna" 0 3))
                (if (= 1 ?f) then (assert (no-usa-brazo)))
                (if (= 2 ?f) then (assert (no-usa-pierna)))
                (if (= 3 ?f) then 
                    (assert (no-usa-brazo))
                    (assert (no-usa-pierna))
                )
                (if (yes-or-no-p "Realiza reabilitacion?") then
                    (bind ?p (pregunta-numerica "De alguna/s de estas partes:\
                            0 -> Brazo\
                            1 -> Pierna\
                            2 -> Brazo y pierna" 0 2))
                    (if (= 1 ?f) then (assert (reabilita-brazo)))
                    (if (= 2 ?f) then (assert (reabilita-pierna)))
                    (if (= 3 ?f) then 
                        (assert (reabilita-brazo))
                        (assert (reabilita-pierna))
                    )      
                )
            )
        )
	
	;ENFERMEDADES
	(defrule question_module::question-enfermedad
		(declare (salience 10))
		(newRutine)
		=>
		(bind ?f (pregunta-numerica "Padece alguna de las siguientes enfermedades?\
                        0-> Ninguna\
                        1-> Enfermedad Cardiovascular\
                        2-> Hipertension\
                        3-> Sobrepeso u obesidad\
                        4-> Diabetes tipo 2\
                        5-> Enfermedad pulmonar obstructiva cronica\
                        6-> Osteoporosis\
                        7-> Cancer\
                        8-> Artritis rematoide\
                        9-> Filerosis quistica" 0 9))
			(assert (enfermedad (numero ?f)))
	)
   
    (defrule question_module::enfermedad-causa-dep
        (declare (salience 10))
        (newRutine)
        (enfermedad(numero ?f))
		(not (pot-dep))
        =>
        (if (not(= 0 ?f)) then
            (assert (pot-dep)))
    )
	
	(defrule question_module::enfermedad-cardiovascular
        (declare (salience 10))
        (newRutine)
        (enfermedad(numero 1))
        =>
        (if (yes-or-no-p "Ha tenido alguna angina de pecho o infarto? [SI/NO]") then
                (assert (infarto))
        )
    )	
		

        
		
	
	
	
        ;TEST DE DEPRESION
        (defrule question_module::quiere_test_dep
            (declare (salience 9))
            (newRutine)
            (pot-dep)
            =>
            (if (yes-or-no-p "Para generar un horario de sesiones mas acorde a sus necesidades y/o habilidades es recomendable realizar un test de depresion.\
      Desea realizarlo? [SI/NO]") then
                (assert (realizar-test))
            )
        )
        
        
        (defrule question_module::test_dep
            (declare (salience 9))
            (newRutine)
            ?f<-(realizar-test)
            =>
                (printout t "Responda sinceramente a las siguientes preguntas:")
                (bind ?p1 (pregunta-numerica "Cual es su estado de animo?\
                                            1 -> Por lo general bastante positivo\
                                            2 -> Depende el dia\
                                            3 -> Ultimamente me siento triste o melancolico" 1 3))
                (bind ?p2 (pregunta-numerica "Como es su dia a dia?\
                                            1 -> Esta lleno de anecdotas; buenas y malas\
                                            2 -> No son muy interesantes\
                                            3 -> Me aburre, se me hace repetitivo" 1 3))
                (bind ?p3 (pregunta-numerica "Cuando miro hacia atras y pienso en las decisiones que he tomado...\
                                            1 -> No me arrepiento de como he actuado estos años. Si me he equivocado ese error me valdra para aprender\
                                            2 -> No todo lo que he hecho ha sido correcto, algunas cosas me gustaria cambiarlas
                                            3 -> Estoy arrepentido de gran parte de lo que he hecho" 1 3))    
                (bind ?p4 (pregunta-numerica "Cuando me comparo con otras personas...\
                                            1 -> Nunca me comparo con la gente, no me hace falta
                                            2 -> A veces gano yo, otras ganan ellos
                                            3 -> Siempre salgo mal parado, la mayoria de gente es mejor que yo" 1 3))
                (bind ?p5 (pregunta-numerica "Respecto al apetito...\
                                            1 -> Cada dia tengo mas hambre, sobretodo de comerme el mundo\ 
                                            2 -> He variado un poco, como mas o no como casi nada, depende el dia\
                                            3 -> No me apetece comer, lo hago por obligacion"1 3))
                (printout t "Gracias!")
                (bind ?sum (+ ?p1 ?p2 ?p3 ?p4 ?p5))
                (bind ?sum (/ ?sum 5))
                (assert (nivel_depresion(nivel ?sum)))
                (retract ?f)
        )
        (defrule question_module::calcular_nivel_dep
            (declare (salience 9))
            (newRutine)
            ?f<-(nivel_depresion(nivel ?v))
            =>
            (if (<= ?v 1.5) then (assert (depresion (nivel 2))))
            (if (and (> ?v 1.4) (<= ?v 2.0)) then (assert (depresion(nivel 1))))
            (if (> ?v 2.0) then (assert (depresion (nivel 0))))
            (retract ?f)
        )



; PASAR MODULO INFERENCIA
(defrule question_module::end_questions
	(declare (salience 0))
	(newRutine)
	=>
	(assert (conclusions))
	(focus inference_module)
)

; --------------------------------------------------------------------------------------------------------------------
; ---------------------------------------------  INFERENCE MODULE  ---------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Módulo para hacer la inferencia de datos según las respuestas a las preguntas


; Definimos el módulo para la inferencia de datos
(defmodule inference_module
	(import MAIN ?ALL)
    (import question_module ?ALL)
    (export ?ALL)
)


(deftemplate inference_module::sesiones (slot numero (type INTEGER) (range 3 7)))
(deftemplate inference_module::sesion (slot num (type INTEGER) (range 1 7)))
(deftemplate inference_module::sesion_aux (slot num (type INTEGER) (range 1 7)))

(deftemplate inference_module::rand (slot num (type INTEGER) ))

(deftemplate inference_module::fuer_aux (slot nivel (type INTEGER) ))
(deftemplate inference_module::eq_aux (slot nivel (type INTEGER) ))
(deftemplate inference_module::cal_aux (slot nivel (type INTEGER) ))
(deftemplate inference_module::res_aux (slot nivel (type INTEGER) ))

(deftemplate inference_module::num_fuerza (slot num (type INTEGER) ))
(deftemplate inference_module::num_eq (slot num (type INTEGER) ))
(deftemplate inference_module::num_cal (slot num (type INTEGER) ))




;Retornar un multislot sense els elements que exerciten les cames
(deffunction inference_module::no_piernas ($?allowed-values)

)



(deffunction inference_module::programaSesion (?s $?allowed-values)
	(make-instance (gensym) of Session (Dia ?s) (Ejercicios $?allowed-values))
)		



;DEFINICION COEFICIENTE
(defrule inference_module::definir_coeficiente
	(declare (salience 10))
	(conclusions)
	?f <-(edad (numero ?n))
	?g <- (capacidad_fisica (valor ?n2))
	?h <- (autocapacidad (valor ?n3))
	=>
	(bind ?aux (- 100 ?n))	(bind ?aux2 (/ ?aux 100))	(bind ?aux3 (* ?aux2 0.4))
	(bind ?aux4 (/ ?n2 10))	(bind ?aux5 (* ?aux4 0.3))
	(bind ?aux7 (/ ?n3 10))	(bind ?aux8 (* ?aux7 0.3))
	(bind ?aux9 (+ ?aux3 ?aux5 ?aux8))
	(assert (coeficiente (coef ?aux9)))
	(retract ?f) (retract ?g) (retract ?h)
)

(defrule inference_module::coef_fa_fuerza
	(declare (salience 10))
	(conclusions)
	?r <- (fa-fuerza)
	?f <- (coeficiente (coef ?v))
	=>
	(modify ?f (coef (+ ?v 0.05)))
	(retract ?r)
	(printout t "VALOR ES " ?v crlf)
)

(defrule inference_module::coef_fa_resistencia
	(declare (salience 10))
	(conclusions)
	?r <- (fa-resistencia)
	?f <- (coeficiente (coef ?v))
	=>
	(modify ?f (coef (+ ?v 0.05)))
	(retract ?r)
	(printout t "VALOR ES " ?v crlf)
)



;AGREGACION TIPOS DE EJERCICIOS
(defrule inference_module::otros_tipos
	(declare (salience 10))
	(conclusions)
	=>
	(assert (res (nivel 1)))
	(assert (fuer (nivel 1)))
	(assert (cal (nivel 1)))
)

;EJERCICIOS DE EQUILIBRIO EN FUNCION DE LA ENFERMEDAD
(defrule inference_module::probables_equilibrio
	(declare (salience 10))
	(conclusions)
	?f<- (enfermedad (numero ?e))
	(not (eq (nivel ?n)))
	=>
	(if (= ?e 2) then	(assert (eq (nivel 1))))
	(if (= ?e 6) then	(assert (eq (nivel 1))))
	(if (= ?e 8) then	(assert (eq (nivel 1))))
	(if (and (and (not(= ?e 2)) (not(= ?e 6))) (not(= ?e 8))) then (assert (eq (nivel 0))))
	(retract ?f)
)

;SI TIENE PROBLEMAS CON LAS PIERNAS, NO REALIZAR EJERCICIOS DE RESISTENCIA Y CALENTAMIENTO
(defrule inference_module::sinpierna
	(declare (salience 10))
	(conclusions)
	(or (no_usar_pierna) (rehabilitar_pierna))
	?s <- (res (nivel ?n))
	?f <- (cal (nivel ?m))
	=>
	(modify ?s (nivel (- 1 ?n)))
	(modify ?f (nivel (- 1 ?m)))
)



;DEFINIR EL NUMERO DE SESIONES (DE 3 A 7)
(defrule inference_module::def_num_ses
	(declare (salience 10))
	(conclusions)
	(res (nivel ?n))
	(coeficiente (coef ?c))
	=>
	(if (< ?c 0.4) then (assert (sesiones (numero 3))))
	(if (and (>= ?c 0.4) (< ?c 0.5)) then (assert (sesiones (numero 4))))
	(if (and (>= ?c 0.5) (< ?c 0.6)) then (assert (sesiones (numero 5))))
	(if (and (>= ?c 0.6) (< ?c 0.7)) then (assert (sesiones (numero 6))))
	(if (>= ?c 0.7) then (assert (sesiones (numero 7))))
)

(defrule inference_module::def_num_ses_sin_res
	(declare (salience 10))
	(conclusions)
	(not (res (nivel ?n)))
	=>
	(assert (sesiones (numero 3)))	
)

;ASIGNAR UN NIVELL DE RESISTENCIA EN FUNCIO DEL COEFICIENT CALCULAT
(defrule inference_module::asignar_res
	(declare (salience 10))
	(conclusions)
	(coeficiente (coef ?c))
	?r<-(res (nivel ?n))
	(depresion (nivel ?v))
	(test (= ?n 1))
	=>
	(if (and (not (= ?v 2)) (and (>= ?c 0.4) (< ?c 0.6))) then (modify ?r (nivel (+ 1 ?n))))
	(if (and (= ?v 0) (>= ?c 0.6)) then (modify ?r (nivel (+ 2 ?n))))
)

;ASIGNAR UN NIVELL DE CALENTAMENT EN FUNCIO DEL COEFICIENT CALCULAT
(defrule inference_module::asignar_cal
	(declare (salience 10))
	(conclusions)
	(coeficiente (coef ?c))
	?r<-(cal (nivel ?n))
	(depresion (nivel ?v))
	(test (= ?n 1))
	=>
	(if (and (not (= ?v 2)) (and (>= ?c 0.4) (< ?c 0.6))) then (modify ?r (nivel (+ 1 ?n))))
	(if (and (= ?v 0) (>= ?c 0.6)) then (modify ?r (nivel (+ 2 ?n))))
)

;ASIGNAR UN NIVELL DE FORÇA EN FUNCIO DEL COEFICIENT CALCULAT
(defrule inference_module::asignar_fuer
	(declare (salience 10))
	(conclusions)
	(coeficiente (coef ?c))
	?r<-(fuer (nivel ?n))
	(test (= ?n 1))
	=>
	(if (and (>= ?c 0.35) (< ?c 0.55)) then (modify ?r (nivel (+ 1 ?n))))
	(if (>= ?c 0.55) then (modify ?r (nivel (+ 2 ?n))))
)

;NO RESISTENCIA AL MÀXIM SI HA TINGUT UN INFART O PREN ALGUN MEDICAMENT RELAXANT
(defrule inference_module::trat_med
	(declare (salience 10))
	(conclusions)
	(or (infarto) (medicamento))
	?r<-(res (nivel ?n))
	=>
	(if (= ?n 3) then (modify ?r (nivel (- ?n 1))))
)

;EN FUNCIO DE LA FRECUENCIA AMB LA QUE FUMI, REDUIR EL NIVELL DELS EXERCICIS DE RESISTENCIA
(defrule inference_module::trat_fumar
	(declare (salience 10))
	(conclusions)
	(fumador (frequencia ?f))
	?r<-(res (nivel ?n))
	=>
	(if (and (= ?n 3)(and (>= ?f 3) (< ?f 7))) then (modify ?r (nivel (- 1 ?n))))
	(if (and (= ?n 3)(>= ?f 7)) then (modify ?r (nivel (- 2 ?n))))
	(if (and (= ?n 2)(>= ?f 7)) then (modify ?r (nivel (- 1 ?n))))
)

;SI NO POT USAR EL BRAÇ, RESTAR UN NIVELL ALS EXERCICIS DE FORÇA
(defrule inference_module::trat_no_brazo
	(declare (salience 10))
	(conclusions)
	(or (no_usar_brazo) (rehabilitar_brazo))
	?r<-(fuer (nivel ?n))
	=>
	(if (not (= ?n 1)) then (modify ?r (nivel (- 1 ?n))))
)




; CREAR INSTANCIES DE LES SESIONS
(defrule inference_module::planificar_sesiones
	(declare (salience 10))
	(conclusions)
	?f <-(sesiones (numero ?n))
	=>
	(if (> ?n 6) then (assert (sesion (num 7))))
	(assert (sesion (num 6)))
	(if (> ?n 5) then (assert (sesion (num 5))))
	(assert (sesion (num 4)))
	(if (> ?n 4) then (assert (sesion (num 3))))
	(assert (sesion (num 2)))
	(if (> ?n 3) then (assert (sesion (num 1))))
	(retract ?f)
)

(defrule inference_module::la_sesion
	(declare (salience 10))
	(conclusions)
	?f <- (fuer (nivel ?n1))
	?g <- (res (nivel ?n2))
	?h <- (cal (nivel ?n3))
	?j <- (eq (nivel ?n4))
	?d <- (sesion (num ?n5))
	(not (fuer_aux (nivel ?n1)))	
	(not (res_aux (nivel ?n2)))	
	(not (cal_aux (nivel ?n3)))	
	(not (eq_aux (nivel ?n4)))
	(not (sucio))
	=>
	(printout t "SESIO  " ?n5 crlf)
	(assert (sesion_aux (num ?n5)))
	(assert (sesion_aux (num ?n5)))
	(assert (fuer_aux (nivel ?n1)))	
	(assert (res_aux (nivel ?n2)))	
	(assert (cal_aux (nivel ?n3)))	
	(assert (eq_aux (nivel ?n4)))	
)

;EXERCICIS DE FORÇA A FER EN UNA SESSIO EN FUNCIO DEL NIVELL
(defrule inference_module::definir_ejs_fuerza
	(declare (salience 10))
	(conclusions)
	?f <- (fuer_aux (nivel ?n))
	=>
	(if (= ?n 0) then	(assert (num_fuerza (num 0))))
	(if (= ?n 1) then	(assert (num_fuerza (num 4))))
	(if (= ?n 2) then	(assert (num_fuerza (num 6))))
	(if (= ?n 3) then	(assert (num_fuerza (num 8))))
	(retract ?f)
	(assert (rand (num (random 0 10))))
)

(defrule inference_module::definir_ejs_fuerza_aux
	(declare (salience 10))
	(conclusions)
	?g <- (num_fuerza (num ?n))
	?t <- (rand (num ?n2))
	?f <- (object (is-a Fuerza) (Num_repeticiones ?n2))
	(test (not(= ?n 0)))
	=>
	(bind ?dur (send ?f get-Duracion))
	(if (not(= ?dur 22)) then
		(send ?f put-Duracion 22)
		(modify ?g (num (- ?n 1)))
	)
	(retract ?t)
	(assert (rand (num (random 0 10))))
)

;EXERCICIS DE EQUILIBRI A FER EN UNA SESSIO EN FUNCIO DEL NIVELL
(defrule inference_module::definir_ejs_eq
	(declare (salience 9))
	(conclusions)
	?f <- (eq_aux (nivel ?n))
	?t <- (rand (num ?n2))
	=>
	(if (= ?n 0) then 	(assert (num_eq (num 0))))
	(if (= ?n 1) then	(assert (num_eq (num 4))))
	(if (= ?n 2) then	(assert (num_eq (num 6))))
	(retract ?f)
	(retract ?t)
	(assert (rand (num (random 0 5))))
)

(defrule inference_module::definir_ejs_eq_aux
	(declare (salience 9))
	(conclusions)
	?g <- (num_eq (num ?n))
	?t <- (rand (num ?n2))
	?f <- (object (is-a Equilibrio) (Num_repeticiones ?n2))
	(test (not(= ?n 0)))
	=>
	(bind ?dur (send ?f get-Duracion))
	(if (not(= ?dur 22)) then
		(send ?f put-Duracion 22)
		(modify ?g (num (- ?n 1)))
	)
	(retract ?t)
	(assert (rand (num (random 0 5))))
)

;EXERCICIS DE CALENTAMENT A FER EN UNA SESSIO EN FUNCIO DEL NIVELL
(defrule inference_module::definir_ejs_cal
	(declare (salience 8))
	(conclusions)
	?f <- (cal_aux (nivel ?n))
	?t <- (rand (num ?n2))
	=>
	(if (= ?n 0) then	(assert (num_cal (num 0))))
	(if (= ?n 1) then	(assert (num_cal (num 5))))
	(if (= ?n 2) then	(assert (num_cal (num 7))))
	(if (= ?n 3) then	(assert (num_cal (num 9))))
	(retract ?f)
	(retract ?t)
	(assert (rand (num (random 0 8))))
)

(defrule inference_module::definir_ejs_cal_aux
	(declare (salience 8))
	(conclusions)
	?g <- (num_cal (num ?n))
	?t <- (rand (num ?n2))
	?f <- (object (is-a Flexibilidad) (Num_repeticiones ?n2))
	(test (not(= ?n 0)))
	=>
	(bind ?dur (send ?f get-Duracion))
	(if (not(= ?dur 22)) then
		(send ?f put-Duracion 22)
		(modify ?g (num (- ?n 1)))
	)
	(retract ?t)
	(assert (rand (num (random 0 8))))
)

;GENERADOR D'INSTANCIES DE SESIO
(defrule inference_module::planificar_sesion
	(declare (salience 7))
	(conclusions)
	?g <- (res_aux (nivel ?n1))
	?r <- (sesion_aux (num ?var))
	?f <-(sesion (num ?numero))
	(test (= ?var ?numero))
	?a <- (num_cal (num ?n))
	?b <- (num_eq (num ?n2))
	?c <- (num_fuerza (num ?n3))
	=>
	(if (not(= ?n1 0)) then
		(bind $?aer (find-instance ((?inst Aerobico)) (= ?inst:Intensidad 0)))
	)
	(bind $?aux1 $?aer)
	(bind $?fuer (find-all-instances ((?inst Fuerza)) (= ?inst:Duracion 22) ))
	(bind $?equi (find-all-instances ((?inst Equilibrio)) (= ?inst:Duracion 22) ))
	(bind $?cal (find-all-instances ((?inst Flexibilidad)) (= ?inst:Duracion 22) ))

	(programaSesion ?numero $?aux1 $?fuer $?equi $?cal)
	(assert (sucio))
	(retract ?f)
	(retract ?r)
	(retract ?g)
	(retract ?a)
	(retract ?b)
	(retract ?c)
)

;NETEJAR APUNTADORS A INSTANCIES A EMPLEAR
(defrule inference_module::limpiar
	(declare (salience 10))
	(conclusions)
	(sucio)
	?f <- (object (is-a Ejercicio) (Duracion 22))
	=>
	(send ?f put-Duracion 15)
)

(defrule inference_module::esta_limpio
	(declare (salience 10))
	(conclusions)
	?f <-(sucio)
	=>
	(retract ?f)
)


;OUPUT_MODULE
(defrule inference_module::finalizarAnalisis
	(declare (salience 0))
	(conclusions)
	=>
	(assert (escribir))
	(focus output_module)	
)
	
	
	
	
; --------------------------------------------------------------------------------------------------------------------
; ---------------------------------------------  OUTPUT MODULE  ---------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
	
(defmodule output_module
    (import MAIN ?ALL)
    (import question_module ?ALL)
	(import inference_module ?ALL)
    (export ?ALL)
)

(defrule output_module::general
		(declare (salience 110))
		(escribir)
		=>
		(printout t "Este es el diario de sesiones asociado a su diagnostico: " crlf)
)

; FUNCIONES

(deffunction output_module::function-indicaciones-ejercicios (?ses) "imprime indicaciones para cada uno de los ejercicios"
)

(deffunction output_module::function-imprimir-sesion (?ses) "imprime la informacion de la sesion pasada por parametro"
	(printout t "|--------------------------------------------------------------------------------------------------------------------------------------|" crlf)
	(printout t "|                                                              Dia " (send ?ses get-Dia)
	"                                                                 |" crlf)
	(printout t "|--------------------------------------------------------------------------------------------------------------------------------------|" crlf)
	(printout t "|                   EJERCICIO                   |              TIPO              |        DURACION        |        REPETICIONES        |" crlf)
	(printout t "|--------------------------------------------------------------------------------------------------------------------------------------|" crlf)
	(bind ?i 1)
	(while (<= ?i (length$ (send ?ses get-Ejercicios))) do
		(bind ?ejercicio (nth$ ?i (send ?ses get-Ejercicios)))
		(printout t "|           " (send ?ejercicio get-Nombre_Ejercicio))
		(loop-for-count (?z 1 (- 48 (str-length (str-cat (send ?ejercicio get-Nombre_Ejercicio))))) do (printout t " ")) ; imprime espacios hasta TIPO
		(printout t (class ?ejercicio))
		(loop-for-count (?z 1 (- 34 (str-length (str-cat (class ?ejercicio)j)))) do (printout t " ")) ; imprime espacios hasta DURACION
		(printout t (send ?ejercicio get-Duracion))
		(loop-for-count (?z 1 (- 26 (str-length (str-cat (send ?ejercicio get-Duracion))))) do (printout t " ")) ; imprime espacios hasta REPETICIONES
		(printout t (send ?ejercicio get-Num_repeticiones))
		(loop-for-count (?z 1 (- 16 (str-length (str-cat (send ?ejercicio get-Num_repeticiones))))) do (printout t " ")) ; imprime espacios hasta el final
		(printout t "|" crlf)
		(bind ?i (+ ?i 1))
	)
)

; REGLAS

(defrule output_module::imprimir-cabecera
	(declare (salience 100))
	(escribir)
	=>
	(printout t "|--------------------------------------------------------------------------------------------------------------------------------------|" crlf)
	(printout t "|                                                                                                                                      |" crlf)
	(printout t "|                                                         CONJUNTO DE SESIONES                                                         |" crlf)
	(printout t "|                                                                                                                                      |" crlf)
	(assert (print-sesiones))
)

(defrule output_module::imprimir-sesiones
	(declare (salience 90))
	(escribir)
	(print-sesiones)
	=>
	(bind $?conjuntoSesiones (find-all-instances ((?ses Session))TRUE))
	(loop-for-count (?i 1 (length$ ?conjuntoSesiones)) do
		(bind ?aux (nth$ ?i ?conjuntoSesiones))
			(function-imprimir-sesion ?aux)
	)
	(printout t "|--------------------------------------------------------------------------------------------------------------------------------------|" crlf)
	(assert (print-indicaciones))
)

(defrule output_module::imprimir-indicaciones-ejercicios
	(declare (salience 80))
	(escribir)
	(print-indicaciones)
	=>

)

(defrule output_module::imprimir-comentarios-adicionales
	(declare (salience 70))
	=>
)








;(deffunction output_module::imprimir_ejercicios (?sesion)
;	(bind ?i 1)
;	(while (<= ?i (length$ (send ?sesion get-Ejercicios)))
;	do
;		(bind ?ejercicio (nth$ ?i (send ?sesion get-Ejercicios)))
;		(printout t " - Realizaremos el ejercicio " (send ?ejercicio get-Nombre_Ejercicio) " durante un tiempo de " (send ?ejercicio get-Duracion) " minutos." crlf)
;		(bind ?i (+ ?i 1))
; )
;)


;(defrule output_module::comentarColesterol
;		(declare (salience 10))
;		(escribir)
;		?f <- (colesterol (nivel ?lev))
;		=>
;		(if (= ?lev 1) then (printout t "Tenga precaucion con los ejercicios de resistencia y no lleve su cuerpo al maximo." crlf))
;		(if (= ?lev 2) then (printout t "No se preocupe por realizar al completo los ejercicios de resistencia, tenga cuidado y no haga esfuerzos excesivos." crlf))
;		(if (= ?lev 3) then (printout t "Evite los esfuerzos excesivos, no llegue a un nivel de cansancio elevado; nosotros nos hemos preocupado de recomendarle ejercicios aptos para usted." crlf))
;		(retract ?f)
;		)
		
;(defrule output_module::sacarPantalla
;	(declare (salience 10))
;	(escribir)
;	?f<-(imprimir_sesion (num ?n))
;	?fc <- (object (is-a Session) (Dia ?n) (Ejercicios $?e))
;	=>
;	(printout t "EJERCICIOS PARA EL DIA " ?n ":" crlf)
;	(imprimir_ejercicios ?fc)
;	(retract ?f)
;)
	
;(defrule output_module::cuidadoCaidas
;	(declare (salience 10))
;	(escribir)
;	?f<-(caida)
;	=>
;	(printout t "Se recomienda realizar los ejercicios con cuidado, focalizandose en los ejercicios de equilibrio. " crlf)
;	(retract ?f)
;	)
	
	
	
	
	
	
	

