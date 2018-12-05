; --------------------------------------------------------------------------------------------------------------------
; -----------------------------------------------  CLASSES  ----------------------------------------------------------
; --------------------------------------------------------------------------------------------------------------------
; Clases definidas en la ontologÃ­a (exportar de CLIPS)

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
; Instancias (de CLIPS tambiÃ©n)

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
	(Nombre_Ejercicio "estiramiento de muÃ±eca")
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
; Definir preguntas para mÃ¡s adelante poder inferir


; Definimos el mÃ³dulo para las preguntas
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

	;PREGUNTA EDAD
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
		
        ;ESTADO CIVIL
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
                    (assert (Casado))
                 else
                    (assert (pot-dep)))
        )	

        ;VALORACION CAPACIDAD FISICA
        (defrule question_module::valoracion_fisica
            (declare (salience 10))
            (newRutine)
            =>
            (bind ?f (pregunta-numerica "Como valoraria su capacidad fisica?" 0 10 ))
            (assert (autocapacidad (valor ?f)))
        )
        (defrule question_module::tiene_dep
            (declare (salience 10))
            (newRutine)
            (autocapacidad(valor ?v))
            => 
            (if (<= ?v 5) then 
                (assert (pot-dep))
                (assert (pregunta-caida)))             
        )
		
	;REALIZA EJERCICIO
	(defrule question_module::realiza_ejercicio
		(declare (salience 10))
		(newRutine)
		=>
	        (bind ?f (pregunta-numerica "Indique la frecuencia con la que realiza ejercicio: \
                            (0 -> no realizo ningun ejercicio y 10 -> realizo ejercicio a diario con buena intensidad)" 0 10))
		(assert (capacidad_fisica (valor ?f)))

	)
        (defrule question_module::tipo-ejercicio
            (declare (salience 11))
            (newRutine)
            (capacidad_fisica (valor ?f))
            =>
            (if (< 0 ?f) then
                (if (yes-or-no-p "Realiza ejercicios de resistencia?") then
                    (assert(fa-resistencia))
                )
                (if (yes-or-no-p "Realiza ejercicios de fuerza?") then
                    (assert (fa-fuerza))))
        )
	
        ;MEDICAMENTOS
        (defrule question_module::toma_medicamento
                (declare (salience 10))
                (newRutine)
                =>
                (if (yes-or-no-p "Toma medicamentos del tipo: Pastillas para dormir, Antihistaminicos y Analgesicos? [si/no]") then
                    (assert (medicamento)))
        )

        ;FUMADOR
        (defrule question_module::es_fumador
            (declare (salience 10))
            (newRutine)
            =>
            (if (yes-or-no-p "Es usted fumador? [si/no]") then
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
		(bind ?f (pregunta-numerica "Sufre de alguna de siguientes enfermedades?\
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
            =>
            (if (= 0 ?f) then
                (assert (pot-dep)))
        )

        
		
	(defrule question_module::caidas
		(declare (salience 11))
		(newRutine)
                ?f<-(pregunta-caida)
		=>
		(if (yes-or-no-p "Ha sufrido alguna caida recientemente? [S/N]") then
			(assert (caida))
		)
               (retract ?f)
	)
        ;TEST DE DEPRESION
        (defrule question_module::quiere_test_dep
            (declare (salience 9))
            (newRutine)
            ?f<-(pot-dep)
            =>
            (if (yes-or-no-p "Para generar un horario de sessiones mas acorde a sus necesidades y/o habilidades es \   
      recomendable realizar un test de depresion.\
      Desea realizarlo?") then
                (assert (realizar-test))
            )
            (retract ?f)
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
            (if (<= ?v 1.5) then (assert (depresion(nivel 2))))
            (if (and (> ?v 1.4) (<= ?v 2.0)) then (assert (depresion(nivel 1))))
            (if (> ?v 2.0) then (assert (depresion(nivel 0))))
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
; MÃ³dulo para hacer la inferencia de datos segÃºn las preguntas

; Definimos el mÃ³dulo para la inferencia de datos
(defmodule inference_module
	(import MAIN ?ALL)
    (import question_module ?ALL)
    (export ?ALL)
)

(deftemplate inference_module::sesiones (slot numero (type INTEGER) (range 3 7)))

(deftemplate inference_module::sesion (slot num (type INTEGER) (range 1 7)))

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




;Retornar un multislot amb ?num elements de $?allowed-values aleatoris			NO VA!!!!!
(deffunction inference_module::randomSlots (?num)
	
;	(bind ?max 10)
;	(bind ?var 0)
;	(while (not(= ?var ?num))		
;		(bind ?i (random 0 ?max))
;		(bind ?aux (find-instance ((?inst Fuerza)) (not (=  ?inst:Duracion 22))))
	;	(bind ?k (send ?aux get-Duracion))
	;	(if (not (= ?k 22)) then
;			(printout t ?aux crlf)
;			(send ?aux put-Duracion 22)
;			(bind ?var (+ ?var 1))
	;	)
;	)
	
	
	
	
	
;	(bind ?max (length$ ?allowed-values))
;	(bind ?i (random 1 ?max))
;	(bind $?aux2 (nth$ ?i ?allowed-values))
;	(bind ?i (random 1 ?max))
;	(bind ?var 1)
;	(while (not(= ?var ?num))		
;		(bind ?aux (nth$ ?i ?allowed-values))
;		(bind ?straux (send ?aux get-Nombre_Ejercicio))
;		(bind ?res 1)
;		(loop-for-count (?i 1 (length$ ?aux2)) do
;			(bind ?aux3 (nth$ ?i ?aux2))
;			(bind ?straux3 (send ?aux3 get-Nombre_Ejercicio))
;			(bind ?mirar (str-compare ?straux ?straux3))
;			(if (= ?mirar 0) then (bind ?res 0))
;		)
;		(if (= ?res 1) then 
;	;				(bind $?aux2 ?aux2 ?aux)
;				(bind ?var (+ ?var 1))
;		)
; 		(bind ?i (random 1 ?max))
;	) 
;	?aux2
	
	
	
	
	
;	(while (< ?num (length$ ?allowed-values))
;		; seleccionar instancia
;		(bind ?aux (nth$ ?i ?allowed-values))
;		; eliminar una instancia de $?allowed-values
;		(printout t "Instancia " ?i "   " (length$ ?allowed-values)  crlf)
;		(slot-delete$ (instance-name ?aux) ?allowed-values ?i ?i)
;		; siguiente Ã­ndice
;		(bind ?max (length ?allowed-values))
;		(bind ?i (random 1 ?max))
;	)
;	; devolver multislot
;	?allowed-values




)



;Entran dos multislots y se devuelve multi1 con todos los elementos
(deffunction inference_module::juntarMultiSlots (?multi1 ?multi2)
	;Recorremos multi2 y vamos aÃ±adiendo cada una de sus instancias a multi1
	(loop-for-count (?i 1 (length$ ?multi2)) do
		(bind ?aux (nth$ ?i ?multi2))
			(slot-insert$ (instance-name ?aux) ?multi1 (length$ ?multi1) ?aux)
	)
	?multi1
)


(deffunction inference_module::programaSesion (?s $?allowed-values)
	(make-instance (gensym) of Session (Dia ?s) (Ejercicios $?allowed-values))
;	(switch ?s (case 1 then (make-instance sesion1 of Session (Dia ?s) (Ejercicios $?allowed-values)))
;			(case 2 then (make-instance sesion2 of Session (Dia ?s) (Ejercicios $?allowed-values)))
;			(case 3 then (make-instance sesion3 of Session (Dia ?s) (Ejercicios $?allowed-values)))	
;			(case 4 then (make-instance sesion4 of Session (Dia ?s) (Ejercicios $?allowed-values)))
;			(case 5 then (make-instance sesion5 of Session (Dia ?s) (Ejercicios $?allowed-values)))
;			(case 6 then (make-instance sesion6 of Session (Dia ?s) (Ejercicios $?allowed-values)))
;			(case 7 then (make-instance sesion7 of Session (Dia ?s) (Ejercicios $?allowed-values)))
;		)
)		

;(deffunction inference_module::randomSlot ($?allowed-values)
;	(bind ?tam (length $?allowed-values))
;	(bind ?i (random 1 ?tam))
;	(bind ?ejercicio (nth$ ?i $?allowed-values))
;	?ejercicio
;)


			
		




;(defrule inference_module::numeroSesiones
;	(declare (salience 10))
;	(conclusions)
;	(capacidad (valor ?valor))
;	=>
;	(if (= ?valor 0) then (assert (sesion (num 5))) (assert (sesion (num 3))) (assert (sesion (num 1))))
;	(if (= ?valor 1) then (assert (sesion (num 7))) (assert (sesion (num 5))) (assert (sesion (num 4))) (assert (sesion (num 3))) (assert (sesion (num 1))))
;	(if (= ?valor 2) then (assert (sesion (num 7))) (assert (sesion (num 6))) (assert (sesion (num 5))) (assert (sesion (num 4))) (assert (sesion (num 3))) (assert (sesion (num 2))) (assert (sesion (num 1))))
;	)
	
;(defrule inference_module::programarSesion
;	(declare (salience 10))
;	(conclusions)
;	?f<-(sesion (num ?s))
;	(capacidad (valor ?valor))
;	(colesterol (nivel ?nivel))
;	=>
;	(bind $?flex (find-all-instances ((?inst Flexibilidad)) (= ?inst:Intensidad ?valor)))
;	(bind $?eq (find-all-instances ((?inst Equilibrio)) (= ?inst:Intensidad ?valor)))
;	(bind $?fuer (find-all-instances ((?inst Fuerza)) (= ?inst:Intensidad ?valor)))
;	
;	(bind ?value 1)
;	(if (= ?nivel 0) then (bind ?value ?valor))
;	(if (= ?nivel 1) then (if (< ?valor 2) then (bind ?value ?valor)))
;	(if (= ?nivel 2) then (bind ?value 0))
;	
;	(if (= ?nivel 3) then
;		(programaSesion ?s (randomSlot $?flex) (randomSlot $?eq) (randomSlot $?fuer))	
;	 else 
;		(bind $?res (find-all-instances ((?inst Aerobico)) (= ?inst:Intensidad ?value)))
;		(programaSesion ?s (randomSlot $?flex) (randomSlot $?eq) (randomSlot $?fuer) (randomSlot $?res))
;	)
;	
;	(retract ?f)
;	(assert (imprimir_sesion (num ?s)))
;	)




;DEFINIR ESTADO CUALITATIVO

(defrule inference_module::definir_coeficiente
	(declare (salience 10))
	(conclusions)
	?f <-(edad (numero ?n))
	?g <- (capacidad_fisica (valor ?n2))
	?h <- (autocapacidad (valor ?n3))
	=>
	(bind ?aux (- 100 ?n))
	(bind ?aux2 (/ ?aux 100))
	(bind ?aux3 (* ?aux2 0.4))
	
	(bind ?aux4 (/ ?n2 10))
	(bind ?aux5 (* ?aux4 0.3))
	
	(bind ?aux6 (- 10 ?n3))
	(bind ?aux7 (/ ?aux6 10))
	(bind ?aux8 (* ?aux7 0.3))
	
	(bind ?aux9 (+ ?aux3 ?aux5 ?aux8))
	(assert (coeficiente (coef ?aux9)))
	
	(retract ?f)
	(retract ?g)
	(retract ?h)
)


(defrule inference_module::otros_tipos
	(declare (salience 10))
	(conclusions)
	=>
	(assert (res (nivel 1)))
	(assert (fuer (nivel 1)))
	(assert (cal (nivel 1)))
)


(defrule inference_module::probables_equilibrio
	(declare (salience 10))
	(conclusions)
	?f<- (enfermedad (numero ?e))
	(not (eq (nivel ?n)))
	=>
	(if (= ?e 2) then	(assert (eq (nivel 1))))
	(if (= ?e 6) then	(assert (eq (nivel 1))))
	(if (= ?e 8) then	(assert (eq (nivel 1))))
	(retract ?f)
)


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


;DEFINIR ESTADO CUANTITATIVO

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

(defrule inference_module::trat_med
	(declare (salience 10))
	(conclusions)
	(or (infarto) (medicamento))
	?r<-(res (nivel ?n))
	=>
	(if (= ?n 3) then (modify ?r (nivel (- 1 ?n))))
)

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

(defrule inference_module::trat_no_brazo
	(declare (salience 10))
	(conclusions)
	(or (no_usar_brazo) (rehabilitar_brazo))
	?r<-(fuer (nivel ?n))
	=>
	(if (not (= ?n 1)) then (modify ?r (nivel (- 1 ?n))))
)


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
	=>
	(printout t "SESIO  " ?n5 crlf)
	(assert (fuer_aux (nivel ?n1)))	
	(assert (res_aux (nivel ?n2)))	
	(assert (cal_aux (nivel ?n3)))	
	(assert (eq_aux (nivel ?n4)))	

)


(defrule inference_module::definir_ejs_fuerza
	(declare (salience 10))
	(conclusions)
	?f <- (fuer_aux (nivel ?n))
	=>
	(printout t "SOY NIVEL DE FUERZA " ?n crlf)
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



(defrule inference_module::definir_ejs_eq
	(declare (salience 9))
	(conclusions)
	?f <- (eq_aux (nivel ?n))
	?t <- (rand (num ?n2))
	=>
	(printout t "SOY NIVEL DE EQUILIBRIO " ?n crlf)
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




(defrule inference_module::definir_ejs_cal
	(declare (salience 8))
	(conclusions)
	?f <- (cal_aux (nivel ?n))
	?t <- (rand (num ?n2))
	=>
	(printout t "SOY NIVEL DE CAL " ?n crlf)
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


(defrule inference_module::planificar_sesion
	(declare (salience 7))
	(conclusions)
	?g <- (res_aux (nivel ?n1))
	?f <-(sesion (num ?numero))
	?a <- (num_cal (num ?n))
	?b <- (num_eq (num ?n2))
	?c <- (num_fuerza (num ?n3))
	=>
	(printout t "SOY NIVEL DE RES " ?n1 crlf)

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
	(retract ?g)
	(retract ?a)
	(retract ?b)
	(retract ?c)
)

(defrule inference_module::limpiar
	(declare (salience 100))
	(conclusions)
	(sucio)
	?f <- (object (is-a Ejercicio) (Duracion 22))
	=>
	(send ?f put-Duracion 15)
)

(defrule inference_module::esta_limpio
	(declare (salience 20))
	(conclusions)
	?f <-(sucio)
	=>
	(retract ?f)
)



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
	
	
	
	
	
	
	

