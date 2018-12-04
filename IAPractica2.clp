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
	(multislot Dia
		(type INTEGER)
		(range 1 7)
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
		(range 0 180)
		(default 0)
;+		(cardinality 1 1)
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
	(multislot Dia
		(type INTEGER)
		(range 1 7)
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
		(range 0 180)
		(default 0)
;+		(cardinality 1 1)
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
	(Num_repeticiones 15)
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
	(Num_repeticiones 15)
	(partes_Ejercitadas "brazos"))

([IAPractica2_Class10011] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "flexion de hombros")
	(Num_repeticiones 15)
	(partes_Ejercitadas "brazos"))

([IAPractica2_Class10013] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "ejercicios de biceps")
	(Num_repeticiones 15)
	(partes_Ejercitadas "brazos"))

([IAPractica2_Class10014] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "extension de triceps")
	(Num_repeticiones 15)
	(partes_Ejercitadas "brazos"))

([IAPractica2_Class10015] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "levantarse de una silla")
	(Num_repeticiones 15)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class10017] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "flexion plantar")
	(Num_repeticiones 15)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class10018] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "flexion de rodilla")
	(Num_repeticiones 15)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class10019] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "extension de rodilla")
	(Num_repeticiones 15)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class10020] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "flexion de cadera")
	(Num_repeticiones 15)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class10021] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "extension de cadera")
	(Num_repeticiones 15)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class10022] of  Fuerza

	(Duracion 7)
	(Intensidad 2)
	(Nombre_Ejercicio "elevar piernas a los lados")
	(Num_repeticiones 15)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class10026] of  Equilibrio

	(Duracion 15)
	(Intensidad 0)
	(Nombre_Ejercicio "flexion plantar")
	(Num_repeticiones 15))

([IAPractica2_Class10027] of  Equilibrio

	(Duracion 15)
	(Intensidad 0)
	(Nombre_Ejercicio "flexion de rodilla")
	(Num_repeticiones 15))

([IAPractica2_Class10028] of  Equilibrio

	(Duracion 15)
	(Intensidad 0)
	(Nombre_Ejercicio "flexion de cadera")
	(Num_repeticiones 15))

([IAPractica2_Class10029] of  Equilibrio

	(Duracion 15)
	(Intensidad 0)
	(Nombre_Ejercicio "extension de cadera")
	(Num_repeticiones 15))

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
	(Num_repeticiones 15))

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
	(Num_repeticiones 15)
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
	(Num_repeticiones 15))

([IAPractica2_Class25] of  Flexibilidad

	(Duracion 7)
	(Intensidad 1)
	(Nombre_Ejercicio "estiramiento de tobillo")
	(Num_repeticiones 15)
	(partes_Ejercitadas
		"brazos"
		"piernas"))

([IAPractica2_Class26] of  Flexibilidad

	(Duracion 7)
	(Intensidad 1)
	(Nombre_Ejercicio "estiramiento de triceps")
	(Num_repeticiones 15)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class27] of  Flexibilidad

	(Duracion 7)
	(Intensidad 1)
	(Nombre_Ejercicio "estiramiento de muñeca")
	(Num_repeticiones 15)
	(partes_Ejercitadas "brazos"))

([IAPractica2_Class3] of  Flexibilidad

	(Duracion 7)
	(Intensidad 1)
	(Nombre_Ejercicio "rotacion de hombros")
	(Num_repeticiones 15)
	(partes_Ejercitadas "brazos"))

([IAPractica2_Class4] of  Flexibilidad

	(Duracion 7)
	(Intensidad 1)
	(Nombre_Ejercicio "estiramiento de quadriceps")
	(Num_repeticiones 15)
	(partes_Ejercitadas
		"brazos"
		"piernas"))

([IAPractica2_Class5] of  Flexibilidad

	(Duracion 7)
	(Intensidad 1)
	(Nombre_Ejercicio "rotacion doble de cadera")
	(Num_repeticiones 15)
	(partes_Ejercitadas "piernas"))

([IAPractica2_Class6] of  Flexibilidad

	(Duracion 2)
	(Intensidad 0)
	(Nombre_Ejercicio "rotacion simple de cadera")
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
            (Casado)
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
                            0 -> no realizo ningun ejercicio y 10 -> realizo ejercicio a diario con buena intensidad)" 0 10)
		(assert (capacidad_fisica (valor ?f)))

	)
        (defrule question_module::tipo-ejercicio
            (declare (salience 10))
            (newRutine)
            (capacidad_fisica(valor ?f))
            =>
            (if (> 0 ?f) then
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
                (declare (salience 10))
                (NewRutine)
                ?f<-(Fuma)
                => 
                (bind ?p (pregunta-numerica "Con que frecuencia fuma?" 1 10))
                (assert (fumador (frequencia ?p)))
                (retract ?f)
        )

        ;CONDICION FISICA A DESTACAR   
        (defrule question_module::condicion_fisica
            (declare (salience 10))
            (NewRutine)
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
        (defrule quiestion_module::enfermedad-causa-dep
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
                (reject (?f))
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
; Módulo para hacer la inferencia de datos según las preguntas

; Definimos el módulo para la inferencia de datos
(defmodule inference_module
	(import MAIN ?ALL)
    (import question_module ?ALL)
    (export ?ALL)
)

(deftemplate inference_module::sesiones (slot numero (type INTEGER) (range 3 7)))

(deftemplate inference_module::sesion (slot num (type INTEGER) (range 1 7)))

(deftemplate inference_module::imprimir_sesion (slot num (type INTEGER) (range 1 7)))



;Retornar un multislot sense els elements que exerciten les cames
(deffunction inference_module::no_piernas ($?allowed-values)

)


;Retornar un multislot amb ?num elements de $?allowed-values aleatoris
(deffunction inference_module::randomSlots (?num $?allowed-values)

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
)

(defrule inference_module::vacio_equilibrio
	(declare (salience 10))
	(conclusions)
	(not (eq (nivel ?n)))
	=>
	(assert (eq (nivel 0)))
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
	=>
	(if (and (not (= ?v 2)) (and (>= ?c 0.4) (< ?c 0.6))) then (modify ?r (nivel (+ 1 ?n))))
	(if (and (= ?v 0) (>= ?c 0.6)) then (modify ?r (nivel (+ 2 ?n))))
)

(defrule inference_module::asignar_fuer
	(declare (salience 10))
	(conclusions)
	(coeficiente (coef ?c))
	?r<-(fuer (nivel ?n))
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
	(assert (sesion (num 2)))
	(assert (sesion (num 4)))
	(assert (sesion (num 6)))
	(if (> ?n 3) then (assert (sesion (num 1))))
	(if (> ?n 4) then (assert (sesion (num 3))))
	(if (> ?n 5) then (assert (sesion (num 5))))
	(if (> ?n 6) then (assert (sesion (num 7))))
	(retract ?f)
)


(defrule inference_module::planificar_sesiones
	(declare (salience 10))
	(conclusions)
	(res (nivel ?n1))
	(cal (nivel ?n2))
	(fuer (nivel ?n3))
	(eq (nivel ?n4))
	?f <-(sesion (num ?numero))
	=>
	(if (not(= ?n1 0)) then
		(bind $?aer (find-instance ((?inst Aerobico)) (= ?inst:Intensidad 0))))
			
	(if (not(= ?n3 0)) then																;if (?n3 = 1,2,3) 4,6,8 exercicis respectivament
;		(bind $?fuer (find-all-instances ((?inst Fuerza)) ))							;if (?n2 = 1,2,3) 5,7,9 exercicis respectivament
;		(if (= ?n3 1) then																;if (?n4 = 1,2) 4,6 ex respectivament
;			)
		
		)
		
	
	
	
	
	
;	(slot-insert$ res1 $?ejs 999 ?aer)
	
	(programaSesion ?numero $?aer)
	(assert (imprimir_sesion (num ?numero)))
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

(deffunction output_module::imprimir_ejercicios (?sesion)
	(bind ?i 1)
	(while (<= ?i (length$ (send ?sesion get-Ejercicios)))
	do
		(bind ?ejercicio (nth$ ?i (send ?sesion get-Ejercicios)))
		(printout t " - Realizaremos el ejercicio " (send ?ejercicio get-Nombre_Ejercicio) " durante un tiempo de " (send ?ejercicio get-Duracion) " minutos." crlf)
		(bind ?i (+ ?i 1))
 )
)


(defrule output_module::general
		(declare (salience 10))
		(escribir)
		=>
		(printout t "Este es el diario de sesiones asociado a su diagnostico: " crlf)
)

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
	
	
	
	
	
	
	

