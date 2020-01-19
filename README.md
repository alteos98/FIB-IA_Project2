# IAPractica2
El prototip inicial del nostre generador d'horaris d'exercicis per a gent d'avançada edat, comproba els següents aspectes:
 - L'edat.
 - Freqüencia amb la qual la persona realitza exercici.
 - Si la persona pateix problemes cardiovasculars (i si en té, li consulta el nivell de colesterol).
 - Si la persona ha patit alguna caiguda recentment.
 
Mitjançant els resultats d'aquestes dades, el sistema genera un horari, amb tres, cinc o set sessions (1 sessió per dia com a màxim)
distribuides entre els set dies de la semana. El número de sessions setmanals depèn de la costum de la persona a la
realització d'exercicis.
Si la persona té problemes cardiovasculars, segons el nivell de colesterol que tingui, se li assignaran exercicis
de resistencia mès suaus; sempre que no arribi al nivell màxim de colesterol, el cual eliminarà completament els
exercicis de resistencia dels resultats.
Si la persona ha patit una caiguda, el sistema li recomenarà precaució i interès alhora de realitzar els exercicis d'equilibri. 

Per a cada sessió, programa un exercici d'escalfament inicial, un d'equilibri, un de força i un de resistencia
(si el colesterol de l'usuari no arriba al nivell màxim).

Les preguntes del sistema mantenen un usable sistema de resposta, i també llencen excepcions adecuades quan l'usuari
introdueix resultats incorrectes.

Cal fer (reset) abans d'executar el sistema, sino les instancies inicials no s'executaran.
El programa detecta aquesta falta i llença una excepció d'advertencia si l'usuari s'ha despistat.
