rm(list=ls())
setwd("~")

###############################
## Tarea 3 - procesar datos ###
## Vamos a practicar las    ###
## funciones que vamos aver ###
###############################

# Metete al secretariado metodología vieja
# http://secretariadoejecutivo.gob.mx/incidencia-delictiva/incidencia-delictiva-fuero-comun.php

# Hasta abajooooo dice Datos de Incidencia Delictiva bla 
# Descarga la base que está hasta abajo que se llama Estatal 1997 - 2017
# decomprime el zip etc etc ... abre la base
# ¿qué es un xlsb? es literal EL UNICO formato que no puede abrir R - entonces abranlo en excel y 
# expórtenlo en .csv en un folder llamado input (ocomo dios te de a entender)


# 1.- Crea tus directorios , uno al inp otro a un folder que sea out
# en el inp copia las baes de pobación que te di en clase

# 2.- importa tu csv a una base que se llame "snsp" usando directorios

# 3.- corre este comando y dime para qué sirve
tolower(names(snsp))

# 4.- cambia todos los nombres a minúscula en un solo comando

# 5.- tabula la modalidad el tipo y el subtipo de delitos 

# 6.- tabula el tipo solo cuando la modaliad sea DELITOS SEXUALES (VIOLACION)

# 7.- tabula el subtipo solo cuando el tipo sea DELITOS VIOLACION

# 8.- Filtra la base para quedarte solo con averiguaciones por violación;
#     guarda esa base filtrada en un nuevo objeto llamado "base_filtrada"
# acuérdate que si vas a usar una función de tidyverse debes prender el paquete

# 9.- Tira las variables modalidad a subtipo 

# 10.- cambiale el formato a la variable inegi por una de 2 caracteres

# 11.- sustituye en la variable "entidad" CIUDAD DE MEXICO por CDMX 

# 12.- reshepea la base de wide a long, creando una nueva variable de "mes" que contenga
# las variables de enero a diciembre y una variable que se llame "total" , llamala base_reshape

# 13.- tira todas tus bases excepto base_reshape

# 14.- agrupa por año inegi y entidad y colapsa (o suma horizontalmente) el total mensual para 
# quedarte con el total anual por año entidad

# 15.- importa las dos bases de población 

# 16.- colapsa las bases de municipio a estatal ambos sexos y 
# unelas en una sola base de población que abarque del 97 al 2017

# 17.- Une las poblaciones de cada estado-año con el tot de averiguaciones por violacion

# 18.- Calcula las tasas anuales estatales por cada 100 mil personas

# 19.- Exporta tu base a tu folder de out , ponle "soy_una_triunfadora.csv"

# 20.- ¿Que concluyes con base en los datos que acabas de procesar?

# 21.- Pregunta conceptual ¿estamos midiendo lo mismo en diferentes estados y 
#      diferentes años cuando hablamos de "violacion"?











