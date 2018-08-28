rm(list=ls())
setwd("~")

#########################################################
#### Clase 3: Básicos de Procesamiento de Datos      ###
#### Vamos a aprender cuáles son y para qué sirven  ###
#### las funcines básicas de Tidyverse y base de R   ###
#### mientras procesamos SESNSP nueva metodología     ###
#########################################################


# paso 1 en todo, pongan sus directorios

inp = "/Users/carolinatorreblanca/Dropbox (Data4)/Data Civica/Clases/leer_datos_18/2_procesar_datos/input" 
out = "" # poner

require(readxl)
# la base está guardada en .xlsx pero R no sabe cómo leer esos archivos, entonces tengo que prender el paquete

base = read_xlsx(paste(inp, "victimas.xlsx", sep="/")) # mismo paquete que para read_excel
# listo, ahora se acuerdan de nuestras funciones  para ver qué trae la base?

str(base)
head(base)
tail(base)
summary(base)
names(base)
View(base)

# Ok, entonces antes de cualquier cosa - los nombre no pueden tener espacios, acentos o ñ

nombres_futuros = c("year", "cve_ent", "ent", "bien_juridico", "tipo_delito", "subtipo", "modalidad", "sexo",
                    "rango_edad") # el resto no están mal 

names(base)
names(base)[1:9] # estos son los que quiero cambiar 
names(base)[1:9] <- nombres_futuros
names(base) # done!


# para ver la base más a fondo
table(base$tipo_delito)
table(base$subtipo)
table(base$modalidad)

# y qué trae Clave_Ent ? alguien sabe qué es ?
table(base$ent, base$cve_ent) # pro tip: tablas cruzadas


# OJO: NO ES POR ORDEN ALFABÉTICO es por orden YOLO
# En este país identificamos a los estados y municipios con clave inegi 
# Es una clave de 5 dígitos - 2 dicen qué entidad y 3 dicen qué municipio
# Ok entonces queremos hacer la clave de dos dígitos (para pegar con otras bases, por ejemplo) ¿que hacemos? 

str(base$cve_ent)

##################################################################
## Cambiar el tipo de variable                                 ###
## numérica -> caracter (con y sin formato) y de regreso       ###
##################################################################

# si lo vulevo caracter ¿por qué no me sirve?
base$cve_ent = as.character(base$cve_ent)
table(base$cve_ent) # no me quedan de dos dígitos los menores a 10 ¿cómo le hago?
# R no inventa el digito que me falta para los números menores a diez
base$cve_ent = as.numeric(base$cve_ent)
#FUNCION NUEVA
base$cve_ent = formatC(base$cve_ent, width = 2, format="d", flag="0")
table(base$cve_ent) # listo!

# pero las claves inegi tiene 5 digitos, se usa que, cuando son datos estatales le pegas "000"

# se acuerdan de como crear variables? Lo vemos más adelante más a detalle
base$inegi <- paste0(base$cve_ent, "000") # paste0 es como paste sin separación

###########################
### Filtrar renglones   ###
###########################

# supongamos que solo nos intersa quedarnos con víctimas de feminicidio, hay que filtrar renglones
# 2 maneras 

# 1: sintaxis de R o [renglon,columna] (que ya saben cómo hacer)
filtrada = base[base$tipo_delito=="Feminicidio",]
table(filtrada$tipo_delito)

# 2: dplyr y filter - primero todas hagan
#install.packages("tidyverse")
require(tidyverse) # este set de paquetes es el que usamos para hacer el 80% de toda la limpieza en la oficina
??tidyverse

# tidyverse es MUY util y tiene la facilidad de que no tienes que poner $ y puedes hacer pipes
# de ahora en adelante vamos a ver (casi) todas las funciones de 2 maneras: nativo R y tidyverse/dplyr
# Ustedes como cientistas pueden elegir la manera más rapida o que más les lata

filtrada_2 = filter(base, tipo_delito=="Feminicidio") # GRAN plus (y GRAN fuente de confisión): si una funcion es de dplyr o tidyverse, no necesitan $
table(filtrada_2$tipo_delito)

# para poner más de una condición podemos usar & , | < y > sin problema 
# Cómo le harían, por ejemplo para quedarse solo con Feminicidio si el año es mayor a 2015 y rango_edad es menores
table(filtrada_2$rango_edad)

ejemplo =

# de hecho hay 3 maneras para esto! hay otra funcion base de R que se llama subset

# 3.- Subset
ejemplo_2 = subset(base, tipo_delito=="Feminicidio")

table(ejemplo_2$tipo_delito)

rm(filtrada_2, ejemplo, ejemplo_2)

# Ahora qué hacemos con esta base? La dejamos bella
View(filtrada)
str(filtrada)
table(filtrada$sexo) # variable inutil
table(filtrada$bien_juridico) # variable inutil
table(filtrada$tipo_delito) # variable inutil
table(filtrada$subtipo) # variable inutil

#########################
### Tirar variables   ###
#########################

filtrada$sexo = NULL # opción 1 base de R, ya lo habíamos visto
filtrada = select(filtrada, -bien_juridico) # opción 2 tidyverse / select (o select not es como lo pienso yo)
names(filtrada) # ni sexo ni bien jurídico

# Tirar más de una variable a la vez? obvia / se puede de las dos maneras
filtrada_tempo <- select(filtrada, -(tipo_delito:subtipo)) # si están juntas en la base con : y select
names(filtrada_tempo) # bye variables

# obvio select sirve no solo para decirle a R que variables no, sino también para elegir variables sí?
caro_rockea = select(filtrada, year, cve_ent, modalidad, Enero:Diciembre) # qué le estoy diciendo?
View(caro_rockea)
rm(caro_rockea)
# como le diríamos que nos queremos quedar con cve_entidad, modalidad, year y solo enero a junio?
su_ejemplo = 
  
rm(filtrada_tempo, vean, su_ejemplo)

# tirar 2 variables con NULL
filtrada$tipo_delito <- filtrada$subtipo <- NULL # noten como se puede usar '=' y '<-' indistintamente
str(filtrada)

####################################
## Reordenar y sortear variables ###
####################################
# ordenar #
# me estresa que inegi está al final quiero que esté al principio: 2 opciones igual - R base y tidyverse 

# 1.- Sintaxis de base 
names(filtrada)
base_ordenada = filtrada[,c("inegi", "cve_ent", "year","ent", "modalidad", "rango_edad", "Enero", 
                            "Febrero", "Marzo", "Abril", "Mayo","Junio", "Julio", "Agosto", 
                            "Septiembre", "Octubre", "Noviembre", "Diciembre")]
base_ordenada # bum, inegi hasta el principio - pero hueva 
ncol(base_ordenada)

# 2.- Select 
base_ordenada_2 = select(filtrada, inegi, cve_ent, year, ent:Diciembre)
base_ordenada_2
ncol(base_ordenada_2)

# sortear # arrange y dplyr
base_sorteada = arrange(filtrada, year) # siempre es ascendiente tons no se ve nada
base_sorteada

base_sorteada = arrange(filtrada, desc(year)) # lo mismo funciona con arrange(filtrada, -year)
base_sorteada # ahora sí, de mayor a menor 

# oooo solo piquenle en la flechita de la variable en el View, estilo excel
rm(base_sorteada, base_ordenada, base_ordenada_2)


#########################
### Crear Variables:  ###
### Mutate y R basico ###
#########################
# no nos interesa una base por mes, queremos una base por año municipio modalidad etc.
# sumar renglones  pero OJO con los NA 
NA + 30 
30 + 0
# Los NA's NO SON CERO - SON INFINITO

table(filtrada$Enero, useNA="always") # por default no lo tabula los NAs 
summary(filtrada, useNA="ifany")

# en este caso no tiene, pero igual les voy a enseñar a sumar renglones y no cagarla en los NAs
# queremos sumar de Enero a diciembre y generar una nueva columna "total"
# R piensa en columnas entonces cuando quieres que haga cosas sobre renglones tienes que especificar

7 + 8 + 44 + 56 + NA 
sum(c(7, 8, 44, 56, NA))
sum(c(7, 8, 44, 56, NA), na.rm = TRUE)

# esto está muy chido para vectores, asi funcionan las columnas en una base, por ejemplo, pero si queremos sumar filas?
??rowSums
names(filtrada)[6:17]
#Son los meses que queremos sumar
filtrada$total = rowSums(filtrada[,6:17], na.rm = TRUE)
# o lo que es lo mismo
filtrada$total = rowSums(filtrada[,names(filtrada)[6:17]], na.rm = TRUE)
# o 
filtrada$total = rowSums(filtrada[,c("Enero", "Febrero", "Marzo", "Abril", "Mayo",
                                     "Junio", "Julio", "Agosto", "Septiembre",
                                     "Octubre", "Noviembre", "Diciembre")], na.rm = TRUE)

### wuuuu! ya va quedando bella la base, hay que tirar los meses que ya no nos sirven
filtrada = select(filtrada, inegi, year:rango_edad, total)

################
## summarize ###
################

# Q: ¿Cuantas observaciones hay por estado-año? ¿Why?
count(filtrada, cve_ent, year) # para ser exactos 12 veces 
table(filtrada$modalidad)
table(filtrada$rango_edad)
nrow(filtrada)

# ¿Entonces qué hacemos para sacar el total de victimas de feminicidio por año entidad?
# Depende como quieras tu base - yo quiero una sin distinción de grupos de edad

# Esto es TODO en el procesamiento de datos: ¿qué debería ser cada renglón?

# Paso 1: agrupar
filtrada = group_by(filtrada, inegi, year, cve_ent, ent, modalidad) # todas menos edad

# Paso 2: summarize (o colapsar) 
filtrada = summarize(filtrada, total = sum(total, na.rm=T))
View(filtrada)
nrow(filtrada)

# summarize o summarise; ambas funcionan - depende si son agringadas o ainglesadas 

################
## reshapear ###
################

# las bases de datos están en dos formatos: largo y ancha -
# ahorita nuestra modalidad está en larga, queremos que cada modalidad sea una columna rellenada por los datos de total 
# cambiar de long a wide se llama reshapear y hay dos funciones: spread (de long a wide) y gather (wide a long)

base_ancha = spread(filtrada, modalidad, total) # ¿Cual quieres que suba / con cuál quieres que rellene?
View(base_ancha) 
names(base_ancha)

# otra vez pedo de los nombres con espacio
nombres_anchos = c("arma_blanca", "arma_fuego", "otro", "no_especificado")
names(base_ancha)[5:ncol(base_ancha)] = nombres_anchos
View(base_ancha)

# de ancha a larga
base_larga = gather(base_ancha, modalidatzzz, totaltzzz, arma_blanca:no_especificado) # 3 cosas ahora 1: como se va a llamar la variable con los nombres, con los datos y cuales 
View(base_larga)
table(base_larga$modalidatzzz)

# ufff no, queremos quitar esos _ de esa variable - ¿como le hacemos?
# evidentemente hay varias estrategias, veamo gsub que es la más intuitiva 

base_larga$modalidatzzz = gsub("_", " ", base_larga$modalidatzzz)

table(base_larga$modalidatzzz)
rm(base_larga)

# regresando a la ancha, solo nos hace falta una columna de total
# como era el rowSums? se me olvida

base_ancha$total = rowSums(base_ancha[,5:ncol(base_ancha)])

######################
## joins y appends ###
######################

# tenemos totales por año, pero ya aprendimos porque es una mala comparación
# necesitamos tasas 
# tengo las poblaciones en 2 archivos diferentes a nivel municipal

pob_1 = read.csv(paste(inp, "pob_mun_sexo_3015.csv", sep="/"), encoding="UTF-8")
str(pob_1)
table(pob_1$año)

pob_2 = read.csv(paste(inp, "pob1618.csv", sep="/"), encoding="UTF-8")
str(pob_2)
table(pob_2$año)

# 5 pasos: 1 - quedarnos con años y sexo deseados 2 - homologar nombres/tipo de variables
# 3.- obtener poblaciones estatales 4 - juntar las dos bases de poblacion (appendear) 
# 5.- pegar la población con la base de feminicidios

pob_1 = filter(pob_1, año==2015 & sexo =="Mujer")

table(pob_2$sexo)
# ojo! aquí es "mujeres"
pob_2 = filter(pob_2, año <= 2017 & sexo =="Mujeres")
pob_2$sexo = gsub("Mujeres", "Mujer", pob_2$sexo)

# en  qué parte de esta base viene el estado?
# exacto pero son los primeros DOS digitos SOLO cuando es de 5 -
# aguascalientes y durango No son el mismo estado!!! formatC

pob_1$ID = formatC(pob_1$ID, width = 5, format="d", flag="0")
pob_2$cvegeo = formatC(pob_2$cvegeo, width = 5, format="d", flag="0")

## SUBSTRINGS! ###
# vamos a crear una variable que sean los primeros 2 caracteres de la columna ID 
# le pongo cve_ent para que se llame igual que en base ancha
??substr

pob_1$cve_ent = substr(pob_1$ID, 1, 2)
pob_2$cve_ent = substr(pob_2$cvegeo, 1, 2)
table(pob_1$cve_ent)
table(pob_2$cve_ent) # Done 

# queremos colapsar sumando - primer paso agrupar y luego un summarize

pob_1 = group_by(pob_1, cve_ent, año)
pob_1 = summarize(pob_1, pob_tot = sum(pob, na.rm=T)) # ¿qué le pasó a sexo?

pob_2 = group_by(pob_2, cve_ent, año)
pob_2 = summarize(pob_2, pob_tot = sum(pob, na.rm=T)) 

# ahora? como le hago para juntar? qué me interesa? más renglones o más columnas?
## Apendear ###
pob_junta = bind_rows(pob_1, pob_2)
table(pob_junta$año) # supertz

rm(pob_1, pob_2)

# ahora solo hay que juntar población con feminicidos - 
# pero no nos interesa juntar cualquier renglón con cualquier renglon
# sino mismo año misma entidad - eso se llama join (merge)
names(base_ancha) 
names(pob_junta) # o a los 2 le pones el mismo nombre o especificas 

base_final = full_join(base_ancha, pob_junta, by=c("year"="año", "cve_ent"))

#############
## mutate ###
#############

#¡casi! solo nos falta calcular la tasa y aprovecahmos para ver mutate 
# mutate crea variables a partir de valores de otras variables / es cool 
# es de dplyr/tidyverse entonces ya sabemos cómo se escribe

base_final = mutate(base_final, tasa_total = total / pob_tot * 100000)
## uff está horrible - hay que redondear 

base_final$tasa_total = round(base_final$tasa_total, digits =2)

# podríamos calcular más de una tasa a la vez? obvio
# eh ahí la ventaja del mutate sobre el $

base_final = mutate(base_final, tasa_arma_fuego = arma_fuego / pob_tot * 100000,
                               tasa_arma_blanca = arma_blanca / pob_tot * 100000)

# podríamos hasta redondear en el mismo comando
base_final = mutate(base_final, tasa_otro = round(otro / pob_tot * 100000, digits =2),
                                  tasa_ne =  round(no_especificado / pob_tot * 100000, digits =2))

base_final$digits <- NULL
base_final$tasa_arma_fuego = round(base_final$tasa_arma_fuego, digits =2)
base_final$tasa_arma_blanca = round(base_final$tasa_arma_blanca, digits =2)


View(base_final)

write.csv(base_final, paste(out, "tasa_feminicidos_estatal.csv"), row.names = F, fileEncoding ="UTF-8")

### Ahora quiero una base que tenga tasa total por año y otra tasa promedio por entidad

base_por_year = ungroup(base_final)
base_por_year = group_by(base_por_year, year)
base_por_year = summarize(base_por_year, tasa_ponderada = weighted.mean(tasa_total, pob_tot))

### ¿Me ayudan?
















































































