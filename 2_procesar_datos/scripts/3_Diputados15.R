rm(list=ls())
setwd("~")

########################################################
# Procesar Datos 2 - Real World Example:               #
# Votaciones Diputados MR 2015                         #
# Carolina Torreblanca - Periodismo de Datos ITAM      #
# 22/04/2017                                           #
########################################################

library(dplyr) # manipular data
library(tidyr) # "tidy data" o el paquete que SIEMPRE van a prender

dir1 <- "/Users/ppmerino/Dropbox (Personal)/Clases/ICAitam/ICAmerino/Clase 1 Procesar datos/Procesar datos/datos/Procesar Datos 2/resultados_dip_2015" # poner ruta 
dir2 <- "/Users/ppmerino/Dropbox (Personal)/Clases/ICAitam/ICAmerino/Clase 1 Procesar datos/Procesar datos/datos/Procesar Datos 2/out" # poner ruta

datos <- read.csv(paste(dir1, "diputados.csv", sep="/"))

# Qué está pasando? dice que es CSV pero claramente no está separado por comas sino por ' | '
datos <- read.table(paste(dir1, "diputados.csv", sep="/" ), sep="|", header=T, stringsAsFactors=F)

#Nuestros básicos, ¿qué tenemos?
str(datos) #uff, 31 variables!
head(datos, 20) # mucha basura arriba
tail(datos, 20) 
summary(datos) # todos caracter, ¿por?
names(datos) #no pos cuanta info

# 1 Limpiar basura y poner nombres buenos
nrow(datos)
datos <- datos[5:nrow(datos),]
head(datos)
#ojo, los nombres de los renglones son caracter, no son un conteo. Ahora nuestro primer rengón se llama "5"

vector_nombres <- datos[1,]
vector_nombres <- as.vector(vector_nombres)
names(datos)<- vector_nombres
head(datos) #wups, todavía falta quitar un renglon

nrow(datos)
datos <- datos[2:nrow(datos),]
names(datos)

#listo, ahora cuales variables nos interesan?
str(datos)

table(datos$CONTABILIZADA, useNA="ifany") #nopos goao
table(datos$OBSERVACIONES, useNA="ifany") #sólo nos vamos a quedar con los que no tengan nada aquí
table(datos$TIPO_CASILLA, useNA="ifany")  # nos da igual
table(datos$UBICACION_CASILLA, useNA="ifany") # urbano/ rural
table(datos$TIPO_ACTA, useNA="ifany") # los tipos 4 son de representación proporcional
table(datos$TOTAL_CIUDADANOS_VOTARON, useNA="ifany") # salebye

#subset 
datos <- datos[datos$OBSERVACIONES==" " & datos$TIPO_ACTA!="4",]

#el resto claramente nos interesa, pero en numéricos
table(datos$OBSERVACIONES, useNA="ifany") # Listou
table(datos$TIPO_ACTA, useNA="ifany")

#Ya no nos sirven
head(datos)
datos <- select(datos, -(ID_CASILLA:NUM_BOLETAS_EXTRAIDAS), -OBSERVACIONES, -CONTABILIZADA)
str(datos)

#Ahora volver numérico todas las votaciones
datos$PAN <- as.numeric(datos$PAN) # que mega flojera, si algo me da flojera significa que hay otra manera

votos <- select(datos, PAN:LISTA_NOMINAL)
votos <- names(votos)
datos[, votos] <- apply(datos[, votos], 2, function(x) as.numeric(x))
str(datos)

# primero subseteo con las variables que me interesan
# luego sustituyo esa base subseteada con el vector de nombres
# luego utilizo ese vector para ir de nombre en nombre sustituyendo como numérico, por eso ,votos define que columnas
# apply es coo un loop sin ser loop... el 2 es sintaxis para columnas de la funcion apply... 
# function(x) le dice que va a aplicar una funcion... luego le dices qué función

# Nuestros datos están a nivel casilla, a nadie le interesan así, lo quiero a nivel distrito.
# lo primero es el formato de nuestro distrito y estado, tiene que tener 5 cifras al juntarse

datos$ESTADO <- as.numeric(datos$ESTADO)
datos$DISTRITO <- as.numeric(datos$DISTRITO)

datos$ESTADO  <- formatC(datos$ESTADO , width = 2, format = "d", flag = "0")
datos$DISTRITO  <- formatC(datos$DISTRITO , width = 2, format = "d", flag = "0")

datos$id <- paste0(datos$ESTADO, datos$DISTRITO)
table(datos$id) # wu! 

# ahora si, colapsemos para tener datos a nivel distrito y no a nivel casilla

datos <- group_by(datos, id)
datos <- summarize(datos, pan = sum(PAN, na.rm = T), pri = sum(PRI , na.rm = T), 
                          prd = sum(PRD , na.rm = T), pvem = sum(PVEM, na.rm = T), 
                          pt = sum(PT, na.rm = T), mc = sum(MOVIMIENTO_CIUDADANO, na.rm = T), 
                          nueva_a = sum(NUEVA_ALIANZA, na.rm = T), morena = sum(MORENA, na.rm = T), 
                          ph = sum(PH, na.rm = T), ps = sum(PS, na.rm = T), c_pri_pvem=sum(C_PRI_PVEM, na.rm = T), 
                          c_prd_pt = sum(C_PRD_PT, na.rm = T), cand_ind_1 = sum(CAND_IND_1, na.rm = T), 
                          cand_ind_2 = sum(CAND_IND_2, na.rm = T), no_r = sum(NO_REGISTRADOS, na.rm = T), 
                          nulos = sum(NULOS, na.rm = T), tot_votos= sum(TOTAL_VOTOS, na.rm = T), lista = sum(LISTA_NOMINAL, na.rm = T))

datos <-ungroup(datos)
datos <- rename(datos, id_distrito = id) 
nrow(datos) # oh por dios, el mismo número que dipudados de mayoría relativa, osea que distritos

#ok pero, ¿quién ganó? aprovechemos para aprender a usar un loop

# primero sumemos las alianzas

# pri = if_else(c_pri_pvem != 0 , sum(c_pri_pvem, pri, pvem), pri) --> sustituye pri cuando c_pri_pvem es distinto de cero con la suma 
# de las tres columnas, sino deja pri

datos <- group_by(datos, id_distrito) 
datos <-   mutate(datos, pri = if_else(c_pri_pvem != 0 , sum(c_pri_pvem, pri, pvem), pri), 
                         prd = if_else(c_prd_pt != 0, sum(c_prd_pt, prd, pt), prd), 
                         d_votogan=max(pan, pri, prd, pvem, pt, mc, nueva_a, morena, ph, ps, cand_ind_1, cand_ind_2, na.rm=T))

datos$d_gan <- as.character(NA)
datos <- ungroup(datos)

# generé una variable vacía pero caracter

########
# LOOP # 
########

tempo <- select(datos, 2:15)

for(x in 1:nrow(datos)){
  datos$d_gan[x] <- names(tempo)[which.max(tempo[x, 1:ncol(tempo)])]
}

head(datos) # Casi listo! si nos vemos muy exquisitos, podríamos sacar porcentajes. 
rm(tempo)

table(datos$d_gan)

write.csv(datos, paste(dir2, "Elecciones_DipMR2015.csv", sep="/"),row.names=F)


