rm(list=ls())
setwd("~")

########################################################
# Procesar Datos  - Real World Example:                #
# Votaciones Diputados MR 2015                         #
########################################################

require(pacman)
p_load(tidyverse, stringr, foreign)

dir1 <- "/Users/carolinatorreblanca/Dropbox (Data4)/Data Civica/Clases/leer_datos_18/2_procesar_datos/input" # poner ruta 
dir2 <- "/Users/carolinatorreblanca/Dropbox (Data4)/Data Civica/Clases/leer_datos_18/2_procesar_datos/output" # poner ruta

datos <- read.csv(paste(dir1, "diputados.csv", sep="/")) # GUAT ! porque no?????

# Qué está pasando? dice que es CSV pero claramente no está separado por comas sino por ' | '
datos <- read.table(paste(dir1, "diputados.csv", sep="/" ), sep="|", header=T, stringsAsFactors=F, as.is = T, fileEncoding = "UTF-8") # que tantas opciones le puse?

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
names(datos) # podríamos solo sustituir todo a mano, pero que hueva, si ya hay un renglón con los nombres ...

vector_nombres <- datos[1,]
vector_nombres <- as.vector(vector_nombres)
names(datos)   <- vector_nombres
head(datos) #wups, todavía falta quitar un renglon
datos <- datos[2:nrow(datos),]

names(datos)

#listo, ahora cuales variables nos interesan?
str(datos)

table(datos$CONTABILIZADA, useNA="ifany") # qué significa esto?
table(datos$OBSERVACIONES, useNA="ifany") # sólo nos vamos a quedar con los que no tengan nada aquí
table(datos$TIPO_CASILLA, useNA="ifany")  # B.- Básica, C.- Contigua, E.- Extraordinaria, S.- Especial
table(datos$UBICACION_CASILLA, useNA="ifany") # urbano/ rural
table(datos$TIPO_ACTA, datos$TIPO_CASILLA, useNA="ifany") # los tipos 4 son de representación proporcional, queremos saber quién ganó en el distrito
table(datos$TOTAL_CIUDADANOS_VOTARON, useNA="ifany") #

# subset con gramática de R, para que se acostumbren a todo
datos <- datos[datos$OBSERVACIONES==" " & datos$TIPO_ACTA!="4",] # solo cuando no hubo bronca y quitamos las especiales de RP

#el resto claramente nos interesa, pero en numéricos
table(datos$OBSERVACIONES, useNA="ifany") # Listou
table(datos$TIPO_ACTA, useNA="ifany")

#Ya no nos sirven
head(datos)
datos = select(datos, -(ID_CASILLA:NUM_BOLETAS_EXTRAIDAS), -OBSERVACIONES, -CONTABILIZADA)
str(datos)

#Ahora volver numérico todas las votaciones
datos$PAN <- as.numeric(datos$PAN) # que mega flojera, si algo me da flojera significa que hay otra manera / lapply sapply tapply son una JOYA

# quiero volver numérico del PAN 4 - hasta lista nominal ncol
datos[,4:ncol(datos)]
datos[4:ncol(datos)] <- sapply(datos[,4:ncol(datos)], as.numeric) 
str(datos)

# Tarea: ¿cómo hacer exactamente lo mismo pero usando mutate_at?

##
##


# Nuestros datos están a nivel casilla, a nadie le interesan así, lo quiero a nivel distrito.
# lo primero es el formato de nuestro distrito y estado, tiene que tener 5 cifras al juntarse

datos$ESTADO <- as.numeric(datos$ESTADO)
datos$ESTADO <- formatC(datos$ESTADO , width = 2, format = "d", flag = "0")
datos$DISTRITO  <- formatC(as.numeric(datos$DISTRITO), width = 2, format = "d", flag = "0") # anidar funciones es la sal de la vida

datos$id <- paste0(datos$ESTADO, datos$DISTRITO)
table(datos$id) # wu! 

# qué onda con las alianzas? cuando es NA es que no hubo alianza

datos = mutate(datos, coalicion_pri = ifelse(is.na(C_PRI_PVEM)==F, 1, 0),
                      coalicion_prdpt = ifelse(is.na(C_PRD_PT)==F, 1, 0))

datos$tot_prd = ifelse(datos$coalicion_prdpt==1, rowSums(datos[c("PRD", "PT", "C_PRD_PT")], na.rm = T), datos$PRD)
datos$tot_pri = ifelse(datos$coalicion_pri==1, rowSums(datos[c("PRI", "PVEM", "C_PRI_PVEM")] ,na.rm = T), datos$PRI)

# ahora si, colapsemos para tener datos a nivel distrito y no a nivel casilla
datos <- group_by(datos, id)
datos <- summarize(datos, pan = sum(PAN, na.rm = T), 
                          tot_pri = sum(tot_pri , na.rm = T), 
                          tot_prd = sum(tot_prd , na.rm = T), 
                          pvem = sum(PVEM, na.rm = T), 
                          pt = sum(PT, na.rm = T), 
                          mc = sum(MOVIMIENTO_CIUDADANO, na.rm = T), 
                          nueva_a = sum(NUEVA_ALIANZA, na.rm = T), 
                          morena = sum(MORENA, na.rm = T), 
                          ph = sum(PH, na.rm = T), 
                          ps = sum(PS, na.rm = T), 
                          cand_ind_1 = sum(CAND_IND_1, na.rm = T), 
                          cand_ind_2 = sum(CAND_IND_2, na.rm = T), 
                          no_r = sum(NO_REGISTRADOS, na.rm = T), 
                          nulos = sum(NULOS, na.rm = T), 
                          tot_votos= sum(TOTAL_VOTOS, na.rm = T), 
                          lista = sum(LISTA_NOMINAL, na.rm = T))

datos = ungroup(datos) # ojo, ungroup es clave
datos = rename(datos, id_distrito = id) 
nrow(datos) # 300 oh por dios, el mismo número que dipudados de mayoría relativa, osea que distritos

#ok pero, ¿quién ganó? aprovechemos para aprender a usar un loop

# primero sumemos las alianzas

# pri = if_else(c_pri_pvem != 0 , sum(c_pri_pvem, pri, pvem), pri) --> sustituye pri cuando c_pri_pvem es distinto de cero con la suma 
# de las tres columnas, sino deja pri

datos <- group_by(datos, id_distrito) 
datos <- mutate(datos, d_votogan=max(pan, tot_pri, tot_prd, pvem, pt, mc, nueva_a, morena, ph, ps, cand_ind_1, cand_ind_2, na.rm=T))

datos$d_gan <- as.character(NA)
datos <- ungroup(datos)

# generé una variable vacía pero caracter

########
# LOOP # 
########

tempo <- select(datos, 2:15) # nos quedamos solo con los competidores 

for(x in 1:nrow(datos)){
  datos$d_gan[x] <- names(tempo)[which.max(tempo[x, 1:ncol(tempo)])]
}

head(datos) # Casi listo! si nos vemos muy exquisitos, podríamos sacar porcentajes. 
rm(tempo)

table(datos$d_gan)

write.csv(datos, paste(dir2, "Elecciones_DipMR2015.csv", sep="/"),row.names=F)


