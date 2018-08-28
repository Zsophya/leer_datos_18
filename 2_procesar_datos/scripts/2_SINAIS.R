rm(list=ls())
setwd("~")

#######################################################
####  Clase 4: Datos de SINAIS - INEGI defunciones  ###
####  Más dplyr + stringr + LOOPS                   ### 
####           WHAT A TIME TO BE ALIVE!             ###
#######################################################

# En realidad lo que primero se hace en un script es prender los paquetes

install.packages(c("stringr", "pacman"))
require(tidyverse)
require(stringr)
require(foreign) 
??stringr # es muy util tambien googolear cran stringr funciona

    datos = "/Users/carolinatorreblanca/Dropbox (Data4)/Data Civica/Clases/leer_datos_18/2_procesar_datos/input/sinais/csvs"
catalogos = "/Users/carolinatorreblanca/Dropbox (Data4)/Data Civica/Clases/leer_datos_18/2_procesar_datos/input/sinais/catalogo"
      out = "/Users/carolinatorreblanca/Dropbox (Data4)/Data Civica/Clases/leer_datos_18/2_procesar_datos/output"

# La estrategia de hoy va a ser abrir el csv de 2016 - procesarlo y
# luego repretir el proceso de manera automatizada para 2012 a 2016
# loops!    
      
hom_16 = read.csv(paste(datos, "DEFUN16.csv", sep="/"), as.is=T, stringsAsFactors = F)      
str(hom_16) # damn! qué son todas estas variables?
View(hom_16)
nrow(hom_16) # 685mil personas registradas 

# 1er paso es decidir qué variables nos interesan
# estado y municipio ¿pero cuál? depende qué quieras ver, no hay info siempre para los 3
# registro > ocurrencia > residencia

table(hom_16$ENT_REGIS)
table(hom_16$ENT_OCURR) # Perdimos 5
table(hom_16$ENT_RESID) # Perdimos mas de mil

names(hom_16) = tolower(names(hom_16)) 
base = select(hom_16, ent_regis, mun_regis, causa_def, sexo, 
              edad, presunto, escolarida, edo_civil, anio_regis)

str(base) 
table(base$sexo) # 422
table(base$edad) # 3274 no especificados
table(base$escolarida)
table(base$presunto) #  24560 presuntos homicidios
table(base$edo_civil)
str(base$edad) #

# arreglar edad y pegar la escolaridad 
# si empieza en 4 y hasta 120 es la edad
# si es 4998 es no especificado y si empieza en 3 2 1 es meses dias horas

base$edad = as.character(base$edad )

################
# nueva función: ifelse -> si, entonces; si no, entonces
base = mutate(base, tempo = substr(edad, 1,1),
                     edad = ifelse(edad =="4998", NA, 
                            ifelse(tempo =="4", substr(edad, 2, 4), 0)))

table(base$edad) ## y ¿qué le pasó a los 4998's?
table(base$edad, useNA = "ifany") # wuuu son los 3274 que teníamos arriba
base$edad = as.numeric(base$edad)

base$ent_regis = formatC(base$ent_regis, width=2, format="d", flag = "0")     
base$mun_regis = formatC(base$mun_regis, width=3, format="d", flag = "0")     
base$inegi = paste0(base$ent_regis, base$mun_regis)
      
# nos quedamos solo con los homicidios
base = filter(base, presunto ==2)
nrow(base)     
      
# ponemos bien los estados civiles, otra opcion en lugar de hacer ifelse es gsub
base$edo_civil = as.character(base$edo_civil)

base$edo_civil = gsub("1", "Soltero", base$edo_civil)      
base$edo_civil = gsub("2", "Divorciado", base$edo_civil)      
base$edo_civil = gsub("3", "Viudo", base$edo_civil)      
base$edo_civil = gsub("4", "Unión Libre", base$edo_civil)      
base$edo_civil = gsub("5", "Casado", base$edo_civil)      
base$edo_civil = gsub("6", "Separado", base$edo_civil)      
base$edo_civil = gsub("7", "", base$edo_civil)      
base$edo_civil = str_replace(base$edo_civil, "8", "Menor de 12")       # como en la vida misma, 20 maneras de hacer la misma cosa
base$edo_civil = str_replace( base$edo_civil, "9", "No especificado")      

# se ve más feo el código - me late más el if_else para escolaridad
# if_else es lo mismo que ifelse solo que de dplyr y más estricto
# voy a hacer una variable nueva en lugar de editar - más seguro y reecodificar

base = mutate(base, escol = if_else(escolarida==1, "Sin escolaridad",
                            if_else(escolarida==2 | escolarida==3, "Preescolar", 
                            if_else(escolarida==4 | escolarida==5, "Primaria",
                            if_else(escolarida==6 | escolarida==7, "Secundaria",
                            if_else(escolarida==8, "Preparatoria",
                            if_else(escolarida==9, "Licenciatura", 
                            if_else(escolarida==10, "Posgrado",
                            if_else(escolarida==88, "Menor de 3", "No especificado")))))))))
      
table(base$escolarida)     
table(base$escol)      

# por último la causa - gracias a cristo el inegi da un catálogo en .dbf y no lo 
# tenemos que llenar a mano

cat_causa <- read.dbf(paste(catalogos, "CATMINDE.dbf", sep="/"), as.is=T)
cat_causa # pero está en un encoding raro
str(cat_causa)

cat_causa$DESCRIP <- gsub("\xa0", "á", cat_causa$DESCRIP)
cat_causa$DESCRIP <- gsub("\x82", "é", cat_causa$DESCRIP)
cat_causa$DESCRIP <- gsub("\xa1", "í", cat_causa$DESCRIP)
cat_causa$DESCRIP <- gsub("\xa2", "ó", cat_causa$DESCRIP)
cat_causa$DESCRIP <- gsub("\xa4", "ñ", cat_causa$DESCRIP)
cat_causa$DESCRIP <- gsub("\xa3", "ú", cat_causa$DESCRIP)

# Ahora? lo pegamos, pero ojo - se llaman diferente
# Les he mentido, hay otra manera de renombrar variables

cat_causa = rename(cat_causa, causa_def=CLAVE, des_causa=DESCRIP)
     base = left_join(base, cat_causa, by="causa_def") # left_join es otro tipo de join 

### Listo! ahora podríamos agrupar y collapsar para saber totales con cualquier
### cruce de estas variables - pero si queremos ver otros años 
### claramente no vamos a echar el copy paste - vamos a 'programar' un proceso 

############
## LOOPS ###
############     

# queremos abrir uno por uno los csvs - editarlo, guardarlo y pegarlo en una base
# abrir el siguiente etc etc. Paso por paso ¿como podemos abrir los csvs? directorios y vectores

# Paso 1: Hacer una base de datos vacía que vamos a ir rellenando
base_final = data.frame()

# Paso 2: vector de los nombres de archivos - vamos a abrir 1 por 1 
nombres <- c("DEFUN12.csv","DEFUN13.csv","DEFUN14.csv","DEFUN15.csv", "DEFUN16.csv") # les eché todo sinais pero para 2011 y antes cambia la codificacion de escolaridad etc

# Paso 3: esto es para nosotras, es generar un progress bar para ver 
# cuanto le falta a nuestro loop en recorrer todos los archivos del vector de arriba
# ora sí sintaxis de LOOPS - por su atención gracias
# hay 3 maneras de empezar un loop: for, while y repeat - yo uso for el 99% de las veces

# vean
x = 1
nombres[x] # necesitamos que x vaya cambiando

pb = txtProgressBar(min=1, max=length(nombres), style=3)
     for(x in 1:length(nombres)) {
       
    tempo = read.csv(paste(datos, nombres[x], sep="/"), as.is=T, stringsAsFactors = F)      
   
     names(tempo) = tolower(names(tempo)) 
     tempo = select(tempo, ent_regis, mun_regis, causa_def, sexo, 
                    edad, presunto, escolarida, edo_civil, anio_regis)
    
     tempo$ent_regis = formatC(tempo$ent_regis, width=2, format="d", flag = "0")     
     tempo$mun_regis = formatC(tempo$mun_regis, width=3, format="d", flag = "0")     
         tempo$inegi = paste0(tempo$ent_regis, tempo$mun_regis)
               tempo = filter(tempo, as.numeric(presunto) ==2)
     
      base_final = bind_rows(base_final, tempo) # echamos la base de cada año a la base vacía
     
      rm(tempo)
      setTxtProgressBar(pb, x)
  
     }
rm(pb)
### wuuuuuu ahora tienen una base gigante de
nrow(base_final) # 114366 personas asesinadas en el periodo

####################################################################################
# ahora sí editamos todas las variables 1 sola vez pero para tooooodoso los años ###
####################################################################################

base_final$edad = as.character(base_final$edad )

base_final = mutate(base_final, tempo = substr(edad, 1,1),
                                 edad = ifelse(edad =="4998", NA, 
                                        ifelse(tempo =="4", substr(edad, 2, 4), 0)))

base_final$edad = as.numeric(base_final$edad)

base_final$edo_civil = as.character(base_final$edo_civil)
base_final$edo_civil = gsub("1", "Soltero", base_final$edo_civil)      
base_final$edo_civil = gsub("2", "Divorciado", base_final$edo_civil)      
base_final$edo_civil = gsub("3", "Viudo", base_final$edo_civil)      
base_final$edo_civil = gsub("4", "Unión Libre", base_final$edo_civil)      
base_final$edo_civil = gsub("5", "Casado", base_final$edo_civil)      
base_final$edo_civil = gsub("6", "Separado", base_final$edo_civil)      
base_final$edo_civil = gsub("7", "", base_final$edo_civil)      
base_final$edo_civil = gsub("8", "Menor de 12", base_final$edo_civil)      
base_final$edo_civil = gsub("9", "No especificado", base_final$edo_civil)      

base_final = mutate(base_final, escol = if_else(escolarida==1, "Sin escolaridad",
                                        if_else(escolarida==2 | escolarida==3, "Preescolar", 
                                        if_else(escolarida==4 | escolarida==5, "Primaria",
                                        if_else(escolarida==6 | escolarida==7, "Secundaria",
                                        if_else(escolarida==8, "Preparatoria",
                                        if_else(escolarida==9, "Licenciatura", 
                                        if_else(escolarida==10, "Posgrado",
                                        if_else(escolarida==88, "Menor de 12", "No especificado")))))))))

base_final$sexo = gsub("1", "Hombre", as.character(base_final$sexo))
base_final$sexo = gsub("2", "Mujer", as.character(base_final$sexo))
base_final$sexo = gsub("9", "No especificado", as.character(base_final$sexo))

base_final = left_join(base_final, cat_causa, by="causa_def") # left_join es otro tipo de join 
# yo les recomendaría exportar una versión raw de esta base

write.csv(base_final, paste(out, "base_cruda.csv", sep="/"), row.names = F, fileEncoding = "UTF-8")

#####################################################
## ahora podemos hacer colapses para ver cruces #####
#####################################################

base_final$tot = 1
por_sexo_escol = group_by(base_final, escol, sexo)
por_sexo_escol = summarize(por_sexo_escol, tot = sum(tot, na.rm=T))

por_sexo_escol = ungroup(por_sexo_escol)
por_sexo_escol = group_by(por_sexo_escol, sexo)
por_sexo_escol = mutate(por_sexo_escol, tot_por_sexo = sum(tot, na.rm=T),
                                        porcent = round(tot / tot_por_sexo * 100, digits=1))

por_sexo_escol = arrange(por_sexo_escol, -porcent)
View(por_sexo_escol)
rm(por_sexo_escol)

por_sexo_civil = group_by(base_final, edo_civil, sexo)
por_sexo_civil = summarize(por_sexo_civil, tot = sum(tot, na.rm=T))

por_sexo_civil = ungroup(por_sexo_civil)
por_sexo_civil = group_by(por_sexo_civil, sexo)
por_sexo_civil = mutate(por_sexo_civil, tot_por_sexo = sum(tot, na.rm=T),
                                             porcent = round(tot / tot_por_sexo * 100, digits=1))

por_sexo_civil = arrange(por_sexo_civil, sexo, -porcent)
rm(por_sexo_civil)

#####################################################
## pero lo cool es lo que podemos saber de las  #####
## causas de muerte: modo y ubucación           #####
#####################################################

# vamos a usar nuestra nueva función str_detect de stringr

table(base_final$des_causa)

###################
## Arma de Fuego ## 
###################

base_final$arma_fuego_1 = as.numeric(str_detect(base_final$des_causa, "armas de fuego"))
base_final$arma_fuego_2 = as.numeric(str_detect(base_final$des_causa, "arma larga"))
base_final$arma_fuego_3 = as.numeric(str_detect(base_final$des_causa, "arma corta"))
base_final$arma_fuego <-  rowSums(base_final[c("arma_fuego_1","arma_fuego_2", "arma_fuego_3")])
base_final$arma_fuego_1 <- base_final$arma_fuego_2 <- base_final$arma_fuego_3 <- NULL

##########################
# Ahorcados o Ahogados ###
##########################

base_final$ahorcamiento_1 = as.numeric(str_detect(base_final$des_causa, "ahorcamiento"))
base_final$ahorcamiento_2 = as.numeric(str_detect(base_final$des_causa, "ahogamiento"))
base_final$ahorcamiento <-  rowSums(base_final[c("ahorcamiento_1", "ahorcamiento_2")])
base_final$ahorcamiento_1 <- base_final$ahorcamiento_2 <- NULL

################################
### vivienda vs vía pública ####
################################

# lugares: 
#  en áreas de deporte y atletismo, áreas de deporte y atletismo / publica 
#  en calles y carreteras  / publica
#  en comercio y área de servicios / publica 
#  en escuelas, otras instituciones y áreas administrativas  públicas / publica
#  en granja / 
#  en institución residencial / vivienda
#  en lugar no especificado / no especificado 
#  en otro lugar especificado  / otro - 
#  en vivienda / vivienda
#  en área industrial y de la construcción / publica

#vivienda 
base_final$vivienda_1 = as.numeric(str_detect(base_final$des_causa, "institución residencial"))
base_final$vivienda_2 = as.numeric(str_detect(base_final$des_causa, "vivienda"))
base_final$vivienda <-  rowSums(base_final[c("vivienda_1", "vivienda_2")])
base_final$vivienda_1 <- base_final$vivienda_2 <- NULL

#################
# Envenenadas ###
#################

base_final$veneno_1 = as.numeric(str_detect(base_final$des_causa, "drogas, medicamentos y sustancias biológicas"))
base_final$veneno_2 = as.numeric(str_detect(base_final$des_causa, "productos químicos y sustancias nocivas"))
base_final$veneno_3 = as.numeric(str_detect(base_final$des_causa, "plaguicidas")) # dudosa igual
base_final$veneno_4 = as.numeric(str_detect(base_final$des_causa, "sustancia corrosiva")) # estoy dudosa si incluir
base_final$veneno   =  rowSums(base_final[c("veneno_1", "veneno_2", "veneno_3", "veneno_4")])
base_final$veneno_1 = base_final$veneno_2 <- base_final$veneno_3 <- base_final$veneno_4 <- NULL

#####################
# Fuerza corporal ###
#####################

base_final$fuerza_corporal= as.numeric(str_detect(base_final$des_causa, "fuerza corporal"))

#############################
# humo fuego llamas       ###
# vapor y cosas calientes ###
#############################

base_final$fuego_vapor_1 = as.numeric(str_detect(base_final$des_causa, "vapor de agua, vapores y objetos calientes"))
base_final$fuego_vapor_2 = as.numeric(str_detect(base_final$des_causa, "gases y vapores"))
base_final$fuego_vapor_3 = as.numeric(str_detect(base_final$des_causa, "humo, fuego y llamas"))
base_final$fuego_vapor   = rowSums(base_final[c("fuego_vapor_1", "fuego_vapor_2", "fuego_vapor_3")])
base_final$fuego_vapor_1 = base_final$fuego_vapor_2 <- base_final$fuego_vapor_3  <- NULL

############################
# Arma blanca            ###
# objeto cortante / romo ###
############################

base_final$arma_blanca_1 = as.numeric(str_detect(base_final$des_causa, "objeto cortante"))
base_final$arma_blanca_2 = as.numeric(str_detect(base_final$des_causa, "objeto romo o sin filo"))
base_final$arma_blanca   = rowSums(base_final[c("arma_blanca_1", "arma_blanca_2")])
base_final$arma_blanca_1 <- base_final$arma_blanca_2  <- NULL 


causas = group_by(base_final, anio_regis, sexo)
causas = summarize(causas, arma_fuego = sum(arma_fuego, na.rm=T),
                           ahorcamiento = sum(ahorcamiento, na.rm=T),
                           vivienda = sum(vivienda, na.rm=T),
                           veneno = sum(veneno, na.rm=T),
                           arma_blanca = sum(arma_blanca, na.rm = T),
                           fuerza_corporal = sum(fuerza_corporal, na.rm=T), 
                           total = sum(tot, na.rm=T))

causas = ungroup(causas)
causas = mutate(causas, porcent_vivienda = round(vivienda / total * 100, digits=1),
                        porcent_armafuego = round(arma_fuego / total * 100, digits=1),
                        porcent_ahorca = round(ahorcamiento / total * 100, digits=1))

## o cualquier porcentaje que les inspire - ya pueden usar SINAIS
## y hacer mil cruces y usar loops !! wuuuuuuuuuuuuuuuuuuuu

write.csv(causas, paste(out, "porcents.csv"), row.names = F, fileEncoding = "UTF-8")











