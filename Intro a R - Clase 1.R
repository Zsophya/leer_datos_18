rm(list=ls())  ## estoy limpiando mi sesión de todo

########################
# Intro a R          ###
# Clase 1            ###  
# Objetos y bases    ###
# Caro 14/02/18      ### 
########################

# 1. Everything that exists is an object. 2. Everything that happens is a function call.

####  Esto es R / donde estoy escribiendo se llama Script,
####  y es literalmente un guion de comandos en orden que va a ejecutar R 
####  en el orden que se los escriba / aquí no se corre nada - todo se corre en la consola
####  Vean, si escriben con un # antes la consola entiende que es un comentario
##### pero si no, lo entiend como comando. Ej:

3
3*6
30 * 600 / (6^8)

error error error # entiende números pero no / Asi se ven los mensajes de ERROR 

# Puedes solo usar la consola para hacer cálculos o puedes guardar esos cálculos en "objetos"
# Por ejemplo, una base de datos es un tipo de objeto

#####################################
######## Tipos de Objetos ###########  
#####################################

## Numéricos ##

# Literal números / puede ser 1 solo número o varios números
un_numero = 3 + 7
un_numero # R ahora se acuerda de un objeto que se llama "un_numero" que es igual a 10

varios_numeros = c(3, 4, 5, 6, 7) # en R, para declarar que algo es un vector le ponemos "c" 
varios_numeros
varios_numeros[2]
varios_numeros[3]

otros_numeros = c(1, 2, 3, 4, 5)
otros_numeros

ultimos_numeros = varios_numeros - otros_numeros # podemos hacer operaciones usando sus nombres
ultimos_numeros

un_buen_de_numeros = c(123:500) # ¿Qué me va a crear?
un_buen_de_numeros

# Nosotras vamos a usar * variables * numericas, que son una forma de vector 

# Hagan un vector númerico del 20 al 30 y llámenlo nuevo_vector aquí:

nuevo_vector = c(20:30)
nuevo_vector

rm(un_numero, varios_numeros, otros_numeros, un_buen_de_numeros, nuevo_vector) # rm es de remove 

####
## Caracteres o Strings ##
####

# Algunas variables en las bases no son número, sino texto - R las maneja chido **dentro de objetos**

mi_nombre = "Carolina" # las comillas le dicen a R que es un caracter
mi_apellido = "Torreblanca"

# igual que en variables numericas, puedes hacer vectores concatenados de texto
mi_edad = c("Tengo", "26", "años") # OJO: el 26 es un caracter para R
mi_edad  
# y manipular los objetos  / por ejemplo: nuestra primera función

nombre_completo = paste(mi_nombre, mi_apellido, sep=" ")
nombre_completo # ¿Este es un vector? ¿Cual es la diferencia?
  
# Creen un solo objeto que contenga nombre y apellido "paste":
  
 rm(mi_apellido, mi_nombre, mi_edad) 
 
 ####
 ## Objetos Lógicos ##
 ####  

# Estos son un poco dificiles de entender pero super útiles - Son Verdadero o Falso 
 
 vector_logico = c(TRUE, T, T, FALSE) # STATA APESTA
 vector_logico
 
# nos van a servir un buen cuando sean expertasas para preguntarle a R cosas como
# ¿Esa observacion tiene un valor faltante?  Pero ahorita no se estresen 

 ####
 ## Factores ##
 ####  
 
# los factores son objetos que en su corazoncito son numéricos 
# sobre los cuales hay una "etiqueta" de texto pegada que es la que vemos
 
# tons empiezas con un vector numerico 
 numeros = 1:3
 numeros
 str(numeros) ## OJO una función básica en la vida: structure
 
 # y le pegas las etiquetas
 numeros = factor(numeros, labels = c("bajo", "medio", "alto"))
 numeros
 str(numeros)
 
# parece como un objeto muy innecesario, no? pero no por ejemplo
 set.seed(42)
 numeros_random = sample(1:3, 10, replace=T) # quiero 10 numeros random entre 1 y 3
 numeros_random
 numeros_random = factor(numeros_random, labels = c("bajo", "medio", "alto"))
# esto es lo que hacen en las encuestas 
# DONT PANIC si no entienden para qué les va servir ahorita 
 
 ####
 ## Matrices ##
 ####  
 
 # Las matrices son objetos de 2 dimensiones: largo y ancho ¿A qué les recuerda?
 # La sintaxis de matrices es 'cuantos renglones, cuantas columnas' 
 
 futura_matriz = 1:10 
 futura_matriz  # es un vector, no? un vector tiene solo 1 atributo: largo
 length(futura_matriz) # es un vector de 10 elementos, pero yo los quiero acomodar en una matriz
 
 matriz_1 =  matrix(futura_matriz, nrow=5, ncol=2)
 matriz_1
 str(matriz_1)
 
 # la pude haber acomodado diferente? obvio
 matriz_2 =  matrix(futura_matriz, nrow=2, ncol=5)
 matriz_2
 
 # pero si quiero hacer una matriz de 10 x 10 no me alcanzan los numeros
 matriz_3 = matrix(futura_matriz, 10, 10)
 matriz_3 # ¿qué hace? los repite / pero intenten hacer una de 8 x 7 con esos numeros
 # solo llena con repeticiones cuando son multiplos 

 futura_matriz_2 = c("hola", "amigas", "las quiero", "mil", "millones")
 matriz_4 = matrix(futura_matriz_2, 5, 5) # 
 matriz_4 # lol 
 
 # podemos hacer subsets de matrices usando su sintaxis,  renglon, columna
 subset_1 = matriz_4[1:2,1:2] # ¿Qué va a salir?
 subset_1
 
 subset_2 = matriz_4[1:2,] # ¿Qué va a salir?
 subset_2 
   
 subset_3 = matriz_4[,3:5] # ¿Qué va a salir?
 subset_3    
   
rm(subset_1, subset_2, subset_3, matriz_1, matriz_2, matriz_3) 
   
####
## BASES DE DATOS ##
####    
   
# Ahora sí, lo mero bueno - bases de datos 
# son como matrices pero tienen otros atributos, como variables y nombres de variables

# empecemos con una matriz random de 100 numeros entre el 0 y el 50
futura_base = matrix(sample(0:50, 100, replace=T), nrow=20, ncol=5)
futura_base

# lo unico que tenemos que decirle es que es una base de datos
base = as.data.frame(futura_base)
base # qué tiene de diferente?
str(base)

# FUNCIONES QUE VAN A SER SUS NUEVAS MEJORES AMIGAS:

names(base) # otra funcion basica, te regresa un vector de los nombres
names(base)[3] # qué me va a dar? el tecer elemento, como en un vector nomral
head(base)
tail(base, 12)
summary(base) # otro perk de las bases de datos 
table(base$V4) # usamos $ para especificarle la variable dentro de una base
table(V4)

# Tirar variables 
base$V1 = NULL
str(base)

# Crear variables (manera más rústica)
base$nueva_variable = "Variable inutil"
base

base$nueva_variable = as.character("Variable inutil")

names(base)[1] = "esperanza_de_vida"
base

# Nos vamos a pasar la vida entera jugando con bases asi que por el momento pausa 

#################################################
#### DIRECTORIOS E IMPORTAR BASES DE DATOS A R ##
#################################################

# Un directorio no es más que el nombre completo de un archivo / donde está guardado
# Ustedes van a importar y exportar sus archivos usando directorios ¿Por?
# Porque es buena practica, porque otra gente puede replicar lo que tu hiciste
# Porque vas a jalar archivos de varios lugares
# Porque hay que ser ordenados en la vida
# Porque NUNCA vas a editar un archivo original

# Si están usando MAC: hagan todo como yo
# Si están usando PC: cuando yo ponga "/" ustedes ponen "\\" 

# Ok, vean la carpeta de la clase, tiene subfolders no? queremos un directori para cada subfolder
# de algunos vamos a jalar datos, en otros vamos a guardar bases editadas

datos = "/Users/carolinatorreblanca/Dropbox (Data4)/ICAitam/ICAmerino/Módulo 1 Procesar datos/Intro a R/datos"
datos # ¿esto qué es? uh hu, es texto - para qué m*erg*s quiero un objeto de texto con la ubicacion de mis archivos?
# para no tener que escribir ese choro una y otra vez mil veces - solo lo pones 1 vez
out = "/Users/carolinatorreblanca/Dropbox (Data4)/ICAitam/ICAmerino/Módulo 1 Procesar datos/Intro a R/out"

# voy a importar el archivo llamado "pobreza_necesaria.csv"
# pero pobreza_necesaria.csv no es su nombre completo, no no
# su nombre completo es "/Users/carolinatorreblanca/Dropbox (Data4)/Data Civica/Clases/R_genero/Clase1/datos/pobreza_necesaria.csv"
# osea le tengo que decir a R qué archivo  donde está ese archivo, R no es adivino

# quiero importar esa base en un objeto que se llame mi_primer_base 
# le tengo que decir en qué formato está la base / no es adivino / es un csv

mi_primer_base = read.csv(paste(datos, "pobreza_necesaria.csv", sep="/"))
paste(datos, "pobreza_necesaria.csv", sep="/")

str(mi_primer_base) # 
head(mi_primer_base)
names(mi_primer_base)
class(mi_primer_base)

# la puedo exportar en out / le tengo que decir qué objeto y como quiero que se llame
write.csv(mi_primer_base, paste(out, "chonita.csv", sep="/"))
rm(mi_primer_base)

# Hay miles de formatos de bases de datos, ya vimos como abrir un csv / ahora un txt
# csv es un comma separatede values - pero puedes separar tus valores con lo que quieras
# el sadico IFE publica en .txt sus datos separados por un |
# el comando es read.delim

ife = read.delim(paste(datos, "dip2006.txt", sep="/"), sep="|") # ¿por qué dos sep's? 
str(ife)
View(ife)

# podemos editar rapido la base y exportarla en otro formato
names(ife) = tolower(names(ife)) # ¿qué hice acá?

write.csv(ife, paste(out, "chonita2.csv", sep="/"))
rm(ife)
# Ok, ahora quiero importar un excel pero ¿qué creen?
# R no sabe abrir exceles "nativamente" no tiene esa funcion
# Lo bueno es que un buen de nerds escriben funciones para R solo hay que descargarlas y "prenderlas" en nuestra sesión

# para descargar funciones (paquetes en realidad) sol ohay que hacer esto:
install.packages("readxl")

# aunque lo tengas instalado SOLO VA A SERVIR SI LO ACTIVAS EN TU SESION 
# siempre siempre que abras R tienes que prender los paquetes que vas a ocupar

require(readxl)
# ahora sí - importamos un excel
un_excel = read_excel(paste(datos, "snsp_mun_nm1517.xlsx", sep="/"))
# tarda un buen, relax
# ese warning es importante? nel - moving on

str(un_excel) # exacto, datos del Secretariado 
table(un_excel$`Bien jurídico afectado`)
table(un_excel$`Tipo de delito`)

# me voy a quedar solo con la falsedad porque la vida es cruel 
# voy a hacer una base llamada filtrado 
filtrado = un_excel[un_excel$`Tipo de delito`=="Falsedad" & un_excel$`Tipo de delito`=="Violencia familiar",]
table(filtrado$`Tipo de delito`)

#¿Qué le estoy diciendote? Quedate con los renglones donde tipo de delito sea igual a falsedad
# noten el doble ==  y con TOOODas las columnas
# si no entendieron no importa, lo vamos a ver mucho mejor despues

# exporto / 
# ahora como delim y yo manualmente le digo que delimite con comas (osea que haga un csv) porque ¡te regalo anarquía!
write.table(filtrado, paste(out, "filtrados.csv", sep="/"), sep=",")

# obvio tambien puedes guardar en exceles write_excel o el formato que quieras 

#########################################
## OTROS PAQUETES PARA IMPORTAR BASES ###
#########################################

 # Hay otros formatos no nativos a R que se van a topar por el mundo
 # como .dta de STATA , .dbf o spss 
 # con el paquete foreign pueden leer la mayoría y si no en google les dicen que paquete necesitan
 # para leer stata 13 necesitan readstata13

install.packages(c("foreign", "readstata13")) # los dos de un jalón
# acuérdense de prenderlos antes de usarlos con require() o library()





