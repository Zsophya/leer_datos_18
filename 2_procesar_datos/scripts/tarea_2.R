
##############
## Tarea 2 ###
##############

########
### Vamos a practicar poner directorios, importar bases y exportar bases ##
########


# 1.- Crea dos carpetas en tu computadora, llama a una "input" y a otra "output"

# 2.- Crea dos objetos de texto, que contengan la ubicación de estas carpetas, llama a uno "inp" y al otro "out" 
# (También llamado 'poner tus directorios') 


# 3.- Descarga la base EN CSV de Inmigrantes internacionales de este link de CONAPO 

http://www.conapo.gob.mx/es/CONAPO/Proyecciones_Datos 

# que está en el apartado "Bases de Datos" y guárdala en tu carpeta input

# 4.- En el mismo link, descarga el diccionario de datos de esa base la que dice (*) y abrela en tu compu 
#     (para que veas que significa cada variable de la base que descargaste)


# 5.- Tmporta tu base de datos a R usando paste y tu directorio a un objeto llamado datos_inmigrantes 


# 6.- Cámbiale el nombre a la variable de año de datos_inmigrantes por "year" Hint usando la función names y  []


# 7.- Tabula la variable de year y después la variable de ent


# 8.- Quédate sólo con renglones en los que la entidad sea Distrito Federal y el año sea menor a 2019 
#     y guarda esto en una nueva base que se llame "filtrada"
# HINT acuérdate como le podemos decir qué renglones y qué columnas usando  [] y del doble ==


# 9.- ¿Cuántos inmigrantes internacionales ha habido (hombres y mujeres de todas las edades) 
#      en la CDMX desde 1990 hasta el 2018? Hint: busca una función que te sume una variable y acuérdate de usar base$variable


# 10.- Bonus question EXPERTS ONLY máximo peligro ¿Cuántos imigrantes internacionales mayores de edad y hombres hubo en el mismo periodo en la CDMX?


# 11.- Exporta tu base de datos "filtrada" en formato csv a tu carpeta output usando tu directorio out y la función paste


##### Liiisto! wuuuuu! 

# ¿Acabaste con la tarea y se te hizo muy fácil o te quedaste con las ganas de más R? -> http://swirlstats.com/students.html



