rm(list=ls())
setwd("~")

###########################
#  Graficar con GGplot2 ###
#       9/03/18         ###
###########################

# R nació sabiendo cómo graficar - 
# Pero las gráficas que sabe hacer por default son difíciles o feas o ambas y su sitnaxis es complicada / 
# El tipo que escribió dplyr escibió un paquete  para graficar 
# Grammar of Graphics - es parte del paquete tidyverse
require(pacman)
p_load(ggmosaic, ggrepel, treemapify, tidyverse)


## Directorios ###

input = "/Users/carolinatorreblanca/Dropbox (Data4)/Data Civica/Clases/leer_datos_18/3_graficas/datos"
grafs = "/Users/carolinatorreblanca/Dropbox (Data4)/Data Civica/Clases/leer_datos_18/3_graficas/grafs"

## Vamos a importar 3 bases que habíamos procesado ya en otras clases ###

sinais = read.csv(paste(input, "base_cruda.csv", sep="/"), as.is = T, stringsAsFactors = F, fileEncoding = "UTF-8")
envipe = read.csv(paste(input, "envipe_grafs.csv", sep="/"), as.is = T, stringsAsFactors = F, fileEncoding = "UTF-8")
snsp   = read.csv(paste(input, "tasa_feminicidos_estatal.csv", sep="/"), as.is = T, stringsAsFactors = F, fileEncoding = "UTF-8")

## GGplot2 funciona con 3 cosas: 
# 1) Qué base 
# 2) Qué geom o figura 
# 3) Qué ejes, Titulos etc. en el aesthethic
## Para decirle qué gráfica queremos hacer - se lo decimos con el geom. 

View(envipe) # ¿Cómo graficarían esto? 

# ggplot(data = que data, aes(x = que en equis, y = que en equis, color = que variable da el color), otras opciones)
# emepzamos siempre con ggplot()
ggplot(data=envipe) # si corro solo esto no pasa nada - no le he dicho que haceeeer solo me hace un marco vacío

ggplot(data=envipe, aes(x = trabajo, y= Hombres)) # qué pasa? ya le dijimos qué ejes y que data pero no qué grafica

###############
##  Barras  ###
## geom_bar ###
###############

ggplot(data=envipe, aes(x = trabajo, y= Hombres)) +
geom_bar(stat="identity") # listo! hicimos la grafica más fea del mundo
# qué fregados es stat? es qué estadistico statistic quieres que ggplot haga para graficar 
# identity es nada

# vean - es lo mismo eso que esto:
ggplot() +
geom_bar(data=envipe, aes(x = trabajo, y= Hombres), stat="identity")
# Cual usar? por ahora el que quieran, eventualmente van a entender la utilidad de esa flexibilidad

ggplot() +
geom_bar(data=envipe, aes(x = trabajo, y= Hombres, color =trabajo), stat="identity") # que hace color

ggplot() +
geom_bar(data=envipe, aes(x = trabajo, y= Hombres, fill =trabajo), stat="identity") # qué hace fill

# y si queremos saber el porcentaje? 
ggplot(data=envipe, aes(x = trabajo, y= Hombres)) +
geom_text(aes(label=Hombres))  # ¿podemos sobreponer estas dos cosas? obvio - con un +

ggplot(data=envipe, aes(x = trabajo, y= Hombres)) +
geom_text(aes(label=Hombres)) +
geom_bar(aes(fill =trabajo), stat="identity") # algunas geoms necesitan aes específicos - como label -

ggplot(data=envipe, aes(x = trabajo, y= Hombres)) +
geom_bar(aes(fill =trabajo), stat="identity") +
geom_text(aes(label=Hombres))  # IMPORTA EL ORDEN EN GGPLOT 2 VA SOBREPONIENDO LAS COSAS

# Y MUJERES?
  ggplot(data=envipe) +
  geom_bar(aes(x = trabajo, y= Hombres, fill =trabajo), stat="identity") +
  geom_bar(aes(x = trabajo, y= Mujeres, fill =trabajo), stat="identity") +
  geom_text(aes(x = trabajo, y= Hombres, label=Hombres)) +
  geom_text(aes(x = trabajo, y= Mujeres, label=Mujeres)) # ya vieron? lo que ponemos adentro de ggplot() aplica pa todo / a veces no queremos eso

# ¿Por qué no sirve?  # CLAVE DE HACER GRAFS: entender cómo necesitas la base 
envipe = gather(envipe, sexo, porcentaje, Hombres, Mujeres)

ggplot(data=envipe, aes(x = sexo, y= porcentaje, fill=trabajo)) +
geom_bar(stat="identity")
# O 
ggplot(data=envipe, aes(x = trabajo, y= porcentaje, fill=sexo)) +
geom_bar(stat="identity") # PERO NADA SUMA MAS DE 100%! es confuso

# positions - dodge o stack - bastante self-explanatory - OBVIO van afueral del aes() / cómo acomodarlo
ggplot(data=envipe, aes(x = trabajo, y= porcentaje, fill=sexo)) +
geom_bar(stat="identity", position="dodge")

ggplot(data=envipe, aes(x = trabajo, y= porcentaje, fill=sexo)) +
geom_bar(stat="identity", position="dodge") + 
geom_text(aes(label=porcentaje)) # hmm .. you got it - tambien tenemos que decirle a text que es dodge

ggplot(data=envipe, aes(x = trabajo, y= porcentaje, fill=sexo)) +
geom_bar(stat="identity", position="dodge") + 
geom_text(aes(label=porcentaje), position=position_dodge(width=1))

# reorder o factor para reordenar 
ggplot(data=envipe, aes(x = reorder(trabajo, -porcentaje), y= porcentaje, fill=sexo)) +
geom_bar(stat="identity", position="dodge") + 
geom_text(aes(label=porcentaje), position=position_dodge(width=1))
  
envipe = arrange(envipe,  sexo,-porcentaje)
envipe$trabajo = factor(envipe$trabajo)

ggplot(data=envipe, aes(x = trabajo, y= porcentaje, fill=sexo)) +
geom_bar(stat="identity", position="dodge") + 
geom_text(aes(label=porcentaje), position=position_dodge(width=1))

# Ya casiiiiiii 
ggplot(data=envipe, aes(x = trabajo, y= porcentaje, fill=sexo)) +
geom_bar(stat="identity", position="dodge") + 
geom_text(aes(label=porcentaje), position=position_dodge(width=1)) + 
labs(title ="Porcentaje de Personas según su ocupación en la última semana", 
     subtitle="por sexo", x = "", y = "Porcentaje", caption = "Fuente: Envipe 2017") + 
theme_minimal() +
theme(axis.text.x = element_text(angle=90, hjust =1),
      title  = element_text(face="bold", size=14)) 
# esta gráfica esta MUY decente - útima nerdes - los colores 
# podemos darle un scale fill de manera manual
# http://colorbrewer2.org/
  
ggplot(data=envipe, aes(x = trabajo, y= porcentaje, fill=sexo)) +
geom_bar(stat="identity", position="dodge") + 
geom_text(aes(label=porcentaje, color=sexo), position=position_dodge(width=1), vjust=-1) + 
scale_fill_manual(values=c("#253494","#bd0026")) +
labs(title ="Porcentaje de Personas según su ocupación en la última semana", 
       subtitle="por sexo", x = "", y = "Porcentaje", caption = "Fuente: Envipe 2017") + 
theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust =1),
        title  = element_text(face="bold", size=14))  
  
# cómo le cambiamos manualmente el color al texto ? IDEAS ? 

ggplot(data=envipe, aes(x = trabajo, y= porcentaje, fill=sexo)) +
  geom_bar(stat="identity", position="dodge") + 
  geom_text(aes(label=porcentaje, color=sexo), position=position_dodge(width=1), vjust=-1) + 
  scale_fill_manual(values=c("#1b7837","#762a83")) +
  scale_color_manual(values=c("#1b7837","#762a83")) +
  labs(title ="Porcentaje de Personas según su ocupación en la última semana", 
       subtitle="por sexo", x = "", y = "Porcentaje", caption = "Fuente: Envipe 2017") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust =1),
        title  = element_text(face="bold", size=14)) 
ggsave(paste(grafs, "1_barras.png", sep="/"), width=12, height=12)

#################
##  Scatters  ###
## geom_point ###
#################

View(snsp) # quiero un scatter de tasa_total vs tasa_arma_fuego 

ggplot(snsp) +
geom_point(aes(x = tasa_total, y = tasa_arma_fuego), color ="red") # differencia con color afuera?

ggplot(snsp) +
geom_point(aes(x = tasa_total, y = tasa_arma_fuego), color ="blue", size = 3) +
labs(title ="Tasa de Feminicidios por 100 mil mujeres", 
     subtitle="Total vs. Cometidos con Arma de Fuego", x = "Tasa Total", 
     y = "Tasa Arma de Fuego", caption = "Fuente: SENSP 2018") +
theme_light() 
ggsave(paste(grafs, "2_scatter_1.png", sep="/"), width=12, height=12)
# super super exportable !  

table(snsp$year) # 3 años

ggplot(snsp) +
geom_point(aes(x = tasa_total, y = tasa_arma_fuego, color=as.factor(year)), size = 3) +
scale_color_manual(values=c("#7fcdbb","#1d91c0", "#081d58")) +
labs(title ="Tasa de Feminicidios por 100 mil mujeres", 
       subtitle="Total vs. Cometidos con Arma de Fuego", x = "Tasa Total", 
       y = "Tasa Arma de Fuego", caption = "Fuente: SENSP 2018", color ="Año") +
theme_bw()

# manualmente darle la escala de Y y de X
summary(snsp$tasa_arma_fuego)
summary(snsp$tasa_total)

ggplot(snsp) +
geom_point(aes(x = tasa_total, y = tasa_arma_fuego, color=as.factor(year)), size = 3) +
scale_color_manual(values=c("#7fcdbb","#1d91c0", "#081d58")) +
labs(title ="Tasa de Feminicidios por 100 mil mujeres", 
       subtitle="Total vs. Cometidos con Arma de Fuego", x = "Tasa Total", 
       y = "Tasa Arma de Fuego", caption = "Fuente: SENSP 2018", color ="Año") +
theme_bw() +
scale_y_continuous(breaks=seq(from=0, to=7, by=.2)) + 
scale_x_continuous(breaks=seq(from=0, to=7, by=.2)) 
ggsave(paste(grafs, "3_scatter_2.png", sep="/"), width=12, height=12)  

# scatter ponderado - modificar el size del punto de acuerdo a una tercer variable - como total
ggplot(snsp) +
  geom_point(aes(x = tasa_total, y = tasa_arma_fuego, size = total, color=as.factor(year))) +
  scale_color_manual(values=c("#7fcdbb","#1d91c0", "#081d58")) +
  labs(title ="Tasa de Feminicidios por 100 mil mujeres", 
       subtitle="Total vs. Cometidos con Arma de Fuego", x = "Tasa Total", 
       y = "Tasa Arma de Fuego", caption = "Fuente: SENSP 2018", color ="Año", size="Total Feminicidios") +
  theme_bw() +
  scale_y_continuous(breaks=seq(from=0, to=7, by=.2)) + 
  scale_x_continuous(breaks=seq(from=0, to=7, by=.2)) # mega cool, no?
ggsave(paste(grafs, "4_scatter_ponderado.png", sep="/"), width=12, height=12)  

## ajuste lineal / regresión! 

ggplot(snsp) +
geom_point(aes(x = tasa_total, y = tasa_arma_fuego, color=as.factor(year), size = total)) +
geom_smooth(aes(x= tasa_total, y= tasa_arma_fuego), method="lm", se=F, color="pink") +
scale_color_manual(values=c("#7fcdbb","#1d91c0", "#081d58")) +
labs(title ="Tasa de Feminicidios por 100 mil mujeres", 
       subtitle="Total vs. Cometidos con Arma de Fuego", x = "Tasa Total", 
       y = "Tasa Arma de Fuego", caption = "Fuente: SENSP 2018", color ="Año") +
  theme_bw() +
  scale_y_continuous(breaks=seq(from=0, to=7, by=.2)) + 
  scale_x_continuous(breaks=seq(from=0, to=7, by=.2))

# ajuste cuadrático CON errores estandar
ggplot(snsp) +
  geom_point(aes(x = tasa_total, y = tasa_arma_fuego, color=as.factor(year), size = total)) +
  geom_smooth(aes(x= tasa_total, y= tasa_arma_fuego), method="lm", formula=y ~ x + I(x^2), color="pink") +
  scale_color_manual(values=c("#7fcdbb","#1d91c0", "#081d58")) +
  labs(title ="Tasa de Feminicidios por 100 mil mujeres", 
       subtitle="Total vs. Cometidos con Arma de Fuego", x = "Tasa Total", 
       y = "Tasa Arma de Fuego", caption = "Fuente: SENSP 2018", color ="Año") +
  theme_bw() +
  scale_y_continuous(breaks=seq(from=0, to=7, by=.2)) + 
  scale_x_continuous(breaks=seq(from=0, to=7, by=.2))

# si quisiéramos que saliera texto en cada punto, como el nombre del estado o la tasa total - geom_text ?
# sí se puede, pero normalmente se hace un cagadero ´

ggplot(snsp) +
  geom_point(aes(x = tasa_total, y = tasa_arma_fuego, color=as.factor(year)), size = 3) +
  geom_text(aes(x = tasa_total, y = tasa_arma_fuego, label=ent), size=3) +
  scale_color_manual(values=c("#7fcdbb","#1d91c0", "#081d58")) +
  labs(title ="Tasa de Feminicidios por 100 mil mujeres", 
       subtitle="Total vs. Cometidos con Arma de Fuego", x = "Tasa Total", 
       y = "Tasa Arma de Fuego", caption = "Fuente: SENSP 2018", color ="Año") +
  theme_bw() +
  scale_y_continuous(breaks=seq(from=0, to=7, by=.2)) + 
  scale_x_continuous(breaks=seq(from=0, to=7, by=.2)) 

# ggrepel El paquete que prendimos / instalamos hasta arriba

ggplot(snsp) +
  geom_point(aes(x = tasa_total, y = tasa_arma_fuego, color=as.factor(year)), size = 3) +
  geom_text_repel(aes(x = tasa_total, y = tasa_arma_fuego, label=ent), size=3) + # la ÚNICA diferencia es el geom
  scale_color_manual(values=c("#7fcdbb","#1d91c0", "#081d58")) +
  labs(title ="Tasa de Feminicidios por 100 mil mujeres", 
       subtitle="Total vs. Cometidos con Arma de Fuego", x = "Tasa Total", 
       y = "Tasa Arma de Fuego", caption = "Fuente: SENSP 2018", color ="Año") +
  theme_bw() +
  scale_y_continuous(breaks=seq(from=0, to=7, by=.2)) + 
  scale_x_continuous(breaks=seq(from=0, to=7, by=.2)) 
## mejor, pero not quite - probablemente solo quiereas los outliers 

          ggplot() +
          geom_point(data=snsp, aes(x = tasa_total, y = tasa_arma_fuego, color=as.factor(year)), size = 3) +
          geom_text_repel(data=filter(snsp, tasa_total > 2.5 | tasa_arma_fuego > .6), aes(x = tasa_total, y = tasa_arma_fuego, label=ent), size=3) + # por eso la flexibilidad de poner mil datas
          scale_color_manual(values=c("#7fcdbb","#1d91c0", "#081d58")) +
          labs(title ="Tasa de Feminicidios por 100 mil mujeres", 
               subtitle="Total vs. Cometidos con Arma de Fuego", x = "Tasa Total", 
               y = "Tasa Arma de Fuego", caption = "Fuente: SENSP 2018", color ="Año") +
          theme_bw() +
          scale_y_continuous(breaks=seq(from=0, to=7, by=.2)) + 
          scale_x_continuous(breaks=seq(from=0, to=7, by=.2))  
ggsave(paste(grafs, "5_scatter_repel.png", sep="/"), width=12, height=12)  

################
## geom_line ###
##   Lineas  ###
################

sinais_lineas = group_by(sinais, anio_regis, sexo)
sinais_lineas$tot = 1
sinais_lineas = summarize(sinais_lineas, tot_personas = sum(tot, na.rm=T))

sinais_lineas = filter(sinais_lineas, sexo !="No especificado")
sinais_lineas$sexo = gsub("1", "Hombre", sinais_lineas$sexo)
sinais_lineas$sexo = gsub("2", "Mujer", sinais_lineas$sexo)

ggplot(sinais_lineas) +
geom_line(aes(x=anio_regis, y=tot_personas, color=sexo))

ggplot(sinais_lineas) +
geom_line(aes(x=anio_regis, y=tot_personas, color=sexo), size=1.2, linetype=5) +
geom_point(aes(x=anio_regis, y=tot_personas), color="black", size=4, shape=16) +
scale_color_manual(values=c("#a50f15","#225ea8")) +
labs(title ="Total de Homicidios según sexo", 
       subtitle="Por año de registro del homicidio", x = "", 
       y = "", caption = "Fuente: SIANIS", color ="Sexo de la víctima") +
  theme_bw() +
  scale_y_continuous(breaks=seq(from=0, to=25000, by=1000)) + 
  scale_x_continuous(breaks=seq(from=2002, to=2016, by=1)) + 
  theme(axis.text.x = element_text(face="bold", size = 12),
             title  = element_text(face="bold", size=14)) 
ggsave(paste(grafs, "6_lines.png", sep="/"), width=12, height=12)  

rm(sinais_lineas)

################
##  Heatmaps ###
## geom_tile ###
################

# para un heatmap necesitamos 3 cosas - los dos ejes y lo que rellena - 

ggplot(snsp, aes(y=ent, x=year, fill=tasa_total)) +
geom_tile(color="black") +
scale_fill_continuous(low="#f7fcfd", high="#4d004b") + # NUEVO - en lugar de darle los colores le damos un rango
labs(title="Tasa de feminicidios por entidad", 
       x="", y="", fill="Tasa por 100 mil mujeres") +
theme_bw() # THE HORROR ! cómo le hacemos

ggplot(snsp, aes(y=ent, x=year, fill=tasa_total)) +
geom_tile(color="black") +
scale_fill_continuous(low="#f7fcfd", high="#4d004b") + # NUEVO - en lugar de darle los colores le damos un rango
labs(title="Tasa de feminicidios por entidad", 
       x="", y="", fill="Tasa por 100 mil mujeres") +
theme_bw() +
coord_fixed()

  ggplot(snsp, aes(x=ent, y=year, fill=tasa_total)) +
  geom_tile(color="black") +
  scale_fill_continuous(low="#f7fcfd", high="#4d004b") + # NUEVO - en lugar de darle los colores le damos un rango
  labs(title="Tasa de feminicidios por entidad", 
       x="", y="", fill="Tasa por 100 mil mujeres") +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  coord_fixed()
 
  ggplot(snsp, aes(x=reorder(ent, -tasa_total), y=year, fill=tasa_total)) +
  geom_tile(color="black") +
  scale_fill_continuous(low="#f7fcfd", high="#4d004b") + # NUEVO - en lugar de darle los colores le damos un rango
  labs(title="Tasa de feminicidios por entidad", 
       x="", y="", fill="Tasa por 100 mil mujeres") +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  coord_fixed()
  ggsave(paste(grafs, "7_heatmaps.png", sep="/"), width=12, height=12) 

################
##  Treemaps ###
##   ups.    ###
################

# treemapify
  
tot_por_sex = group_by(sinais, sexo, escol)  
tot_por_sex$tot = 1
tot_por_sex  = summarize(tot_por_sex, total = sum(tot, na.rm=T))
  
tot_por_sex = filter(tot_por_sex, sexo !="No especificado")
tot_por_sex$sexo = gsub("1", "Hombre", tot_por_sex$sexo)
tot_por_sex$sexo = gsub("2", "Mujer", tot_por_sex$sexo)

tot_por_sex = ungroup(tot_por_sex)
tot_por_sex = group_by(tot_por_sex, sexo)

tot_por_sex = mutate(tot_por_sex, tot_sexo = sum(total),
                     porcent = round(total / tot_sexo * 100, digits=1))


ggplot(tot_por_sex, aes(area=total, fill=c(sexo), label=paste(escol, sexo))) + 
geom_treemap() +
geom_treemap_text(place = "centre", grow = T, color ="black")

# si queremos subgrupo? 
ggplot(tot_por_sex, aes(area=total, fill=escol , subgroup=sexo, label=paste(sexo, escol))) + 
  geom_treemap() +
  geom_treemap_text(place = "centre", grow = T, color ="black") +
  geom_treemap_subgroup_border(color = "white")

ggplot(tot_por_sex, aes(area=total, fill=escol , subgroup=sexo, label=paste(sexo, escol))) + 
  geom_treemap() +
  geom_treemap_text(place = "centre", grow = T, color ="black") +
  geom_treemap_subgroup_border(color = "white") +
  labs(title="¿Qué escolaridad tenían las víctimas de homicidio?", subtitle = "2002 a 2016", 
       caption="Fuente: INEGI", fill="Escolaridad ") +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(color = "black", size=16, hjust=0, face="bold"),
        plot.caption = element_text(color= "black", size=10,face="bold" ),
        axis.text.x = element_text(color="black", angle=90, size=10, hjust=1)) +
  coord_fixed()
ggsave(paste(graf, "8_treemap.jpg", sep="/"), width=12, height=12)


################
##   Areas   ###
## geom_area ###
################

area = group_by(sinais, escol, anio_regis)  
area = filter(area, escol != "No especificado" & escol !="Menor de 12")
area$tot = 1
area  = summarize(area, total = sum(tot, na.rm=T))

area = ungroup(area)
area = group_by(area, anio_regis)

area = mutate(area, tot_anual = sum(total),
                     porcent = round(total / tot_anual * 100, digits=1))

ggplot(area, aes(x=anio_regis, y=porcent, fill=escol)) +
geom_area() # EL HORROR!!!

# las escolaridades significan cosas, no? Se acuerdan de factores?

area$escolaridad = factor(area$escol, 
                          levels=c("Posgrado", "Licenciatura", "Preparatoria", "Secundaria",
                                   "Primaria", "Preescolar","Sin escolaridad"))

ggplot(area, aes(x=anio_regis, y=porcent, fill=escolaridad)) +
      geom_area() +
      scale_x_continuous(breaks=seq(from=2002, to=2016, by=1)) +
      scale_fill_manual(values=c("#d73027", "#f46d43", "#fdae61", "#fee090", 
                                 "#e0f3f8", "#abd9e9", "#4575b4")) +
      labs(title="Porcentaje de personas asesinadas por escolaridad \n 2002-2016",
           x="Año de registro", y="%", fill="Escolaridad") +
      theme_classic() # y así es como me di cuenta que habían cambiado escolaridad jajajajaja
ggsave(paste(grafs, "9_area.png", sep="/"), width=12, height=12) 

rm(area)

#####################
##   Spineplots   ###
##  geom_mosaic   ###
#####################

# require(ggmosaic)

spine = group_by(sinais, edo_civil, sexo)  
spine = filter(spine, edo_civil != "No especificado" & edo_civil !="Menor de 12" & sexo != "No especificado")
spine$tot = 1

spine$sexo = gsub("1", "Hombre", spine$sexo)
spine$sexo = gsub("2", "Mujer", spine$sexo)

spine$edo_civil = factor(spine$edo_civil, 
                          levels=c("Soltero", "Unión Libre", "Casado", "Separado",
                                   "Divorciado", "Viudo"))

spine  = summarize(spine, total = sum(tot, na.rm=T))
spine = ungroup(spine)
spine = group_by(spine, sexo)

spine = mutate(spine, tot_sexo = sum(total),
               porcent = round(total / tot_sexo * 100, digits=1))

# geom_mosaic necesita 1 cosas - qué va en equis y qué va en y (pero no se llama y sino peso-porque tiene ancho)
# SIEMPRE tienes que poner x dentro de un product() porque lo pide el pinche paquete
#product scales are especially introduced for use with mosaic plots: they are a hybrid of continuous and discrete

ggplot(spine) +
geom_mosaic(aes(weight = porcent, x = product(edo_civil))) # no USE ¿por? el chiste es comparar 2 variables no?

ggplot(spine) +
  geom_mosaic(aes(weight = porcent, x = product(edo_civil, sexo))) # mucho mejor - pero el fillll

# ¿Qué es alpha? algo muy cool  / es la transparencia del relleno
ggplot(spine) +
geom_mosaic(aes(weight = porcent, x = product(edo_civil, sexo), fill=product(edo_civil)), 
                alpha=1, na.rm=TRUE) +
        scale_fill_manual(values=c("#ffffcc", "#feb24c","#fd8d3c",
                                   "#fc4e2a", "#e31a1c", "#800026")) +
        labs(y="% de homicidios según estado civil", 
             x="% de homicidios por sexo de víctima", caption="Fuente: SINAIS 2002-2016", 
             title="Estado Civil de las víctimas de homicidio en México \n según sexo", 
             fill="Edo Civil") +
        theme_bw() 
ggsave(paste(grafs, "10_spineplot.png", sep="/"), width=12, height=12) 


#####################
##    Jitters     ###
##  geom_jitter   ###
#####################

# es una manera de ver como se distribuye una variable continua vs otra de categorías - ¿Cómo?
# gráfica de distribución por ejemplo de tasas en los 3 años de snsp

ggplot(snsp) +
geom_point(aes(x=year, y=tasa_total)) # el problema es que los puntos se enciman no? 

# el jitter lo que hace es le mete "ruido" aleatorio para reacomodar puntos que esten en el mismo lugar

ggplot(snsp) +
geom_jitter(aes(x=year, y=tasa_total)) # cual es la diferencia? ¿Para qué sirve?

ggplot(snsp) +
geom_jitter(aes(x=year, y=tasa_total), color="maroon", size=1.3) +
labs(y="Tasa de feminicidios por 100 mil mujeres", 
       x="", caption="Fuente: SENSP", 
       title="Distribución de tasa total de feminicidio \n 3 años reportados") +
  theme_bw() 
ggsave(paste(grafs, "11_jitter.png", sep="/"), width=12, height=12) 


# geom_violin geom_segment geom_hline geom_vline 


