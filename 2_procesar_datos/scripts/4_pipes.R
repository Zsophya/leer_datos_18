rm(list=ls())
setwd("~")

##########################
##  LOCALIDADES KELLOGG ##
##  Caro / SURVEY SRVY  ##
##  25 julio 2017       ##
##  Valar Morghulis     ##
##########################

library(dplyr)
require(tidyverse)
library(readstata13)

dir1 <- "/Users/ppmerino/Dropbox (Personal)/Clases/ICAitam/ICAmerino/Clase 1 Procesar datos/Procesar datos/datos/Procesar Datos 2/"
dir2 <- "/Users/ppmerino/Dropbox (Personal)/Clases/ICAitam/ICAmerino/Clase 1 Procesar datos/Procesar datos/datos/Procesar Datos 2/Out"

poblacion <- read.dta13(paste(dir1, "PobMun90_15_edad.dta", sep="/"), convert.factors=F)

pob <- poblacion %>%
       select(ID, edad, sexo, pob2015) %>%
       mutate(CVE_ENT = substr(ID, 1, 2), CVE_MUN = substr(ID, 3, 5)) %>%
       group_by(CVE_ENT, CVE_MUN, edad) %>%
       summarize(pob = sum(pob2015, na.rm=T)) %>%
       ungroup()  %>%
       group_by(CVE_MUN, CVE_ENT) %>%
       mutate(pob_tot = sum(pob, na.rm=T), porcent = round(pob / pob_tot * 100, digits = 1)) %>%
       filter(edad <= 24 & edad >= 3)  %>%
       summarize(porcent_edadesc = sum(porcent, na.rm=T))  %>%
       ungroup()  %>%
       mutate(id = paste0(CVE_ENT, CVE_MUN))  %>%
       filter(CVE_ENT == "07" | CVE_ENT == "31" | CVE_ENT == "23" | CVE_ENT == "04")

write.csv(pob, paste(dir2, "pob_edad_escolar.csv", sep="/"), row.names=F)