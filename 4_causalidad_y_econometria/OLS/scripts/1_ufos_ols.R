rm(list=ls())
setwd("~")

###########################
# OLS                     #
# UFO sightings y % votos #
###########################

require(pacman)
p_load(foreign, tidyverse, readstata13, GGally, car, devtools)
install_github("IndrajeetPatil/ggstatsplot")

inp = "/Users/carolinatorreblanca/Dropbox (Data4)/Data Civica/Clases/leer_datos_18/4_causalidad_y_econometria/OLS/datos"

# http://www.nuforc.org/webreports/ndxevent.html  // lo vamos a escrapear cuando aprendamos scraping

data <- read.dta13(paste(inp, "ufos_final.dta", sep="/"))

#############################
# Entendamos nuestros datos #
#############################

summary(data) # datos de avistamientos a nivel condado y de resultados de las primarias
str(data)
View(data)

# una buena primera cosa de hacer sería renombrar las vars que vamos a usar para que todo tuviera sentido
names(data)

data = rename(data, pobtot=pop010210, evan_mil = evanrate, catolicos_mil=cthrate, protes_mil=pcrate,
                    porcent_repub_12=Rep_2012, porcent_demo_12=Dem_2012)

# queremos ver la relación entre avistamientos * tasa * y % candidato; ¿por qué tasa?

data = mutate(data, tasa_ufos = round(tot_ufos / pobtot * 1000, 1),
                      gano_12 = ifelse(porcent_demo_12 > porcent_repub_12 & is.na(porcent_repub_12)==F, 
                                       "Obama", "Romney"))
summary(data$tasa_ufos)

# hipótesis: condados que votaron mucho por trump van a tener muchos avistamientos

#######################
# Algunas grafiquitas #
#######################

# Trump
ggplot(data, aes(y=tasa_ufos, x=porcTrump)) +
  geom_point(color="red", alpha=.5) +
  stat_smooth(method="lm", se=F,formula=y ~ x, color="black")

ggplot(subset(data, tasa_ufos < 3), aes(y=tasa_ufos, x=porcTrump)) +
  geom_point(color="red", alpha=.5) +
  stat_smooth(method="lm", se=F,formula=y ~ x, color="black")

ggplot(subset(data, tasa_ufos < 2), aes(y=tasa_ufos, x=porcTrump)) +
  geom_point(color="red", alpha=.5) +
  stat_smooth(method="lm", se=F,formula=y ~ x, color="black") 

# Hillary 

ggplot(data, aes(y=tasa_ufos, x=porcClinton)) +
geom_point(color="blue", alpha=.5) +
stat_smooth(method="lm", se=F,formula=y ~ x, color="black")
  
 ggplot(subset(data, tasa_ufos < 3), aes(y=tasa_ufos, x=porcClinton)) +
 geom_point(color="blue", alpha=.5) +
 stat_smooth(method="lm", se=F,formula=y ~ x, color="black")

# Rubio

 ggplot(data, aes(y=tasa_ufos, x=porcRubio)) +
   geom_point(color="red", alpha=.5) +
   stat_smooth(method="lm", se=F,formula=y ~ x, color="black")
 
 ggplot(subset(data, tasa_ufos < 3), aes(y=tasa_ufos, x=porcRubio)) +
   geom_point(color="red", alpha=.5) +
   stat_smooth(method="lm", se=F,formula=y ~ x, color="black") 
 
# Sanders

ggplot(data, aes(y=tasa_ufos, x=porcSanders)) +
geom_point(color="blue", alpha=.5) +
stat_smooth(method="lm", se=F,formula=y ~ x, color="black")
 
ggplot(subset(data, tasa_ufos < 3), aes(y=tasa_ufos, x=porcSanders)) +
geom_point(color="blue", alpha=.5) +
stat_smooth(method="lm", se=F,formula=y ~ x, color="black")

## Suena a que están relacionadas estas variables, a ver qué dice esto. 

### kdensities ###

ggplot(data, aes(x=porcTrump)) +
geom_density() # 

ggplot(data, aes(x=porcSanders)) +
geom_density()

ggplot(data, aes(x=porcRubio)) +
geom_density()

ggplot(data, aes(x=porcCruz)) + 
geom_density()

ggplot(data, aes(x=porcClinton)) + 
geom_density()

ggplot(data, aes(x=tasa_ufos)) + 
geom_density()

ggplot(data, aes(x=evan_mil)) + 
geom_density()

# o a ver
ggplot(data, aes(x=tasa_ufos, fill=gano_12)) + 
  geom_density(alpha=.5)

ggplot(data, aes(x=evan_mil, fill=gano_12)) + 
  geom_density(alpha=.5)

################
### boxplot ###
###############

ggplot(data, aes(x=gano_12, y=tasa_ufos, fill=gano_12)) + 
geom_boxplot() +
theme_minimal()

# Ok, pero ahora según quién gano la primaria?
data$ganador_republicano = ""
data$ganador_democrata = ""

# uff vamos a programar una función, que no cunda el pánico

limpiar <- function(x) {
  x = ifelse(is.na(x)==T, 0, x)
}

tempo <- select(data, porcBush, porcCarson, porcCruz:porcRubio, porcTrump)
tempo <- mutate_at(tempo, 1:ncol(tempo), funs(limpiar))

for(x in 1:nrow(data)){
  data$ganador_republicano[x] <- names(tempo)[which.max(tempo[x, 1:ncol(tempo)])]
}

table(data$ganador_republicano)

tempo <- select(data, porcClinton, porcSanders) # nos quedamos solo con los competidores 
tempo <- mutate_at(tempo, 1:ncol(tempo), funs(limpiar))

for(x in 1:nrow(data)){
  data$ganador_democrata[x] <- names(tempo)[which.max(tempo[x, 1:ncol(tempo)])]
}
table(data$ganador_democrata)

data = mutate(data, ganador_democrata = ifelse(ganador_democrata=="porcClinton" & porcClinton==0, NA, ganador_democrata),
                    ganador_democrata = ifelse(ganador_democrata=="porcSanders" & porcSanders==0, NA, ganador_democrata),
                    ganador_republicano = ifelse((porcTrump==0 & ganador_republicano == "porcTrump") | 
                                                 (porcCarson==0 & ganador_republicano == "porcCarson") |
                                                 (porcCruz==0 & ganador_republicano == "porcCruz") |
                                                 (porcKasich==0 & ganador_republicano == "porcKasich")|
                                                 (porcRubio==0 & ganador_republicano == "porcRubio")|
                                                 (porcBush==0 & ganador_republicano == "porcBush"), NA, ganador_republicano))

table(data$ganador_democrata)
table(data$ganador_republicano)

ggplot(data, aes(x=ganador_republicano, y=tasa_ufos, fill=ganador_republicano)) + 
  geom_boxplot() +
  theme_minimal()

ggplot(data, aes(x=ganador_democrata, y=tasa_ufos, fill=ganador_democrata)) + 
  geom_boxplot() +
  theme_minimal()



### tabulate ###  
table(data$tlondres, data$glondres)

ggplot(data, aes(x=tlondres, y=glondres)) +
  geom_point() +
  geom_text(aes(label=codigo), hjust=-0.2) +
  stat_smooth(method="lm", se=F,formula=y ~ x + I(x^2), color="#cb181d")

### stem ###
stem(data$glondres)
stem(data$expvida)

### matrix ###

ggpairs(data[,c("tlondres", "expvida", "femlab", "decil10_1", "gnppc", "tax", "dem", "gdp11_bill", "pop2010")])


# Conocemos nuestros datos, conocemos los outliers, podemos correr regresiones y jugar con distintas especificaciones

fit <- lm(tlondres ~ expvida, data=data) # correr OLS con tlondres como dependiente y expvida como independiente
fit # Sólo te muestra los coeficientes
summary(fit) # Para ver la tabla de regresión
anova(fit) # Para ver análisis de la varianza

fit <- lm(tlondres ~ expvida + femlab, data=data)
summary(fit)
anova(fit)

fit <- lm(tlondres ~ expvida + femlab + decil10_1, data=data)
summary(fit)
anova(fit) 

fit <- lm(tlondres ~ expvida + femlab + decil10_1 + gnppc, data=data)
summary(fit)
anova(fit) 

fit <- lm(tlondres ~ expvida + femlab + decil10_1 + gnppc + tax, data=data)
summary(fit)
anova(fit) 

fit <- lm(tlondres ~ expvida + femlab + decil10_1 + gnppc + tax + dem, data=data)
summary(fit)
anova(fit) 

fit <- lm(tlondres ~ expvida + femlab + decil10_1 + gnppc + tax + dem + logpob, data=data)
summary(fit)
anova(fit) 

fit <- lm(tlondres ~ expvida + femlab + decil10_1 + gnppc + tax + dem + pop2010 + gdp11_bill, data=data, na.action=na.exclude)
summary(fit)
anova(fit) 


# Este modelo parece explicar más, y conocemos el valor agregado de cada variable 
tempo <- data[,c("tlondres", "expvida", "femlab", "decil10_1", "gnppc", "tax", "dem", "gdp11_bill", "pop2010")]

round(cor(tempo, use = "complete.obs"),2) # OJO, el segundo argumento es para que ignore los NAs, si no nos salen puros NAs en la correlación

rcorr(as.matrix(tempo)) # Nos da 3 tablas. 1 es la correlación, 2 es el número de observaciones, 3 es el p-value
# No puede calcular la correlación directo de un data.frame, por eso lo convertimos a matriz

# veamos rápido outliers en residuales

data$r <- rstandard(fit) # Sacamos es residual estandarizado (en SDs pues)
stem(data$r)
data <- arrange(data, r)
data[1:10, c("codigo", "r")]
data[92:102, c("codigo", "r")]

data$r <- NULL


# avplot -> JOYA
fit <- lm(tlondres ~ expvida + femlab + decil10_1 + gnppc + tax + dem + pop2010 + gdp11_bill, data=data)

avPlot(model=fit, variable="expvida", id.n=length(fit))
avPlots(model=fit, id.n=length(fit))


# unas pruebitas de hipótesis
linearHypothesis(fit, "expvida=0")
linearHypothesis(fit, c("expvida=0", "femlab=0"))


# qué pasa si quitamos a EU ?
fit <- lm(tlondres ~ expvida + femlab + decil10_1 + gnppc + tax + dem + pop2010 + gdp11_bill, data=subset(data, codigo!="USA"))
summary(fit)
anova(fit) 

avPlots(model=fit, id.n=length(fit))

# leverage plot
leveragePlots(model=fit, id.n=length(fit))

# leverage vs residuals
par(mfrow = c(2,2))
plot(fit)

# Leverage:  An observation with an extreme value on a predictor variable is a point with high leverage *

# qué pasa si nos quedamos sólo con los que obtuvieron menos de 30 medallas? *
fit <- lm(tlondres ~ expvida + femlab + decil10_1 + gnppc + tax + dem + pop2010 + gdp11_bill, data=subset(data, tlondres<30), na.action=na.exclude)
summary(fit)
anova(fit) 

avPlots(model=fit, id.n=length(fit))


### VEAMOS LOS RESIDUALES

fit <- lm(tlondres ~ expvida + femlab + decil10_1 + gnppc + tax + dem + pop2010 + gdp11_bill, data=data, na.action=na.exclude)
data$esperadas <- predict(fit)
data$res <- resid(fit)

ggplot(data, aes(x=res)) +
  geom_density(aes(y=..density..), fill="blue", alpha=0.3, position="stack") +
  stat_function(fun = dnorm, args = list(mean = mean(data$res, na.rm=T), sd = sd(data$res, na.rm=T)))

ggplot(data, aes(sample=res)) + geom_qq()

# ó
qqnorm(residuals(fit), ylab="Residuals")
qqline(residuals(fit))

# ó

par(mfrow = c(2,2))
plot(fit)

# heterocedasticidad - Residuales vs fitted

summary(lm(esperadas ~ res, data = data))

# TESTS H0: dist homog #
ncvTest(fit)
# Este es el Breusch-Pagan test 
#P es menor a 0.05 entonces podemos rechazar la H nula de que la varianza es no constante

ggplot(data, aes(x=res)) + geom_density() 


ggplot(data, aes(x=esperadas, y=tlondres)) + 
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  geom_text(aes(label=codigo), hjust=-0.2)


ggplot(data, aes(x=res, y=esperadas)) + 
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  geom_text(aes(label=codigo), hjust=-0.2)

ggplot(data, aes(x=res, y=tlondres, label=codigo)) + 
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  geom_text(aes(label=codigo), hjust=-0.2)

ggplot(data, aes(x=res, y=gdp11_bill, label=codigo)) + 
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  geom_text(aes(label=codigo), hjust=-0.2)

ggplot(data, aes(x=res, y=expvida, label=codigo)) + 
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  geom_text(aes(label=codigo), hjust=-0.2)

summary(lm(res ~ expvida + femlab + decil10_1 + gnppc + tax + 
             dem + pop2010 + gdp11_bill, data = data))

# multicolinealidad #
# recordemos
tempo <- data[, c("expvida", "femlab", "decil10_1", "gnppc", "tax", "dem", "pop2010", "gdp11_bill")]
round(cor(tempo, use = "complete.obs"),2)

vif(fit)

