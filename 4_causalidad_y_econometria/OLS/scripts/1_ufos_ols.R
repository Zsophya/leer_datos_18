rm(list=ls())
setwd("~")

###########################
# OLS                     #
# UFO sightings y % votos #
###########################

require(pacman)
p_load(foreign, tidyverse, readstata13, GGally, car)

inp = "/Users/carolinatorreblanca/Dropbox (Data4)/Data Civica/Clases/leer_datos_18/4_causalidad_y_econometria/OLS/datos"

# http://www.nuforc.org/webreports/ndxevent.html  // lo vamos a escrapear cuando aprendamos scraping

data <- read.dta13(paste(inp, "ufos_final.dta", sep="/"))

#############################
# Entendamos nuestros datos #
#############################

summary(data)
str(data)
dim(data)

#######################
# Algunas grafiquitas #
#######################

ggplot(data, aes(x=tbeijing, y=tlondres)) +
  geom_point() +
  geom_text(aes(label=codigo), hjust=-0.2) +
  stat_smooth(method="lm", se=F,formula=y ~ x + I(x^2), color="#cb181d")

ggplot(data, aes(x=expvida, y=tlondres)) +
  geom_point() +
  geom_text(aes(label=codigo), hjust=-0.2) +
  stat_smooth(method="lm", se=T,formula=y ~ x + I(x^2), color="#cb181d") +
  scale_x_log10()

ggplot(data, aes(x=femlab, y=tlondres)) +
  geom_point() +
  geom_text(aes(label=codigo), hjust=-0.2) +
  stat_smooth(method="lm", se=F,formula=y ~ x + I(x^2), color="#cb181d")

ggplot(data, aes(x=gnppc, y=tlondres)) +
  geom_point() +
  geom_text(aes(label=codigo), hjust=-0.2) +
  stat_smooth(method="lm", se=F,formula=y ~ x + I(x^2), color="#cb181d")

ggplot(data, aes(x=tax, y=tlondres)) +
  geom_point() +
  geom_text(aes(label=codigo), hjust=-0.2) +
  stat_smooth(method="lm", se=F,formula=y ~ x + I(x^2), color="#cb181d")

ggplot(data, aes(x=gdp11_bill, y=tlondres)) +
  geom_point() +
  geom_text(aes(label=codigo), hjust=-0.2) +
  stat_smooth(method="lm", se=F,formula=y ~ x + I(x^2), color="#cb181d")

### kdensities ###
plot(density(data$tlondres, na.rm=T)) # Esta es la opción base, pero no pueden tener missings, por eso el na.rm=T
ggplot(data, aes(x=tlondres)) + geom_density() # ggplot ignora solito los NAs, pero nos avisa que existen
ggplot(data, aes(x=glondres)) + geom_density()
ggplot(data, aes(x=gdp11_bill)) + geom_density()
ggplot(data, aes(x=femlab)) + geom_density()
ggplot(data, aes(x=tax)) + geom_density()
ggplot(data, aes(x=expvida)) + geom_density()
ggplot(data, aes(x=gnppc)) + geom_density()


# o podemos entender distribuciones entre submuestras

ggplot() + 
  geom_density(data=subset(data, tlondres>0), aes(x=expvida), fill="blue", alpha=0.3) +
  geom_density(data=subset(data, tlondres==0), aes(x=expvida), fill="red", alpha=0.3)



### boxplot ###
tempo <- data %>%
  select(codigo, tlondres, glondres, expvida, tax, femlab, gdp11_bill) %>%
  melt(id="codigo")

ggplot(tempo, aes(x=variable, y=value, fill=variable)) + geom_boxplot()

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

