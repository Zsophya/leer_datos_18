version 14
log using "/Users/merino/Dropbox (Personal)/Clases/ICAitam/ICAmerino/Clase 5 OLS, Interactivos y Clarify/OLS/Datos/olimpicos.log", replace
set more off;

**********************************************;
* nombre archivo: olimpicos.do               *;
* fecha 290916                               *;
* Autor    Jose Merino                       *;
* Proposito   clase ols                      *;
* Datos usados     olimpicos.dta             *;
* Archivo anterior  ninguno                  *;
* Status   trabajando                        *;
* PC       Mac D4                            *;
**********************************************;

cd "/Users/merino/Dropbox (Personal)/Clases/ICAitam/ICAmerino/Clase 5 OLS, Interactivos y Clarify/OLS/Datos/"


use olimpicos, clear

*****************************
* entendamos nuestros datos *
*****************************

sum 
des 



**********
* GRAFS  *
**********

twoway (scatter tlondres tbeijing, msymbol(Oh) mlabel(codigo)) (qfit  tlondres tbeijing)

twoway (scatter tlondres expvida, msymbol(Oh) mlabel(codigo)) (qfitci  tlondres expvida), 

twoway (scatter tlondres femlab, msymbol(Oh) mlabel(codigo)) (qfit  tlondres femlab)

twoway (scatter tlondres gnppc, msymbol(Oh) mlabel(codigo)) (qfit  tlondres gnppc), xsca(log)

twoway (scatter tlondres tax, msymbol(Oh) mlabel(codigo)) (qfit  tlondres tax)

twoway (scatter tlondres gdp11_bill, msymbol(Oh) mlabel(codigo)) (qfit  tlondres gdp11_bill)

**************
* Kdensities *
**************

kdensity tlondres
kdensity glondres 
kdensity gdp11_bill
kdensity femlab
kdensity tax
kdensity expvida
kdensity gnppc

* o podemos entender distribuciones entre submuestras *

twoway (kdensity expvida if tlondres>0 & tlondres<.) (kdensity expvida if tlondres==0)

* alguien tiene una idea de algo que valga la pena ver? *


***********
* boxes   *
***********

gr box tlondres glondres expvida tax femlab gdp11_bill



************
* tabulate *
************

tab tlondres glondres

twoway (scatter glondres tlondres, msymbol(Oh) mlabel(codigo)) (qfit  glondres tlondres)

************
* stem     *
************


stem glondres
stem expvida, prune


************
* matrix   *
************

graph matrix tlondres expvida femlab decil10_1 gnppc tax dem gdp11_bill pop2010


** Conocemos nuestros datos, conocemos los outliers, podemos correr regresiones y jugar con distintas especificaciones *


reg tlondres expvida

reg tlondres expvida femlab

reg tlondres expvida femlab decil10_1

reg tlondres expvida femlab decil10_1 gnppc

reg tlondres expvida femlab decil10_1 gnppc tax 

reg tlondres expvida femlab decil10_1 gnppc tax dem

reg tlondres expvida femlab decil10_1 gnppc tax dem pop2010

reg tlondres expvida femlab decil10_1 gnppc tax dem pop2010 gdp11_bill

*gen Lpop2010=log(pop2010)
*gen Lgdp11=log(gdp11)
*gen Lgnppc=log(gnppc)

reg tlondres expvida femlab decil10_1 Lgnppc tax dem Lpop2010 Lgdp11


* Este modelo parece explicar más, y conocemos el valor agregado de cada variable *

corr tlondres expvida femlab decil10_1 gnppc tax dem gdp11_bill pop2010

pwcorr tlondres expvida femlab decil10_1 gnppc tax dem gdp11_bill pop2010, obs sig
pwcorr tlondres expvida femlab decil10_1 Lgnppc tax dem Lpop2010 Lgdp11, obs sig
* veamos rápido outliers en residuales *

predict r, rstudent
predict r1, res
stem r
sort r
list codigo r in 1/20
list codigo r in 92/102
kdensity r
twoway (kdensity r) (kdensity r1)
drop r r1


* help avplot JOYA *

avplot expvida, mlabel(codigo)
avplots, mlabel(codigo)


* unas pruebitas de hipótesis *

test expvida=0 
test expvida femlab
test femlab


* qué pasa si quitamos a EU ? *


reg tlondres expvida femlab decil10_1 gnppc tax dem pop2010 gdp11_bill if codigo !="USA"
avplots, mlabel(codigo)

reg tlondres expvida femlab decil10_1 Lgnppc tax dem Lpop2010 Lgdp11 if codigo !="USA"
avplots, mlabel(codigo)

* avplot graphs an added-variable plot (a.k.a. partial-regression leverage plot,
* partial regression plot, or adjusted partial residual plot) 


* leverage vs residuals *
lvr2plot, mlabel(codigo)

reg tlondres expvida femlab decil10_1 Lgnppc tax dem Lpop2010 Lgdp11 if codigo !="RUS"
lvr2plot, mlabel(codigo)


*residual vs fitted*
rvfplot, mlabel(codigo) 

*component plus residual*
*partial resifual plot, shows the relationship between IV and DV given controls*
* e+beta*xi vs xi*
cprplot expvida, mlabel(codigo) 

* Leverage:  An observation with an extreme value on a predictor variable is a point with high leverage *

* qué pasa si nos quedamos sólo con los que obtuvieron menos de 30 medallas? *

reg tlondres expvida femlab decil10_1 gnppc tax dem pop2010 gdp11_bill if tlondres<10 & tlondres>0
avplots, mlabel(codigo) 

reg tlondres expvida femlab decil10_1 Lgnppc tax dem Lpop2010 Lgdp11 if tlondres<30
avplots, mlabel(codigo)

*** VEAMOS LOS RESIDUALES **

quiet reg tlondres expvida femlab decil10_1 gnppc tax dem pop2010 gdp11_bill
predict esperadas, xb
predict res, residuals 
kdensity res, normal 
pnorm res
twoway (scatter tlondres esperadas, mlabel(codigo)) (lfit tlondres esperadas)
twoway (scatter tlondres res, mlabel(codigo)) (lfit tlondres res)
drop res esperadas

quiet reg tlondres expvida femlab decil10_1 Lgnppc tax dem Lpop2010 Lgdp11 
predict esperadas, xb
predict res, residuals 
kdensity res, normal 
pnorm res
twoway (scatter tlondres esperadas, mlabel(codigo)) (lfit tlondres esperadas)
twoway (scatter tlondres res, mlabel(codigo)) (lfit tlondres res)



reg tlondres expvida femlab decil10_1 Lgnppc tax dem Lpop2010 Lgdp11

* heterocedasticidad *;

rvfplot, yline(0) 


* TESTS H0: dist homog *

estat imtest
estat hettest

twoway (scatter res esperadas , mlabel(codigo)) (lfit res esperadas)
twoway (scatter  res tlondres, mlabel(codigo)) (lfit res tlondres)
twoway (scatter  res gdp11_bill, mlabel(codigo)) (lfit res gdp11_bill)
twoway (scatter res expvida , mlabel(codigo)) (lfit res expvida)


*por construcción*
reg res expvida femlab decil10_1 Lgnppc tax dem Lpop2010 Lgdp11
reg res expvida femlab decil10_1 gnppc tax dem pop2010 gdp11_bill
drop res esperadas

* multicolinealidad *;

quiet reg tlondres expvida femlab decil10_1 gnppc tax dem pop2010 gdp11_bill

* recordemos *;

corr expvida femlab decil10_1 gnppc tax dem pop2010 gdp11_bill

vif

* preocupate si sale por arriba de 10 *

log close
clear
exit

