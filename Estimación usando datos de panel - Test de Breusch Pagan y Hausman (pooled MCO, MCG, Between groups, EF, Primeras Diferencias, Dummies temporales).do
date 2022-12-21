/*

Descripción: se desea conocer el gasto de gasolina respecto al ingreso per-capita. Para ello, se estimarán modelos MCO agrupado, MCG, Between-groups, EF y Primeras Diferencias. Los datos se extraerán de Balgati OECDcar.


* LN(Gas/Car): 	Logaritmo del consumo de gasolina por auto (lgaspcar)
* LN(Y/N): 		Logaritmo del ingreso per-capita real (lincomep)
* LN(Pmg/Pgdp): Logaritmo del precio de auto con motor a gasolina / deflactor PIB (lrpmg)
* LN(Car/N): 	Logaritmo del stock de autos per-capita (lcarpcap)
* NONEURO: 		Dummy =1 para EEUU, Canadá, Japón y Turquía.
* Se tiene una muestra de 18 países OECD, para 19 años (caso de macropanel T>N).
*/

use "D:\1_PUCP\Ciclo 2022-1\Eco micro\Clases - EcoMicro\18. baltagiOECDcar.dta", clear 
egen cntry=group(country)

xtset cntry year  // indica a Stata que estamos ante datos de panel. Además, indica cual es N y T.

xtdescribe        // muestra si es un panel balanceado
xtline lgaspcar   // muestra tendencias de cada individuo en todos los periodos

* Estadísticas descriptivas
xtsum lgaspcar lincomep lrpmg lcarpcap
	/*
	overall: Std. Dev. de la variable (como si fueran observaciones simples)
	between: Std. Dev. de la variable promedio de cada país (variabilidad entre promedio de países)
	within : Std. Dev. de la variable dentro de cada país en el tiempo (se desea que tenga variabilidad en el tiempo)

*/

recode cntry (3=1) (10=1) (16=1) (18=1) (else=0), gen(noneuro)

xtsum lgaspcar lincomep lrpmg lcarpcap noneuro
				// Std. Dev. between: muestra la variabilidad entre los países, se puede comparar la variabilidad.
				// Std. Dev. within: variabilidad en el tiempo de cada país.
				  
				// noeuro no cambia en el tiempo, Std. Dev = 0, el estimado Within Groups la elimina :(



* Gráficos tipo bivariado
scatter lgaspcar lincomep, mlabel(country) || lfit lgaspcar lincomep  // (línea MCO)

		// Por cada país, vemos que a medida que aumenta el ingreso per capita, disminuye el consumo de gasolina por auto.
		// Como cada país muestra tener un intercepto diferente, queda bastante claro que estamos ante Heterogeneidad 
		// Inobservable, los países son muy diferentes entre ellos, pues tienen diferentes interceptos.
		
		// La recta pooled MCO que asume Homogeneidad Total, no tiene sentido porque entiende cada observación 
		// individualmente y asigna un intercepto en común a todos los países, lo cual queda claro en el gráfico que son muy 
		// diferentes. Ejm, Suecia tiene menor intercepto que Turquía.
		
		// Además, si la base hubiera consistido en solo Suecia, Grecia y Japón, la recta pooled MCO saldría positiva a pesar
		// de que la tendencia sea negativa intra país (Paradoja de Simpson).
		
scatter lgaspcar lincomep, mlabel(cntry) || lfit lgaspcar lincomep



*********************************************************************************
* Ejemplo 1: Modelo bivariado de gasto de gasolina respecto al ingreso per-capita (ejercicio manual)
*********************************************************************************
*Regresión Pooled-MCO de un bivariado
reg lgaspcar lincomep       // a pesar de ser significativo, la estimación está incorrecta por los supuestos arriba mencionados.

*Cálculo de promedio en el tiempo por cada variable 
egen mlgaspcar=mean(lgaspcar), by(cntry)
egen mlincomep=mean(lincomep), by(cntry)

scatter mlgaspcar mlincomep, mlabel(country) || lfit mlgaspcar mlincomep    // regresión Between Groups 
																			// hay mucha variabilidad no observable

* Estimación Between groups (manual)
reg mlgaspcar mlincomep if year ==1960         	// sale no significativa tal vez por pocos datos


* Gráfico Within Groups
* Generación de desvíos respecto a la media de cada individuo (media de cada group)
gen deslgaspcar = lgaspcar - mlgaspcar
gen deslincomep = lincomep - mlincomep

scatter deslgaspcar deslincomep || lfit deslgaspcar deslincomep    
			// comportamiento de la media de cada país, centrado en el eje 0
			// recta within groups, captura la variabilidad de cada individuo en el tiempo
			// la forma de hacerlo: traslada las observaciones de cada país al eje 0
			// Interpretación: cuando hay una variación del ingreso en el timepo, se produce una variación negativa del 
			//                 gasto en el tiempo

* Estimación within groups (manual)
reg deslgaspcar deslincomep, nocons

* Estimación en primeras diferencias (manual)
reg d.lgaspcar d.lincomep, nocons  		// d. : genera primeras diferencias (tiene mayor varianza)



************************************************************
* Ejemplo 2: Modelo multivariado utilizando los comandos xt
************************************************************

reg   lgaspcar lincomep lrpmg lcarpcap noneuro			// estimacipon pooled MCO
outreg2 using myfile, replace cttop(MCO-pooled)    		// para generar tablas

xtreg  lgaspcar lincomep lrpmg lcarpcap noneuro, re		// estimación efectos aleatorios MCG Balestra-Nerlove

														// sigma_u = .1600934  , es la raiz cuadrada del alfa
														// sigma_e = .09233034 , es la raiz cuadrada del epsilon
														// rho : indica el porcentaje de la variabilidad de los errores 
														//       corresponde al componente de Heterogeneidad inobservable.
														
outreg2 using myfile, cttop(EA) addstat(sig^2_alfa, e(sigma_u), sigma^2, e(sigma_e))
estimates store randomEffects  	// guarda estimación con nombre random



* ======================= *
*  Test de Breusch-Pagan  *
* ======================= * 

* para evaluar si estamos ante Homogeneidad total o Heterogeneidad inobservable
xttest0 	// Prob = 0.00
			// Se rechaza la hipótesis nula (Homogeneidad Total). Se debe usar modelo de Heterogeneidad Inobservable.
			// Se descarta la estimación MCO.
			
			// Se rechaza largamente, pues chi tabla = 3.84, mientras  chibar2(01) =  1254.18
			// Entonces como chibar2(01) > chi tabla, se rechaza con bastante seguridad.
			
* =========================================================================================================================	



xtreg  lgaspcar lincomep lrpmg lcarpcap noneuro, be		// estimación Between groups
outreg2 using myfile, cttop(Between)

xtreg  lgaspcar lincomep lrpmg lcarpcap noneuro, fe		// estimación Efectos fijos, la costante no es constante, sino una estimación de los alfas promedio (alfa sombrero barra).
outreg2 using myfile, cttop(Within) addstat(sig^2_alfa, e(sigma_u), sigma^2, e(sigma_e)) see excel
estimates store fixed



* ================= * 
*  Test de Hausman  * 
* ================= * 

hausman fixed randomEffects		// Prob = 0.00
								// Se rechaza la hipotesis nula (Efectos Aleatorios). Se debe usar modelo de Efectos fijos.

hausman fixed randomEffects, sigmamore		// corrige el test para que salga el sigma positivo definido: ya no utilizando
											// la resta (V_b-V_B), si no solamente V_b.
											// prob < 0.05, se rechaza la hipotesis nula, debemos usar Efectos fijos.

hausman fixed randomEffects, sigmaless		// corrige el test para que salga el sigma positivo definido: ya no utilizando
											// la resta (V_b-V_B), si no solamente V_B.
											// prob < 0.05, se rechaza la hipotesis nula, debemos usar Efectos fijos.

* ================================================================================================================
									
* Primeras diferencias
reg d.lgaspcar d.(lincomep lrpmg lcarpcap noneuro), nocons




********************************
* Agregando dummies temporales *
********************************

xtline lcarpcap		// Muestra que hay tendencia en el tiempo, en las variables. Problema que Efectos fijos Temporales resuelve.

xtreg  lgaspcar lincomep lrpmg lcarpcap noneuro i.year, fe		// i. genera dummies para la variable tiempo
																
						// La estimación muestra que el efecto de las dummies va en aumento. 
						// Eso evidencia la presencia de una tendencia ascendente.
						// Además, el ingreso per cápita no es significativo. Eso dice que el ingreso probablemente no
						// explica el gasto de gasolina. Entonces el ingreso y gasto suben en el tiempo, pero quizas por otro factor.

						
* ====================================================================== *
* Test de significancia conjunta para las dummies de tiempo (temporales) * (efectos fijos temporales, segun evaluación semanal 7)
* ====================================================================== *

testparm i.year			// prob = 0.00, se reachaza la hipótesis nula (que el coeficiente de las dummies sean 0).



*******************************************************************************************************************************
*** Panel dinámico ************************************************************************************************************
*******************************************************************************************************************************

use "D:\1_PUCP\Ciclo 2022-1\Eco micro\Clases - EcoMicro\17. baltagiOECDcar.dta", clear 
egen cntry = group(country)

xtset cntry year

/*
Si hubiese un componente inercial muy fuerte, en el Y con el pasado, y eso no ha sido modelado, este componente inercial debería observarse en el error idiosincrático (fuerte correlación de ε con el pasado). Esto haría al estimador de Primeras Diferencias el más eficiente. O mejor, pasar al modelo de Panel Dinámico. 
*/

xtregar lgaspcar lincomep lrpmg lcarpcap noneuro, fe	// regresión simple Efectos fijos	
		// Regresión panel que asume que los residuos presentan correlación tipo AR(1).
		// ε_it = rho_ε_it-1 + error
		
		// rho_arv = 0.85890468 muestra una fuerte correlación. Esto indica que el error idiosincrático presenta 
		// fuerte correlación (ya no es bien comportado).   
		
		// Se puede mejorar el modelo con una modelación de panel dinámico. 
		
* Modelo Panel dinámico *
drop if year > 1966
xtabond lgaspcar lincomep lrpmg lcarpcap, vce(robust) 	
	// Arellano-Bond básico, un rezago por defecto y MC2E por defecto
	// MC2E con 19 instrumentos
				
	// El coeficiente (0.4008) del rezago es significativo y estable (entre -1, 1).
	// Además, la autocorrelación del consumo con respecto al consumo pasado es regularmente fuerte.

xtabond lgaspcar lincomep lrpmg lcarpcap, twostep vce(robust) lags(2) 	
estat abond
	// Twostep GMM (Método de los momentos generalizados), autorregresivo con dos rezagos AR(2) de la endógena.
	// En la 1ra etapa, se calcula una matriz ponderadora con los estimadores MC2E.
	// En la 2da etapa, se reestima los parámetros ponderando con la matriz W ponderadora.
				
	// Los dos rezagos son significativos (fuerte inercia), aunque el primero es solo al 10%
	 

xtabond lgaspcar lincomep lrpmg lcarpcap, twostep lags(2)	// Sin VCE(robust) para el test de Sargan
estat sargan		
	// Hipótesis nula: overidentifying restrictions are valid   E[Z por el error] = 0
	
	// Prob > chi2 = 0.5103, no se reachaza la hipótesis nula. Entonces, se acepta E[Z por el error] = 0.
	// Consiguientemente, nos quedamos con la estimación xtabond lgaspcar lincomep lrpmg lcarpcap, twostep vce(robust) lags(2) 	













