* Descripción: se desea conocer los determinantes de la participación laboral de las mujeres en el mercado de trabajo peruano. Para esto, se realizarán estimaciones logit y probit que utilizarán datos de la encuesta ENAHO 2019. 

cd "C:\Bases_datos\ENAHO_Metodologia_ACTUALIZADA\Condiciones_de_vida_y_pobreza_ENAHO\2019_Anual"
use "enaho01-2019-200.dta", clear

* vivienda: lugar físico
* hogar:    los que comen de la misma olla
* familia:  relación de parentezco


////   Construyendo la base de datos   /////

* Se trabajará en base a datos del hogar

* número de niños
gen ninos0_5=1 if p208a>=0 & p208a<=5

egen nninos=sum(ninos0_5), by(conglome vivienda hogar)		//esta es la cantidad de niños de 0 a 5 años en el hogar

merge 1:1 conglome vivienda hogar codperso using "enaho01a-2019-500.dta", keepus(ocu500)
keep if _merge==3
drop _merge



*** Participación laboral
tab ocu500
recode ocu500 (1/3=1 "PEA") (4=0 "NO PEA") (0=.), gen(participa) 	// dummy 0,1
 
*** Edad
tab p208a		//notar que 99=missing

* Eliminamos a los adultos mayores
drop if p208a>65

*** Participación por sexo
tab p207					//tabla de frecuencia
tab p207 participa			//tabla cruzada
tab p207 participa, row		//la tabla de arriba en porcentajes (hombres: 84.19%, mujeres: 69.81)
tab p207 participa [iw = facpob07], row 	// utilizamos el factor de expansión para tener estadísticas
											// de la población, mas no solo de la muestra.
tab participa if p207==1
tab participa if p207==2

* Nos quedamos con la muestra de mujeres de 14 a 65 años
drop if p207==1			// drop hombres

* Variable edad p208a
ren p208a edad

*** Estado civil
tab p209			// como las entradas tiene un número por detrás
gen convcas=1 if p209<=2 & p209>=1		//ojo, no funciona bien si hay missings. En tal caso mejor usar recode
replace convcas=0 if convcas==.
tab convcas    		// 52.42% están casadas, 47.58% no están casadas 

*** Educación
*Traemos las demás variables de educacíón p301b y p301c
merge 1:1 conglome vivienda hogar codperso using "enaho01a-2019-300.dta", keepus(p301a p301b p301c)
											// hombres y niños no se emparejarán pues en la base
											// master solo tenemos a mujeres entre 14-65 años
keep if _merge==3   // borramos las observaciones que no hicieron match
drop _merge

* generando escolaridad
* conociendo variable p301a
tab p301a
tab p301a, nolabel

recode p301a (1/4=0) (5/6=6) (7/10=11) (11=16) (12=0), gen(intermedia)  // ponemos los años del nivel anterior (esta manera de generar la escolaridad no es la mejor estrategia, pues García dice que se puede hacer con mayor detalle).

egen aprobado=rowtotal(p301b p301c)   		// egen ... rowtotal para poder sumar missing
gen escolaridad=intermedia+aprobado

*** Salud
merge 1:1 conglome vivienda hogar codperso using enaho01a-2019-400.dta, keepus(p401h1 p401h2 p401)
keep if _merge==3   //borramos las observaciones que no hicieron match
drop _merge

* generando 3 dummy
replace p401h1=0 if p401h1==2    
replace p401h2=0 if p401h2==2
replace p401  =0 if p401  ==2

* renombrando variable p401 a enfermedad
rename p401 enfermedad


*** Ingresos del hogar
merge m:1 conglome vivienda hogar using sumaria-2019.dta, keepus(inghog2d)									
keep if _merge==3
drop _merge

* Nos quedamos solo con las zonas urbanas
drop if estrato >=6

label variable edad "Edad en años"

*** Regiones
gen costa=1 if dominio<=3
replace costa=0 if costa==.
gen sierra=1 if dominio>=4 & dominio<=6
replace sierra=0 if sierra==.
gene selva=1 if dominio==7
replace selva=0 if selva==.
gen lima_metrop=1 if dominio==8
replace lima_metrop=0 if lima_metrop==.


///  Scatters y gráficos  ///

* Se intenta conocer la distribución de los datos.

* scatters de la participación de las mujeres según edad
scatter participa edad					// De esta manera, no se ve nada realmente.

* gráficos de cajas (la línea del medio es la mediana de la edad)
graph hbox edad, over(participa, descending)

graph box edad, over(participa)			// rango intercuartil  = 75percentile - 25 percentile
										// lower adyacent line = 25percetile  - 1.5*(rango intercuartil)
										// upper adyacent line = 75percentile + 1.5*(rango intercuartil)

	
	
*********************		
***  REGRESIONES  ***
*********************

* influencia de variables en la participación laboral femenina		
		
		
* Variables globales
global vars = "edad c.edad#c.edad escolaridad c.escolaridad#c.escolaridad convcas nninos enfermedad costa sierra selva"
				// c.edad#c.edad 	: así Stata entiende que es el cuadrado a la hora de calcular los Efectos Marginales
				// c.     			: significa continua
				// convcas 			: 1 para vonviviente o casada, 0 para no casada
				// p401 			: 1 si padece enfermedad, 0 si no padece enfermedad
// Stata entiende que costa sierra selva son parte de la dummy, y habría multicolinealidad si se incluyera la faltante lima_metrop.
										
		
		
*****************************************		
***   Modelo de Probabilidad Lineal   ***
*****************************************

reg participa $vars
				// Notamos que la variable p401 no es significativa, entonces no podemos decir nada sobre esta variable. 
				// Notar que se ha tomado a Lima como base.
				// Las estimaciones indican la variación porcentual de la probabilidad de participar
						
						
outreg2 using myfile, replace cttop(MPL)	// tablas para exportar

margins, dydx(*) post	// Efectos marginales de todas las variables
						// Análisis: por cada año adicional de edad, se incrementa la probabilidad de trabajar en 0.69%.
outreg2 using myfile2, replace cttop(MPL)



***************
***  Logit  ***           
***************

* En la estimación logit, no se interpreta la magnitud del coeficiente estimado, pero sí el signo y significancia. Notar que los coeficientes estimados son variaciones en la probabilidad de ocurrencia de la variante dependiente. Sin embargo, para conocer el verdadero impacto se deben evaluar los Efectos Marginales de cada variable: margins, dydx(*) atmean.

logit participa $vars
outreg2 using myfile, cttop(Logit)

margins, dydx(*) atmean post    // utiliza el individuo promedio
								// impacto en porcentajes en la variable que va de 0 a 1
								// Sin embargo, hay que también utilizar los Efectos Marginales según cada 
								// posibilidad
outreg2 using myfile2, cttop(Logit)    



****************
***  Probit  ***     
****************
         
* En la estimación probit, no se interpreta la magnitud del coeficiente estimado, pero sí el signo y significancia. Notar que los coeficientes estimados son variaciones en la probabilidad de ocurrencia de la variante dependiente. Sin embargo, para conocer el verdadero impacto se deben evaluar los Efectos Marginales de cada variable: margins, dydx(*) atmean.

probit participa $vars
outreg2 using myfile, cttop(Probit) see excel  	// crea tabla en excel

margins, dydx(*) atmean post
outreg2 using myfile2, cttop(Probit) see excel



///  EVALUCIÓN DE LA ESTIMACIÓN  ///

***  Evaluación modelo logit  

logit participa $vars 

estat classification		
					// Sensitivity (Sensibilidad)  = Recall of positive class.
					// Specificity (Especificidad) = Recall of negative class.
					// con c = 0.5
// El modelo predice bien cuando la mujer va a atrabajar 90.21%, pero predice mal cuando no va a trabajar 36.68%. 
					
estat classification, cut(0.68)
					// Los falsos positivos están bien pero los falsos negativos no están muy bien pronosticados 49.04%.

* El otro médoto: 
* ROC
lroc				// En microdatos tener un ajuste perfecto es imposible. Un ajuste mayor a 75% es bueno???



***  Evaluación modelo probit 
probit participa $vars 

estat classification		// Sensitivity, Specificity o Sensibilidad , Especificidad
estat classification, cut(0.68)


* Notemos que el efecto marginal varía según la edad de la persona. Mientras el efecto marginal promedio puede ser pequeño, si analizamos a lo largo de las diferentes edades vemos su verdadero impacto. Por ello, se puede notar que no es el mismo efecto marginal cuando la persona tiene 18, 30 o 60 años. 

margins, dydx(edad) at(edad=15)  	// cuanto la mujer tiene 15 años, pasar a 16 años aumenta en .0364402 de probabilidad
margins, dydx(edad) at(edad=18)
margins, dydx(edad) at(edad=21)
margins, dydx(edad) at(edad=24)
margins, dydx(edad) at(edad=27)
margins, dydx(edad) at(edad=30)
margins, dydx(edad) at(edad=33)
margins, dydx(edad) at(edad=36)
margins, dydx(edad) at(edad=39)
margins, dydx(edad) at(edad=42)
margins, dydx(edad) at(edad=45)
margins, dydx(edad) at(edad=48)
margins, dydx(edad) at(edad=51)
margins, dydx(edad) at(edad=54)
margins, dydx(edad) at(edad=57)
margins, dydx(edad) at(edad=60)
margins, dydx(edad) at(edad=63)

margins, dydx(escolaridad) at(escolaridad=3)
margins, dydx(escolaridad) at(escolaridad=6)
margins, dydx(escolaridad) at(escolaridad=9)
margins, dydx(escolaridad) at(escolaridad=12)
margins, dydx(escolaridad) at(escolaridad=15)

margins, dydx(nninos) at(nninos=1)
margins, dydx(nninos) at(nninos=6)
