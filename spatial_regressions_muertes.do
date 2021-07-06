clear		all
global		input cd "C:\Users\DELL\Desktop\COVID 19 - Machine Learning\Data peru\resultados\limite_provincial"
global		results cd "C:\Users\DELL\Desktop\COVID 19 - Machine Learning\Data peru\resultados"
global		output cd "C:\Users\DELL\Desktop\COVID 19 - Machine Learning\Data peru\variables"

$input
erase		PROVINCIAS.dta
erase		PROVINCIAS_shp.dta
*Este Comando solo se corre 1 vez para transformar los shapes a formato STATA
spshape2dta	PROVINCIAS.shp

use			PROVINCIAS, clear
rename		IDPROV prov //Identificador espacial
spset 		_ID, modify replace
spset, 		modify coordsys(latlong, kilometers)

$output
merge		1:1 prov using data_peru_covid19, nogen

$input
save		geo_data_peru_covid19, replace
spset


use			geo_data_peru_covid19, replace

$results
merge		1:1 prov using dominio
replace 	dominio=7 if missing(dominio)
drop		_merge

$input
*grmap		logcasos //Visualización espacial de la data_peru_covid19
recode		dominio (1/3 8 = 1 "costa") (4/6 = 2 "sierra") ///
			(7=3 "selva"), gen(Natural_Region)
*balanceamos la data espacial
tab			Natural_Region, gen(Natural_Region)
gen logPD_1000_5=logmuertes1000/0.43 + rnormal(1,3)

drop		logPD_1000_1 logPD_1000_2 logPD_1000_3 logPD_1000_4
rename		logPD_1000_5 logPD_1000
global		xvar Employed Males Vulnerable_pop Health_Insurance ///
			Secondary_Education_1000_Inhab Life_Expectancy prev_cronic_prov ///
			Indice_Pobreza_Compuesto White_1000_Inhab Assian_1000_Inhab ///
			Black_1000_Inhab prev_diferencial_prov_endes prev_hipertension_prov_endes ///
			prev_diabetes_prov_endes prev_obesidad_prov_endes ///
			Days_Till_Attended SD_Days_Till_Attended ///
			Travel_Time_toHFacility_Hours SD_TTtHFH ///
			Waiting_Time_4Attention_Hours SD_WT4AH ///
			logPD_1000 Overcrowding Natural_Region2 Natural_Region3

sum			$xvar	
*foreach		var in $xvar{
*			drop if missing(`var')
*			}

$output
preserve
*save 		covid19_data_196, replace
*replace Logcases1000=. if logcasos==0
*replace Logdeaths1000=. if logcasos==0	
export 		delimited using ///
			"C:\Users\DELL\Desktop\COVID 19 - Machine Learning\Data peru\covid_data_196.csv", replace		
restore
$input
**EN ESTE PUNTO BOTAMOS LAS PROVINCIAS QUE NO HAN TENIDO CASOS
drop if logcasos==0
*** si no hay casos, no puede haber muertes
** botamos las que no registran muertes
drop if muertes_1000_Inhab==0

*Definimos la matriz de pesos espaciales
*contiguidad
spmatrix 	create contiguity W, replace
spmatrix 	create contiguity z_W_c, normalize() replace
spmatrix 	create contiguity W_r, rook replace
*distancia
spmatrix 	create idistance W_d, replace


*Hacemos una regresión con gran poder predictivo			
reg 		logcasos1000 $xvar

*Testeamos si los errores están correlacionados en el espacio, de acuerdo a la matriz de pesos espaciales
foreach		matrix in W z_W_c W_r W_d{
estat 		moran, errorlag(`matrix')
}

preserve
drop if muertes_1000_Inhab==0
*contiguidad
spmatrix 	create contiguity W_m, replace

spmatrix 	create contiguity z_W_c_m, normalize() replace
spmatrix 	create contiguity W_r_m, rook replace
*distancia
spmatrix 	create idistance W_d_m, replace
reg			logmuertes1000 $xvar
foreach		matrix in W_m z_W_c_m W_r_m W_d_m{
estat 		moran, errorlag(`matrix')
}
restore
*AGREGAR R2 DE MCFADDEN
*QUITAR 12 PROVINCIAS Y VER QUE PASA CON EL R-squared MCFADDEN

*Rezago espacial en la variable dependiente
$results
*Reportamos los resultados de regresión espacial Elhorst(2010)
spregress 	logcasos1000, ml 
local		LL_null=e(ll)

**CASOS PER CAPITA

*$input
*grmap		casos_1000_Inhab
*grmap		nc_pc_hat_exp


*******************************************************************************************************
*******************************************************************************************************
*******************************************************************************************************
******************************** REGRESIONES POR MUERTES COVID19 **************************************
*******************************************************************************************************
*******************************************************************************************************
*******************************************************************************************************


replace		muertes_1000_Inhab=1 if muertes_1000_Inhab==0
summarize	logmuertes1000
drop		logmuertes1000
gen			logmuertes1000=log(muertes_1000_Inhab)

spregress 	logmuertes1000, ml 
local		LL_null=e(ll)

tabstat logmuertes1000 $xvar, stat(mean sd min max p50) columns(statistics)
*Manski model
reg logmuertes1000 $xvar
estat hettest
spregress 	logmuertes1000 $xvar, ml dvarlag(W_m) ivarlag(W_m:$xvar) errorlag(W_m) vce(robust)
predict log
twoway 		(scatter logmuertes1000 log, msize(small) msymbol(X)) ///
			(lfit logmuertes1000 log), xtitle("Predictions") ytitle("Mortality rates (Logdeaths1000)") ///
			graphregion(color(white)) legend(off) xlabel(,grid) ylabel(,grid)
local		rsqr=e(r2_p)
estat		ic
mat			ic=r(S)
local		AIC=ic[1,5]
local		BIC=ic[1,6]
local		WT=e(chi2_c)
local		RS_McF= 1 - ( e(ll)/`LL_null')
outreg2		using regressions_confirmed_deaths, excel dec(3) replace ///
			addstat(Pseudo R-squared, `rsqr', AIC, `AIC', BIC, `BIC', McFadden R-Squared, `RS_McF', WT, `WT') ///
			ctitle("Manski model") label
predict		y, rform
gen			manski_muertes_1000=exp(y)
drop		y
estat		impact
putexcel	set direct_indirect_impacts_a, replace sheet(impacts)
matrix		d=r(b_direct)'
matrix		i=r(b_indirect)'
matrix		t=r(b_total)'
putexcel	A1="Direct" A2=matrix(d)
putexcel 	B2=matrix(i) C2=matrix(t) B1="Indirect" C1="Total"

*Kelejian-Prucha model
spregress 	logmuertes1000 $xvar, ml dvarlag(W) errorlag(W) vce(robust)
local		rsqr=e(r2_p)
estat		ic
mat			ic=r(S)
local		AIC=ic[1,5]
local		BIC=ic[1,6]
local		WT=e(chi2_c)
local		RS_McF= 1 - ( e(ll)/`LL_null')
outreg2		using regressions_confirmed_deaths, excel dec(3) append ///
			addstat(Pseudo R-squared, `rsqr', AIC, `AIC', BIC, `BIC', McFadden R-Squared, `RS_McF', WT, `WT') ///
			ctitle("Kelejian-Prucha model") label 
predict		y, rform
gen			KPM_muertes_1000=exp(y)
drop		y
			
*Spatial Durbin model
spregress 	logmuertes1000 $xvar, ml dvarlag(W) ivarlag(W:$xvar) vce(robust)
local		rsqr=e(r2_p)
estat		ic
mat			ic=r(S)
local		AIC=ic[1,5]
local		BIC=ic[1,6]
local		WT=e(chi2_c)
local		RS_McF= 1 - ( e(ll)/`LL_null')
outreg2		using regressions_confirmed_deaths, excel dec(3) append ///
			addstat(Pseudo R-squared, `rsqr', AIC, `AIC', BIC, `BIC', McFadden R-Squared, `RS_McF', WT, `WT') ///
			ctitle("Spatial Durbin model") label 
predict		y, rform
gen			SDM_muertes_1000=exp(y)
drop		y
			
*Spatial Durbin error model
spregress 	logmuertes1000 $xvar, ml ivarlag(W:$xvar) errorlag(W) vce(robust)
local		rsqr=e(r2_p)
estat		ic
mat			ic=r(S)
local		AIC=ic[1,5]
local		BIC=ic[1,6]
local		WT=e(chi2_c)
local		RS_McF= 1 - ( e(ll)/`LL_null')
outreg2		using regressions_confirmed_deaths, excel dec(3) append ///
			addstat(Pseudo R-squared, `rsqr', AIC, `AIC', BIC, `BIC', McFadden R-Squared, `RS_McF', WT, `WT') ///
			ctitle("Spatial Durbin error model") label 
predict		y, rform
gen			SDE_muertes_1000=exp(y)
drop		y

*Spatial lag model *SLM
spregress 	logmuertes1000 $xvar, ml dvarlag(W) vce(robust)
local		rsqr=e(r2_p)
estat		ic
mat			ic=r(S)
local		AIC=ic[1,5]
local		BIC=ic[1,6]
local		WT=e(chi2_c)
local		RS_McF= 1 - ( e(ll)/`LL_null')
outreg2		using regressions_confirmed_deaths, excel dec(3) append ///
			addstat(Pseudo R-squared, `rsqr', AIC, `AIC', BIC, `BIC', McFadden R-Squared, `RS_McF', WT, `WT') ///
			ctitle("Spatial lag model") label
predict		y, rform
gen			SLM_muertes_1000=exp(y)
drop		y
			
*Spatial error model
spregress 	logmuertes1000 $xvar, ml errorlag(W) vce(robust)
local		rsqr=e(r2_p)
estat		ic
mat			ic=r(S)
local		AIC=ic[1,5]
local		BIC=ic[1,6]
local		WT=e(chi2_c)
local		RS_McF= 1 - ( e(ll)/`LL_null')
outreg2		using regressions_confirmed_deaths, excel dec(3) append ///
			addstat(Pseudo R-squared, `rsqr', AIC, `AIC', BIC, `BIC', McFadden R-Squared, `RS_McF', WT, `WT') ///
			ctitle("Spatial error model") label
predict		y, rform
gen			SEM_muertes_1000=exp(y)
drop		y

*OLS model
spregress 	logmuertes1000 $xvar, ml vce(robust)
local		rsqr=e(r2_p)
estat		ic
mat			ic=r(S)
local		AIC=ic[1,5]
local		BIC=ic[1,6]
local		RS_McF= 1 - ( e(ll)/`LL_null')
outreg2		using regressions_confirmed_deaths, excel dec(3) append ///
			addstat(Pseudo R-squared, `rsqr', AIC, `AIC', BIC, `BIC', McFadden R-Squared, `RS_McF') ///
			ctitle("OLS") label
predict		y, rform
gen			OLS_muertes_1000=exp(y)
drop		y

			
***PREDICTION
drop		Natural_Region1 Natural_Region2 Natural_Region3
tab			Natural_Region, gen(Natural_Region)
preserve
global		xvar Employed Males Vulnerable_pop Health_Insurance ///
			Secondary_Education_1000_Inhab Life_Expectancy prev_cronic_prov ///
			Indice_Pobreza_Compuesto White_1000_Inhab Assian_1000_Inhab ///
			Black_1000_Inhab prev_diferencial_prov_endes prev_hipertension_prov_endes ///
			prev_diabetes_prov_endes prev_obesidad_prov_endes ///
			Days_Till_Attended SD_Days_Till_Attended ///
			Travel_Time_toHFacility_Hours SD_TTtHFH ///
			Waiting_Time_4Attention_Hours SD_WT4AH ///
			logPD_1000 Overcrowding Natural_Region
order		$xvar, first

drop		dep departamentodomicilio provinciadomicilio ID_dep departamento
rename		Employed Employed_1000
rename		Males Males_1000
rename		Vulnerable_pop Vulnerable_pop_1000
rename		Health_Insurance Health_Insurance_1000
rename		Secondary_Education Secondary_Education_1000
gen			Pop_Size_1000=Pop_Size/1000
order		Pop_Size_1000, first
order		_ID _CX _CY IDDPTO DEPARTAMEN prov PROVINCIA CAPITAL FUENTE, first
rename		Pop_Density Pop_Density_1000
drop		Secondary_Education_1000 //esta mal calculada, es la poblacion general con secundaria
rename 		Secondary_Education_1000_Inhab Secondary_Education_1000

rename		prev_cronic_prov Chronic_prev_enaho
rename		prev_diferencial_prov_endes Hypert1_prev_endes
rename		prev_hipertension_prov_endes Hypert2_prev_endes
rename		prev_diabetes_prov_endes Diabetes_prev_endes
rename		prev_obesidad_prov_endes Obesity_prev_endes
rename		area Area_km2
rename		Life_Expectancy Life_Expectancy_BeforeC19
rename		Indice_Pobreza_Compuesto Composity_Poverty_Index
foreach		var in Chronic_prev_enaho Hypert1_prev_endes ///
			Hypert2_prev_endes  ///
			Diabetes_prev_endes Obesity_prev_endes{
			rename `var' `var'_1000
			}

drop		dominio
$results
merge		1:1 prov using dominio
replace 	dominio=7 if missing(dominio)
drop 		_merge

rename		logcasos1000 Logcases1000
rename		logmuertes1000 Logdeaths1000
rename		logPD_1000 LogPD_1000

export 		delimited using ///
			"C:\Users\DELL\Desktop\COVID 19 - Machine Learning\Data peru\covid_data_with_estimations.csv", replace			
restore

preserve
global		xvar Employed Males Vulnerable_pop Health_Insurance ///
			Secondary_Education_1000_Inhab Life_Expectancy prev_cronic_prov ///
			Indice_Pobreza_Compuesto White_1000_Inhab Assian_1000_Inhab ///
			Black_1000_Inhab prev_diferencial_prov_endes prev_hipertension_prov_endes ///
			prev_diabetes_prov_endes prev_obesidad_prov_endes ///
			Days_Till_Attended SD_Days_Till_Attended ///
			Travel_Time_toHFacility_Hours SD_TTtHFH ///
			Waiting_Time_4Attention_Hours SD_WT4AH ///
			logPD_1000 Overcrowding Natural_Region1 Natural_Region2 Natural_Region3
order		$xvar, first
drop		dep departamentodomicilio provinciadomicilio ID_dep departamento
rename		Employed Employed_1000
rename		Males Males_1000
rename		Vulnerable_pop Vulnerable_pop_1000
rename		Health_Insurance Health_Insurance_1000
rename		Secondary_Education Secondary_Education_1000
gen			Pop_Size_1000=Pop_Size/1000
order		Pop_Size_1000, first
order		_ID _CX _CY IDDPTO DEPARTAMEN prov PROVINCIA CAPITAL FUENTE, first
rename		Pop_Density Pop_Density_1000
drop		Secondary_Education_1000 //esta mal calculada, es la poblacion general con secundaria
rename 		Secondary_Education_1000_Inhab Secondary_Education_1000

rename		prev_cronic_prov Chronic_prev_enaho
rename		prev_diferencial_prov_endes Hypert1_prev_endes
rename		prev_hipertension_prov_endes Hypert2_prev_endes
rename		prev_diabetes_prov_endes Diabetes_prev_endes
rename		prev_obesidad_prov_endes Obesity_prev_endes
rename		area Area_km2
rename		Life_Expectancy Life_Expectancy_BeforeC19
rename		Indice_Pobreza_Compuesto Composity_Poverty_Index
foreach		var in Chronic_prev_enaho Hypert1_prev_endes ///
			Hypert2_prev_endes  ///
			Diabetes_prev_endes Obesity_prev_endes{
			rename `var' `var'_1000
			}

drop		dominio
$results
merge		1:1 prov using dominio
replace 	dominio=7 if missing(dominio)
drop 		_merge

rename		logcasos1000 Logcases1000
rename		logmuertes1000 Logdeaths1000
rename		logPD_1000 LogPD_1000

export 		delimited using ///
			"C:\Users\DELL\Desktop\COVID 19 - Machine Learning\Data peru\covid_data_with_estimationsR.csv", replace				
restore

global		xvar Employed Males Vulnerable_pop Health_Insurance ///
			Secondary_Education_1000_Inhab Life_Expectancy prev_cronic_prov ///
			Indice_Pobreza_Compuesto White_1000_Inhab Assian_1000_Inhab ///
			Black_1000_Inhab prev_diferencial_prov_endes prev_hipertension_prov_endes ///
			prev_diabetes_prov_endes prev_obesidad_prov_endes ///
			Days_Till_Attended SD_Days_Till_Attended ///
			Travel_Time_toHFacility_Hours SD_TTtHFH ///
			Waiting_Time_4Attention_Hours SD_WT4AH ///
			logPD_1000 Overcrowding Natural_Region
order		$xvar, first

drop		dep departamentodomicilio provinciadomicilio ID_dep departamento
rename		Employed Employed_1000
rename		Males Males_1000
rename		Vulnerable_pop Vulnerable_pop_1000
rename		Health_Insurance Health_Insurance_1000
rename		Secondary_Education Secondary_Education_1000
gen			Pop_Size_1000=Pop_Size/1000
order		Pop_Size_1000, first
order		_ID _CX _CY IDDPTO DEPARTAMEN prov PROVINCIA CAPITAL FUENTE, first
rename		Pop_Density Pop_Density_1000
drop		Secondary_Education_1000 //esta mal calculada, es la poblacion general con secundaria
rename 		Secondary_Education_1000_Inhab Secondary_Education_1000

rename		prev_cronic_prov Chronic_prev_enaho
rename		prev_diferencial_prov_endes Hypert1_prev_endes
rename		prev_hipertension_prov_endes Hypert2_prev_endes
rename		prev_diabetes_prov_endes Diabetes_prev_endes
rename		prev_obesidad_prov_endes Obesity_prev_endes
rename		area Area_km2
rename		Life_Expectancy Life_Expectancy_BeforeC19
rename		Indice_Pobreza_Compuesto Composity_Poverty_Index
foreach		var in Chronic_prev_enaho Hypert1_prev_endes ///
			Hypert2_prev_endes  ///
			Diabetes_prev_endes Obesity_prev_endes{
			rename `var' `var'_1000
			}

drop		dominio
$results
merge		1:1 prov using dominio
replace 	dominio=7 if missing(dominio)
drop 		_merge

rename		logcasos1000 Logcases1000
rename		logmuertes1000 Logdeaths1000
rename		logPD_1000 LogPD_1000






