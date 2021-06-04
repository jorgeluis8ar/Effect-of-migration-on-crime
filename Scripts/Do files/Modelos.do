clear all
set more off, perm
cls

cd "/Users/jorgeochoa/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/dta/"

use "base final.dta",clear

replace amenazas = 0 if amenazas == .
replace delitos = 0 if delitos == .
replace lesiones = 0 if lesiones == .
replace hurtopersonas = 0 if hurtopersonas == .
replace hurtoresidencias = 0 if hurtoresidencias == .
replace homicidios = 0 if homicidios == .

gen total_crime = amenazas +delitos +lesiones +hurtopersonas +hurtoresidencias +homicidios

global controles y_total_2009 g_total_2009 indesarrollo_mun_2009 SGP_propgeneral_2009 gpc_2005 gini_2005 pobre_2005 nbi_2005 ipm_2005 gdp_agri gdp_indu gdp_serv cumu_terro cumu_homic libros_2005 educ_prom_2005 cole_2005  distancia_promediada

gen time = fecha - 624

global time_controles 

foreach x of global controles {
	dis "`x'"
	gen time_`x' = `x'*time
	label var time_`x' "Control: `x' interactuado con tiempo"
	global time_controles ${time_controles} time_`x'		
}

global estadisticas amenazas delitos lesiones hurtopersonas hurtoresidencias homicidios total_crime inmigrantes_1_ano inmigrantes_5_anos extr_pob_1_ano extr_pob_5_ano total_migra ${controles}

matrix define estadi = J(38,7,.)
matrix colnames estadi = "Year" "Obs" "Mean" "St Dev" "Min" "Median" "Max"
matrix rownames estadi = ${estadisticas}
matlist estadi

local fila = 1
foreach x of global estadisticas{
	
	quiet sum `x',d
	
	matrix estadi[`fila',2] = r(N)	
	matrix estadi[`fila',3] = r(mean)
	matrix estadi[`fila',4] = r(sd)
	matrix estadi[`fila',5] = r(min)
	matrix estadi[`fila',6] = r(p50)
	matrix estadi[`fila',7] = r(max)
	
	local fila = `fila' + 1	
	
}
mat li estadi, format(%15,3f)

drop if cod_dane == "NA"
drop if cod_dane == "NO RE"

label var cumulative_migration  "Migración acumulada venezolanos 2012-01 a 2019-12"

** Modelo IV con instrumento bartick ----------------------------------------------

** Instumento Bartick

gen bartick  = cumulative_migration*part_ven_ajustada_2005
gen bartick2 = (cumulative_migration*part_ven_ajustada_2005)/pobl_tot
gen bartick3 = (cumulative_migration*part_ven_ajustada_2005)/distancia_promediada

label var bartick  "Instrumento basado en Bartick(1991). (Migracion acumulada * % venzolanos en municipio)"
label var bartick2 "Instrumento basado en Bartick(1991). (Migracion acumulada * % venzolanos en municipio)/Poblacion total"
label var bartick3 "Instrumento basado en Bartick(1991). (Migracion acumulada * % venzolanos en municipio)distancia promedio a puntos de migracion"

egen dep  = group(fecha coddepto)

gen homi_sine = log(homicidios + (homicidios^2 + 1)^1/2)
gen amen_sine = log(amenazas + (amenazas^2 + 1)^1/2)
gen deli_sine = log(delitos + (delitos^2 + 1)^1/2)
gen lesi_sine = log(lesiones + (lesiones^2 + 1)^1/2)
gen hupe_sine = log(hurtopersonas + (hurtopersonas^2 + 1)^1/2)
gen hure_sine = log(hurtoresidencias + (hurtoresidencias^2 + 1)^1/2)
gen tota_sine = log(total_crime + (total_crime^2 + 1)^1/2)

label var homi_sine "Transformación SHI: sinh-1(x) = log(x + (x 2 + 1)1/2)"
label var amen_sine "Transformación SHI: sinh-1(x) = log(x + (x 2 + 1)1/2)"
label var deli_sine "Transformación SHI: sinh-1(x) = log(x + (x 2 + 1)1/2)"
label var lesi_sine "Transformación SHI: sinh-1(x) = log(x + (x 2 + 1)1/2)"
label var hupe_sine "Transformación SHI: sinh-1(x) = log(x + (x 2 + 1)1/2)"
label var hure_sine "Transformación SHI: sinh-1(x) = log(x + (x 2 + 1)1/2)"
label var tota_sine "Transformación SHI: sinh-1(x) = log(x + (x 2 + 1)1/2)"


quiet ivreghdfe tota_sine (entra_pred_ven = bartick3) promedio_luz_editada ${time_controles}, absorb(cod_dane dep) savefirst first sfirst ffirst cluster(cod_dane) savefprefix(betas_primera)
quiet outreg2 using "resultados/segunda etapa.xls", replace cti("Prueba : Segunda etapa") keep(entra_pred_ven) addstat(F-test,e(F),Clusters,e(N_clust),R2-ajustado, e(r2_a)) addtext(EF Departamento X Tiempo, Si, EF municipio,si)
quiet reghdfe entra_pred_ven bartick3 promedio_luz_editada ${time_controles} if tota_sine != ., absorb(cod_dane dep) cluster(cod_dane)
quiet outreg2 using "resultados/primera etapa.xls", replace cti("Prueba : Primera etapa") keep(bartick3) addstat(F-test,e(F),Clusters,e(N_clust),R2-ajustado, e(r2_a)) addtext(EF Departamento X Tiempo, Si, EF municipio,si) nocons
quiet reghdfe tota_sine bartick3 promedio_luz_editada ${time_controles} if tota_sine != ., absorb(cod_dane dep) cluster(cod_dane)
quiet outreg2 using "resultados/forma reducida.xls", replace cti("Prueba : forma reducida") keep(bartick3) addstat(F-test,e(F),Clusters,e(N_clust),R2-ajustado, e(r2_a)) addtext(EF Departamento X Tiempo, Si, EF municipio,si) nocons	


global sine_criemes amen_sine deli_sine lesi_sine hupe_sine hure_sine homi_sine tota_sine

foreach migracion in entra_pred_ven esti_ven_migra_proyeccion_1{

	foreach g of global sine_criemes{
		quiet sum `g'
		local promedio = r(mean)
		quiet ivreghdfe `g' (`migracion' = bartick3) promedio_luz_editada ${time_controles}, absorb(cod_dane dep) cluster(cod_dane) savefirst first sfirst ffirst savefprefix(betas_primera)
		local beta = _b[`migracion']
		local efecto_economico = (`promedio'*100*`beta')
		quiet outreg2 using "resultados/segunda etapa.xls", append cti("`g' : Segunda etapa, migracion: `migracion'") keep(`migracion') addstat(F-test,e(F),Clusters,e(N_clust),R2-ajustado, e(r2_a)) addtext(EF Departamento X Tiempo, Si, EF municipio,si, Efecto económico, `efecto_economico')
		quiet reghdfe `migracion' bartick3 promedio_luz_editada ${time_controles} if `g' != ., absorb(cod_dane dep) cluster(cod_dane)
		quiet outreg2 using "resultados/primera etapa.xls", append cti("`g' : Primera etapa, migracion: `migracion'") keep(bartick3) addstat(F-test,e(F),Clusters,e(N_clust),R2-ajustado, e(r2_a)) addtext(EF Departamento X Tiempo, Si, EF municipio,si) nocons
		quiet reghdfe `g' bartick3 promedio_luz_editada ${time_controles} if `g' != ., absorb(cod_dane dep) cluster(cod_dane)
		quiet outreg2 using "resultados/forma reducida.xls", append cti("`g' : forma reducida, migracion: `migracion'") keep(bartick3) addstat(F-test,e(F),Clusters,e(N_clust),R2-ajustado, e(r2_a)) addtext(EF Departamento X Tiempo, Si, EF municipio,si) nocons	
		

	}
}

seeout using "resultados/segunda etapa.txt"

seeout using "resultados/primera etapa.txt"

seeout using "resultados/forma reducida.txt"

quiet reghdfe amen_sine entra_pred_ven promedio_luz_editada ${time_controles}, a(dep cod_dane) cluster(cod_dane)
quiet outreg2 using "resultados/efectos_fijos.xls", replace keep(entra_pred_ven) cti("`f' FE") addtext(EF Departamento X Tiempo, Si, EF municipio,si) addstat(F-test,e(F),Clusters,e(N_clust),R2-ajustado, e(r2_a))

foreach migracion in entra_pred_ven esti_ven_migra_proyeccion_1{
	foreach f of global sine_criemes {
		quiet sum `g'
		local promedio = r(mean)
		quiet reghdfe `f' `migracion' promedio_luz_editada ${time_controles}, a(dep cod_dane) cluster(cod_dane)
		local beta = _b[`migracion']
		local efecto_economico = (`promedio'*100*`beta')
		quiet outreg2 using "resultados/efectos_fijos.xls", append keep(`migracion') cti("`f' FE") addtext(EF Departamento X Tiempo, Si, EF municipio,si) addstat(F-test,e(F),Clusters,e(N_clust),R2-ajustado, e(r2_a), Efecto económico, `efecto_economico')
	}
}
seeout using "resultados/efectos_fijos.txt"

cap drop ptile
cap drop grupo

xtile ptile = distancia_promediada,nq(100)

gen grupo = 1 if ptile <=10
replace grupo = 2 if ptile <=20 & ptile > 10
replace grupo = 3 if ptile <=30 & ptile > 20
replace grupo = 4 if ptile <=40 & ptile > 30
replace grupo = 5 if ptile <=50 & ptile > 40
replace grupo = 6 if ptile <=60 & ptile > 50
replace grupo = 7 if ptile <=70 & ptile > 60
replace grupo = 8 if ptile <=80 & ptile > 70
replace grupo = 9 if ptile <=90 & ptile > 80
replace grupo = 10 if ptile <=100 & ptile > 90

foreach x of global sine_criemes{
	forvalues i=1(1)10{
		dis "El valor `i' para el crimen `x'"
		quiet ivreghdfe `x' (entra_pred_ven = bartick3) promedio_luz_editada ${time_controles} if grupo == `i', absorb(cod_dane dep) cluster(cod_dane) savefirst first sfirst ffirst savefprefix(betas_primera)
		quiet lincomest _b[entra_pred_ven]
		quiet est store `x'_`i'_distancia
	}
}		  

label var homi_sine "Homicidios* - Tasa X 100. habitantes"
label var amen_sine "Amenazas* - Tasa X 100. habitantes"
label var deli_sine "Delitos sexuales* - Tasa X 100.000 habitantes"
label var lesi_sine "Lesiones personales* - Tasa X 100.000 habitantes"
label var hupe_sine "Hurto a persona* - Tasa X 100.000 habitantes"
label var hure_sine "Hurto a residencias* - Tasa X 100.000 habitantes"
label var tota_sine "Crímenes totales* - Tasa X 100.000 habitantes"

foreach x of global sine_criemes {
coefplot (`x'_1_distancia, rename((1) = "Primer grupo")  \                    ///
		  `x'_2_distancia, rename((1) = "Segundo grupo") \                    ///
		  `x'_3_distancia, rename((1) = "Tercer grupo")  \                    ///
		  `x'_4_distancia, rename((1) = "Cuarto grupo")  \                    ///
		  `x'_5_distancia, rename((1) = "Quinto grupo")  \                    ///
		  `x'_6_distancia, rename((1) = "Sexto grupo")  \                     ///
		  `x'_7_distancia, rename((1) = "Séptimo grupo")  \                   ///
		  `x'_8_distancia, rename((1) = "Octavo grupo")  \                    ///
		  `x'_9_distancia, rename((1) = "Noveno grupo")  \                    ///
		  `x'_10_distancia, rename((1) = "Decimo grupo") label("Amenazas")) , ///
		  ciopts(recast(rcap)) msymbol(O) vertical                            ///
		  ylabel(, labsize(small) nogrid) xlabel(, labsize(small) angle(vertical) nogrid) ///
		  mlcolor(black)  title("Efectos por grupos de distancia a la frontera") ///
		  yscale(lc(black)) xscale(lc(black)) name(graf_`x',replace)          ///
		  graphregion(color(white)) yline(0, lcolor(maroon))                  ///
		  subtitle("`: var label `x''", size(medium))                         ///
		  caption("*:Transformación Seno Hiperbólica Inversa",size(small))
graph export "grafica/`x'.pdf", as(pdf) name("graf_`x'") replace
}
cap drop *_distancia
