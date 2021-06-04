clear all
set more off, perm
cls

cd "~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos"
cap log using "dta/resultados/JorgeOchoaRincon.log", append
import delimited "csv/migracion ibanezrozo.csv", delimiter(comma) varnames(1) encoding(UTF-8) stringcols(4 7) clear 

drop v1

gen cod_dane = dpto_ccdgo+mpio_ccdgo
gen fecha = ym(ano,mes)
format fecha %tm

order fecha cod_dane mpi dpto inmigrantes_1_ano inmigrantes_5_anos extr_pob_1_ano extr_pob_5_ano  pob_total pob_total_cab_municipal pob_total_resto_municipal  dis_mun_migra dis_mun_punt_min sum_weigths total_migra mig_vene

drop mes dpto_ccdgo mpio_ccdgo mpio_cnmbr 

foreach x in inmigrantes_1_ano inmigrantes_5_anos  extr_pob_1_ano extr_pob_5_ano  pob_total pob_total_cab_municipal pob_total_resto_municipal dis_mun_migra dis_mun_punt_min sum_weigths mig_vene distancia_promediada{

	cap replace `x' = "." if `x' == "NA"
	destring `x' , replace

}

label var fecha "Fecha: año - mes"
label var cod_dane "Código DANE del municipio"
label var mpi "Nombre municipio"
label var dpto "Nombre del Departamento donde se encuentra el municipio"
label var inmigrantes_1_ano "Número de personas provenientes otro país hace un año - Censo Poblacional DANE 2018"
label var inmigrantes_5_anos "Número de personas provenientes otro país hace 5 años - Censo Poblacional DANE 2018"
label var extr_pob_1_ano "Proporción población extranjera un año: (inmigrantes_1_ano/pob_total)*100"
label var extr_pob_5_ano "Proporción población extranjera 5 años :(inmigrantes_5_anos/pob_total)*100"
label var pob_total "Población total del municipio - Censo Poblacional DANE 2018"
label var pob_total_cab_municipal "Población total en la cabecera del municipio - Censo Poblacional DANE 2018"
label var pob_total_resto_municipal "Población total resto del municipio - Censo Poblacional DANE 2018"
label var dis_mun_migra "Distancia mínima del centroide del municipio a un punto de migración"
label var dis_mun_punt_min "Distancia mínima del centroide del municipio a uno de los 5 puntos de migración en la fronterea venezolana"
label var sum_weigths "Inverso de la suma ponderada de la distancia a los 5 puntos de migración venezuela"
label var total_migra "Migración venezolana total en todos los puntos de migración em Colombia"
label var mig_vene "Migración predicha venezolana"
label var ano "Año"
label var distancia_promediada "Distancia promedio centroide muncipio y 5 puntos fronterizos"

save "dta/mig vene iba rozo.dta",replace

merge 1:m fecha cod_dane using "dta/crimenes totales.dta", gen(m1)
//use "dta/crimenes totales.dta",clear
dis 7720 + 8710 + 8*13464

drop if year <= 2011
drop if ano == 2020

drop departamento municipio

encode cod_dane, gen(codi_dane)
xtset codi_dane fecha, monthly
xtdescribe

bysort cod_dane (fecha): gen cumulative_migration = sum(total_migra)

merge m:1 cod_dane ano using "dta/controles cede.dta", gen(m3)
merge m:1 cod_dane ano using "dta/luces procesadas.dta", gen(m4)

drop m1 m3 m4
* Rozo & Vargas (2015)
gen esti_ven_migra_censo_1 = (cumulative_migration*(extr_pob_1_ano/100))/pob_total
gen esti_ven_migra_proyeccion_1 = (cumulative_migration*(extr_pob_1_ano/100))/pobl_tot
gen esti_ven_migra_censo_5 = (cumulative_migration*(extr_pob_5_ano/100))/pob_total
gen esti_ven_migra_proyeccion_5 = (cumulative_migration*(extr_pob_5_ano/100))/pobl_tot
* Ibañez & Rozo (2020)
gen entra_pred_ven  = (cumulative_migration*sum_weigths)


label var esti_ven_migra_censo_1 "Migración venezolana estimada Rozo & Vargas (2015) - Pob censal 2018 y prop extranjeros 1 año"
label var esti_ven_migra_censo_5 "Migración venezolana estimada Rozo & Vargas (2015) - Pob censal 2018 y prop extranjeros 5 años"
label var esti_ven_migra_proyeccion_1 "Migración venezolana estimada Rozo & Vargas (2015) - Proyecciones censo 2005 y prop extranjeros 1 año"
label var esti_ven_migra_proyeccion_5 "Migración venezolana estimada Rozo & Vargas (2015) - Proyecciones censo 2005 y prop extranjeros 5 años"
label var entra_pred_ven "Entrada migración predicha Ibañez & Rozo (2020)"

sort cod_dane fecha

foreach z in amenazas delitos lesiones hurtopersonas hurtoresidencias homicidios{
	
	replace `z' =  (`z'/pobl_tot) * 100000
	label var `z' ""
	dis "`: var label `z'' x 100.000 hab"
} 

label var amenazas "Amenazas - Tasa x 100.000 habitantes"
label var delitos "Delitos sexuales - Tasa x 100.000 habitantes"
label var lesiones "Lesiones personales - Tasa x 100.000 habitantes"
label var hurtopersonas "Hurto a personas - Tasa x 100.000 habitantes"
label var hurtoresidencias "Hurto a residencias - Tasa x 100.000 habitantes"
label var homicidios "Homicidios - Tasa x 100.000 habitantes"
label var year_month "Identificador mes año R"

merge m:1 cod_dane using "dta/censo 2005.dta", gen(m4)
drop m4
save "dta/base final.dta",replace
cap log close
