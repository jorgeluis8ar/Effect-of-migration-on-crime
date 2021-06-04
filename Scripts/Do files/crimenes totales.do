clear all
set more off, perm
cls

cd "/Users/jorgeochoa/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos"

import delimited "csv/crimenes total.csv",clear

replace amenazas = "." if amenazas =="NA"
replace delitos = "." if delitos =="NA"
replace lesiones = "." if lesiones =="NA"
replace hurtopersonas = "." if hurtopersonas =="NA"
replace hurtoresidencias = "." if hurtoresidencias =="NA"
replace homicidios = "." if homicidios =="NA"
replace prom_edad_homi = "." if prom_edad_homi =="NA"

destring amenazas delitos lesiones hurtopersonas hurtoresidencias homicidios prom_edad_homi, replace

drop v1 year_month

gen fecha = ym(year,mes)
format fecha %tm

order fecha departamento municipio cod_dane
order year mes, last

label var fecha "Fecha año - mes"
label var departamento "Nombre departamento DANE"
label var municipio "Nombre municipio DANE"
label var cod_dane "Código DANE del municipio"
label var amenazas "Número total de amenazas para la fecha"
label var delitos "Número total de delitos sexuales para la fecha"
label var lesiones "Número total de lesiones personales para la fecha"
label var hurtopersonas "Número total de hurtos a personas para la fecha"
label var hurtoresidencias "Número total de hurtos a residencias para la fecha"
label var homicidios "Número total de homicidios para la fecha"
label var prom_edad_homi "Promedio de edad de los homicidios ocurridos en la fecha"
label var year "Año de ocurrencia"
label var mes "Mes de ocurrencia"

save "dta/crimenes totales.dta",replace
