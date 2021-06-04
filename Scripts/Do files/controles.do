clear all
set more off, perm
cls

cd "~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos"
cap log using "dta/resultados/JorgeOchoaRincon.log", replace
use "/Users/jorgeochoa/Downloads/PANEL_CARACTERISTICAS_GENERALES(2020).dta", clear

keep coddepto codmpio  depto municipio ano pobl_tot altura discapital disbogota pib_agricola pib_industria pib_servicios pib_total gini pobreza nbi* IPM ipm_serv_pinf_p ipm_tdep_p ipm_accsalud_p gpc 


keep if ano >= 2000
by municipio, sort : egen float gpc_2005 = mean(gpc)
by municipio, sort : egen float gini_2005 = mean(gini)
by municipio, sort : egen float pobre_2005 = mean(pobreza)
by municipio, sort : egen float nbi_2005 = mean(nbi) if ano >2000&ano<2015
by municipio, sort : egen float nbi_c_2005 = mean(nbicabecera) if ano<2018
by municipio, sort : egen float nbi_r_2005 = mean(nbiresto) if ano < 2018
by municipio, sort : egen float ipm_2005 = mean(IPM) if ano < 2018
by municipio, sort : egen float ipm_pinf_2005 = mean(ipm_serv_pinf_p) if ano < 2018
by municipio, sort : egen float ipm_tdep_2005 = mean(ipm_tdep_p) if ano < 2018
by municipio, sort : egen float ipm_acsalud_2005 = mean(ipm_accsalud_p) if ano < 2018
sort ano


keep coddepto codmpio depto municipio ano pobl_tot altura discapital disbogota pib_agricola pib_industria pib_servicios pib_total *_2005 pib_*

foreach x in nbi_2005 nbi_c_2005 nbi_r_2005 ipm_2005 ipm_pinf_2005 ipm_tdep_2005 ipm_acsalud_2005 {

	egen `x'_2 = mean(`x'), by(municipio) 

}

drop nbi_2005 nbi_c_2005 nbi_r_2005 ipm_2005 ipm_pinf_2005 ipm_tdep_2005 ipm_acsalud_2005

rename (nbi_2005_2 nbi_c_2005_2 nbi_r_2005_2 ipm_2005_2 ipm_pinf_2005_2 ipm_tdep_2005_2 ipm_acsalud_2005_2) (nbi_2005 nbi_c_2005 nbi_r_2005 ipm_2005 ipm_pinf_2005 ipm_tdep_2005 ipm_acsalud_2005)

gen gdp_agri = pib_agricola if ano ==2009
gen gdp_indu = pib_industria if ano ==2009
gen gdp_serv = pib_servicios if ano ==2009
gen gdp_tota = pib_total if ano ==2009

egen gdp_agri_2 = mean(gdp_agri), by(municipio)
egen gdp_indu_2 = mean(gdp_indu), by(municipio)
egen gdp_serv_2 = mean(gdp_serv), by(municipio)
egen gdp_tota_2 = mean(gdp_tota), by(municipio)

drop gdp_agri gdp_indu gdp_serv gdp_tota pib_*

rename (gdp_agri_2 gdp_indu_2 gdp_serv_2 gdp_tota_2) (gdp_agri gdp_indu gdp_serv gdp_tota)

tostring codmpio,gen(cod_dane)
gen chara = length(cod_dane)
replace cod_dane = "0" + cod_dane if chara == 4
drop chara
drop if ano <2012

label var gpc_2005 "Gasto per cápita municipa - 2005"
label var gini_2005 "Índice de gini municipal - 2005"
label var pobre_2005 "Incidencia de la pobreza municipal - 2005"
label var nbi_2005 "NBI - Necesidades básicas Insatisfechas  - 2005"
label var nbi_c_2005 "NBI - Censo 2005 Cabecera municipal"
label var nbi_r_2005 "NBI - Censo 2005 rural disperso (resto)"
label var ipm_2005 "índice de pobreza multidimensional total - 2005"
label var ipm_pinf_2005 "ipm acceso servicios de primera infancia: porcentaje de población en privación"
label var ipm_tdep_2005 "ipm tasa dependencia económica: porcertanje de la población en privación"
label var ipm_acsalud_2005 "ipm accesos salud dada necesidad: porcentaje población en privación"
label var gdp_agri "Participación agricultura en el PIB total municipal"
label var gdp_indu "Participación industria en el PIB total municipal"
label var gdp_serv "Participación servicios en el PIB total municipal"
label var gdp_tota "PIB total municipal"

save "dta/carac_gene_cede.dta",replace

cls
use "/Users/jorgeochoa/Downloads/PANEL_CONFLICTO_Y_VIOLENCIA(2020).dta",clear

keep codmpio ano terrorismot homicidios

tostring codmpio,gen(cod_dane)
gen chara = length(cod_dane)
replace cod_dane = "0" + cod_dane if chara == 4
drop chara

bysort codmpio (ano) : gen cumu_terro = sum(terrorismot)
bysort codmpio (ano) : gen cumu_homic = sum(homicidios)

by codmpio, sort : egen float terrorismot_2003_2010 = mean(terrorismot) if ano < 2011&ano>2002
by codmpio, sort : egen float homicidios_2003_2010 = mean(homicidios) if ano < 2011&ano>2002

egen terrorismot_2003_2010_2 = mean(terrorismot_2003_2010), by(codmpio) 
egen homicidios_2003_2010_2 = mean(homicidios_2003_2010), by(codmpio) 

drop terrorismot_2003_2010 homicidios_2003_2010
rename (terrorismot_2003_2010_2 homicidios_2003_2010_2) (terrorismot_2003_2010 homicidios_2003_2010)

drop if ano <2012

label var terrorismot_2003_2010 "Promedio actos de terrorismo 2003-2010"
label var homicidios_2003_2010 "Promedio de homicidios 2003-2010"
label var cumu_terro "Hechos acumulados de terrorismo 2003 ->"
label var cumu_homic "Hechos acumulados de homicidios 2003 ->"

save "dta/confl_violencia_cede.dta",replace

cls
use "/Users/jorgeochoa/Downloads/PANEL_BUEN_GOBIERNO(2019).dta",clear

keep cod ano y_total g_total indesarrollo_dep indesarrollo_mun inv_total SGP_propgeneral 


tostring codmpio,gen(cod_dane)
gen chara = length(cod_dane)
replace cod_dane = "0" + cod_dane if chara == 4
drop chara

drop if ano <2000

by codmpio, sort : egen float y_total_2009 = mean(y_total) if ano == 2009
by codmpio, sort : egen float g_total_2009 = mean(g_total) if ano == 2009
by codmpio, sort : egen float indesarrollo_dep_2009 = mean(indesarrollo_dep) if ano == 2009
by codmpio, sort : egen float indesarrollo_mun_2009 = mean(indesarrollo_mun) if ano == 2009
by codmpio, sort : egen float inv_total_2009 = mean(inv_total) if ano == 2009
by codmpio, sort : egen float SGP_propgeneral_2009 = mean(SGP_propgeneral) if ano == 2009

egen y_total_2009_2 = mean(y_total_2009), by(codmpio) 
egen g_total_2009_2 = mean(g_total_2009), by(codmpio) 
egen indesarrollo_dep_2009_2 = mean(indesarrollo_dep_2009), by(codmpio) 
egen indesarrollo_mun_2009_2 = mean(indesarrollo_mun_2009), by(codmpio) 
egen inv_total_2009_2 = mean(inv_total_2009), by(codmpio) 
egen SGP_propgeneral_2009_2 = mean(SGP_propgeneral_2009), by(codmpio) 

drop y_total_2009 g_total_2009 indesarrollo_dep_2009 indesarrollo_mun_2009 inv_total_2009 SGP_propgeneral_2009

rename (y_total_2009_2 g_total_2009_2 indesarrollo_dep_2009_2 indesarrollo_mun_2009_2 inv_total_2009_2 SGP_propgeneral_2009_2) (y_total_2009 g_total_2009 indesarrollo_dep_2009 indesarrollo_mun_2009 inv_total_2009 SGP_propgeneral_2009)

drop y_total g_total indesarrollo_dep indesarrollo_mun inv_total SGP_propgeneral 

label var codmpio "Código DANE del municipio"
label var cod_dane "Código DANE del municipio"
label var ano "Año"
label var y_total_2009 "Ingresos totales 2009 = ingresos corrientes + ingresos de capital"
label var g_total_2009 "Gastos totales 2009 = Gastos corrientes + Gastos de capital"
label var indesarrollo_dep_2009 "Índice de desarrollo departamental 2009"
label var indesarrollo_mun_2009 "Índice de desarrollo municipal 2009"
label var inv_total_2009 "Inversión total 2009"
label var SGP_propgeneral_2009 "Total inversión proposito general SGP 2009"
drop if ano <2012

save "dta/buen_gobierno_cede.dta",replace

cls
use "/Users/jorgeochoa/Downloads/PANEL_DE_EDUCACION(2019).dta",clear

keep codmpio ano librosleidosprom anos_est_mun  col_total

tostring codmpio,gen(cod_dane)
gen chara = length(cod_dane)
replace cod_dane = "0" + cod_dane if chara == 4
drop chara

by codmpio, sort : egen float libros_2005 = mean(librosleidosprom) if ano == 2005
by codmpio, sort : egen float educ_prom_2005 = mean(anos_est_mun) if ano == 2005
by codmpio, sort : egen float cole_2005 = mean(col_total) if ano == 2005


egen libros_2005_2 = mean(libros_2005), by(codmpio) 
egen educ_prom_2005_2 = mean(educ_prom_2005), by(codmpio) 
egen cole_2005_2 = mean(cole_2005), by(codmpio) 


drop libros_2005 educ_prom_2005 cole_2005 librosleidosprom anos_est_mun  col_total

rename (libros_2005_2 educ_prom_2005_2 cole_2005_2)(libros_2005 educ_prom_2005 cole_2005)

drop if ano <2012 

label var libros_2005 "Libros leidos 2005"
label var educ_prom_2005 "Años promedio de eduación 2005"
label var cole_2005 "Número de colegios 2005"

save "dta/educacion_cede.dta",replace

cls
use "/Users/jorgeochoa/Downloads/PANEL_SALUD_Y_SERVICIOS.dta",clear

keep codmpio ano tacued talcan taseo 

tostring codmpio,gen(cod_dane)
gen chara = length(cod_dane)
replace cod_dane = "0" + cod_dane if chara == 4
drop chara

by codmpio, sort : egen tacued_2009 = mean(tacued) if ano == 2009
by codmpio, sort : egen talcan_2009 = mean(talcan) if ano == 2009
by codmpio, sort : egen taseo_2009 = mean(taseo) if ano == 2009

egen tacued_2009_2 = mean(tacued_2009), by(codmpio) 
egen talcan_2009_2 = mean(talcan_2009), by(codmpio) 
egen taseo_2009_2 = mean(taseo_2009), by(codmpio) 

drop tacued_2009 talcan_2009 taseo_2009 tacued talcan taseo 
rename (tacued_2009_2 talcan_2009_2 taseo_2009_2)(tacued_2009 talcan_2009 taseo_2009)

label var tacued_2009 "Cobertura de acueducto - 2009"
label var talcan_2009 "Cobertura de aseo - 2009"
label var taseo_2009 "Cobertura de alcantarillado - 2009"

drop if ano <2012 

drop if cod_dane =="05999"
drop if cod_dane =="08999"
drop if cod_dane =="13999"
drop if cod_dane =="15999"
drop if cod_dane =="17999"
drop if cod_dane =="18999"
drop if cod_dane =="19856"
drop if cod_dane =="19999"
drop if cod_dane =="20999"
drop if cod_dane =="23685"
drop if cod_dane =="23999"
drop if cod_dane =="25999"
drop if cod_dane =="27086"
drop if cod_dane =="27999"
drop if cod_dane =="41000"
drop if cod_dane =="41999"
drop if cod_dane =="44999"
drop if cod_dane =="47999"
drop if cod_dane =="50999"
drop if cod_dane =="52524"
drop if cod_dane =="52999"
drop if cod_dane =="54999"
drop if cod_dane =="63999"
drop if cod_dane =="66999"
drop if cod_dane =="68999"
drop if cod_dane =="70000"
drop if cod_dane =="70999"
drop if cod_dane =="73999"
drop if cod_dane =="75004"
drop if cod_dane =="75008"
drop if cod_dane =="75032"
drop if cod_dane =="75036"
drop if cod_dane =="75040"
drop if cod_dane =="75052"
drop if cod_dane =="75056"
drop if cod_dane =="75068"
drop if cod_dane =="75076"
drop if cod_dane =="75100"
drop if cod_dane =="75124"
drop if cod_dane =="75152"
drop if cod_dane =="75156"
drop if cod_dane =="75174"
drop if cod_dane =="75180"
drop if cod_dane =="75188"
drop if cod_dane =="75191"
drop if cod_dane =="75192"
drop if cod_dane =="75208"
drop if cod_dane =="75214"
drop if cod_dane =="75218"
drop if cod_dane =="75222"
drop if cod_dane =="75238"
drop if cod_dane =="75249"
drop if cod_dane =="75250"
drop if cod_dane =="75254"
drop if cod_dane =="75276"
drop if cod_dane =="75328"
drop if cod_dane =="75332"
drop if cod_dane =="75340"
drop if cod_dane =="75356"
drop if cod_dane =="75364"
drop if cod_dane =="75372"
drop if cod_dane =="75376"
drop if cod_dane =="75380"
drop if cod_dane =="75384"
drop if cod_dane =="75388"
drop if cod_dane =="75392"
drop if cod_dane =="75428"
drop if cod_dane =="75484"
drop if cod_dane =="75528"
drop if cod_dane =="75530"
drop if cod_dane =="75533"
drop if cod_dane =="75554"
drop if cod_dane =="75558"
drop if cod_dane =="75578"
drop if cod_dane =="75591"
drop if cod_dane =="75600"
drop if cod_dane =="75604"
drop if cod_dane =="75608"
drop if cod_dane =="75620"
drop if cod_dane =="75630"
drop if cod_dane =="75659"
drop if cod_dane =="75662"
drop if cod_dane =="75674"
drop if cod_dane =="75724"
drop if cod_dane =="75740"
drop if cod_dane =="75748"
drop if cod_dane =="75752"
drop if cod_dane =="75756"
drop if cod_dane =="75780"
drop if cod_dane =="75784"
drop if cod_dane =="75796"
drop if cod_dane =="75804"
drop if cod_dane =="75826"
drop if cod_dane =="75840"
drop if cod_dane =="75850"
drop if cod_dane =="75858"
drop if cod_dane =="75862"
drop if cod_dane =="75998"
drop if cod_dane =="75999"
drop if cod_dane =="76000"
drop if cod_dane =="76999"
drop if cod_dane =="81999"
drop if cod_dane =="85999"
drop if cod_dane =="86999"
drop if cod_dane =="91000"
drop if cod_dane =="91999"
drop if cod_dane =="94000"
drop if cod_dane =="94999"
drop if cod_dane =="95000"
drop if cod_dane =="95999"
drop if cod_dane =="97000"
drop if cod_dane =="97999"
drop if cod_dane =="99000"
drop if cod_dane =="99572"
drop if cod_dane =="99760"

save "dta/salud_servicios_cede.dta",replace
cls

quiet use "dta/buen_gobierno_cede",clear
merge m:1 cod_dane ano using "dta/carac_gene_cede.dta", gen(buen_gene)
 
merge 1:m cod_dane ano using "dta/confl_violencia_cede.dta", gen(buen_gene_vio)

merge m:1 cod_dane ano using "dta/educacion_cede.dta", gen(buen_gene_vio_edu)

merge m:1 cod_dane ano using "dta/salud_servicios_cede.dta", gen(buen_gene_vio_edu_serv)

drop if cod_dane == "."

drop buen_*

save "dta/controles cede.dta",replace
cap log close
