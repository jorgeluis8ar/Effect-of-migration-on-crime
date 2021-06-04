# Elaborado por: Jorge Luis Ochoa Rincón
# Nota: No se usan acentos ni caracteres especiales para evitar conflictos entre los diferentes sistemas operativos.

cat('\f')
rm(list=ls())
options('scipen'=100, 'digits'=4) # Forzar a R a no usar e+
setwd("~/OneDrive - Universidad de los andes/PEG/Economía del Crimen/Proyecto de investigacion")
getwd()


paquetes = c("tidyverse",'sf','grid','png','ggplot2','dplyr','haven','raster','data.table','plotly')
sapply(paquetes,require,character.only=T)

files = list.files('Crimenes') %>% paste0('Crimenes/',.,'/')
archivos = lapply(1:length(files),function(x) list.files(files[x]) %>% paste0(files[x],.))  %>% unlist()

load.list <- function(path){
  print(path)
  dta <- readxl::read_excel(path)
  indicador <- dta[,1] %>% as.matrix() %>% grep('DEPARTAMENTO', .)
  if (length(indicador)==0) {
    indicador <- dta[,1] %>% as.matrix() %>% grep('FECHA', .)  
  }
  if (length(indicador)==0) {
    indicador <- dta[,1] %>% as.matrix() %>% grep('Fecha', .)  
  }
  nombres <- dta[indicador,1:ncol(dta)] %>% as.vector() %>% as.character()
  colnames(dta) <- tolower(nombres)
  colnames(dta)[grep('fecha hecho', colnames(dta))] <- 'fecha'
  indicador2 <- dta[,1] %>% as.matrix() %>% grep('TOTAL', .)
  dta <- dta[(indicador+1):(indicador2-1),]
  dta$`fecha hecho` <- as.numeric(dta$fecha)
  return(dta)
}

data_list <- lapply(archivos, function(x) load.list(path =x))

amenazas <- plyr::rbind.fill(data_list[1:10] %>% data.table::rbindlist(use.names = T, fill = T))
delitoss <- plyr::rbind.fill(data_list[11:20] %>% data.table::rbindlist(use.names = T, fill = T))
homi <- plyr::rbind.fill(data_list[21:30] %>% data.table::rbindlist(use.names = T, fill = T))
hurto_p <- plyr::rbind.fill(data_list[31:40] %>% data.table::rbindlist(use.names = T, fill = T))
hurto_r <- plyr::rbind.fill(data_list[41:50] %>% data.table::rbindlist(use.names = T, fill = T))
lesion <- plyr::rbind.fill(data_list[51:60] %>% data.table::rbindlist(use.names = T, fill = T))

homi$dane <- if_else(is.na(homi$`codigo dane`)==T,homi$`código dane`,homi$`codigo dane`)
homi$cod_dane <- substr(homi$dane,1,5)
homi$cod_dane_dpto <- substr(homi$dane,1,2)
homi$movil_agresor <- if_else(is.na(homi$`movil agresor`)==T,homi$`móvil agresor`,homi$`movil agresor`)
homi$movil_victima <- if_else(is.na(homi$`movil victima`)==T,homi$`móvil victima`,homi$`movil victima`)
homi$clase_sitio <- if_else(is.na(homi$`clase de sitio`)==T,homi$`clase sitio`,homi$`clase de sitio`)
homi$pais_nacimiento <- if_else(is.na(homi$`pais nace`)==T,homi$`país de nacimiento`,homi$`pais nace`)
homi$clase_empleado <- if_else(is.na(homi$`clase de empleado`)==T,homi$`clase empleado`,homi$`clase de empleado`)
homi$prof_sio <- if_else(is.na(homi$profesiones)==T,homi$profesión,homi$profesiones)
homi$day <- if_else(is.na(homi$dia)==T,homi$día,homi$dia)

variables <- c(grep("fecha", colnames(homi)),
               grep("departamento", colnames(homi)),
               grep("municipio", colnames(homi)),
               grep("day", colnames(homi)),
               grep("hora", colnames(homi)),
               grep("barrio", colnames(homi)),
               grep("zona", colnames(homi)),
               grep("clase_sitio", colnames(homi)),
               grep("arma empleada", colnames(homi)),
               grep("movil_agresor", colnames(homi)),
               grep("movil_victima", colnames(homi)),
               grep("edad",colnames(homi)),
               grep("sexo", colnames(homi)),
               grep("estado civil", colnames(homi)),
               grep("pais_nacimiento", colnames(homi)),
               grep("clase_empleado", colnames(homi)),
               grep("prof_sio", colnames(homi)),
               grep("escolaridad", colnames(homi)),
               grep("cod_dane", colnames(homi)),
               grep("cantidad", colnames(homi)))

homi <- homi[,c(1,21,2,3,42,5,6,7,38,9,36,37,12,13,14,39,40,41,18,34,35,32)]
homi$fecha <- as.Date(as.numeric(homi$fecha), origin = "1899-12-30")
homi <- mutate(homi, year_month=zoo::as.yearmon(homi$fecha))
homi$hora <- as.numeric(homi$hora)
homi$edad <- as.numeric(homi$edad)
homi$cantidad <- as.numeric(homi$cantidad)
homi$year <- format(homi$fecha,"%Y")

homi2 <- homi %>% group_by(year_month) %>% summarise(Total_mes = sum(cantidad, na.rm = T),total_mes = n())
homi2$Total<-if_else(homi2$Total_mes==0,as.double(homi2$total_mes), homi2$Total_mes)
homicidios <- ggplot(data = homi2,aes(x = year_month,y = total_mes)) +geom_line()
ggplotly(homicidios)
# homi %>% group_by(cod_dane,year_month,year) %>% summarise(Homi_mes = sum(cantidad,na.rm = T), total_mes = n())
saveRDS(object = homi,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Procesada/homicidios 2010-2012.RDS')
saveRDS(object = homi2,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Graficas/grafica homicidio.RDS')
rm(homi,homi2)

variables <- c(grep("fecha", colnames(amenazas)),
               grep("departamento", colnames(amenazas)),
               grep("municipio", colnames(amenazas)),
               grep("dane", colnames(amenazas)),
               grep("genero", colnames(amenazas)),
               grep("cantidad", colnames(amenazas)))

amenazas <- amenazas[,c(5,9,1,2,3,6,8)]

colnames(amenazas)[5] <- "cod_dane"
amenazas$cod_dane <- substr(amenazas$cod_dane,1,5)
amenazas$cod_dane_dpto <- substr(amenazas$cod_dane,1,2)
amenazas$fecha <- as.Date(as.numeric(amenazas$fecha), origin = "1899-12-30")
amenazas$cantidad <- as.numeric(amenazas$cantidad)
amenazas$year <- format(amenazas$fecha,"%Y")
amenazas <- mutate(amenazas, year_month=zoo::as.yearmon(amenazas$fecha))
amenazas2 <- amenazas %>% group_by(year_month) %>% summarise(Total_mes = sum(cantidad, na.rm = T),total_mes = n())
ggplot(data = amenazas2,aes(x = year_month,y = Total_mes)) +geom_line()

saveRDS(object = amenazas,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Procesada/amenazas 2010-2012.RDS')
saveRDS(object = amenazas2,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Graficas/grafica amenazas.RDS')
rm(amenazas,amenazas2)

variables <- c(grep("fecha", colnames(delitoss)),
               grep("departamento", colnames(delitoss)),
               grep("municipio", colnames(delitoss)),
               grep("dane", colnames(delitoss)),
               grep("genero", colnames(delitoss)),
               grep("cantidad", colnames(delitoss)))

delitoss <- delitoss[,c(5,9,1,2,3,6,8)]

colnames(delitoss)[5] <- "cod_dane"
delitoss$cod_dane <-ifelse(nchar(delitoss$cod_dane)==7,paste0('0',delitoss$cod_dane),delitoss$cod_dane)
delitoss$cod_dane <- substr(delitoss$cod_dane,1,5)
delitoss$cod_dane_dpto <- substr(delitoss$cod_dane,1,2)
delitoss$fecha <- as.Date(as.numeric(delitoss$fecha), origin = "1899-12-30")
delitoss$cantidad <- as.numeric(delitoss$cantidad)
delitoss$year <- format(delitoss$fecha,"%Y")
delitoss <- mutate(delitoss, year_month=zoo::as.yearmon(delitoss$fecha))
delitoss2 <- delitoss %>% group_by(year_month) %>% summarise(Total_mes = sum(cantidad, na.rm = T),total_mes = n())
ggplot(data = delitoss2,aes(x = year_month,y = Total_mes)) +geom_line()

saveRDS(object = delitoss,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Procesada/delitoss 2010-2012.RDS')
saveRDS(object = delitoss2,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Graficas/grafica delitoss.RDS')
rm(delitoss,delitoss2)

variables <- c(grep("fecha", colnames(lesion)),
               grep("departamento", colnames(lesion)),
               grep("municipio", colnames(lesion)),
               grep("dane", colnames(lesion)),
               grep("genero", colnames(lesion)),
               grep("cantidad", colnames(lesion)))

lesion <- lesion[,c(5,9,1,2,3,6,8)]

colnames(lesion)[5] <- "cod_dane"
lesion$cod_dane <-ifelse(nchar(lesion$cod_dane)==7,paste0('0',lesion$cod_dane),lesion$cod_dane)
lesion$cod_dane <- substr(lesion$cod_dane,1,5)
lesion$cod_dane_dpto <- substr(lesion$cod_dane,1,2)
lesion$fecha <- as.Date(as.numeric(lesion$fecha), origin = "1899-12-30")
lesion$cantidad <- as.numeric(lesion$cantidad)
lesion$year <- format(lesion$fecha,"%Y")
lesion <- mutate(lesion, year_month=zoo::as.yearmon(lesion$fecha))
lesion2 <- lesion %>% group_by(year_month) %>% summarise(Total_mes = sum(cantidad, na.rm = T),total_mes = n())
ggplot(data = lesion2,aes(x = year_month,y = Total_mes)) +geom_line()

saveRDS(object = lesion,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Procesada/lesiones 2010-2012.RDS')
saveRDS(object = lesion2,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Graficas/grafica lesiones.RDS')
rm(lesion,lesion2)


variables <- c(grep("fecha", colnames(hurto_p)),
               grep("departamento", colnames(hurto_p)),
               grep("municipio", colnames(hurto_p)),
               grep("dane", colnames(hurto_p)),
               grep("genero", colnames(hurto_p)),
               grep("cantidad", colnames(hurto_p)))

hurto_p <- hurto_p[,c(5,9,1,2,3,6,8)]

colnames(hurto_p)[5] <- "cod_dane"
hurto_p$cod_dane <-ifelse(nchar(hurto_p$cod_dane)==7,paste0('0',hurto_p$cod_dane),hurto_p$cod_dane)
hurto_p$cod_dane <- substr(hurto_p$cod_dane,1,5)
hurto_p$cod_dane_dpto <- substr(hurto_p$cod_dane,1,2)
hurto_p$fecha <- as.Date(as.numeric(hurto_p$fecha), origin = "1899-12-30")
hurto_p$cantidad <- as.numeric(hurto_p$cantidad)
hurto_p$year <- format(hurto_p$fecha,"%Y")
hurto_p <- mutate(hurto_p, year_month=zoo::as.yearmon(hurto_p$fecha))
hurto_p2 <- hurto_p %>% group_by(year_month) %>% summarise(Total_mes = sum(cantidad, na.rm = T),total_mes = n())
ggplot(data = hurto_p2,aes(x = year_month,y = Total_mes)) +geom_line()

saveRDS(object = hurto_p,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Procesada/hurto a personas 2010-2012.RDS')
saveRDS(object = hurto_p2,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Graficas/grafica hurto a personas.RDS')
rm(hurto_p,hurto_p2)


variables <- c(grep("fecha", colnames(hurto_r)),
               grep("departamento", colnames(hurto_r)),
               grep("municipio", colnames(hurto_r)),
               grep("dane", colnames(hurto_r)),
               grep("genero", colnames(hurto_r)),
               grep("cantidad", colnames(hurto_r)))

hurto_r <- hurto_r[,c(5,9,1,2,3,6,8)]

colnames(hurto_r)[5] <- "cod_dane"
hurto_r$cod_dane <-ifelse(nchar(hurto_r$cod_dane)==7,paste0('0',hurto_r$cod_dane),hurto_r$cod_dane)
hurto_r$cod_dane <- substr(hurto_r$cod_dane,1,5)
hurto_r$cod_dane_dpto <- substr(hurto_r$cod_dane,1,2)
hurto_r$fecha <- as.Date(as.numeric(hurto_r$fecha), origin = "1899-12-30")
hurto_r$cantidad <- as.numeric(hurto_r$cantidad)
hurto_r$year <- format(hurto_r$fecha,"%Y")
hurto_r <- mutate(hurto_r, year_month=zoo::as.yearmon(hurto_r$fecha))
hurto_r2 <- hurto_r %>% group_by(year_month) %>% summarise(Total_mes = sum(cantidad, na.rm = T),total_mes = n())
ggplot(data = hurto_r2,aes(x = year_month,y = Total_mes)) +geom_line()

saveRDS(object = hurto_r,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Procesada/hurto a residencias 2010-2012.RDS')
saveRDS(object = hurto_r2,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Graficas/grafica hurto a residencias.RDS')
rm(hurto_r,hurto_r2)

