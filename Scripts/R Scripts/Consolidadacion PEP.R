# Codigo creado por: Jorge Luis Ochoa Rincon

#---------------------------------------------------
#     Limpiando la consola y cargando paquetes
#---------------------------------------------------
cat("\f")
rm(list=ls())
options("scipen"=100, "digits"=4)
setwd("~/Downloads/procesada pep/")

paquetes=c("tidyverse","data.table","ggplot2",'viridis','readxl','dplyr')
lapply(paquetes,require,character.only=T)
rm(paquetes)


# Definiendo los archivos -------------------------------------------------

archivos <- list.files() %>% paste0('~/Downloads/procesada pep/',.)

load.files <- function(camino){
  
  df <- read.csv(file = camino)  
  df$fechaexpedición <- as.character(df$fechaexpedición)
  df$fecha_expedicion <- as.Date(df$fechaexpedición,format("%m/%d/%Y"))

  df$ano_expedicion <- format(df$fecha_expedicion,format('%Y'))
  df$mes_expedicion <- format(df$fecha_expedicion,format('%m'))
  df$dia_expedicion <- format(df$fecha_expedicion,format('%d'))
  
  colnames(df) <- nombres
  
  df$fecha_renovacion <- as.character(df$fecha_renovacion)
  df$fecha_renovacion <- as.Date(df$fecha_renovacion,format("%m/%d/%Y"))
  df$ano_reno <- format(df$fecha_renovacion,format('%Y'))
  df$mes_reno <- format(df$fecha_renovacion,format('%m'))
  df$dia_reno <- format(df$fecha_renovacion,format('%d'))
  
  df <- df[,c(32,2,16,5,6,17,1,20,33,31,8,11,12,18,13,34,35,36,37,19,38,39,40,23,26,27,29,30,21)]
  
  return(df)
}

nombres <- c("PTENT_VERSIO_PEP",'departamento','ano','ano_renovacion','ciudad_hosp',
             'cod_dane','fecha_corte','cod_dane_dpto','dia_renovacion','dia',
             'estado_permiso','estado_renovacion','fecha_expedicion','mes',
             'mes_renovacion','municipio','pep','ptec_tipo_ejecucion','fecha_renovacion',
             'version_pep','rango_edad','regional','sexo','top_dpto','color_dpto',
             'dias_restantes','femenino','femenino_renovacion','masculino','motivo_inactivacion_registro',
             'registros','id','pep_trabajo','fecha_expedicion2','ano_expedicion','mes_expedicion',
             'dia_expedicion')

datos_pep <- lapply(archivos, function(x) load.files(camino = x))

pep <- plyr::rbind.fill(datos_pep[1] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(fase = "PEP I"),
                        datos_pep[2] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(fase = "PEP II"),
                        datos_pep[3] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(fase = "PEP III"),
                        datos_pep[4] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(fase = "PEP IV"),
                        datos_pep[5] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(fase = "PEP V"),
                        datos_pep[6] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(fase = "PEP VI"),
                        datos_pep[7] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(fase = "PEP VII"),
                        datos_pep[8] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(fase = "PEP VIII"),
                        datos_pep[9] %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(fase = "PEP IX"))
rm(datos_pep, nombres)

pep$departamento <- as.character(pep$departamento)
pep$municipio <- as.character(pep$municipio)
pep$ciudad_hosp <- as.character(pep$ciudad_hosp)
pep$cod_dane <- as.character(pep$cod_dane)
pep$pep <- as.character(pep$pep)
pep$PTENT_VERSIO_PEP <- as.character(pep$PTENT_VERSIO_PEP)
pep$estado_permiso <- as.character(pep$estado_permiso)
pep$estado_renovacion <- as.character(pep$estado_renovacion)
pep$ptec_tipo_ejecucion <- as.character(pep$ptec_tipo_ejecucion)
pep$sexo <- as.character(pep$sexo)
pep$rango_edad <- as.character(pep$rango_edad)

pep$fecha_expedicion_r <- as.Date(pep$fecha_expedicion, format("%m/%d/%Y"))
pep$fecha_expedicion_mes <- zoo::as.yearmon(pep$fecha_expedicion, "%m/%d/%Y")
pep %>% subset(cod_dane == "11001") %>% group_by(fecha_expedicion_mes,fase) %>% summarise(total = sum(registros,na.rm = T)) %>% 
  ggplot(data = .) + geom_line(aes(x = fecha_expedicion_mes, y = total, colour = fase))

pep %>% group_by(fecha_expedicion_mes,fase) %>% summarise(total = sum(registros,na.rm = T)) %>% 
  ggplot(data = .) + geom_line(aes(x = fecha_expedicion_mes, y = total, colour = fase))

