# Elaborado por: Jorge Luis Ochoa Rincón
# Nota: No se usan acentos ni caracteres especiales para evitar conflictos entre los diferentes sistemas operativos.

rm(list = ls())
setwd("~/Downloads")
library(sf)
library(ggplot2)
library(dplyr)
df <- read.csv('Entradas_de_extranjeros_a_Colombia.csv')
df$Nacionalidad <- as.character(df$Nacionalidad)
df$Mes <- as.character(df$Mes)
df$Latitud...Longitud <- as.character(df$Latitud...Longitud)
df$Latitud...Longitudgg <- stringr::str_remove_all(df$Latitud...Longitud,"[()]")

# ll <- strsplit(df$Latitud...Longitud,split = ",")
# g <- ll[1] %>% unlist()
# 
# for (element in 2:nrow(df)) {
#   g <- rbind(g,ll[element] %>% unlist())
# }
# 
# df <- cbind(df,g)
# colnames(df)[ncol(df)-1] <- "Latitud"
# colnames(df)[ncol(df)] <- "Longitud"

df$mes <- ifelse(df$Mes=="Enero",1,
       ifelse(df$Mes=="Febrero",2,
              ifelse(df$Mes=="Marzo",3,
                     ifelse(df$Mes=="Abril",4,
                            ifelse(df$Mes=="Mayo",5,
                                   ifelse(df$Mes=="Junio",6,
                                          ifelse(df$Mes=="Julio",7,
                                                 ifelse(df$Mes=="Agosto",8,
                                                        ifelse(df$Mes=="Septiembre",9,
                                                               ifelse(df$Mes=="Octubre",10,
                                                                      ifelse(df$Mes=="Noviembre",11,
                                                                             ifelse(df$Mes=="Diciembre",12,0))))))))))))

df$fecha <- zoo::as.yearmon(paste0(df$Año,"-",df$mes))
#
venezuela <- df[df$Nacionalidad=="Venezuela",]
# venezuela <- st_as_sf(venezuela,coords=c('Longitud','Latitud'),crs=st_crs(mapa))
# # ggplot() + geom_sf(data = mapa) + geom_sf(data = venezuela[1])
# # ggplot(data = venezuela[1]) + geom_sf()

puntos_mig <- venezuela %>% group_by(Latitud...Longitud,`Año`) %>% summarise(Total=sum(Total)) %>% arrange(desc(Total)) %>% arrange(`Año`)
saveRDS(object = puntos_mig,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/migracion venezuela puntos acumulado.RDS')
mig <- venezuela %>% group_by(mes,Año) %>% summarise(Total = sum(Total)) %>% arrange(Año) %>% as.data.frame()
# mig <- st_drop_geometry(mig)
mig$year_month <- lubridate::ym(paste0(mig$Año,mig$mes))
mig$year_month <- format(mig$year_month,format = "%Y-%m")
mig$year_month <- zoo::as.yearmon(mig$year_month)
write.csv(x = mig,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/csv/migracion venezuela 2012 - 2020(pr).csv')
saveRDS(object = mig,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/migracion venezuela 2012 - 2020(pr).RDS')

# ggplot(data = mig) + geom_line(aes(x=fecha,y=Total))
dc <- data.frame(fecha = df$fecha,nacionalidad = df$Nacionalidad,Total = df$Total,
                 lat = df$Latitud,long = df$Longitud,nudge_y=rep(c(0.001),101937))

dc <- dc %>% mutate(latp=lat,longp=long)
dc <- st_as_sf(dc,coords=c('longp','latp'),crs=4686)
dc <- dc[dc$nacionalidad=="Venezuela",]
# ggplot()+ geom_sf(data = mapa)+geom_sf(data = dc %>% group_by(lat,long) %>% summarise(Total =sum(Total)),aes(size = Total)) 
