# Elaborado por: Jorge Luis Ochoa Rincón
# Nota: No se usan acentos ni caracteres especiales para evitar conflictos entre los diferentes sistemas operativos.

# Limpiando el ambiente ---------------------------------------------------

cat('\f')
rm(list=ls())
options('scipen'=100, 'digits'=4) # Forzar a R a no usar e+
setwd("~/Documents/Universidad/Taller de R/Clases/clase_13")
getwd()


# Librerias y/o paquetes --------------------------------------------------

paquetes = c('tidyverse','rgdal','sf','sp','raster','viridis','dplyr','ggplot2')
sapply(paquetes,require,character.only=T) 
rm(paquetes)


# Cargando los datos ------------------------------------------------------

muni <- read_sf('~/Downloads/mpio/mpio.shp')

variables <- c(grep("DPTO", colnames(muni)),
               grep("NOMBRE_DPT", colnames(muni)),
               grep("NOMBRE_MPI", colnames(muni)),
               grep("NOMBRE_CAB", colnames(muni)),
               grep("MPIO", colnames(muni)))
variables
muni <- muni[,variables]
# ggplot(data = muni) + geom_sf()

ligths.procesing <- function(raster, municipios){
  
  municipios$promedio_luz <- NA
  municipios$promedio_luz_editada <- NA
  
  for (ciudad in c(seq(1:908),seq(from= 910,to = 1074,by = 1),seq(from= 1084,to = 1097,by = 1),seq(from= 1100,to = 1119,by = 1))) {

    target <- municipios[ciudad,]
    city <- municipios$NOMBRE_MPI[ciudad]
    
    cat("Iteración: ",ciudad,'\nCiudad:',city,"\n")
    
    target <- st_transform(target,st_crs(raster))
    names(raster) = "Luces Colombia 2018-12"
    l_target = crop(raster,target) %>% mask(target)
    polygon = rasterToPolygons(l_target)
    if (is.null(l_target)==T) {
      values(l_target) <- if_else(is.na(values(l_target))==T,0,values(l_target))
      polygon = rasterToPolygons(l_target) %>% st_as_sf()
      polygon$edited <- if_else(100*log(polygon$Luces.Colombia.2018.12+2)>=255,255,100*log(polygon$Luces.Colombia.2018.12+2))
      
      municipios$promedio_luz[ciudad] <- mean(polygon$Luces.Colombia.2018.12)
      municipios$promedio_luz_editada[ciudad] <- mean(polygon$edited)
    }
    if (is.null(l_target)==F) {
      
    polygon = rasterToPolygons(l_target) %>% st_as_sf()
    
    polygon$edited <- if_else(100*log(polygon$Luces.Colombia.2018.12+2)>=255,255,100*log(polygon$Luces.Colombia.2018.12+2))

    municipios$promedio_luz[ciudad] <- mean(polygon$Luces.Colombia.2018.12)
    municipios$promedio_luz_editada[ciudad] <- mean(polygon$edited)
    }
    cat("Se inputó el valor en las variables\n\n")
  }
  
  return(municipios)
}
luces_col_201802 <- raster(x = 'data/night light/colombia_201907.tif')
muni4 <- ligths.procesing(raster = luces_col_201802,municipios = muni)
muni4$year <- 2019
muni4$month <- 7
saveRDS(object = muni4,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/Luces/75N180W/201907.RDS')


ligths.procesing2 <- function(raster, municipios){
  
  municipios$promedio_luz <- NA
  municipios$promedio_luz_editada <- NA
  
  # for (ciudad in setdiff(seq(1,1122,1),c(seq(1:908),seq(from= 910,to = 1074,by = 1),seq(from= 1084,to = 1097,by = 1),seq(from= 1100,to = 1119,by = 1)))) {
  for (ciudad in c(1075,1076,1077,1078,1079,1080,1081,1082,1083,1098,1099)) {
  
    
    
    target <- municipios[ciudad,]
    city <- municipios$NOMBRE_MPI[ciudad]
    
    cat("Iteración: ",ciudad,'\nCiudad:',city,"\n")
    
    target <- st_transform(target,st_crs(raster))
    names(raster) = "Luces Colombia 2018-12"
    l_target = crop(raster,target) %>% mask(target)
    polygon = rasterToPolygons(l_target)
    if (is.null(l_target)==T) {
      values(l_target) <- if_else(is.na(values(l_target))==T,0,values(l_target))
      polygon = rasterToPolygons(l_target) %>% st_as_sf()
      polygon$edited <- if_else(100*log(polygon$Luces.Colombia.2018.12+2)>=255,255,100*log(polygon$Luces.Colombia.2018.12+2))
      
      municipios$promedio_luz[ciudad] <- mean(polygon$Luces.Colombia.2018.12)
      municipios$promedio_luz_editada[ciudad] <- mean(polygon$edited)
    }
    if (is.null(l_target)==F) {
      
      polygon = rasterToPolygons(l_target) %>% st_as_sf()
      
      polygon$edited <- if_else(100*log(polygon$Luces.Colombia.2018.12+2)>=255,255,100*log(polygon$Luces.Colombia.2018.12+2))
      
      municipios$promedio_luz[ciudad] <- mean(polygon$Luces.Colombia.2018.12)
      municipios$promedio_luz_editada[ciudad] <- mean(polygon$edited)
    }
    cat("Se inputó el valor en las variables\n\n")
  }
  
  return(municipios)
}

luces_col_201802 <- raster(x = 'data/night light/colombia_201906 00N180W.tif')
muni4 <- ligths.procesing2(raster = luces_col_201802,municipios = muni)
muni4$year <- 2019
muni4$month <- 6
saveRDS(object = muni4,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/Luces/00N180W/201906.RDS')

archivos_OON180W <-  list.files(path = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/Luces/00N180W') %>% 
                     paste0('~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/Luces/00N180W/',.)
archivos_75N180W <-  list.files(path = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/Luces/75N180W') %>% 
                     paste0('~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/Luces/75N180W/',.)

cargar.raster <- function(path){
  
  ras <- readRDS(file = path)
  ras <- st_drop_geometry(ras)
  return(ras)
  
}

lista_OON180W <- lapply(archivos_OON180W, function(x)cargar.raster(path = x))
lista_75N180W <- lapply(archivos_75N180W, function(x)cargar.raster(path = x))


luces <- plyr::rbind.fill(lista_OON180W %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(raster = "OON180W"),
                          lista_75N180W %>% data.table::rbindlist(use.names = T,fill = T) %>% mutate(raster = "75N180W"))
rm(lista_OON180W,lista_75N180W,luces_col_201802,muni,muni4,poblacion)

luces$duplicado <- ifelse(duplicated(paste0(luces$year,luces$month,luces$MPIOS,luces$raster))==T,1,0)

luces$util <-  ifelse(luces$raster=="OON180W"& !is.na(luces$promedio_luz)==T,1,0)
luces$util2 <- ifelse(luces$raster=="75N180W"& !is.na(luces$promedio_luz)==T,1,0)
luces$final <- ifelse(luces$util==0,luces$util2,luces$util)
luces <- luces %>% subset(final==1)
head(luces)

luces <- luces[,c(6,3,7,8,9,10,11)]

write.csv(x = luces,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/csv/luces editadas.csv')
