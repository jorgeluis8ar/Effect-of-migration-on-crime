# Codigo creado por: Jorge Luis Ochoa Rincon
# Codigo: 201711018
# Taller de R - Task 1

# Limpiando la consola y cargando paquetes --------------------------------

cat("\f")
rm(list=ls())
options("scipen"=100, "digits"=4)
setwd("~/Downloads")

paquetes=c("tidyverse","data.table","ggplot2",'viridis','sf','ggspatial','dplyr','zoo')
lapply(paquetes,require,character.only=T)
rm(paquetes)

df <- readxl::read_excel(path = 'reporte.xlsX')

df <- df[,c(2,3)]

colnames(df) <- c("cod_dane","nombre_casos")
df <- df %>% subset(is.na(nombre_casos)==F)
df <- df[df$cod_dane != "Sexo",]
df <- df[df$cod_dane != "Hombre",]
df <- df[df$cod_dane != "Mujer",]
df <- df[-c(2229),]

df$codi_dane <- substr(x = df$cod_dane,start = (stringr::str_locate(string = df$cod_dane,pattern = "#")[1]+2),stop = nchar(df$cod_dane))
df$name_dane <- substr(x = df$nombre_casos,start = (stringr::str_locate(string = df$nombre_casos,pattern = "_")[1]+1),stop = nchar(df$nombre_casos))
df$name_dane <- ifelse(test = (df$cod_dane=="Total"),yes = "",no = df$name_dane)

substr(x = df$nombre_casos,start = (stringr::str_locate(string = df$nombre_casos,pattern = "_")[1]+1),stop = nchar(df$nombre_casos))

df$cod_dane <- ifelse(df$cod_dane == "Total",NA, df$cod_dane)
df$codi_dane <- ifelse(df$codi_dane == "",NA, df$codi_dane)
df$name_dane <- ifelse(df$name_dane == "",NA, df$name_dane)

df <- df %>% tidyr::fill(codi_dane,.direction = "down")
df <- df %>% tidyr::fill(name_dane,.direction = "down")
df <- df[,c(2,3,4)]

df$eliminar <- as.numeric(df$nombre_casos)

df <- df %>% subset(is.na(eliminar)==F)


participacion <- haven::read_dta(file = 'Hogares/base procesada2.dta')


data <- merge(x = df,y = participacion,by.x = "codi_dane",by.y = "cod_dane",all = T)

data <- data[,c(5,3,1,2,6,7,8,9,10)]

colnames(data) <- c("dpto","mpio","cod_dane","pob_censo","encuestados","part_vene","extranjeros","alfabeta","anos_educacion")

data$pob_censo <- as.numeric(data$pob_censo)

data <- data %>% mutate(porcentaje_pob = encuestados/pob_censo,
                        part_ven_ajustada = (pob_censo*part_vene)/encuestados,
                        vene = part_ven_ajustada*pob_censo,
                        extranje_ajustada = (pob_censo*extranjeros)/encuestados,
                        alfabeta_ajustada = (pob_censo*alfabeta)/encuestados)

write.csv(x = data,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/csv/censo 2005.csv')
shape <- read_sf('mpio/mpio.shp')
shape <- st_transform(shape,st_crs(4686))
shape <- shape[,c(5,7,11,8)]
spatial <- merge(x = shape,y = data,by.x = "MPIOS",by.y = "cod_dane",all = T)
spatial <- spatial[order(-spatial$part_ven_ajustada),] 
spatial$grupo <- NA
spatial$grupo[1:152] <-   "0.003088 - 0.459262"
spatial$grupo[153:304] <- "0.000836 - 0.003026"
spatial$grupo[305:455] <- "0.000035 - 0.000823"
spatial$grupo[456:1112] <- "0"

cols  <- c("0.003088 - 0.459262" = "#000000", "0.000836 - 0.003026" = "#3f403f", "0.000035 - 0.000823" = "#757474", "0" = "#adadad")

censo_2005 <- ggplot(data = spatial) + geom_sf(aes(fill = grupo), color = 'black', size = 0.2) +
                scale_fill_manual(values=cols,na.value  = "#ffffff",labels = c("0","0.000035 - 0.000823","0.000836 - 0.003026","0.003088 - 0.459262","No se tiene información")) + 
                annotation_scale(location = "bl", width_hint = 0.35) +
                annotation_north_arrow(location = "bl", which_north = "true", 
                                       pad_x = unit(0.9, "in"), pad_y = unit(0.5, "in"),
                                       style = north_arrow_fancy_orienteering) +
                labs(x = "", y = "", fill = "Población Venezolana en 2005")+
                coord_sf(xlim = c(-79.739, -66.674), ylim = c(-4.247, 12.5), expand = FALSE) +
                theme_bw() + theme(legend.position = c(0.78, 0.815),
                                   legend.background = element_rect(fill = NA, color = NA),
                                   panel.grid = element_line(size = 0))
base <- haven::read_dta('~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/dta/base final.dta')

base <- base[base$fecha == 624,c(1,2,3,5,6,7,8)]
spatial2 <- merge(x = shape,y = base,by.x = "MPIOS",by.y = "cod_dane",all = T)        

spatial2 <- spatial2 %>% subset(is.na(MPIOS)==F)
spatial2 <- spatial2 %>% subset(duplicated(MPIOS)==F)
spatial2 <- spatial2[order(-spatial2$extr_pob_1_ano),] 

centroid <- st_centroid(x = spatial2,of_largest_polygon = T)

censo_2018 <- ggplot(data = spatial) + geom_sf(aes(fill = grupo), color = 'black', size = 0.2) +
                geom_sf(data = centroid, aes(size = extr_pob_1_ano), alpha = 0.7, fill = "#c40d00",color = "black",shape= 21)+
                scale_size(labels = c("0", "0.001 - 10","10.001 - 20", "20.001 - 30.632"))+
                scale_fill_manual(values=cols,na.value  = "#ffffff") + 
                annotation_scale(location = "bl", width_hint = 0.35) +
                annotation_north_arrow(location = "bl", which_north = "true", 
                                       pad_x = unit(0.9, "in"), pad_y = unit(0.5, "in"),
                                       style = north_arrow_fancy_orienteering) +
                labs(x = "", y = "", size = "Población extranjera en 2018")+
                coord_sf(xlim = c(-79.739, -66.674), ylim = c(-4.247, 12.5), expand = FALSE) +
                theme_bw() + theme(legend.position = c(0.78, 0.815),
                                   legend.background = element_rect(fill = NA, color = NA),
                                   panel.grid = element_line(size = 0))+
                guides(fill =FALSE)

ggsave(plot = censo_2005,filename = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Resultados/Mapas/Censo 2005.pdf', width = 15, height = 18, units = "cm")
ggsave(plot = censo_2018,filename = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Resultados/Mapas/Censo 2018 extranjeros.pdf', width = 15, height = 18, units = "cm")