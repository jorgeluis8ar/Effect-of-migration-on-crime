# Elaborado por: Jorge Luis Ochoa Rincón
# Nota: No se usan acentos ni caracteres especiales para evitar conflictos entre los diferentes sistemas operativos.
# Limpiendo la consola ----------------------------------------------------

cat("\f")
rm(list=ls())
options("scipen"=100, "digits"=4) # Forzar a R a no usar e+
setwd("~/Downloads")
getwd()

# Paquetes y algunas funciones ----------------------------------------------------------------

paquetes = c("tidyverse",'data.table','ggplot2','dplyr','haven','sf','viridis','plotly','ggspatial','tmap')
sapply(paquetes,require,character.only=T) 
rm(paquetes)
source(file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Scripts/Migración/Funciones.R')
  
archivos <- list.files(path = 'Censo/') %>% paste0('Censo/',.)
# lapply(archivos, function(x) load.files(x))
archivos2 <- list.files(path = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/Procesada/') %>% 
             paste0('~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/Procesada/',.)

# Cargando datos ----------------------------------------------------------

C_2018 <- lapply(archivos2, function(x) import.files(x))
Censo_2018 <- plyr::rbind.fill(C_2018 %>% data.table::rbindlist(use.names = T,fill = T))
Censo_2018 <- Censo_2018 %>% subset(is.na(codigo)==F)
rm(C_2018)

muni2 <- read_sf('mpio/mpio.shp')
muni <- read_sf('~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/Mapa Colombia/colombia.shp',type = 3)
colnames(muni)[4] <- 'codigo'
mig <- readRDS(file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/puntos migracion.RDS')

paises <- readRDS(file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/Paises.RDS')

mig_t <- readRDS(file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/migracion venezuela puntos acumulado por ano.RDS')

mig_por_mes <- readRDS(file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/migracion venezuela 2012 - 2020(pr).RDS')

poblacion <- read.csv(file = '~/Downloads/censo_2018 poblacion.csv')

str(poblacion)
poblacion$codigo <-  as.character(poblacion$codigo)
poblacion$DPTO <- as.character(poblacion$DPTO)
poblacion$MPI <- as.character(poblacion$MPI)

poblacion$codigo <- if_else(nchar(poblacion$codigo)==4,paste0("0",poblacion$codigo),poblacion$codigo)

final <- merge(x = poblacion, y = Censo_2018, all.x = T)
final <- merge(x = muni,y = final,all = T)
final$inmigrantes_1_ano <- ifelse(is.na(final$inmigrantes_1_ano)==T,0,final$inmigrantes_1_ano)
final$inmigrantes_5_anos <- ifelse(is.na(final$inmigrantes_5_anos)==T,0,final$inmigrantes_5_anos)

final$extr_pob_5_ano <- (final$inmigrantes_5_anos/final$pob_total)*100
final$extr_pob_1_ano <- (final$inmigrantes_1_ano/final$pob_total)*100

# ggplot(data = muni) + geom_sf(size = 0.1, color = 'black', alpha = 0.8)
# final2 %>% mutate(rate_cut = cut_interval(extr_pob_5_ano, n = 6)) %>%
# ggplot() + geom_sf(aes(fill = rate_cut))

# final2 %>% mutate(rate_cut = cut_interval(extr_pob_1_ano, n = 6)) %>%
#   ggplot() + geom_sf(aes(fill = rate_cut))+
#   scale_fill_manual(values=c('#fad9d9','#ffbdfe','#ff96fd',"#ff00fb","#7d007b","#4a0149"))+ 
#   geom_sf(data = mig)
# scale_fill_manual(values=c("#4a0149", "#7d007b", "#ff00fb",'#ff96fd','#ffbdfe','#fad9d9'))

# Distancias punto de migración más cercano -------------------------------

final <- st_transform(final, st_crs(paises))

mig <- mutate(mig, duplicados =ifelse(duplicated(paste0(mig$lat,mig$long))==T,1,0))
mig <- mig %>% subset(duplicados==0)
mig <- st_transform(mig, st_crs(final))
muni_centroid <- st_centroid(x = final,of_largest_polygon = T)
muni_centroid <- st_transform(x = muni_centroid,crs = st_crs(paises))
# st_distance(x = muni_centroid,y = mig) 

puntos_mig_vene <- mig_t[c(16,31,33,36,39),]

puntos <- apply(st_distance(x = muni_centroid,y = puntos_mig_vene), 1, which.min)
# puntos[sapply(puntos, function(x) length(x)==0L)] <- NA
puntos <- puntos %>% unlist()
dist <- st_distance(x = muni_centroid,y = puntos_mig_vene)

# Distancia minima puntos venezuela --------------------------------------------------------

final$dis_mun_punt_min =NA
for (municipio in 1:nrow(final)) {
  dist2 <- dist[municipio,puntos[municipio]]
  print(dist2)
  final$dis_mun_punt_min[municipio] <- dist2
}

# Distancia minima puntos general --------------------------------------------------------

puntos <- apply(st_distance(x = muni_centroid,y = mig_t), 1, which.min)
# puntos[sapply(puntos, function(x) length(x)==0L)] <- NA
puntos <- puntos %>% unlist()
dist <- st_distance(x = muni_centroid,y = mig_t)

final$dis_mun_migra =NA
for (municipio in 1:nrow(final)) {
  dist2 <- dist[municipio,puntos[municipio]]
  print(dist2)
  final$dis_mun_migra[municipio] <- dist2
}

# Ibañez & Rozo : Distancia inversa ---------------------------------------

puntos <- c("Maicao","Puerto Carreño", "Arauca","Cúcuta","Puerto Santander")
nombres <- c("Paraguachón","Muelle Internacional sobre el Rio Orinoco – Margen Izquierdo", "Puente internacional Gral. José Antonio Páez","Puente Internacional Simón Bolívar","Puente Internacional Puente Unión – Puerto Santander")
puntos_mig_vene <- merge(x = puntos_mig_vene,
                         y = data.frame(Puntos = puntos, Nombre = nombres, `Latitud...Longitud` = puntos_mig_vene$Latitud...Longitud),
                         by = "Latitud...Longitud")

acum <- apply(st_drop_geometry(puntos_mig_vene[,c(2,3,4,5,6,7,8,9,10)]), 2, function(x) sum(x,na.rm = T))
puntos_mig_vene <- mutate(puntos_mig_vene, acum_2012 = acum[1],
                          acum_2013 = acum[2],
                          acum_2014 = acum[3],
                          acum_2015 = acum[4],
                          acum_2016 = acum[5],
                          acum_2017 = acum[6],
                          acum_2018 = acum[7],
                          acum_2019 = acum[8],
                          acum_2020 = acum[9])

puntos_mig_vene <- mutate(puntos_mig_vene, peso_2012 = X2012/acum_2012,
                          peso_2013 = X2013/acum_2013,
                          peso_2014 = X2014/acum_2014,
                          peso_2015 = X2015/acum_2015,
                          peso_2016 = X2016/acum_2016,
                          peso_2017 = X2017/acum_2017,
                          peso_2018 = X2018/acum_2018,
                          peso_2019 = X2019/acum_2019,
                          peso_2020 = X2020/acum_2020)

puntos_mig_vene <- mutate(puntos_mig_vene,pesos_promedio = apply(st_drop_geometry(puntos_mig_vene[,c(24,25,26,27,28,29,30,31)]), 1, function(x) mean(x, na.rm = T)))
dist <- st_distance(x = muni_centroid,y = puntos_mig_vene)

final$sum_weigths <- NA
final$distancia_promediada <- NA
for (municipio in 1:nrow(final)) {
  dist2 <- as.numeric(dist[municipio,1]*puntos_mig_vene$pesos_promedio[1])
  dist3 <- as.numeric(dist[municipio,2]*puntos_mig_vene$pesos_promedio[2])
  dist4 <- as.numeric(dist[municipio,3]*puntos_mig_vene$pesos_promedio[3])
  dist5 <- as.numeric(dist[municipio,4]*puntos_mig_vene$pesos_promedio[4])
  dist6 <- as.numeric(dist[municipio,5]*puntos_mig_vene$pesos_promedio[5])
  
  dist2 <- if_else(is.na(dist2)==T,0,dist2)
  dist3 <- if_else(is.na(dist3)==T,0,dist3)
  dist4 <- if_else(is.na(dist4)==T,0,dist4)
  dist5 <- if_else(is.na(dist5)==T,0,dist5)
  dist6 <- if_else(is.na(dist6)==T,0,dist6)
  
  suma <- dist2 + dist3 + dist4 + dist5 + dist6
  result <- (1/suma)
  print(result)
  final$sum_weigths[municipio] <- result
  final$distancia_promediada [municipio] <- (suma/5)
}

variables <- c(grep("MPI", colnames(final)),
               grep("DPT", colnames(final)),
               grep("CAB", colnames(final)),
               grep("inmigrantes_", colnames(final)),
               grep("Personas total...6", colnames(final)),
               grep("extr_pob_", colnames(final)),
               grep("missing_extranjero", colnames(final)),
               grep("dis_mun_punt_min", colnames(final)),
               grep("dis_mun_migra", colnames(final)),
               grep("sum_weigths", colnames(final)),
               grep("pob_total", colnames(final)),
               grep("distancia_promediada", colnames(final)))
final <- final[, variables]
rm(suma,variables,dist,dist2,dist3,dist4,dist5,dist6,municipio,nombres,puntos,result,archivos,archivos2,acum)

files <- paste0('2012','-',seq(1,12,1))
files <- c(files,paste0('2013','-',seq(1,12,1)))
files <- c(files,paste0('2014','-',seq(1,12,1)))
files <- c(files,paste0('2015','-',seq(1,12,1)))
files <- c(files,paste0('2016','-',seq(1,12,1)))
files <- c(files,paste0('2017','-',seq(1,12,1)))
files <- c(files,paste0('2018','-',seq(1,12,1)))
files <- c(files,paste0('2019','-',seq(1,12,1)))

lista <- lapply(files, function(x) data_sets(base = final,date = x))
df <- plyr::rbind.fill(lista %>% data.table::rbindlist(use.names = T,fill = T))
df$ano <- as.numeric(df$ano)
df$mes <- as.numeric(df$mes)

df_final <-merge(x = df, y = mig_por_mes, by.x = c('ano','mes'), by.y = c('Año','mes'), all = T)
df_final <- df_final %>% mutate(mig_vene = Total*sum_weigths)
colnames(df_final)[19] <- 'Total_migra'
df_final$MPIO_CCDGO <- as.character(df_final$MPIO_CCDGO)
df_final$DPTO_CCDGO <- as.character(df_final$DPTO_CCDGO)
df_final <- df_final %>% subset(is.na(MPIO_CCDGO)==F)
write.csv(x = df_final,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/csv/migracion ibanezrozo.csv')

df_final$cod_dane <- paste0(df_final$DPTO_CCDGO,df_final$MPIO_CCDGO)

mapa <- merge(x = muni2, y = df_final,by.x = "MPIOS", by.y = "cod_dane")
mapa <- mapa %>% subset(duplicated(paste0(mapa$MPIOS,mapa$distancia_promediada))==F)
mapa <- st_transform(x = mapa,crs = 4686)
mapa$categoria <- ifelse(mapa$distancia_promediada <= 84189, "Fronterizos",
                  ifelse(mapa$distancia_promediada>=118144, "Lejanos", "Medianos"))
ggplot(data = mapa) + geom_sf(aes(fill = categoria)) +coord_sf(xlim = c(-79.3, -66.7), ylim = c(-4.45, 12.55), expand = FALSE) + theme_bw()+
  theme(legend.position = "bottom")
ggsave(filename = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Resultados/Mapas/Municipios1.pdf',
       width = 12,height = 19,units = 'cm') 
mapa %>% mutate(rate_cut = cut_number(distancia_promediada, n = 3)) %>%
 ggplot() + geom_sf(aes(fill = rate_cut)) +coord_sf(xlim = c(-79.3, -66.7), ylim = c(-4.45, 12.55), expand = FALSE) + theme_bw()+
  theme(legend.position = "bottom")
ggsave(filename = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Resultados/Mapas/Municipios2.pdf',
       width = 12,height = 19,units = 'cm') 
mapa %>% mutate(rate_cut = cut_interval(distancia_promediada, n = 3)) %>%
  ggplot() + geom_sf(aes(fill = rate_cut)) +coord_sf(xlim = c(-79.3, -66.7), ylim = c(-4.45, 12.55), expand = FALSE) + theme_bw()+
  theme(legend.position = "bottom")
ggsave(filename = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Resultados/Mapas/Municipios3.pdf',
       width = 12,height = 19,units = 'cm') 


mig_por_mes$fecha <-zoo::as.yearmon(paste0(mig_por_mes$Año,"-",mig_por_mes$mes))

final2 <- merge(x = final,y = mig_por_mes, by = c('Año','mes'))

final2 <- final2 %>% mutate(mig_pred = Total*sum_weigths)
# st_distance(x = muni_centroid, y = paises[c(6),])

ggplot(data = mapa) + geom_sf(aes(fill=sum_weigths)) +
                                 scale_fill_gradient2(low = "white", high = "black", na.value = NA)+
                                 labs(fill = "Distancia inversa")+
                                 coord_sf(xlim = c(-82, -65), ylim = c(-5, 13), expand = FALSE)

ggplot(data = final3) + geom_sf(aes(fill = sum_weigths),show.legend = F) +
  geom_sf(data=puntos_mig_vene, shape = 23, colour = 'black', fill='darkgreen', size = 5)+
  # scale_fill_gradient(low = "white", high = "black", na.value = NA)  +
  scale_fill_gradient2(low = "white", high = "black", na.value = NA)+
  labs(fill = "Distancia inversa")+
  # scale_fill_grey(start = 0.1,end = 1)+
    # scale_fill_manual(values=c('#e3e1e1','#000000','#242424',"#3b3a3a","#5c5b5b","#878686","#a8a7a7"))+
  coord_sf(xlim = c(-82, -65), ylim = c(-5, 13), expand = FALSE)

# ggplot(data =final3) + geom_density(aes(x = extr_pob_1_ano))

ggplot(data = final3) + geom_sf(aes(fill = sum_weigths),show.legend = F) + geom_sf(data = paises) +
  geom_sf(data = muni_centroid,aes(size = extr_pob_1_ano),fill = "red",alpha = 0.6,shape = 21)+
  geom_sf(data=puntos_mig_vene, shape = 23, colour = 'black', fill='darkgreen', size = 5)+
  scale_fill_gradient(low = "white", high = "black", na.value = NA)+
  annotate(geom = "text", x = -78, y = -1.2, label = "Ecuador", 
           fontface = "italic", color = "grey22", size = 6)+
  annotate(geom = "text", x = -74, y = -3.2, label = "Perú", 
           fontface = "italic", color = "grey22", size = 6)+
  annotate(geom = "text", x = -81, y = 6, label = "Panamá", 
           fontface = "italic", color = "grey22", size = 6)+
  annotate(geom = "text", x = -71, y = 8.5, label = "Venezuela", 
           fontface = "italic", color = "grey22", size = 6)+
  annotate(geom = "text", x = -67.5, y = -1.2, label = "Brasil", 
           fontface = "italic", color = "grey22", size = 6)+ 
  labs(size = "Proporción extranjeros")+
  theme(axis.title = element_blank(),
        element_line(color = gray(0.8), linetype = "dashed", size = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "aliceblue"))+
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(1.35, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(xlim = c(-82, -65), ylim = c(-5, 13), expand = FALSE)








final2 %>% mutate(rate_cut = cut_interval(extr_pob_1_ano, n = 6)) %>%
  ggplot() + geom_sf(aes(fill = rate_cut)) + geom_sf(data = paises,aes(fill= "Paises vecinos")) +
  annotate(geom = "text", x = -78, y = -1.2, label = "Ecuador", 
           fontface = "italic", color = "grey22", size = 6)+
  annotate(geom = "text", x = -74, y = -3.2, label = "Perú", 
           fontface = "italic", color = "grey22", size = 6)+
  annotate(geom = "text", x = -81, y = 6, label = "Panamá", 
           fontface = "italic", color = "grey22", size = 6)+
  annotate(geom = "text", x = -67, y = 7, label = "Venezuela", 
           fontface = "italic", color = "grey22", size = 6)+
  annotate(geom = "text", x = -67.5, y = -1.2, label = "Brasil", 
           fontface = "italic", color = "grey22", size = 6)+
  # scale_fill_manual(values=c('#fad9d9','#ffbdfe','#ff96fd',"#ff00fb","#7d007b","#4a0149","#f2efe1"))+
  scale_fill_manual(values=c("#4a0149", "#7d007b", "#ff00fb",'#ff96fd','#ffbdfe','#fad9d9',"#f2efe1"))+
  labs(fill = "Migración")+
  theme(axis.title = element_blank(),
        element_line(color = gray(0.8), linetype = "dashed", size = 0.5),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "aliceblue"))+
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(1.35, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(xlim = c(-82, -65), ylim = c(-5, 13), expand = FALSE)

final2 %>% mutate(rate_cut = cut_interval(dis_mun_punt_min, n = 6)) %>%
  ggplot() + geom_sf(aes(fill = rate_cut))+
  scale_fill_manual(values=c('#fad9d9','#ffbdfe','#ff96fd',"#ff00fb","#7d007b","#4a0149"))+
  geom_sf(data = paises) +
  coord_sf(xlim = c(-82, -65), ylim = c(-5, 13), expand = FALSE)


