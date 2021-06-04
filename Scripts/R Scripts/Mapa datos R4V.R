# Elaborado por: Jorge Luis Ochoa Rincón
# Nota: No se usan acentos ni caracteres especiales para evitar conflictos entre los diferentes sistemas operativos.

library(rjson)
library(ggspatial)
library(dplyr)
library(sf)
library(ggrepel)
library(ggplot2)

# Datos -------------------------------------------------------------------

residencia <- "https://data2.unhcr.org/population/get/sublocation/root?widget_id=235848&sv_id=39&population_group=5081&forcesublocation=0&fromDate=1900-01-01"
resi <- fromJSON(file = residencia)
resi <- plyr::rbind.fill(resi$data %>% data.table::rbindlist(use.names = T,fill = T))

# World shape file --------------------------------------------------------

world <- read_sf('~/Downloads/country/country.shp')

# Prepocessing ------------------------------------------------------------

resi$geomaster_name <- ifelse(test = resi$geomaster_name=="Bolivia (Plurinational State of)", "Bolivia",resi$geomaster_name)

df <- merge(x = world,y = resi,by.x = "CNTRY_NAME", by.y = "geomaster_name")
df$centroid_lat <- as.numeric(df$centroid_lat)
df$centroid_lon <- as.numeric(df$centroid_lon)
df$individuals <- as.numeric(df$individuals)

df2 <- st_as_sf(x = st_drop_geometry(df), coords = c("centroid_lon", "centroid_lat"), crs = st_crs(world))

resi$lon <- as.numeric(resi$centroid_lon)
resi$lat <- as.numeric(resi$centroid_lat)
resi <- st_as_sf(x = resi, coords = c("centroid_lon", "centroid_lat"), crs = st_crs(world))
resi <- rbind(st_drop_geometry(resi[resi$geomaster_name != "Curaçao",c(1,15,19,20)]),data.frame(geomaster_name = "Venezuela",individuals = 0,lon = -63.030241,lat = 7.11489))
resi$geomaster_name <- as.character(resi$geomaster_name)

resi <- resi %>% mutate(x_nudge = case_when( geomaster_name == 'Colombia' ~ -14
                              ,geomaster_name == 'Peru' ~ -8.5
                              ,geomaster_name == 'Chile' ~ -7.2
                              ,geomaster_name == 'Argentina' ~ 12.2
                              ,geomaster_name == 'Ecuador' ~ -6.5
                              ,geomaster_name == 'Brazil' ~ 5.5
                              ,geomaster_name == 'Panama' ~ -7
                              ,geomaster_name == 'Mexico' ~ 9
                              ,geomaster_name == 'Uruguay' ~ 8.5
                              ,geomaster_name == 'Guyana' ~ 6.4
                              ,geomaster_name == 'Dominican Republic' ~ 0
                              ,geomaster_name == 'Costa Rica' ~ -10
                              ,geomaster_name == 'Bolivia' ~ -11
                              ,geomaster_name == 'Paraguay' ~ -6
                              ,geomaster_name == 'Venezuela' ~ -3
                              ,TRUE ~ 0)
         ,y_nudge = case_when( geomaster_name == 'Colombia' ~ -1
                               ,geomaster_name == 'Peru' ~ 0
                               ,geomaster_name == 'Chile' ~ 0
                               ,geomaster_name == 'Argentina' ~ -2.2
                               ,geomaster_name == 'Ecuador' ~ 0
                               ,geomaster_name == 'Brazil' ~ 0
                               ,geomaster_name == 'Panama' ~ -1.7
                               ,geomaster_name == 'Mexico' ~ 0
                               ,geomaster_name == 'Uruguay' ~ -1.5
                               ,geomaster_name == 'Guyana' ~ 2.4
                               ,geomaster_name == 'Dominican Republic' ~ -3
                               ,geomaster_name == 'Costa Rica' ~ -1
                               ,geomaster_name == 'Bolivia' ~ 0
                               ,geomaster_name == 'Paraguay' ~ 0
                               ,geomaster_name == 'Venezuela' ~ 1.5
                               ,TRUE ~ 0))

# Map ---------------------------------------------------------------------

mapa <- ggplot(data = df) + geom_sf(fill = 'antiquewhite1') +
  geom_sf(data = world[world$CNTRY_NAME %in% c("Venezuela"),], fill  = "red", alpha=0.2)+
  geom_sf(data = world[world$CNTRY_NAME %in% c('Nicaragua','Honduras',"Guatemala"),]
          ,fill ="antiquewhite1")+
  geom_sf(data = df2,aes(size = individuals), fill = 'blue',colour = 'black', alpha=0.3,shape = 21)+
  annotation_scale(location = "bl", width_hint = 0.35) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.9, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_text_repel(data = resi,
                  # size = 1,
                  aes(x = lon, y = lat, label = geomaster_name), 
                  family = 'Avenir',
                  segment.color = "#333333",
                  color = "black", 
                  nudge_x = resi$x_nudge,
                  nudge_y = resi$y_nudge)+
  geom_text_repel(data = resi,
                  # size = 1,
                aes(x = lon, y = lat, label = individuals), 
                family = 'Avenir',
                segment.color = NA,
                color = "black", 
                nudge_x = resi$x_nudge+1,
                nudge_y = resi$y_nudge-1.8)+
  labs(x='',y='',caption = "Datos: Plataforma de Coordinación para Refugiados y Migrantes de Venezuela\n Cálculos propios",
       size = "Migrantes (Permisos Residencia)")+
  scale_size(range = c(0, 20),
             breaks = c(10000,100000,200000 ,400000,600000),
             labels = c("[1.000-100.000)","[10.000-100.000)","[100.000-200.000)",'[200.000-400.000)','>600.000'))+
  coord_sf(xlim = c(-110, -40), ylim = c(-42, 25), expand = FALSE) +
  theme(panel.grid.major = element_line(color = gray(0.1),
                                        linetype = "dashed", 
                                        size = 0.1),
        panel.background = element_rect(fill = "aliceblue"),
        legend.position = c(0.18, 0.37),
        legend.background = element_rect(fill = NA, color = "black"),
        legend.box.background = element_rect(fill =NA))
mapa