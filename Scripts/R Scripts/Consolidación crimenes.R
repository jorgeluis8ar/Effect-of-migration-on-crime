# Elaborado por: Jorge Luis Ochoa Rincón
# Nota: No se usan acentos ni caracteres especiales para evitar conflictos entre los diferentes sistemas operativos.

amenazas <- readRDS(file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Procesada/amenazas 2010-2012.RDS')
amenazas$mes <- format(amenazas$fecha, format= ('%m'))

amenazas %>% group_by(year_month) %>% summarise(Total = sum(cantidad,na.rm=T)) %>% 
  ggplot(data = .) + geom_line(aes(x = year_month,y = Total))

amenazas_muni <- amenazas %>% 
                 group_by(year_month,departamento,municipio,cod_dane,year,mes) %>% 
                 summarise(Total = sum(cantidad,na.rm = T))

saveRDS(object = amenazas_muni,
        file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Municipio/RDS/amenazas.RDS')
rm(amenazas,amenazas_muni)

delitoss <- readRDS(file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Procesada/delitoss 2010-2012.RDS')
delitoss$mes <- format(delitoss$fecha, format= ('%m'))

delitoss %>% group_by(year_month) %>% summarise(Total = sum(cantidad,na.rm=T)) %>% 
  ggplot(data = .) + geom_line(aes(x = year_month,y = Total))

delitoss_muni <- delitoss %>% 
  group_by(year_month,departamento,municipio,cod_dane,year,mes) %>% 
  summarise(Total = sum(cantidad,na.rm = T))

saveRDS(object = delitoss_muni,
file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Municipio/RDS/delitos_sexuales.RDS')
rm(delitoss,delitoss_muni)

homi <- readRDS(file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Procesada/homicidios 2010-2012.RDS')
homi$mes  <- format(homi$fecha, format= ('%m'))

homi2 <- homi %>% group_by(year_month) %>% 
  summarise(Total = sum(cantidad,na.rm=T), total =n()) 
homi2$diff <- homi2$Total-homi2$total
homi2$cantidad <- ifelse(homi2$Total==0,homi2$total,homi2$Total)
ggplot(data = homi2,aes(x = year_month,y = cantidad)) + geom_line()

homi_muni <- homi %>% group_by(year_month,departamento,municipio,cod_dane,year,mes) %>% 
                       summarise(Total = sum(cantidad,na.rm=T),
                       total =n(),
                       edad = mean(edad,na.rm = T))
homi_muni$diff <- homi_muni$Total-homi_muni$total
homi_muni$Total <- ifelse(homi_muni$Total==0,homi_muni$total,homi_muni$Total)

saveRDS(object = homi_muni,
        file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Municipio/RDS/homicidios.RDS')
rm(homi,homi_muni,homi2)



hurto_p <- readRDS(file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Procesada/hurto a personas 2010-2012.RDS')
hurto_p$mes <- format(hurto_p$fecha, format= ('%m'))

hurto_p %>% group_by(year_month) %>% summarise(Total = sum(cantidad,na.rm=T)) %>% 
  ggplot(data = .) + geom_line(aes(x = year_month,y = Total))

hurto_p_muni <- hurto_p %>% 
  group_by(year_month,departamento,municipio,cod_dane,year,mes) %>% 
  summarise(Total = sum(cantidad,na.rm = T))

saveRDS(object = hurto_p_muni,
        file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Municipio/RDS/hurto a personas.RDS')
rm(hurto_p,hurto_p_muni)






hurto_r <- readRDS(file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Procesada/hurto a residencias 2010-2012.RDS')
hurto_r$mes <- format(hurto_r$fecha, format= ('%m'))

hurto_r %>% group_by(year_month) %>% summarise(Total = sum(cantidad,na.rm=T)) %>% 
  ggplot(data = .) + geom_line(aes(x = year_month,y = Total))

hurto_r_muni <- hurto_r %>% 
  group_by(year_month,departamento,municipio,cod_dane,year,mes) %>% 
  summarise(Total = sum(cantidad,na.rm = T))

saveRDS(object = hurto_r_muni,
        file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Municipio/RDS/hurto a residencias.RDS')
rm(hurto_r,hurto_r_muni)





lesiones <- readRDS(file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Procesada/lesiones 2010-2012.RDS')
lesiones$mes <- format(lesiones$fecha, format= ('%m'))

lesiones %>% group_by(year_month) %>% summarise(Total = sum(cantidad,na.rm=T)) %>% 
  ggplot(data = .) + geom_line(aes(x = year_month,y = Total))

lesiones_muni <- lesiones %>% 
  group_by(year_month,departamento,municipio,cod_dane,year,mes) %>% 
  summarise(Total = sum(cantidad,na.rm = T))

saveRDS(object = lesiones_muni,
        file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Municipio/RDS/lesiones personales.RDS')
rm(hurto_r,hurto_r_muni)

files <- list.files(path = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Municipio/RDS/') %>% 
        paste0('~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Municipio/RDS/',.)

lista_archivos <- sapply(files, readRDS)

amenazas <- lista_archivos[[1]]
delitoss <- lista_archivos[[2]]
homicidi <- lista_archivos[[3]]
hurto_pe <- lista_archivos[[4]]
hurto_re <- lista_archivos[[5]]
lesiones <- lista_archivos[[6]]

rm(lesiones_muni,lista_archivos,files)

colnames(amenazas)
colnames(delitoss)
colnames(homicidi)
colnames(hurto_pe)
colnames(hurto_re)
colnames(lesiones)

duplicated(paste0(amenazas$year,amenazas$mes,amenazas$cod_dane)) %>% table()
duplicated(paste0(delitoss$year,delitoss$mes,delitoss$cod_dane)) %>% table()
duplicated(paste0(homicidi$year,homicidi$mes,homicidi$cod_dane)) %>% table()
duplicated(paste0(hurto_pe$year,hurto_pe$mes,hurto_pe$cod_dane)) %>% table()
duplicated(paste0(hurto_re$year,hurto_re$mes,hurto_re$cod_dane)) %>% table()
duplicated(paste0(lesiones$year,lesiones$mes,lesiones$cod_dane)) %>% table()

amenazas_delitos <- merge(x = amenazas,y = delitoss,
                          by = c('year','mes','cod_dane','year_month','departamento','municipio'),
                          all = T)
colnames(amenazas_delitos) <- c(colnames(amenazas_delitos)[1:6],"Amenazas",'Delitos')

ame_del_les <- merge(x = amenazas_delitos,y = lesiones,
                          by = c('year','mes','cod_dane','year_month','departamento','municipio'),
                          all = T)
colnames(ame_del_les)[ncol(ame_del_les)] <- "Lesiones"
colnames(ame_del_les)

ame_del_les_per <- merge(x = ame_del_les,y = hurto_pe,
                     by = c('year','mes','cod_dane','year_month','departamento','municipio'),
                     all = T)
colnames(ame_del_les_per)[ncol(ame_del_les_per)] <- "Hurto personas"
colnames(ame_del_les_per)

ame_del_les_per_res <- merge(x = ame_del_les_per,y = hurto_re,
                         by = c('year','mes','cod_dane','year_month','departamento','municipio'),
                         all = T)
colnames(ame_del_les_per_res)[ncol(ame_del_les_per_res)] <- "Hurto residencias"
colnames(ame_del_les_per_res)

final <- merge(x = ame_del_les_per_res,y = homicidi[,c(1,2,3,4,5,6,7,9)],
               by = c('year','mes','cod_dane','year_month','departamento','municipio'),
               all = T)

colnames(final)[(ncol(final)-1):ncol(final)] <- c('homicidios','prom_edad_homi')
colnames(final)

write.csv(x = final,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/csv/crimenes total.csv')
saveRDS(object = final,file = '~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/RDS/crimenes/Final/crimenes total.RDS')

