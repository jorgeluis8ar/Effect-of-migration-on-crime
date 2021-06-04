# Elaborado por: Jorge Luis Ochoa Rincón
# Nota: No se usan acentos ni caracteres especiales para evitar conflictos entre los diferentes sistemas operativos.

load.files <- function(path){
  
  depto <- read_dta(file = path)
  depto$codigo <-paste0(depto$u_dpto,depto$u_mpio)
  
  covariates <- c("codigo","tipo_reg",'ua_clase', 'u_dpto', 'u_mpio', "u_vivienda", 
                  'pa_lug_nac','pa_vivia_1ano', 'pa_vivia_5anos')
  
  depto <- depto[,covariates]
  inmigrantes_1<- depto[depto$pa_vivia_1ano==4,] %>% group_by(codigo) %>% summarize(inmigrantes_1_ano = sum(pa_vivia_1ano,na.rm = T))
  inmigrantes_5<- depto[depto$pa_vivia_5anos==4,] %>% group_by(codigo) %>% summarize(inmigrantes_5_anos = sum(pa_vivia_5anos,na.rm = T))
  
  final <- merge(x = inmigrantes_1,y = inmigrantes_5,by = 'codigo',all = T)
  inicial <- (stringr::str_locate(string = path ,pattern = "Censo/")[2]+1)
  ultimo  <- (stringr::str_locate(string = path,pattern = ".DTA")[1]-1)
  nombre <- substr(path,inicial,ultimo)
  file_name <- paste0('~/OneDrive - Universidad de los andes/PEG/Econometria avanzada/Proyecto/Bases de datos/Procesada/',
                      nombre,".RDS")
  saveRDS(object = final,file = file_name)
  
  return(cat("\nSe procesó y guardó la base de datos\nArchivo procesado:",path,"\nRuta:",file_name,"\nNombre:",nombre,".RDS\n\n\n"))
}
import.files <- function(path){
  
  df <- readRDS(file = path)
  return(df)
  
}

data_sets <- function(base, date=''){
  base <- st_drop_geometry(base)
  fecha2 <- strsplit(x = date,split = "-") %>% unlist
  ano2 <- fecha2[1]
  mes2 <- fecha2[2]
  base2 <- base %>% mutate(ano = ano2,mes=mes2)
  
  return(base2)
}