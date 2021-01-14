# This script is similar to travel_survey but it is only for cities made by Daniel in the World Bank
# Last update: September 2020

library(foreign) 
library(RODBC)

####Bogota - Colombia (WB) ####
rm(list = ls())
source("code/producing_trips_rd/used_functions.R")
options(scipen = 50)

# Import datasets
hogar <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Bogota/Encuesta de Movilidad 2019/BD EODH2019 FINAL v14022020/Archivos XLSX/HogaresEODH2019.xlsx")
personas <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Bogota/Encuesta de Movilidad 2019/BD EODH2019 FINAL v14022020/Archivos XLSX/PersonasEODH2019.xlsx")
#vehiculos <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Bogota/Encuesta de Movilidad 2019/BD EODH2019 FINAL v14022020/Archivos XLSX/VehículosEODH2019.xlsx")
viaje <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Bogota/Encuesta de Movilidad 2019/BD EODH2019 FINAL v14022020/Archivos XLSX/ViajesEODH2019.xlsx")
# In etapa_viaje there's the informatio of Walk_to_pt
etapa_viaje <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Bogota/Encuesta de Movilidad 2019/BD EODH2019 FINAL v14022020/Archivos XLSX/EtapasEODH2019.xlsx")

# According to the documentation they defined a hierarchy between modes to
# define the main mode of each trip. This hierarchy is in page 126, table 4.4
# The resulting table with main mode and duration calculated in this dataset:
duracion <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Bogota/Encuesta de Movilidad 2019/BD EODH2019 FINAL v14022020/Archivos XLSX/Aux_DuraciónEODH2019.xlsx")

# The number of rows of these datasets is the same to what is mentioned in page
# 210 of the document ".../Informes de Indicadores/200109_Etapa_V_v3_Caracterización de la Movilidad.pdf"

#---- 
# (No need to run)
## Replicating some results to be sure about how to use these datasets
# Number of surveys per UTAM (page 20 of same document mentioned above)
# SQL code in page 214
# These results are the same
View(hogar %>% group_by(Utam) %>% summarise(n()))

# Number of people per household using weights
# SQL code in page 215
# These results are the same as in page 32, figure 3.4
cant_personas_hogar <- hogar %>% 
  inner_join(personas, by = c("Id_Hogar" = "id_hogar")) %>% 
  group_by(Id_Hogar) %>% summarise(cantidad = n())

View(hogar %>% inner_join(cant_personas_hogar, by = "Id_Hogar") %>% 
       filter(municipio == "11001") %>%
       rowwise() %>% 
       mutate(sumoff_exp_h = cantidad*Factor) %>% 
       group_by(p5_estrato) %>% 
       summarise(sum(sumoff_exp_h)/sum(Factor))
)

# Number of trips per mode in the whole dataset
(n_trips <- duracion %>% group_by(modo_principal) %>% 
    summarise(suma = sum(f_exp)) %>% arrange(desc(suma)))
sum(n_trips$suma) # This is the same as mentioned in page 83, paragraph 4.5

# Number of trips per mode in Bogota
(n_trips_bog <- duracion %>% left_join(hogar, by = c("id_hogar" = "Id_Hogar")) %>% 
    filter(municipio == "11001") %>% 
    group_by(modo_principal) %>% summarise(suma = sum(f_exp)) %>% 
    arrange(desc(suma)))
sum(n_trips_bog$suma) # This is the same as mentioned in page 83, paragraph 4.5

# Number of trips per mode considering only pedestrian trips longer than 15 minutes
# and other mode trips longer than 3
# sort(unique(duracion$duracion))
# table(duracion$duracion, duracion$modo_principal)
#personas2 <- personas %>%  select(id_hogar, id_persona, p4_edad)
duracion_mayores_15 <- duracion %>% 
  # I tried to see whether there was children younger than 5 years old, but there werent
  #left_join(personas2, by = c("id_hogar", "id_persona")) %>% 
  filter(modo_principal != "A pie" | 
           (modo_principal == "A pie" & duracion >= 15))
# Manually I had to come up with the upper bound of duration. Using 540 leads to
# a difference of just 1 trip. The problem is that the number of trips in other
# municipalities changes, which is not right.
# sort(unique(duracion[duracion$modo_principal == "A pie",]$duracion))

(n_trips_15 <- duracion_mayores_15 %>% 
    #filter(p4_edad <= 103) %>% 
    group_by(modo_principal) %>% 
    summarise(suma = sum(f_exp)) %>% 
    arrange(desc(suma)))
sum(n_trips_15$suma) # This is NOT the same as mentioned in page 83, paragraph 4.5
#write.table(n_trips_15, "clipboard", sep = "\t")

# Number of trips per municipality (trip larger than 15 minutes)
(n_trips_15_mun <- duracion_mayores_15 %>% 
    left_join(hogar, by = c("id_hogar" = "Id_Hogar")) %>% 
    group_by(municipio) %>% summarise(suma = sum(f_exp)) %>% 
    arrange(desc(suma)))
# In the previous output the following municipalities have the same number of trips
# as mentioned in page 86, table 4.2
# Bojaca, Cajica, Chia, Cota, El Rosal, Facatativa, Funza, Gachancipa, La Calera,
# Madrid, Mosquera, Sibaté, Soacha, Sopo, Tabio, Tenjo, Tocancipa, Zipaquira.
# Therefore the difference is with the number of trips is only in Bogota

# Number of trips per mode in Bogota
(n_trips_15_bog <- duracion_mayores_15 %>% left_join(hogar, by = c("id_hogar" = "Id_Hogar")) %>% 
    filter(municipio == "11001") %>% 
    #group_by(modo_principal) %>% 
    group_by(Utam) %>% 
    summarise(suma = sum(f_exp)) %>% 
    arrange(desc(suma)))
sum(n_trips_15_bog$suma) # This is NOT the same as mentioned in page 83, paragraph 4.5

#personas2 <- personas %>%  select(id_hogar, id_persona, p4_edad)
duracion_mayores_15_bog <- duracion_mayores_15 %>% left_join(hogar, by = c("id_hogar" = "Id_Hogar")) %>% 
  filter(municipio == "11001") %>% 
  left_join(personas2, by = c("id_hogar", "id_persona"))

View(head(duracion_mayores_15_bog))
sort(unique(duracion_mayores_15_bog$p4_edad))

(n_trips_15_bog <- duracion_mayores_15_bog %>% 
    filter(p4_edad > 4 & p4_edad <= 103) %>% 
    group_by(modo_principal) %>% summarise(suma = sum(f_exp)))
sum(n_trips_15_bog$suma) # This is NOT the same as mentioned in page 83, paragraph 4.5
## I couldn't find the difference between the dataset and what is reported in the document
## I tried several things but I guess there's something wrong with the dataset
#----
## Preprocessing the dataset
# Since duracion dataset already have computed trip duration and defined the
# main mode of transport, I'm going to use it.

# Import hierarchy to transform from spanish modes to ithim modes
# This hierarchy is in page 126, table 4.4 of the same document
# ToDo: Talk to cambridge about the translation, specifically about cable and informal modes
# Definition and pictures of modes are in "...\WorldBank\Data\Colombia\Bogota\Encuesta de Movilidad 2019\Formularios\190219_Tarjetas 4-6_DPR_.pdf"
main_mode <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Jerarquia.xlsx", sheet = "Bogota")

# Selecting variables I need in personas and hogar
personas2 <- personas %>%  select(id_hogar, id_persona, p4_edad, Sexo)
hogar2 <- hogar %>% select(Id_Hogar, municipio)

# Check whether modes in duracion dataset are the same that in main_mode
# unique(duracion$modo_principal) %in% unique(main_mode$ModoPrincipal) # OK

# Filtering duracion for only bogota and creating new variables
duracion_bog <- duracion %>% 
  # Merge municipality and filtering bogota
  left_join(hogar2, by = c("id_hogar" = "Id_Hogar")) %>% filter(municipio == "11001") %>% 
  # Merge age and sex and renaming varibles
  left_join(personas2, by = c("id_hogar", "id_persona")) %>% 
  mutate(participant_id = paste0(id_hogar, id_persona),
         trip_id = paste0(id_hogar, id_persona, id_viaje),
         age = p4_edad,
         sex = ifelse(Sexo == "Hombre", "male", "female"),
         trip_duration = duracion,
         trip_mode = main_mode$ITHIM[
           match(modo_principal, main_mode$ModoPrincipal)]
         )

# Checking variables created
#table(duracion_bog$sex, duracion_bog$Sexo, useNA = "always") # OK
#table(duracion_bog$trip_mode, duracion_bog$modo_principal, useNA = "always") # OK
#length(unique(duracion_bog$trip_id)) == nrow(duracion_bog) # OK

write_csv(duracion_bog, 'data/local/bogota_wb2/bogota_wb2_trip.csv')

trip <- read_csv('data/local/bogota_wb2/bogota_wb2_trip.csv')


# Standardized travel modes
trip <- standardize_modes(trip, mode = c('trip'))
names(trip)
rd <- trip %>% select(participant_id, age, sex, trip_id, trip_mode, trip_duration)

rd$year <- "2019"
rd$gdppc2014 <- 17497
rd$population2014 <- 9135800

# Export
write_csv(rd, 'inst/extdata/local/bogota_wb2/trips_bogota_wb2.csv')
write_csv(rd, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Bogota/Cleaned/trips_bogota_wb2.csv')
write_csv(rd, 'data/local/bogota_wb2/trips_bogota_wb2.csv')


####Medellin - Colombia (WB) ####
rm(list = ls())
source("code/producing_trips_rd/used_functions.R")
options(scipen = 50)

# Import datasets
# These datasets don't come with sampling weights, so it's not possible to replcate
# some results. Besides, the pdf documents that I have don't report main indicators
# nor I could open the dashboard from the website.
trips <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/EOD2017/EOD_2017_DatosViajes.xlsx", sheet = "DATOS VIAJES") 
trips <- trips[rowSums(is.na(trips)) != ncol(trips), ] # Removing rows that only have NAs
# When reading person there are some warnings related to people without age
person <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/EOD2017/EOD_2017_DatosViajes.xlsx", sheet = "DATOS MORADORES")
hogar <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/EOD2017/EOD_2017_DatosViajes.xlsx", sheet = "DATOS HOGARES")

#---- 
# (No need to run)
## Replicating some results to be sure about how to use these datasets
# Number of households is the same 
# length(unique(hogar$ID_HOGAR)) #16340
# unique(hogar$ID_MUNICIPIO) # 10 municipalities

View(head(trips))
names(trips)

# Looking at the number of trips
aa <- trips %>% mutate(trip_id = paste0(ID_HOGAR, ID_MORADOR, SEC_VIAJE))
length(unique(aa$trip_id)) == nrow(aa) # There's one trip duplicated
which(duplicated(aa$trip_id) == T)
aa$trip_id[59151]
View(aa[aa$trip_id == "133037511282612",]) # it's from pereira


#---- 

# Import hierarchy to decide main mode in each trip
main_mode <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Jerarquia.xlsx", sheet = "Medellin")

# In person, create an ID for each person taking into account its household
person <- person %>% 
  mutate(id_person = paste(ID_HOGAR, ORDEN_MORADOR, sep = "-"),
         participant_id = paste0(ID_HOGAR, ORDEN_MORADOR),
         sex = ifelse(GENERO == "1", "male", "female"),
         age = EDAD)
#length(unique(person$id_person)) == nrow(person) #OK
#length(unique(person$participant_id)) == nrow(person) #OK

# In trips, create new variables:
# ID for each person taking into account its household
# Trip id
trips2 <- trips %>% 
  mutate(id_person = paste(ID_HOGAR, ID_MORADOR, sep = "-"),
         participant_id = paste0(ID_HOGAR,ID_MORADOR),
         id_trip = paste(id_person, SEC_VIAJE, sep = "-"),
         trip_id = paste0(participant_id, SEC_VIAJE),
         trip_duration = as.numeric(difftime(HORA_D, HORA_O, units = "mins")),
         # Replace modes by its hierarchy
         mode_e1 = main_mode$Jerarquia[
           match(DESC_MODO_TTE_E1, main_mode$Modo_transporte_formulario_EODH)],
         mode_e2 = main_mode$Jerarquia[
           match(DESC_MODO_TTE_E2, main_mode$Modo_transporte_formulario_EODH)],
         mode_e3 = main_mode$Jerarquia[
           match(DESC_MODO_TTE_E3, main_mode$Modo_transporte_formulario_EODH)],
         mode_e4 = main_mode$Jerarquia[
           match(DESC_MODO_TTE_E4, main_mode$Modo_transporte_formulario_EODH)],
         mode_e5 = main_mode$Jerarquia[
           match(DEC_MODO_TTE_E5, main_mode$Modo_transporte_formulario_EODH)],
         mode_e6 = main_mode$Jerarquia[
           match(DESC_MODO_TTE_E6, main_mode$Modo_transporte_formulario_EODH)],
         mode_e7 = main_mode$Jerarquia[
           match(DESC_MODO_TTE_E7, main_mode$Modo_transporte_formulario_EODH)]
  ) %>%
  rowwise() %>% mutate(
    main_modes = min(mode_e1, mode_e2, mode_e3, mode_e4, mode_e5, mode_e6,
                     mode_e7, na.rm = T),
    trip_mode = main_mode$ITHIM[
      match(main_modes, main_mode$Jerarquia)]
  )

# Merge sex and age variables
trips3 <- merge(trips2, person[,c("participant_id", "sex", "age")], 
                by = "participant_id", all.x = T) 

# Remove dataframes I don't need anymore
rm(person, trips, trips2)

write_csv(trips3, 'data/local/medellin_wb/medellin_wb_trip.csv')

trip <- read_csv('data/local/medellin_wb/medellin_wb_trip.csv')


# Standardized travel modes
trip <- standardize_modes(trip, mode = c('trip'))

rd <- trip %>% select(participant_id, age, sex, trip_id, trip_mode,
                      trip_duration)

# This data needs an update
rd$year <- "2017"
rd$gdppc2014 <- 18998 # https://aqtr.com/association/actualites/medellin-road-becoming-knowledge-economy
# Population of the whole valle de aburra
rd$population2014 <- 3591963 #https://en.wikipedia.org/wiki/The_Metropolitan_Area_of_the_Aburr%C3%A1_Valley 

# Export
write_csv(rd, 'inst/extdata/local/medellin_wb/trips_medellin_wb.csv')
write_csv(rd, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/Cleaned/trips_medellin_wb.csv')
write_csv(rd, 'data/local/medellin_wb/trips_medellin_wb.csv')


####Cali - Colombia (WB) ####
rm(list = ls())
source("code/producing_trips_rd/used_functions.R")
options(scipen = 50)


#' Importing datasets
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Cali/EODH2015/Base de datos EODH 2015/2. Módulos/"

#' *Hogares*
#' The number of rows is the same that appears in pag 98, paragraph 4.2 of file
#' "160216_Producto 4_Indicadores EODH.pdf"
hogar <- read_excel(paste0(route, "MOD_A_ID_HOGAR_.xlsx"))

#'*Personas*
#' In the original file the first row is the label of each variable and the
#' second row is the meaning of that label. I had to remove this second row and
#' delete some images that were inside the file to read it easily here in R.
#' This file is now called: MOD_B_PERSONAS_Modified.xlsx
#' When reading person there are some warnings related to people without age
#' The number of rows is the same that appears in pag 100, paragraph 4.4 of file
#' "160216_Producto 4_Indicadores EODH.pdf"
person <- read_excel(paste0(route, "MOD_B_PERSONAS_Modified.xlsx"))

#' *Vehiculos*
#' The number of rows is the same that appears in pag 102, paragraph 4.6 of file
#' "160216_Producto 4_Indicadores EODH.pdf"
vehiculos <- read_excel(paste0(route, "MOD_C_VEHICULOS.xlsx"))

#'*Trips*
#' According to the documentation of the survey, the dataset with imputed trips
#' is the one that we should use (pag 102, "160216_Producto 4_Indicadores EODH.pdf")
#' The number of rows is the same that appears in pag 103, paragraph 4.12 of file
#' "160216_Producto 4_Indicadores EODH.pdf"
#' Before importing this dataset I added a new column called "EMPTY" because values
#' didn't correspond to the label of the variable. By creating this empty label
#' everything makes more sense now. This file is now called:
#' "MOD_D_VIAJES_IMP_Modified.xlsx"
trips <- read_excel(paste0(route, "MOD_D_VIAJES_IMP_Modified.xlsx"))
trips <- trips[rowSums(is.na(trips)) != ncol(trips), ] # Removing files that only have NAs

#View(head(trips))

#'*Hierarchy main mode*
#' In this case I didn't have to import any hierarchy to define the main mode of
#' each trip because only P_E1_14_D have data for all rows, and only in
#' P_E3_14_D there were 773 rows with info (less than 2%). The remaining
#' variables about stages didn't have any information
#' However, I have to make the translation of these modes taking into account
#' the meaning of each number from Pag 3 "WorldBank\Data\Colombia\Cali\EODH2015\Base de datos EODH 2015\1. Formatos\150508_EODH_TABLAS_AUX_V06.pdf"
main_mode <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Jerarquia.xlsx", sheet = "Cali")

#---- 
# (No need to run)
## Replicating some results to be sure about how to use these datasets
# Number of valid surveys by socioeconomial level
table(hogar$ESTRATO) # OK Pag 112, figure 5.4
table(hogar$UTAM, hogar$ESTRATO)
sum(hogar$F_EXP) # Total of households, pag 114, paragraph 5.11
hogar %>% group_by(MUNICIPIO) %>% 
  summarise(suma = sum(F_EXP)) %>% 
  arrange(desc(suma)) # OK Pag 116 figure 5.9

# Average people per household
# Same numbers as in pag 119, figure 5.12
cant_personas_hogar <- hogar %>% 
  inner_join(person, by = "ORDEN") %>% 
  group_by(ORDEN) %>% summarise(cantidad = n())

View(hogar %>% inner_join(cant_personas_hogar, by = "ORDEN") %>% 
       #filter(municipio == "11001") %>%
       rowwise() %>% 
       mutate(sumoff_exp_h = cantidad*F_EXP) %>% 
       group_by(MUNICIPIO) %>% 
       summarise(sum(sumoff_exp_h)/sum(F_EXP))
    )


# Number of people per municipality
t_person <- person %>% left_join(hogar[, c("ORDEN", "MUNICIPIO")], by = "ORDEN") %>% 
  group_by(MUNICIPIO) %>% summarise(total = sum(F_EXP)) %>% 
  mutate(prop = total / sum(total)*100) # Ok Pag 123, Figure 5.19
sum(t_person$total) # I have 3 more people, although results in this document are not consistent

# Total number of trips
sum(trips$F_EXP) # There's one more trip. Pag 146 paragraph 5.52

# Mode Share
trips %>% group_by(P_E1_14_D) %>% summarise(total = sum(F_EXP)) %>% 
  mutate(prop = total / sum(total)*100) %>% 
  arrange(desc(prop))

sum(t_person$total) # I have 3 more people, although results in this document are not consistent
#---- 


# In person, create an ID for each person taking into account its household
#table(person$P_04_B, useNA = "always")
#table(person$P_05_B, useNA = "always")
person <- person %>%
  mutate(id_person = paste(ORDEN, ID_PER, sep = "-"),
         participant_id = paste0(ORDEN, ID_PER),
         sex = ifelse(P_04_B == "HOMBRE", "male", "female"),
         age = P_05_B)
#length(unique(person$id_person)) == nrow(person) #OK
#length(unique(person$participant_id)) == nrow(person) #OK

#' In trips, create new variables:
#' ID for each person taking into account its household
#' Trip id
#table(trips$P_E1_14_D, useNA = "always")
trips2 <- trips %>%
  mutate(id_person = paste(ORDEN, ID_PER, sep = "-"),
         participant_id = paste0(ORDEN, ID_PER),
         id_trip = DONANTE,
         trip_id = paste0(participant_id, NO_VIAJE),
         trip_duration = T_VIAJE,
         trip_mode = main_mode$ITHIM[
           match(P_E1_14_D, main_mode$Codigo)]
  )

#table(trips2$P_E1_14_D, trips2$trip_mode)

# Merge sex and age variables
trips3 <- merge(trips2, person[,c("participant_id", "sex", "age")],
                by = "participant_id", all.x = T)

# Remove dataframes I don't need anymore
rm(person, trips, trips2)

# Standardized travel modes for the package
trips4 <- standardize_modes(trips3, mode = c('trip'))

rd <- trips4 %>% select(participant_id, age, sex, trip_id, trip_mode, trip_duration)

# This data needs an update
rd$year <- "2015"
rd$gdppc2014 <- 11111 
rd$population2014 <- 11111 

# Export
write_csv(rd, 'inst/extdata/local/cali_wb/trips_cali_wb.csv')
write_csv(rd, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Cali/Cleaned/trips_cali_wb.csv')
write_csv(rd, 'data/local/cali_wb/trips_cali_wb.csv')


#### Sao Paulo - Brazil (WB) ####
rm(list = ls())
source("code/producing_trips_rd/used_functions.R")
options(scipen = 50)
trips <- read.dbf("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Brazil/SaoPaulo/OD 2017/Banco de dados/OD_2017.dbf")


#### Mexico City - Mexico (WB) ####
rm(list = ls())
source("code/producing_trips_rd/used_functions.R")
options(scipen = 50)

# Notes (first approach to datasets by hand on excel)
# In tvivieda.csv, variable "ent" = 9 is Mexico city, comparing the sum of the 
# factor and comparing it with resultados_eod_2017.pdf. "ent" = 15 or 13
# corresponds to "Municipios conurbados del estado de Mexico y Tizayuca"
# Same applies to thogar.csv and tsdem.csv
# In tsdem, edad = 99 means "Edad no especificada"

# Import datasets
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Mexico/MexicoCity/Encuesta2017/Microdatos/CSV/"

vivienda <- read_csv(paste0(route, "tvivienda_eod2017/conjunto_de_datos/tvivienda.csv"))
hogar <- read_csv(paste0(route, "thogar_eod2017/conjunto_de_datos/thogar.csv"))
personas <- read_csv(paste0(route, "tsdem_eod2017/conjunto_de_datos/tsdem.csv"))
trips <- read_csv(paste0(route, "tviaje_eod2017/conjunto_de_datos/tviaje.csv"))
stages <- read_csv(paste0(route, "ttransporte_eod2017/conjunto_de_datos/ttransporte.csv"))

#'*Hierarchy main mode*
#' In this case I didn't have to import any hierarchy to define the main mode of
#' each trip because there's enough information at stage level
#' However, I have to make the translation of these modes taking into account
#' the meaning of each number from Pag 3 
main_mode <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Mexico/Jerarquia.xlsx", sheet = "MexicoCity")

#---- 
# (No need to run)
## Replicating some results to be sure about how to use these datasets
# Vivienda
sum(vivienda$factor) # Ok same as pag 16 of resultados_eod_2017.pdf

# Hogar
sum(hogar$factor) # Ok same as pag 16 of resultados_eod_2017.pdf

# Personas
sum(personas$factor) # Ok same as pag 16 of resultados_eod_2017.pdf
personas %>% mutate(edad = as.integer(edad)) %>% filter(edad >= 6 & edad != 99) %>%
summarise(sum(factor)) # Ok same as pag 16 of resultados_eod_2017.pdf

# Trips in weekdays
trips %>% filter(p5_3 == 1) %>% # Filter trips during weekdays
  summarise(sum(factor)) # This value is the same
# as in pag 28 of resultados_eod_2017.pdf, however when looking at tables
# already provided "C:\Users\danie\Documents\Daniel_Gil\Consultorias\2020\WorldBank\Data\Mexico\MexicoCity\Tabulados\ViajesEntreSemana\tabulados_eod_2017_entre_semana.xlsx" the number of trips is different

# Trips in weekdays by mode
trips %>% filter(p5_3 == 1) %>% # Filter trips during weekdays
  rowwise() %>% 
  mutate(p5_14_01_w = ifelse(p5_14_01 == 1, factor, 0),
         p5_14_02_w = ifelse(p5_14_02 == 1, factor, 0),
         p5_14_07_w = ifelse(p5_14_07 == 1, factor, 0)) %>% 
  group_by(p5_3) %>% # Used it to get a single value
  summarise(automovil = sum(p5_14_01_w),
            colectivo = sum(p5_14_02_w),
            bicicleta = sum(p5_14_07_w)) # These values are the same as in appendix Cuadro_4.1
# Trips in Mexico city in weekdays
mexico_trips <- trips %>% filter(p5_3 == 1 & p5_7_7 == "09") 

mexico_trips %>% # Filter trips during weekdays
  summarise(sum(factor)) # This value is the same as in the same appendix "Cuadro_4.1C"

# Tried to replicate Cuadro_4.1A but I realized that is based on trips dataset
# and not stages
# mexico_stages <- stages %>% left_join(trips[,c("id_via", "p5_7_7")], 
#                                             by = "id_via") %>% 
#   filter(p5_3 == 1 & p5_7_7 == "09") 
# (aux <- mexico_stages %>% group_by(p5_14) %>% summarise(suma = sum(factor)) %>% 
#   mutate(suma/sum(suma)))
# write.table(aux, "clipboard", sep = "\t")
# names(stages)

# Trips in Other municipalities
# I couldn't replicate the same results in the appendix. In Cuadro_4.1C the sum
# of trips in Mexico city and other municipalities is not the same either. So
# there must be some preprocessing to get the same results
trips %>% filter(p5_3 == 1 & !p5_7_7 %in% c("09", "99")) %>% # Filter trips during weekdays
  summarise(sum(factor))

#---- 
## Preprocessing of dataset
# First thing to note is that the whole dataset have information for trips in
# weekdays and saturdays. So I have to filter trips in weekdays, otherwise I
# would be counting more trips for people that did trips on both days to a 
# single day. Example id_soc = 1327

# Filter weekdays trips
trips <- trips %>% filter(p5_3 == 1)
stages <- stages %>% filter(p5_3 == 1)

# Check the number of trips in both datasets
# length(unique(trips$id_via)) == nrow(trips) # ok
# length(unique(trips$id_via)) == length(unique(stages$id_via)) # ok

# Create new variables
stages2 <- stages %>% 
  left_join(trips[, c("id_soc", "id_via")], by = "id_via") %>% 
  mutate(participant_id = id_soc,
         trip_id = id_via,
         stage_id = id_tra,
         p5_16_1_1 = as.numeric(p5_16_1_1),
         p5_16_1_2 = as.numeric(p5_16_1_2),
         stage_duration = ifelse(p5_16_1_1 != 99, 
                                 (p5_16_1_1 * 60) + p5_16_1_2,
                                 NA),
         stage_mode = main_mode$ITHIM[
           match(p5_14, main_mode$Codigo)],
         stage_hierarchy = main_mode$Jerarquia[
           match(p5_14, main_mode$Codigo)],
         sex = ifelse(sexo == 1, "male", "female"),
         age = edad)
#View(tail(stages2))

# Compute trip duration and trip mode from stages dataset
trip_info <- stages2 %>% group_by(trip_id) %>% 
  summarise(trip_duration = sum(stage_duration, na.rm = T),
            min_mode = min(stage_hierarchy),
            trip_mode = main_mode$ITHIM[
              match(min_mode, main_mode$Jerarquia)])
#View(head(trip_info,20))

# Paste trip information to stages
stages2 <- stages2 %>% left_join(trip_info, by = "trip_id")

# Checking some variables
#length(unique(trips$id_soc)) == length(unique(stages2$participant_id)) # Same number of people
#length(unique(trips$id_via)) == length(unique(stages2$trip_id)) # Same number of trips
#table(stages2$sexo, stages2$sex)


# Standardized travel modes
stages3 <- standardize_modes(stages2, mode = c('trip', 'stage'))
names(stages3)
rd <- stages3 %>% select(participant_id, age, sex, trip_id, trip_mode,
                      trip_duration, stage_id, stage_mode, stage_duration)

# From Lambed
rd$year <- 2017
rd$gdppc2014 <- 19239
rd$population2014 <- 20976700 

# Export
write_csv(rd, 'inst/extdata/local/mexico_city_wb/trips_mexico_city_wb.csv')
write_csv(rd, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Mexico/MexicoCity/Cleaned/trips_mexico_city_wb.csv')
write_csv(rd, 'data/local/mexico_city_wb/trips_mexico_city_wb.csv')


#### Santiago - Chile (WB) ####
rm(list = ls())
source("code/producing_trips_rd/used_functions.R")
options(scipen = 50)

# Notes (first approach to dataset by hand on excel)
# In tvivieda.csv, variable "ent" = 9 is Mexico city, comparing the sum of the 
# factor and comparing it with resultados_eod_2017.pdf. "ent" = 15 or 13 corresponds
# to "Municipios conurbados del estado de Mexico y Tizayuca"
# Same applies to thogar.csv and tsdem.csv
# In tsdem, edad = 99 means "Edad no especificada"

# Import datasets
# Before Importing here I exported all tables to csv files because of access 
# issues with 32 bits.
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Chile/Santiago/Trips/CSV/"

hogar <- read_xlsx(paste0(route, "Hogar.xlsx"))
personas <- read_xlsx(paste0(route, "Persona.xlsx"))
trips <- read_xlsx(paste0(route, "Viaje.xlsx"))
stages <- read_xlsx(paste0(route, "Etapa.xlsx"))

#---- 
# (No need to run)
## Replicating some results to be sure about how to use these datasets
## Comparing everything with "WorldBank\Data\Chile\Santiago\Trips\Reports\Actualizacion_recolecc_STU_Santiago_IX Etapa_EOD Stgo 2012_Inf_Final vol 2.pdf"
# Hogar
sum(hogar$Factor) # Ok same as pag 55, tabla 17 

# Personas
sum(personas$Factor) # Ok same as pag 55, tabla 17 

# Trips
# Definition of trip_mode is in pag 74
sum(trips$FactorFindesemanaEstival, na.rm = T)

# Stages
