# Script to get injury data in the cities made by Daniel in the World Bank
# August 2020
# Last update: September 2020

# Loading libraries
library(tidyverse)
library(readxl)
library(writexl)
library(reshape2)

##' *Medellin - Colombia*
rm(list = ls())
# Read lookup table for standardized modes 
smodes <- read_csv('data/global/modes/standardized_modes.csv') %>% 
  separate_rows(original, sep = ';') %>% 
  mutate(across(where(is.character), str_trim))
#smodes$original

accidents <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/Injuries/Incidentalidad_completa2014-2019.xlsx", sheet = "Incidentes") 
vehicles <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/Injuries/Incidentalidad_completa2014-2019.xlsx", sheet = "Vehículos") 
victims <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/Injuries/Incidentalidad_completa2014-2019.xlsx", sheet = "Vïctimas") 

# Renaming columns to remove spaces
names(victims)
names(victims) <- c("NRO_RADICADO", "Gravedad_victima", "Id", "Fecha_Accidente",
                    "Hora_Accidente", "Clase_Accidente", "Direccion_Accidente",
                    "Sexo", "Edad", "Condicion", "Vehiculo_victima")
names(accidents)
names(accidents) <- c("NRO_RADICADO", "CLASE_ACCIDENTE", "DIRECCION", "CBML",
                      "Gravedad_Incidente", "nombre_comuna", "Barrio", "Año",
                      "FECHA_ACCIDENTE", "HORA_ACCIDENTE", "codigo_comuna",
                      "Comuna")

# Find modes in these datasets to translate them to english
# I pasted this information in Incidentalidad_completa2014-2019_daniel.xlsx 
# sheet = "Modes", and then I made the equivalence by hand
aux = unique(c(unique(vehicles$CLASE_VEHICULO), unique(toupper(victims$Vehiculo_victima))))
#write.table(smodes$original, "clipboard", sep = "\t")
#write.table(aux, "clipboard", sep = "\t")

# Once I defined the equivalence and priority manually, I import it again
mode_equivalence <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/Injuries/Incidentalidad_completa2014-2019_daniel.xlsx", sheet = "Modes_export") 

## Filter only vehicles involved in fatalities
names(accidents)
vehicles2 <- vehicles %>% 
  left_join(accidents[,c("NRO_RADICADO", "Gravedad_Incidente")], 
            by = "NRO_RADICADO") %>% 
  filter(Gravedad_Incidente == "Con muertos") %>% 
  arrange(NRO_RADICADO) %>% drop_na(CLASE_VEHICULO) %>% 
  select(-Gravedad_Incidente) %>% 
  mutate(# vehicle is the translation
    vehicle = mode_equivalence$Equivalence[
    match(CLASE_VEHICULO, mode_equivalence$CLASE_VEHICULO)],
    # priority is a number assigned taking into account the size of the vehicle.
    # It's used when there are more than 2 vehicles involved in a single accident.
    # In these cases the largest vehicle is assigned. For instance, if a cyclist
    # dies in an accident where a bus and a car were involved, then we assumed
    # that the cyclist died because of the bus (because is larger in size)
         priority = mode_equivalence$Priority[
           match(CLASE_VEHICULO, mode_equivalence$CLASE_VEHICULO)])
  
# Check translation and priority
#table(vehicles2$vehicle, vehicles2$CLASE_VEHICULO, useNA = "always") # OK
#table(vehicles2$vehicle, vehicles2$priority, useNA = "always") # OK

# Column to identify vehicles of the same accident. This needs an ordered dataset
vehicles2$vehicle_number <- sequence(rle(as.character(vehicles2$NRO_RADICADO))$lengths)

## Vehicles from long to wide to get vehicles involved in each accident in a single column
vehicles3 <- as.data.frame(vehicles2) %>% 
  reshape(idvar = c("NRO_RADICADO", "AÑO_ACCIDENTE", "FECHA_ACCIDENTE"), 
          timevar = "vehicle_number", direction = "wide")# %>% 
  # rowwise() %>% 
  # mutate(max_priority = max(priority.1, priority.2, priority.3, priority.4, 
  #                           priority.5, priority.6, priority.7, priority.8, 
  #                           priority.9, priority.10, priority.11, priority.12, 
  #                           priority.13, priority.14, priority.15, na.rm = T),
  #        largest_vehicle = mode_equivalence$Equivalence[
  #          match(max_priority, mode_equivalence$Priority)])
names(vehicles3)
#View(tail(vehicles3,100))
#length(unique(vehicles3$NRO_RADICADO)) == nrow(vehicles3) #OK, one row per accident




## Merge vehicle information to victims and filtering only fatalities
names(victims)
names(vehicles3)
victims$NRO_RADICADO <- as.character(victims$NRO_RADICADO)
victims2 <- victims %>% 
  # Translate to english "vehiculo_victima", which is used when there's no vehicle
  # information
  mutate(victim_vehicle = mode_equivalence$Equivalence[
    match(toupper(Vehiculo_victima), mode_equivalence$CLASE_VEHICULO)],
    victim_vehicle_priority = mode_equivalence$Priority[
      match(toupper(Vehiculo_victima), mode_equivalence$CLASE_VEHICULO)]) %>% 
  left_join(vehicles3, by = "NRO_RADICADO") %>% 
  filter(Gravedad_victima == "Muertos") %>% drop_na(NRO_RADICADO) %>% 
  mutate(event_id = NRO_RADICADO,
         year = AÑO_ACCIDENTE,
         cas_age = Edad,
         cas_sex = ifelse(Sexo == "M", "male", "female"))

# Check translation and priority
#table(victims2$victim_vehicle, victims2$Vehiculo_victima, useNA = "always") # OK
#table(victims2$victim_vehicle, victims2$victim_vehicle_priority, useNA = "always") # OK

#write_xlsx(victims2, "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/Injuries/fatalities2.xlsx")

injuries <- read_xlsx("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/Injuries/fatalities2.xlsx", sheet = "Sheet1")

# Recode cas mode to standard mode
injuries$cas_mode <- smodes$inj_vic_lng[match(tolower(injuries$cas_mode), smodes$original)]

# Recode strike mode to standard mode
injuries$strike_mode <- smodes$inj_str_lng[match(tolower(injuries$strike_mode), smodes$original)]  

# Select only columns we need
names(injuries)
injuries2 <- injuries %>% select("event_id", "year", "cas_age", "cas_sex",
                                 "cas_mode", "strike_mode" )
# Write file to the right folder
write_csv(injuries2, 'inst/extdata/local/medellin_wb/injuries_medellin_wb.csv')
write_csv(injuries2, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/Cleaned/injuries_medellin_wb.csv')
write_csv(injuries2, 'data/local/medellin_wb/injuries_medellin_wb.csv')


####################################################################################################
#Todo lo que está de aca para abajo es cuando lo intenté a hacer en codigo, pero tiene muchas reglas especificas






##################################################
# Trying with code
names(victims2)
for(i in 1:nrow(victims2)) {
  i=8
  cond <- victims2[i,"Condicion"]
  vic_veh <- victims2[i,"victim_vehicle"]
  list <- unlist(victims2[i,c(paste0("vehicle.",1:15))])
  list2 <- unlist(victims2[i,c(paste0("priority.",1:15))])
  if(cond == "Peatón"){
    index <- which.max(list2) # Identify largest vehicle
    victims2[i,"strike_mode"] <- list[index]
  } else {
    
    View(head(victims2))
  index <- which.max(list == vic_veh)  
  
}
  
  View(smodes)

##################################################
# When I tried to do it manually
## Classifying cas_mode and strike_mode
unique(victims2$Condicion)
# If a victim is a a pedestrian, then cas_mode is assigned to it.
victims2$cas_mode <- ifelse(victims2$Condicion == "Peatón", "pedestrian", NA)
# If a victim is a pedestrian and there's only one car involved then assigned it 
# to strike_mode
victims2$strike_mode <- ifelse(victims2$Condicion == "Peatón" &
                                 is.na(victims2$vehicle.2),
                               ifelse(!is.na(victims2$vehicle.1),
                                      victims2$vehicle.1, 
                                      victims2$victim_vehicle),
                               NA)
# If a victim is a pedestrian and there're more than one car involved, 
# then assigned it the largest one in size to strike_mode
victims2$strike_mode <- ifelse(victims2$Condicion == "Peatón" &
                                 !is.na(victims2$vehicle.2) & 
                                 is.na(victims2$strike_mode),
                               victims2$largest_vehicle, victims2$strike_mode)


# If a victim is a cyclist, then cas_mode is assigned to it.
victims2$cas_mode <- ifelse(victims2$Condicion == "Ciclista", 
                            victims2$victim_vehicle, NA)

#para strike_mode en ciclistas
#Primero filtre los que son vacios en CLASE_VEHICULO.2, y si en vehicle.1 es bicicleta
#le puse NOV, de no other vehicle, porque asumo que se cayó o se estrelló contra un obejto fijo.
#Aca tambien revise que la edad y el sexo fueran la misma.
# Luego seleccionando los demas en vehicle.1 le asigne el modo que había y si estaba
#vacio le puse no other vehicle bajo el mismo supuesto
# Luego quite el filtro de CLASE_VEHICULO.2 y seleccione los que no estaban vacios ahi, pero
# tambien filtre en bacios los que estaban en CLASE_VEHICULO.3, para replicar el proces.
#a estos le asigne el vehiculo mas grande (en vehicle.1 o vehicle.2) que no fuera la bici

# Motociclistas
# Para cas_mode en motociclistas dejé lo mismo que en victim_vehicle
#para strike_mode en motociclistas
#Primero filtre los que son vacios en CLASE_VEHICULO.2, y si en vehicle.1 es motorcycle
#le puse NOV, de no other vehicle, porque asumo que se cayó o se estrelló contra un obejto fijo.
#Aca tambien revise que la edad y el sexo fueran la misma.
# Si no tienen nada de información en vehicle.1, lo dejé tambien como nov, bajo el mismo supuesto
# Luego quite el filtro de CLASE_VEHICULO.2 y seleccione los que no estaban vacios ahi, pero
# tambien filtre en vacios los que estaban en CLASE_VEHICULO.3, para replicar el proces.
#a estos le asigne el vehiculo mas grande (en vehicle.1 o vehicle.2) que no fuera la moto
# Si aparece algun unknown, lo dejo asi


# 


View(table(victims2$strike_mode))

View(head(victims2,100))
unique(victims2$victim_vehicle)



##################################################
# Segundo intento a mano
A los que tienen Condicion = Ciclista se les asignó cas_mode = bicycle
A los que tienen condicion = ciclista, Clase_Accidente = Caida ocupante, se les asigno strike_mode = nov
A los que tienen condicion = ciclista, Clase_Accidente = Volcamiento, se les asignó strike_mode = nov
A los que tienen condicion = ciclista, Clase_Accidente = Atropello, se les asignó strike_mode = nov xq no tienen info de otros vehiculos
A los que tienen condicion = ciclista, Clase_Accidente = choque, y:
  vehicle.1 = vacio, se les asigno strike_mode = nov
  vehicle.1 no es vacio, se les asigno a strike_mode el modo mas grande en tamaño
  



##############################################################
View(head(victims2))
names(vehicles3)
table(victims2$Gravedad_victima)
table(victims2$)
## Translating vehicles involved to english
unique(vehicles$CLASE_VEHICULO) # E
write.table(smodes$original, "clipboard", sep = "\t")
vehicles <- vehicles %>% 
  mutate(
    vehicle = case_when(
      CLASE_VEHICULO %in% c("MOTOCARRO", "CUATRIMOTO", "MOTOTRICICLO") ~ "3wheeled",
      CLASE_VEHICULO %in% c("BICICLETA", "CUADRICICLO") ~ "bicycle",
      CLASE_VEHICULO %in% c("BUSETA", "BUS", "BUS ARTICULADO") ~ "bus",
      CLASE_VEHICULO %in% c("AUTOMOVIL", "CAMIONETA", "CAMPERO") ~ "car",
      CLASE_VEHICULO %in% c("TRACTOCAMION", "VOLQUETA", "CAMION D.TROQUE",
                            "TRACTO-CAMION REMOLQUE", "CAMION CARGA EXTENSA",
                            "SEMIREMOLQUE") ~ "heavy transport",
      CLASE_VEHICULO %in% c("MICRO BUS", "MINI BUS") ~ "minibus",
      CLASE_VEHICULO %in% c("MOTOCICLETA", "MOTONETA", "MOTOCICLO", "CICLOMOTO",
                            "CICLOMOTOR") ~ "motorcycle",
      CLASE_VEHICULO %in% c("MAQ. INDUSTRIAL", "ABONADORA", "MONTACARGA",
                            "TRACCION ANIMAL", "CARRETILLA ELEVADORA", 
                            "GRUA", "RETROEXCAVADORA", "MAQ. AGRICOLA", 
                            "BULDOZER", "REMOLQUE", "MINICARGADOR", "CARGADOR", 
                            "APLANADORA", "COSECHADORA", "ESCAVADORA", 
                            "PAVIMENTADORA", "AUTOHORMIGONERA", "MAQUINA BARREDORA", 
                            "VOLQUETA D.TROQUE", "FRESADORA", "COMPACTADORA", 
                            "EXCAVADORA") ~ "other",
      CLASE_VEHICULO %in% c("TRICICLO") ~ "rickshaw",
      CLASE_VEHICULO %in% c("TRACTOR") ~ "tract",
      CLASE_VEHICULO %in% c("CAMION", "AMBULANCIA") ~ "truck",
      CLASE_VEHICULO %in% c("DESCONOCIDA", "DESCONOCIDO", NA) ~ "unknown"
    )
  )

# write.table(table(vehicles$CLASE_VEHICULO, vehicles$vehicle, useNA = "always"), "clipboard", sep = "\t")

# Recode cas mode to standard mode
whw_lng$cas_mode <- smodes$inj_vic_lng[match(tolower(whw_lng$cas_mode), smodes$original)]

# Recode strike mode to standard mode
whw_lng$strike_mode <- smodes$inj_str_lng[match(tolower(whw_lng$strike_mode), smodes$original)]  
# Renaming columns to remove spaces
names(victims)
names(victims) <- c("Radicado", "Gravedad_victima", "Id", "Fecha_Accidente",
                    "Hora_Accidente", "Clase_Accidente", "Direccion_Accidente",
                    "Sexo", "Edad", "Condicion", "Vehiculo_victima")

# Filtering only fatalities
victims2 <- subset(victims, Gravedad_victima == "Muertos")



##' *Cali - Colombia*
rm(list = ls())
# Read lookup table for standardized modes 
smodes <- read_csv('data/global/modes/standardized_modes.csv') %>% 
  separate_rows(original, sep = ';') %>% 
  mutate(across(where(is.character), str_trim))
#smodes$original


