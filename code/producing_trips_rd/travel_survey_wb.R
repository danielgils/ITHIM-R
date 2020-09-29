# This script is similar to travel_survey but it is only for cities made by Daniel in the World Bank
# Last update: September 2020


####Medellin - Colombia (WB) ####
rm(list =ls())
source("code/producing_trips_rd/used_functions.R")
options(scipen = 50)

# Import datasets
trips <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/EOD2017/EOD_2017_DatosViajes.xlsx", sheet = "DATOS VIAJES") 
trips <- trips[rowSums(is.na(trips)) != ncol(trips), ] # Removing files that only have NAs
# When reading person there are some warnings related to people without age
person <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/EOD2017/EOD_2017_DatosViajes.xlsx", sheet = "DATOS MORADORES")
#hogar <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/EOD2017/EOD_2017_DatosViajes.xlsx", sheet = "DATOS HOGARES")

# Import hierarchy to decide main mode in each trip
main_mode <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/EOD2017/Jerarquia.xlsx", sheet = "Medellin")

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
         mode_e1 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E1, main_mode$Modo_transporte_formulario_EODH)],
         mode_e2 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E2, main_mode$Modo_transporte_formulario_EODH)],
         mode_e3 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E3, main_mode$Modo_transporte_formulario_EODH)],
         mode_e4 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E4, main_mode$Modo_transporte_formulario_EODH)],
         mode_e5 = main_mode$Jerarquia_modo_principal[
           match(DEC_MODO_TTE_E5, main_mode$Modo_transporte_formulario_EODH)],
         mode_e6 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E6, main_mode$Modo_transporte_formulario_EODH)],
         mode_e7 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E7, main_mode$Modo_transporte_formulario_EODH)]
  ) %>%
  rowwise() %>% mutate(
    main_modes = min(mode_e1, mode_e2, mode_e3, mode_e4, mode_e5, mode_e6,
                     mode_e7, na.rm = T),
    trip_mode = main_mode$ITHIM[
      match(main_modes, main_mode$Jerarquia_modo_principal)]
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

rd <- trip %>% select(participant_id, age, sex, trip_id, trip_mode, trip_duration)

# Export
write_csv(rd, 'inst/extdata/local/medellin_wb/trips_medellin_wb.csv')
write_csv(rd, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/Cleaned/trips_medellin_wb.csv')
write_csv(rd, 'data/local/medellin_wb/trips_medellin_wb.csv')


####Cali - Colombia (WB) ####
rm(list =ls())
options(scipen = 50)

# Import datasets
trips <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/EOD2017/EOD_2017_DatosViajes.xlsx", sheet = "DATOS VIAJES") 
trips <- trips[rowSums(is.na(trips)) != ncol(trips), ] # Removing files that only have NAs
# When reading person there are some warnings related to people without age
person <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/EOD2017/EOD_2017_DatosViajes.xlsx", sheet = "DATOS MORADORES")

# Import hierarchy to decide main mode in each trip
main_mode <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/EOD2017/Jerarquia.xlsx", sheet = "Medellin")

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
         mode_e1 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E1, main_mode$Modo_transporte_formulario_EODH)],
         mode_e2 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E2, main_mode$Modo_transporte_formulario_EODH)],
         mode_e3 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E3, main_mode$Modo_transporte_formulario_EODH)],
         mode_e4 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E4, main_mode$Modo_transporte_formulario_EODH)],
         mode_e5 = main_mode$Jerarquia_modo_principal[
           match(DEC_MODO_TTE_E5, main_mode$Modo_transporte_formulario_EODH)],
         mode_e6 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E6, main_mode$Modo_transporte_formulario_EODH)],
         mode_e7 = main_mode$Jerarquia_modo_principal[
           match(DESC_MODO_TTE_E7, main_mode$Modo_transporte_formulario_EODH)]
  ) %>%
  rowwise() %>% mutate(
    main_modes = min(mode_e1, mode_e2, mode_e3, mode_e4, mode_e5, mode_e6,
                     mode_e7, na.rm = T),
    trip_mode = main_mode$ITHIM[
      match(main_modes, main_mode$Jerarquia_modo_principal)]
  )

# Merge sex and age variables
trips3 <- merge(trips2, person[,c("participant_id", "sex", "age")], 
                by = "participant_id", all.x = T) 

# Remove dataframes I don't need anymore
rm(person, trips, trips2)

write_csv(trips3, 'data/local/medellin_wb/medellin_wb_trip.csv')

trip <- read_csv('data/local/medellin_wb/medellin_wb_trip.csv')
source("code/producing_trips_rd/used_functions.R")

# Standardized travel modes
trip <- standardize_modes(trip, mode = c('trip'))

rd <- trip %>% select(participant_id, age, sex, trip_id, trip_mode, trip_duration)

# Reorder and select columns
write_csv(rd, 'inst/extdata/local/medellin_wb/trips_medellin_wb.csv')
