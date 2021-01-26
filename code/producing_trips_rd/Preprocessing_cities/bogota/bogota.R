#' ---
#' title: "Preprocessing of Bogota's travel dataset"
#' author: "Daniel"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---

#' # **Understanding phase**
#+ warning=FALSE, message=FALSE, echo=FALSE
# Loading libraries
library(foreign) 
#library(RODBC)
library(kableExtra)
library(readxl)
library(tidyverse)


# Cleaning workspace
rm(list = ls())

# Printing options
options(scipen = 50)

#' ## Documentation
#' Documentation is located in ".../Colombia/Bogota/Encuesta de Movilidad 2019/".
#' These files were sent by the team from Mobility Secretariat of Bogota, but
#' can also be found here: https://www.simur.gov.co/portal-simur/datos-del-sector/encuestas-de-movilidad/.
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1", "File2", "File3", "File4"),
  Description = c("Technical report and final results",
                  "Report with only final results",
                  "Pictures of trip modes",
                  "Questionnaire - Trip section"),
  Title = c("Caracterizacion de la movilidad - Encuesta de Movilidad de Bogota 2019",
            "Resultados de la encuesta de movilidad de Bogota y municipios vecinos",
            "Tarjeta 4 Medios de Transporte",
            "Formulario de viaje"),
  File = c("Informes de Indicadores/200109_Etapa_V_v3_Caracterización de la Movilidad.pdf",
           "Informes de Indicadores/Anexo H - Cartilla digital_Resulados de la Encuesta de Movilidad 2019.pdf",
           "Formularios/190219_Tarjetas 4-6_DPR_.pdf",
           "Formularios/190220_Módulo D_DPR (viajes).pdf")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip
#' 1. *Trip:* All trips that are longer than 3 minutes for all modes except
#' walking, where trips should be longer or equal to 15 minutes. 
#' Definition of trip in page 120 of **File2**: *Move from one part to another made by one person with a specific reason/motive, A definite hour of start and end, a mode of transport, and a duration greater than 3 minutes. Or a move from one part to another with reason/motive work or study of any duration.*
#' 
#' 2. *Collection:* Trips collected in this survey correspond to those made the 
#' day of reference, i.e., the day before the survey. **Results presented are** 
#' **for trips made in a single day.**
#' 
#' 
#' ## Replicate main results from raw datasets
#' To create this report I have to set the full route of each file, regardless
#' the location of the working directory.
#'
#' Loading standardize_modes function:
#+ warning=FALSE, message=FALSE
external_route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/code/producing_trips_rd/"
#external_route <- "CAMBRIDGE_ROUTE V DRIVE"
#source(paste0(external_route, "used_functions.R")) 

#' It didn't work with knitr so I had to paste this function here. 
#' 
#' **Note: Before running this script, make sure this function is up to date**
standardize_modes <- function(trip, mode){
  # Read lookup table
  smodes <- read_csv('C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/global/modes/standardized_modes.csv')
  # Separate rows 
  smodes <- smodes %>% separate_rows(original, sep = ';')
  
  smodes <- smodes %>% 
    mutate(across(where(is.character), str_trim))
  
  if (length(mode) == 1) {
    if (mode == 'stage')
      trip$stage_mode <- smodes$exhaustive_list[match(trip$stage_mode, smodes$original)]
    else
      trip$trip_mode <- smodes$exhaustive_list[match(trip$trip_mode, smodes$original)]
  }else if (length(mode) == 2) {
    if (all(mode %in% c('stage', 'trip'))) {
      trip$trip_mode <- smodes$exhaustive_list[match(trip$trip_mode, smodes$original)]
      trip$stage_mode <- smodes$exhaustive_list[match(trip$stage_mode, smodes$original)]
    }
  }
  
  return(trip)
  
}

#' ### Importing datasets
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Bogota/Encuesta de Movilidad 2019/BD EODH2019 FINAL v14022020/Archivos XLSX/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# Households (hh)
hh <- read_excel(paste0(route, "HogaresEODH2019.xlsx")) 
# People
people <- read_excel(paste0(route, "PersonasEODH2019.xlsx"))
# Vehicles
#vehicles <- read_excel(paste0(route, "VehículosEODH2019.xlsx"))
# Trips
trips <- read_excel(paste0(route, "ViajesEODH2019.xlsx"))

#+ warning=FALSE, message=FALSE, cache=TRUE
# Stages: there's information about walk_to_pt stages
stages <- read_excel(paste0(route, "EtapasEODH2019.xlsx"), col_types = "text")


#' ### Number of rows
#' The first thing to do is verify that the number of rows of each dataset is 
#' the same to what is mentioned in page 201 (Tabla 6.1) of **File1**.
nrow(hh)
nrow(people)
nrow(trips)
nrow(stages)

#' In all of them is the same as reported, then we can continue.
#' 
#' ### Number of surveys per utam
#' As before, I'll verify that the number of surveys per UTAM is the same to
#' what is mentioned in page 20 (Tabla 2.1) of **File1**. The SQL code for this
#' output is in page 214 (Paragraph 6.26).
#View(hogar %>% group_by(Utam) %>% summarise(n()))
#print(hh %>% group_by(Utam) %>% summarise(n()), n = 50) 
hh %>% group_by(Utam) %>% summarise(n()) %>% 
  kbl() %>% kable_classic(full_width = F)
#' Results are the same.
#' 
#' ### Average number of people per household by socio-economic strata (using weights)
#' Compare this with what is mentioned in page 32 (Figura 3.4) of **File1**. 
#' The SQL code for this output is in page 215 (Paragraph 6.32).
# First compute number of people per household
people_per_hh <- hh %>% 
  inner_join(people, by = c("Id_Hogar" = "id_hogar")) %>% 
  group_by(Id_Hogar) %>% summarise(cantidad = n())

# Then compute the average by strata
hh %>% inner_join(people_per_hh, by = "Id_Hogar") %>% 
  filter(municipio == "11001") %>%
  rowwise() %>% 
  mutate(sumoff_exp_h = cantidad*Factor) %>% 
  group_by(p5_estrato) %>% 
  summarise(sum(sumoff_exp_h)/sum(Factor)) %>% 
  kbl() %>% kable_classic(full_width = F)

#' Results are the same.
#' 
#' ### Number of trips per municipality in the whole dataset
#' When talking about trips, the results presented in the report correspond to
#' those that are longer than 3 minutes for all modes except walking, where
#' trips should be longer or equal to 15 minutes.
#' 
#' To get the duration of each trip I used a file that has this already computed,
#' as well, as the main mode. The computation of the duration is explained in
#' page 202 (paragraphs 6.44 to 6.47). Now, regarding the main mode, in the
#' documentation says that **they defined a hierarchy between modes to define ** 
#' **the main mode of each trip**. This hierarchy is in page 126, table 4.4 of
#' **File1**.

# Reading file with trip duration and main mode
duration <- read_excel(paste0(route, "Aux_DuraciónEODH2019.xlsx"))

# Filtering out walking trips with duration shorter than 15 minutes.
duration_longer_15 <- duration %>% 
  filter(modo_principal != "A pie" | 
           (modo_principal == "A pie" & duracion >= 15))

# Computing number of trips per municipality
n_trips_larger_15 <- duration_longer_15 %>% 
    inner_join(hh[,c("municipio", "Id_Hogar")], 
               by = c("id_hogar" = "Id_Hogar")) %>% 
    #filter(p4_edad <= 103) %>% 
    group_by(municipio) %>% 
    summarise(suma = sum(f_exp)) %>% 
    arrange(desc(suma)) %>% 
  add_row(municipio = 99999, suma = sum(duration$f_exp))

n_trips_larger_15 %>% kbl() %>% kable_classic(full_width = F)

#' Comparing this table to *Tabla 4.2* (page 71 of **File1**), all municipalities
#' but Bogota have the same number of trips. I tried several filters to see 
#' whether there is something that I'm missing in Bogota, like trying to come up
#' with an upper bound of trip duration. At the end I got that using a limit of 
#' 540 minutes leads to a difference of just 1 trip in the total. The problem is
#' that the number of trips in other municipalities changes, which is not right.
#' 
#' Next, I'm comparing the total number of trips without the 15 minutes filter to
#' see if the reason for this inconvenience is the filter.
n_trips <- duration %>% 
  inner_join(hh[,c("municipio", "Id_Hogar")], 
             by = c("id_hogar" = "Id_Hogar")) %>% 
  group_by(municipio) %>% 
  summarise(suma = sum(f_exp)) %>% 
  arrange(desc(suma)) %>% 
  add_row(municipio = 99999, suma = sum(duration$f_exp))

n_trips %>% kbl() %>% kable_classic(full_width = F)

#' These values are exactly the same as mentioned in paragraph 4.5 for Bogota and
#' the grand total, so there must be an error in the filter (which I doubt) or in
#' the sampling weights.
#' 
#' Since I couldn't figure out a way to replicate Bogota's results, I decided to 
#' continue using only this filter of walking trips longer than 15 minutes.
#' 
#' ### Mode share in all municipalities (longer than 15 minutes)
#' Compare this with what is mentioned in page 113 (Figura 4.40) of **File1**.
#' Here trips should be larger than 15 minutes for walking.
#' The SQL code for this output is in page 203 (Paragraph 6.47).

# Compute absolute frequency
mode_share_longer_15 <- duration_longer_15 %>% group_by(modo_principal) %>% 
  summarise(suma = sum(f_exp)) 

# Compute proportion
mode_share_longer_15$proportion <- mode_share_longer_15$suma / 
  sum(duration_longer_15$f_exp) * 100

mode_share_longer_15 %>% arrange(desc(proportion)) %>% 
  kbl() %>% kable_classic(full_width = F)
#' Results are similar but not the same.
#' 
#' ### Mode share in all municipalities (all trips)
mode_share <- duration %>% group_by(modo_principal) %>% 
  summarise(suma = sum(f_exp)) 

# Compute proportion
mode_share$proportion <- mode_share$suma / 
  sum(duration$f_exp) * 100
mode_share %>% arrange(desc(proportion)) %>% 
  kbl() %>% kable_classic(full_width = F)
#' Results are the same.
#' 
#'  **To Do**: Check in other cities the definition of a trip. Here I can use
#' the dataset filtering walking trips longer than 15 minutes or use the complete
#' dataset ignoring this filter.
#' 
#'
#' # **Preprocessing phase**
#' ## Filtering Bogota trips only
#' Since the survey was conducted in 29 municipalities and we are only interested
#' in Bogota, then these trips are the only ones used. 
#' **Note**: I am using here all trips regardless of walking trips shorter than 
#' 15 minutes. 
#' 
#' **To Do**: If we choose to work with only walking trips longer than 15 
#' minutes, then I'll have to run this again
duration_bogota <- duration %>% 
  inner_join(hh[,c("municipio", "Id_Hogar")], 
             by = c("id_hogar" = "Id_Hogar")) %>% 
  filter(municipio == "11001")

stages_bogota <- stages %>% 
  inner_join(hh[,c("municipio", "Id_Hogar")], 
             by = c("id_hogar" = "Id_Hogar")) %>% 
  filter(municipio == "11001")

trips_bogota <- trips %>% 
  inner_join(hh[,c("municipio", "Id_Hogar")], 
             by = c("id_hogar" = "Id_Hogar")) %>% 
  filter(municipio == "11001")

#' ## Classification and translation of trip modes
#' As it was mentioned before, there is already a table with all trip modes
#' collected in the survey in **File1** (page 111, Tabla 4.4). So I copied it 
#' and pasted it in an excel file to make the classification and translation. 
#' This is the result:
main_mode <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Hierarchy.xlsx", sheet = "Bogota")
main_mode %>% kbl() %>% kable_classic()

#' The first two columns of this table have been defined in the documentation,
#' so I created the new classification (in Spanish) and its equivalence to Ithim,
#' taking into account travel modes defined in standardized_modes file (.../ITHIM-R/data/global/modes/standardized_modes.csv)
#' 
#' **Note**: Definition and pictures of modes are in **File3**.
#'
#' ## Information at stage or trip level?
#' There is information at stage level although it seems that is not enough 
#' because of the duration of each stage. 
#' To understand how the information was collected at stage level, I selected a
#' person at random and followed the questionnaire (see **File4**) while checking
#' the information in the datasets. The person selected has ID_Hogar = 1117 and 
#' ID_persona = 2.
#' 
#' In *trips* and *duration* datasets there's information about the start and end
#' hour of the trip, and trip_mode (following the hierarchy mentioned). In 
#' *duration* datasets there's information about stage_mode and minutes walking
#' to the stage_mode, not the duration in the stage_mode. For instance, the first
#' trip made by this person says:
trips_bogota %>% filter(id_hogar == "1117", id_persona == "2", 
                        id_viaje == "1") %>% 
  select(id_hogar, id_persona, id_viaje, hora_inicio_viaje, p30_camino_minutos,
         p31_hora_llegada) %>% 
  mutate(hora_inicio_viaje = format(ISOdatetime(1900,1,1,0,0,0, tz = "GMT") + 
                                      as.difftime(hora_inicio_viaje*24, 
                                                  unit = "hours"), "%H:%M"),
         p31_hora_llegada = format(ISOdatetime(1900,1,1,0,0,0, tz = "GMT") + 
                                      as.difftime(p31_hora_llegada*24, 
                                                  unit = "hours"), "%H:%M"),) %>%
  kbl() %>% kable_classic()

stages_bogota %>% filter(id_hogar == "1117", id_persona == "2", 
                        id_viaje == "1") %>% 
  select(id_hogar, id_persona, id_viaje, id_etapa, p18_id_medio_transporte,
         p19_camino_minutos, p21_tiempo_arrancar_vehic) %>% 
  kbl() %>% kable_classic()
  
#' From both datasets I can see that the person started her trip at 6:30, 
#' then she had to walk for 5 minutes, to take the Bus 
#' (p18_id_medio_transporte = 4) where she had to wait 20 min. for it. Then she 
#' walked 5 minutes to take the Transmilenio where she had to wait 12 min. for it
#' , when she arrived she had to walk 15 minutes to get to her destination, at 
#' 7:30. 
#' 
#' It's clear that the number of minutes in Bus and Transmilenio is not there, 
#' even though there's an estimation of time waiting for the bus to come. I did
#' try to assume that variable *p21_tiempo_arrancar_vehic* could be the time
#' spent in the mode, so the total number of minutes was 5+20+5+21+15 = 63
#' minutes (close to computed duration). However, the following example 
#' contradicts this assumption.
#' 
#' In this example, the person selected has ID_Hogar = 12801 and ID_persona = 2.
trips_bogota %>% filter(id_hogar == "12801", id_persona == "2", 
                        id_viaje == "1") %>% 
  select(id_hogar, id_persona, id_viaje, hora_inicio_viaje, p30_camino_minutos,
         p31_hora_llegada) %>% 
  mutate(hora_inicio_viaje = format(ISOdatetime(1900,1,1,0,0,0, tz = "GMT") + 
                                      as.difftime(hora_inicio_viaje*24, 
                                                  unit = "hours"), "%H:%M"),
         p31_hora_llegada = format(ISOdatetime(1900,1,1,0,0,0, tz = "GMT") + 
                                     as.difftime(p31_hora_llegada*24, 
                                                 unit = "hours"), "%H:%M"),) %>%
  kbl() %>% kable_classic()

stages_bogota %>% filter(id_hogar == "12801", id_persona == "2", 
                         id_viaje == "1") %>% 
  select(id_hogar, id_persona, id_viaje, id_etapa, p18_id_medio_transporte,
         p19_camino_minutos, p21_tiempo_arrancar_vehic) %>% 
  kbl() %>% kable_classic()

#' In this case, the person started her trip at 11:00, then she had to walk for 
#' 5 minutes,to take the Transmilenio, where she had to wait 5 minutes for it.
#' Then she took another Transmilenio, waiting 8 minutes for it. Finally she had
#' to walk 8 minutes to get to her destination, at 13:00. 
#' 
#' The total number of minutes here is 5+5+8+8=26 minutes and the time spent in
#' the trip was of 120 minutes. Hence is not possible to assign a specific
#' duration to each stage.
#' 
#' **For this reason I conclude that even though there's information at stage**
#' **level, it's not enough to get the duration for each stage. Therefore,**
#' **I will continue working at trip level**.
#' 
#' **ToDo: since the proportion of trips with more than one stage is relatively**
#' **small, we could compute the difference between the trip duration and the**
#' **time spent walking to the PT, and then split this duration equally in the**
#' **remaining modes. In this way, we used all information provided for trips**
#' **with only one stage and have a rough estimate of duration for trips with**
#' **more than one stage.**
#' 
#' ## Row for each trip, translate trip_mode and create duration, sex and age
#' Since trips and duration datasets have the same information, I take duration
#' as a reference because it already has computed trip duration. But first I have
#' to make sure that the number of trips in both datasets is exactly the same.
trips_bogota <- trips_bogota %>% 
  mutate(trip_id_paste = paste(id_hogar, id_persona, id_viaje, sep = "-"))
duration_bogota <- duration_bogota %>% 
  mutate(trip_id_paste = paste(id_hogar, id_persona, id_viaje, sep = "-"))

#' The number of unique trip IDs is the same in both datasets and comparing these
#' trips I got a sum of 106.403, which corresponds to the number of rows in trips
#' dataset. Therefore, I can conclude that both datasets have the same trips.
length(unique(trips_bogota$trip_id_paste)) == length(unique(duration_bogota$trip_id_paste))

sum(sort(unique(trips_bogota$trip_id_paste)) == sort(unique(duration_bogota$trip_id_paste)))

#' Now, creating the variables I need:
duration_bogota <- duration_bogota %>% 
  left_join(people[, c("id_hogar", "id_persona", "p4_edad", "Sexo")], 
            by = c("id_hogar", "id_persona")) %>% 
  mutate(participant_id = paste0(id_hogar, id_persona),
         trip_id = paste0(id_hogar, id_persona, id_viaje),
         age = p4_edad,
         sex = ifelse(Sexo == "Hombre", "male", "female"),
         trip_duration = duracion,
         trip_mode = main_mode$ITHIM[
           match(modo_principal, main_mode$TripMode)]
  )

#' I verify that every trip has the sex and age of the person who did it. Since
#' the sum of NAs is zero, then I can conclude that every trip has the
#' information.
sum(is.na(duration_bogota$sex))
sum(is.na(duration_bogota$age))
sum(is.na(duration_bogota$trip_duration))
sum(is.na(duration_bogota$trip_mode))

#' ## Create variables for quick report
#' **ToDo: see what variables are needed for quality check function**
#' 
#' ## Standardize trip modes
#' There's already a function that standardize these modes so the package can use
#' these trips. I made sure to use translate trip modes so that the function
#' works perfectly (take a look at the *original* variable of *smodes* dataframe
#' in this function).

trips_export <- standardize_modes(duration_bogota, mode = c('trip'))
unique(duration_bogota$trip_mode)
unique(trips_export$trip_mode)
#' 
#' *standardize_modes* function converts bicycle to cycle, train to rail, and 
#' rickshaw to auto_rickshaw.
#'
#'
#' # **Exporting phase**
#' ## Variables to export
#' Now I filter the columns I need
trips_export <- trips_export %>% 
  select(participant_id, age, sex, trip_id, trip_mode, trip_duration)

#' ## Export dataset
#' I'm exporting this dataset to three different locations:
#' 
#' 1. In .../inst/exdata folder so the dataset is installed with the package
#' 2. In Data/Colombia/Bogota/Cleaned, to have a copy 
#' 3. in data/local/bogota_wb to export the dataset with the variables for the
#' quick report. 
#' In this case I called it bogota_wb2, because Lady made the preprocessing of
#' this dataset before, and I don't want to lose it. Lady's datset are bogota_Wb.
write_csv(trips_export, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/bogota_wb2/trips_bogota_wb2.csv')
write_csv(trips_export, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Bogota/Cleaned/trips_bogota_wb2.csv')
write_csv(trips_export, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/bogota_wb2/trips_bogota_wb2.csv')


