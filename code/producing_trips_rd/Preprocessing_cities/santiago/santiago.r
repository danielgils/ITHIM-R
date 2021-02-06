#' ---
#' title: "Preprocessing of Santiago's travel dataset"
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


#+ warning=FALSE, message=FALSE, echo=FALSE
# Cleaning workspace
rm(list = ls());gc()

# Printing options
options(scipen = 50)

#' ## Documentation
#' Documentation is located in ".../Chile/Santiago/Trips/Reports".
#' These files were found in: http://www.sectra.gob.cl/encuestas_movilidad/encuestas_movilidad.htm
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1", "File2"),
  Description = c("Technical report and final results",
                  "Report with only final results"),
  Title = c("Encuesta Origen Destino Santiago 2021 Informe Final Volumen II",
            "Informe ejecutivo Origen Destino de Viajes 2012 "),
  File = c("Actualizacion_recolecc_STU_Santiago_IX Etapa_EOD Stgo 2012_Inf_Final vol 2.pdf",
           "Actualizacion_recolecc_STU_Santiago_IX Etapa_EOD Stgo 2012_Inf_Ejec.pdf")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip (pendiente)
#' 1. *Trip:* All trips that are longer than 3 minutes for all modes except
#' walking, where trips should be longer or equal to 15 minutes. 
#' Definition of trip in page 120 of **File2**: *Movement from one part to another made by one person with a specific reason/motive, A definite hour of start and end, a mode of transport, and a duration greater than 3 minutes. Or a movement from one part to another with reason/motive work or study of any duration.*
#' 
#' 2. *Collection:* Trips collected in this survey correspond to those made the 
#' during the week (Dia Laboral Normal) and also the Saturday, Sunday and
#' Holidays.
#' **Results presented are separately for weekdays and weekends**
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
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Chile/Santiago/Trips/CSV/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# Households (hh)
hh <- read_excel(paste0(route, "Hogar.xlsx")) 

#+ warning=FALSE, message=FALSE, cache=TRUE
# People
people <- read_excel(paste0(route, "Persona.xlsx"))
#' It looks like the age of each person is in a different dataset
people_age <- read_excel(paste0(route, "EdadPersonas.xlsx"))
people <- people %>% inner_join(people_age, by = "Persona")

# Vehicles
#vehicles <- read_excel(paste0(route, "Vehiculo.xlsx"))

#+ warning=FALSE, message=FALSE, cache=TRUE
# Trips
trips <- read_excel(paste0(route, "Viaje.xlsx"))
#' It looks like the classification used to get the results from the document, c
#' comes from another dataset.
trips_mode <- read_excel(paste0(route, "ViajesDifusion.xlsx"))
trips <- trips %>% inner_join(trips_mode, by = "Viaje")

#+ warning=FALSE, message=FALSE, cache=TRUE
# Stages: there's information about walk_to_pt stages
stages <- read_excel(paste0(route, "Etapa.xlsx"))


#' ### Number households and people
#' The first thing to do is verify that the number of hh and people is 
#' the same to what is mentioned in page 55 (Tabla 17) of **File1**.
sum(hh$Factor)
sum(people$Factor)

#' Results are the same.
#' 
#' ### Number of people per household
#' Compare this with what is mentioned in page 55 (Tabla 17) of **File1**. 
people_per_hh <- hh %>% 
inner_join(people, by = c("Hogar" = "Hogar")) %>% 
  group_by(Hogar) %>% summarise(cantidad = n())

# Then compute the average 
hh %>% inner_join(people_per_hh, by = "Hogar") %>% 
  rowwise() %>% 
  mutate(sumoff_exp_h = cantidad*Factor,
         cluster_id = 1) %>% 
  group_by(cluster_id) %>% 
  summarise(sum(sumoff_exp_h)/sum(Factor)) %>% 
  kbl() %>% kable_classic(full_width = F)

#' Results is the same.
#' 
#' ### Number of people by education
#' Compare this with what is mentioned in page 56 (Tabla 19) of **File1**. 
people %>% 
  group_by(Estudios) %>% summarise(Total = sum(Factor))

#' Results are the same.
#' 
#' ### Number of hh by sector
#' Compare this with what is mentioned in page 60 (Tabla 22) of **File1**. 
data.frame(household = hh %>% 
             group_by(Sector) %>% summarise(Total = sum(Factor)),
           n_people =  people %>% 
             left_join(hh[, c("Hogar", "Sector")], by = "Hogar") %>% 
             group_by(Sector)  %>% summarise(Total = sum(Factor))
)

#' Results are the same.
#' 
#' ### Mode share
#' Compare this with what is mentioned in page 76 (Tabla 34) of **File1**. 

# trips %>% group_by(ModoDifusion) %>% 
#   summarise(Total = sum(FactorLaboralNormal, na.rm = T))
# 
# asdf <- trips %>% left_join(people, by = c("Hogar", "Persona")) %>% 
#   mutate(new_factor = Factor * FactorLaboralNormal,
#          new_factor2 = Factor * Factor_LaboralNormal, 
#          aux = 1) %>% group_by(aux) %>% 
#   summarise(Total = sum(new_factor, na.rm = T),
#             Total2 = sum(new_factor2, na.rm = T))
# 
#          names(asdf)
# asdf <- people %>% left_join(trips, by = c("Hogar", "Persona")) %>% 
#   mutate(new_factor = Factor * FactorLaboralNormal,
#          aux = 1) %>% group_by(aux) %>% 
#   summarise(Total = sum(new_factor, na.rm = T))
#   group_by()
# names(asdf)
# names(people)
# sum(is.na(people$Factor))


#' # **Preprocessing phase**
#' ## Filtering people from Santiago only
#' Since the survey was conducted in 45 comunas (page 3 of **File2**) and I don't
#' know yet whether we have injuries information then I will analyze all of 
#' them because there's no a single variable that determines which comunas are
#' from Santiago 
# 
# people_santiago <- people %>% 
#   inner_join(hh[,c("Hogar", "Sector")], 
#              by = "ORDEN") %>% 
#   filter(MUNICIPIO == "CALI")

#' I verify that there are no duplicates in people dataset
people <- people %>% 
  mutate(participant_id_paste = paste(Hogar, Persona, sep = "-"))
length(unique(people$participant_id_paste)) == nrow(people)

names(people)

#' ## Classification and translation of trip modes
#' In the trip dataset there's already a classification of trip modes. It is
#' important to note that there are different variables that define trip mode, 
#' but none of them seems to be useful for the package, because it combines
#' modes such as motorcycle and metro in a single category. For this reason, I 
#' decided to use the classification used in stages dataset.To create the 
#' following table I used the information I found in the data dictionary
#' and then defined a hierarchy to be applied, finally I translated it. 
#' This is the result:
main_mode <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Chile/Hierarchy.xlsx", sheet = "Santiago")
main_mode %>% kbl() %>% kable_classic()

#' The first two columns of this table have all modes found in stages dataset.
#' The third column has a classification where some of the modes are grouped into
#' bigger categories (I created it in Spanish). Last column has its equivalence
#' so it can be used in the package taking into account travel modes defined in
#' standardized_modes file (.../ITHIM-R/data/global/modes/standardized_modes.csv)
#' 
#' Now with respect to trip purpose, there are two different classifications.
#' I decided to use *PropositoAgregado" because it has the categories we need.
#' This is the result:
purpose <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Chile/Hierarchy.xlsx", sheet = "Santiago")
purpose %>% kbl() %>% kable_classic()

#' The first two columns have been taken from the dataset and data dictionary,
#' and the third column is the translation and classification of these motives. 
#' 
#' **ToDo: add in hierarchy file the classification that Lambed used**
#' 
#' #' ## Information at stage or trip level?
#' There is information at stage level although it seems that is not enough 
#' because of the duration of each stage. There is information about minutes
#' walking before and after taking other mode, but there's no information about
#' the time spent in each stage. 
#' 
#' **For this reason I conclude that even though there's information at stage**
#' **level, it's not enough to get the duration for each stage. Therefore,**
#' **I will continue working at trip level, using trip modes defined in the**
#' **imputed dataset.**
#' 
#' **ToDo: since the proportion of trips with more than one stage is relatively**
#' **small, we could compute the difference between the trip duration and the**
#' **time spent walking to the PT, and then split this duration equally in the**
#' **remaining modes. In this way, we used all information provided for trips**
#' **with only one stage and have a rough estimate of duration for trips with**
#' **more than one stage.**
#' 
#' ## Row for each trip, translate trip_mode and create duration, sex and age
#' Trip dataset already has a row for each trip, so I have to create the 
#' variables I need. 
#' 
#' To create trip_mode I have to extract first the modes used from variable
#' *MediosUsados*.
# asdf <- map(trips$MediosUsados, function(x){
#   modes <- unlist(str_split(x, ";"))
#   hierarchy <- unlist(map(modes, function(y){
#     return(main_mode$Hierarchy[match(y, main_mode$Code)])
#   }))
#   minimum = min(hierarchy)
#   return(main_mode$ITHIM[match(minimum, main_mode$Hierarchy)])
# })

trips_v2 <- trips %>% 
  mutate(trip_id = Hogar,
         trip_duration = TiempoViaje,
         #' To create trip_mode I have to extract first the modes used from
         #' variable *MediosUsados*.
         trip_mode = unlist(map(trips$MediosUsados, function(x){
           modes <- unlist(str_split(x, ";"))
           hierarchy <- unlist(map(modes, function(y){
             return(main_mode$Hierarchy[match(y, main_mode$Code)])
           }))
           minimum = min(hierarchy)
           return(main_mode$ITHIM[match(minimum, main_mode$Hierarchy)])
         })),
         # As I said before, I used PropositoAgregado because it has a
         # classification we can use
         trip_purpose = ifelse(is.na(PropositoAgregado), "other",
                           purpose$ITHIM[match(PropositoAgregado, purpose$Code)]),
         trip_id_paste = paste(Hogar, Persona, Viaje, sep = "-"))

#' I verify that there are no duplicates in trip dataset
length(unique(trips_v2$trip_id_paste)) == nrow(trips_v2)

#' **ToDo: Try to implement time duration following the protocol from Bogota's**
#' **documentation, because it takes into account differences between days.**
#' **This file is "Caracterización de la movilidad – Encuesta de Movilidad de Bogotá 2019", page 202, paragraph 6.44**
#' 
#' 
#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
report <- people %>% 
  left_join(trips_v2, by = c("Hogar", "Persona")) %>% 
  mutate(cluster_id = 1,
         household_id = Hogar,
         participant_id = Persona,
         age = Edad,
         sex = ifelse(Sexo == 1, "Male", "Female"),
         participant_wt = Factor,
         meta_data = NA) %>% 
  select(cluster_id, household_id, participant_id, sex, age, participant_wt,
         trip_id, trip_mode, trip_duration, trip_purpose) 

report$meta_data[1] <- 7164400
report$meta_data[2] <- 23929 
report$meta_data[3] <- "Travel Survey"
report$meta_data[4] <- 2012
report$meta_data[5] <- "1 day"
report$meta_data[6] <- "Yes" #Stage level data available
report$meta_data[7] <- "All purpose"#Overall trip purpose
report$meta_data[8] <- "Yes" # Short walks to PT
report$meta_data[9] <- "No" # Distance available
report$meta_data[10] <- "train" # missing modes 


#' I verify that every trip has the sex and age of the person who did it. Since
#' the sum of NAs is zero, then I can conclude that every trip has the
#' information.
sum(is.na(report$sex))
sum(is.na(report$age))
sum(is.na(report$trip_duration))
sum(is.na(report$trip_mode))

#' There are some trips without duration from the beginning, this is why there 
#' are more NAs here
#' 
#' Since there are some trips without duration, I removed them for now. But I 
#' think it's better to compute duration using Bogota's protocol.
#' **I leave this part in stand by, while I figure something out**
#trips_cali_v2 <- trips_cali_v2 %>% drop_na(trip_duration)

#' Export dataset to make the report
write_csv(report, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/santiago_wb/santiago_trips_wb.csv')

#' ## Standardize trip modes
#' There's already a function that standardize these modes so the package can use
#' these trips. I made sure to use translate trip modes so that the function
#' works perfectly (take a look at the *original* variable of *smodes* dataframe
#' in this function).

trips_export <- standardize_modes(report, mode = c('trip'))
unique(report$trip_mode)
unique(trips_export$trip_mode)

#' *standardize_modes* function converts walk to pedestrina, bicycle to cycle,
#' metro to rail, van to car.
#' 
#' ## Creating again IDs
trips_export <- trips_export %>% mutate(
  participant_id = as.integer(as.factor(paste(cluster_id, household_id,
                                              participant_id, sep = "_"))),
  trip_id = as.integer(as.factor(paste(cluster_id, household_id,
                                       participant_id, trip_id, sep = "_"))))

#' # **Exporting phase**
#' ## Variables to export
#' Now I filter the columns I need
trips_export <- trips_export %>% 
  select(participant_id, age, sex, trip_id, trip_mode, trip_duration)

#' ## Export dataset
#' I'm exporting this dataset to two different locations:
#' 
#' 1. In .../inst/exdata folder so the dataset is installed with the package
#' 2. In Data/Chile/Santiago/Cleaned, to have a copy 
#' quick report. 
write_csv(trips_export, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/santiago_wb/trips_santiago_wb.csv')
write_csv(trips_export, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Chile/Santiago/Cleaned/trips_santiago_wb.csv')
