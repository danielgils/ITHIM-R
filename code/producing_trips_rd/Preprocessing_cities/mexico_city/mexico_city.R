#' ---
#' title: "Preprocessing of Mexico City's travel dataset"
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
library(kableExtra)
library(readxl)
library(tidyverse)


#+ warning=FALSE, message=FALSE, echo=FALSE
# Cleaning workspace
rm(list = ls());gc()

# Printing options
options(scipen = 50)

#' ## Documentation 
#' Documentation is located in ".../Mexico/MexicoCity/Encuesta2017/Documentacion/"
#' . These files were found in https://www.inegi.org.mx/programas/eod/2017/
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1", "File2", "File3"),
  Description = c("Report with only final results (slides)",
                  "Methodology",
                  "Conceptual design"),
  Title = c("Encuesta Origen-Destino en hogares de la zona metropolitana del valle de Mexico",
            "Encuesta Origen-Destino en hogares de la zona metropolitana del valle de Mexico 2017 EOD - Documento metodologico",
            "Encuesta Origen-Destino en hogares de la zona metropolitana del valle de Mexico 2017 EOD - Diseño conceptual"),
  File = c("resultados_eod_2017.pdf",
           "Metodologia/metodologia_eod_2017.pdf",
           "Metodologia/conceptual_eod_2017.pdf")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip
#' 1. *Trip:* All trips with no constraints of duration.
#' Definition of trip in page 21 of **File3**: *Movement from one part to another made by one person with a specific reason/motive, using one or multiple modes of transport.**
#' 
#' 2. *Collection:* Trips collected in this survey correspond to those made the 
#' during the week (Tuesday, Wednesday, Thursday) and also the Saturday.
#' **Results presented are separately for weekdays and weekends**
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
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Mexico/MexicoCity/Encuesta2017/Microdatos/CSV/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# Houses 
house <- read_csv(paste0(route, 
                         "tvivienda_eod2017/conjunto_de_datos/tvivienda.csv"))

#+ warning=FALSE, message=FALSE, cache=TRUE
# Households (hh)
hh <- read_csv(paste0(route, "thogar_eod2017/conjunto_de_datos/thogar.csv"))

#+ warning=FALSE, message=FALSE, cache=TRUE
# People
people <- read_csv(paste0(route, "tsdem_eod2017/conjunto_de_datos/tsdem.csv"))

#+ warning=FALSE, message=FALSE, cache=TRUE
# Trips
trips <- read_csv(paste0(route, "tviaje_eod2017/conjunto_de_datos/tviaje.csv"))

#+ warning=FALSE, message=FALSE, cache=TRUE
# Stages
stages <- read_csv(paste0(route, "ttransporte_eod2017/conjunto_de_datos/ttransporte.csv"))

#' ### Total houses, households and people
#' The first thing to do is verify that the number of houses, households and
#' people is the same to what is mentioned in page 16 of **File1**. The data
#' dictionary of each table is in the folder of each dataset.
sum(house$factor) 
sum(hh$factor)
sum(people$factor) # Ok same as pag 16 of resultados_eod_2017.pdf
people %>% mutate(edad = as.integer(edad)) %>% 
  filter(edad >= 6 & edad != 99) %>%
  summarise(sum(factor)) 

# Number of houses in Mexico and other municipalities
house %>% mutate(mexico = ifelse(ent == "09", 1, 0)) %>% 
  group_by(mexico) %>% summarise(total = sum(factor))

# Number of hh in Mexico and other municipalities
hh %>% mutate(mexico = ifelse(ent == "09", 1, 0)) %>% 
  group_by(mexico) %>% summarise(total = sum(factor))

# Number of people in Mexico and other municipalities
people %>% mutate(mexico = ifelse(ent == "09", 1, 0)) %>% 
  group_by(mexico) %>% summarise(total = sum(factor))

#' In all of them is the same as reported, then we can continue.
#' 
#' Distribution of hh that have vehicles 
#' To check that the number of hh with vehicles is the same to what is
#' mentioned in page 18 of **File1**. This number is estimated
#' using sampling weights.
# people_per_hh <- hh %>% 
#   inner_join(people, by = c("Id_Hogar" = "id_hogar")) %>% 
#   group_by(Id_Hogar) %>% summarise(cantidad = n())
# 
# # Then compute the average by strata
# hh %>% inner_join(people_per_hh, by = "Id_Hogar") %>% 
#   filter(municipio == "11001") %>%
#   rowwise() %>% 
#   mutate(sumoff_exp_h = cantidad*Factor) %>% 
#   group_by(p5_estrato) %>% 
#   summarise(sum(sumoff_exp_h)/sum(Factor)) %>% 
#   kbl() %>% kable_classic(full_width = F)

#' ### Trips in weekdays
#' To check that the number of trips made in weekdays is the same to what is 
#' mentioned in page 28 of **File1**. This number is estimated using sampling
#' weights.
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

# Trips with origen in Mexico city in weekdays
trips %>% filter(p5_3 == 1 & p5_7_7 == "09") %>% # Filter trips during weekdays
  rowwise() %>% 
  mutate(p5_14_01_w = ifelse(p5_14_01 == 1, factor, 0),
         p5_14_02_w = ifelse(p5_14_02 == 1, factor, 0),
         p5_14_07_w = ifelse(p5_14_07 == 1, factor, 0)) %>% 
  group_by(p5_3) %>% # Used it to get a single value
  summarise(automovil = sum(p5_14_01_w),
            colectivo = sum(p5_14_02_w),
            bicicleta = sum(p5_14_07_w)) # These values are the same as in appendix Cuadro_4.1A


trips %>% filter(p5_3 == 1 & p5_7_7 == "09") %>% # Filter trips during weekdays
  summarise(sum(factor)) # This is the same as the total in Cuadro_4.1A


# Trips in Other municipalities
# I couldn't replicate the same results in the appendix. In Cuadro_4.1C the sum
# of trips in Mexico city and other municipalities is not the same either. So
# there must be some preprocessing to get the same results
# trips %>% filter(p5_3 == 1 & !p5_7_7 %in% c("09", "99")) %>% # Filter trips during weekdays
#   summarise(sum(factor))
# 
# trips %>% filter(p5_3 == 1 & p5_7_7 != "09") %>% # Filter trips during weekdays
#   summarise(sum(factor)) # This is the same as the total in Cuadro_4.1A


#' # **Preprocessing phase**
#' ## Filtering people from Mexico city only
#' Since the survey was conducted in multiple municipalities and I don't know yet
#' whether we have injuries information then I will analyze only mexico city.
trips_weekday <- trips %>% filter(p5_3 == 1)
trips_weekend <- trips %>% filter(p5_3 == 2)

people_mexico <- people %>% filter(ent == "09") 
names(people)
#' I verify that there are no duplicates in people dataset
people_mexico <- people_mexico %>% 
  mutate(participant_id_paste = paste(id_hog, id_soc, sep = "-"))
length(unique(people_mexico$participant_id_paste)) == nrow(people_mexico)

#' ## Classification and translation of trip modes and purpose
#' Even though there's enough information at stage level, I still need to define
#' the main mode for each trip. So I made a hierarchy to get it and translate it
main_mode <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Mexico/Hierarchy.xlsx", sheet = "MexicoCity")
main_mode %>% kbl() %>% kable_classic()

#' The first two columns of this table have been defined in the data dictionary,
#' so I created the equivalence to Ithim taking into account travel modes defined in standardized_modes file (.../ITHIM-R/data/global/modes/standardized_modes.csv). The last column correspond to Lambed's classification.
#' 
#' Now with respect to trip purpose, I only had to translate them. This is the
#' result:
purpose <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Mexico/Hierarchy.xlsx", sheet = "MexicoCity_purpose")
purpose %>% kbl() %>% kable_classic()

#' The first two columns have been taken from the data dictionary,
#' and the third column is the translation and classification of these motives. 
#' Last column correspond to what Lambed defined.

#' ## Information at stage or trip level?
#' There is enough information at stage level so I have to clean everything at 
#' this level.
#' 
#' For now, I will use the information at trip level to continue working as in
#' the other cities. 
#' **ToDo: I have to leave this dataset at stage level and explain here how is the information saved in these datasets.**
#' 
#' ## Row for each trip, translate trip_mode and create duration, sex and age
#' Trip dataset already has a row for each trip, so I have to create the 
#' variables I need. 
#' 
#' Before computing time duration I saw whether the people that have 99 don't
#' have other information so we can assign them 0.
identical(which(trips_weekday$p5_9_1 == "99"),
          which(trips_weekday$p5_9_2 == "99"))

identical(which(trips_weekday$p5_10_1 == "99"),
          which(trips_weekday$p5_10_2 == "99"))

identical(which(trips_weekday$p5_9_1 == "99"),
          which(trips_weekday$p5_10_2 == "99"))

#' Since the three comparisons indicate that the people that have 99 is the same
#' in all variables, then I will assign them 0 minutes.
#' 
#' Then I will see whether there are NAs in trip mode variables. The following
#' output suggest that there are not. Then I can just simply categorize them by
#' hierarchy
for (i in 1:20) {
  if (i < 10) 
    print(sum(is.na(trips_weekday %>% select(paste0("p5_14_0",i)))))
  else
    print(sum(is.na(trips_weekday %>% select(paste0("p5_14_",i)))))
}

#' I create the trip varibles that I need in the **weekday** dataset
trips_weekday_v2 <- trips_weekday %>% 
  mutate(trip_id = id_via,
         p5_9_1 = as.numeric(p5_9_1),
         p5_9_2 = as.numeric(p5_9_2),
         p5_10_1 = as.numeric(p5_10_1),
         p5_10_2 = as.numeric(p5_10_2),
         # trip_duration = ifelse(p5_10_2 == "99", 0, 
         #                        as.numeric(difftime(strptime(paste(p5_10_1,p5_10_2, sep = ":"),"%H:%M"), strptime(paste(p5_9_1,p5_9_2, sep = ":"),"%H:%M")))),
         trip_duration = (p5_10_1 - p5_9_1)*60 + (p5_10_2 - p5_9_2),
         trip_duration = ifelse(trip_duration < (-450), 1440 + trip_duration, trip_duration),
         trip_duration = ifelse(trip_duration < 0, 0 - trip_duration, trip_duration),
         trip_duration = ifelse(trip_duration == 0, NA, trip_duration),
         # Defining hierarchy
         auto = ifelse(p5_14_01 == 1, 
                       main_mode$Hierarchy[main_mode$Code == "01"], 99),
         colectivo = ifelse(p5_14_02 == 1, 
                            main_mode$Hierarchy[main_mode$Code == "02"], 99),
         taxi_app = ifelse(p5_14_03 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "03"], 99),
         taxi = ifelse(p5_14_04 == 1, 
                       main_mode$Hierarchy[main_mode$Code == "04"], 99),
         metro = ifelse(p5_14_05 == 1, 
                        main_mode$Hierarchy[main_mode$Code == "05"], 99),
         autobus_m1 = ifelse(p5_14_06 == 1, 
                             main_mode$Hierarchy[main_mode$Code == "06"], 99),
         bicicleta = ifelse(p5_14_07 == 1, 
                            main_mode$Hierarchy[main_mode$Code == "07"], 99),
         autobus = ifelse(p5_14_08 == 1, 
                          main_mode$Hierarchy[main_mode$Code == "08"], 99),
         moto = ifelse(p5_14_09 == 1, 
                       main_mode$Hierarchy[main_mode$Code == "09"], 99),
         trolebus = ifelse(p5_14_10 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "10"], 99),
         metrobus = ifelse(p5_14_11 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "11"], 99),
         tren_ligero = ifelse(p5_14_12 == 1, 
                              main_mode$Hierarchy[main_mode$Code == "12"], 99),
         tren_sub = ifelse(p5_14_13 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "13"], 99),
         camina = ifelse(p5_14_14 == 1, 
                         main_mode$Hierarchy[main_mode$Code == "14"], 99),
         mexicable = ifelse(p5_14_15 == 1, 
                            main_mode$Hierarchy[main_mode$Code == "15"], 99),
         bicitaxi = ifelse(p5_14_16 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "16"], 99),
         mototaxi = ifelse(p5_14_17 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "17"], 99),
         escolar = ifelse(p5_14_18 == 1, 
                          main_mode$Hierarchy[main_mode$Code == "18"], 99),
         personal = ifelse(p5_14_19 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "19"], 99),
         otro = ifelse(p5_14_20 == 1, 
                       main_mode$Hierarchy[main_mode$Code == "20"], 99),
         trip_purpose = purpose$ITHIM[
           match(p5_13, purpose$Code)]) %>% 
  # Now compute the main mode by looking at the hierarchy
  rowwise() %>% mutate(
    main_modes = min(auto, colectivo, taxi_app, taxi, metro, autobus_m1,
                     bicicleta, autobus, moto, trolebus, metrobus,
                     tren_ligero,tren_sub, camina, mexicable, bicitaxi,
                     mototaxi, escolar, personal, otro, na.rm = T),
    trip_mode = main_mode$ITHIM[match(main_modes, main_mode$Hierarchy)],
    trip_id_paste = paste(id_soc, id_via, sep = "-")) 

#' I verify that there are no duplicates in trip dataset
length(unique(trips_weekday_v2$trip_id_paste)) == nrow(trips_weekday_v2)

#'
#' I create the trip varibles that I need in the **weekend** dataset
trips_weekend_v2 <- trips_weekend %>% 
  mutate(trip_id = id_via,
         p5_9_1 = as.numeric(p5_9_1),
         p5_9_2 = as.numeric(p5_9_2),
         p5_10_1 = as.numeric(p5_10_1),
         p5_10_2 = as.numeric(p5_10_2),
         # trip_duration = ifelse(p5_10_2 == "99", 0, 
         #                        as.numeric(difftime(strptime(paste(p5_10_1,p5_10_2, sep = ":"),"%H:%M"), strptime(paste(p5_9_1,p5_9_2, sep = ":"),"%H:%M")))),
         trip_duration = (p5_10_1 - p5_9_1)*60 + (p5_10_2 - p5_9_2),
         trip_duration = ifelse(trip_duration < (-450), 1440 + trip_duration, trip_duration),
         trip_duration = ifelse(trip_duration < 0, 0 - trip_duration, trip_duration),
         trip_duration = ifelse(trip_duration == 0, NA, trip_duration),
         # Defining hierarchy
         auto = ifelse(p5_14_01 == 1, 
                       main_mode$Hierarchy[main_mode$Code == "01"], 99),
         colectivo = ifelse(p5_14_02 == 1, 
                            main_mode$Hierarchy[main_mode$Code == "02"], 99),
         taxi_app = ifelse(p5_14_03 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "03"], 99),
         taxi = ifelse(p5_14_04 == 1, 
                       main_mode$Hierarchy[main_mode$Code == "04"], 99),
         metro = ifelse(p5_14_05 == 1, 
                        main_mode$Hierarchy[main_mode$Code == "05"], 99),
         autobus_m1 = ifelse(p5_14_06 == 1, 
                             main_mode$Hierarchy[main_mode$Code == "06"], 99),
         bicicleta = ifelse(p5_14_07 == 1, 
                            main_mode$Hierarchy[main_mode$Code == "07"], 99),
         autobus = ifelse(p5_14_08 == 1, 
                          main_mode$Hierarchy[main_mode$Code == "08"], 99),
         moto = ifelse(p5_14_09 == 1, 
                       main_mode$Hierarchy[main_mode$Code == "09"], 99),
         trolebus = ifelse(p5_14_10 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "10"], 99),
         metrobus = ifelse(p5_14_11 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "11"], 99),
         tren_ligero = ifelse(p5_14_12 == 1, 
                              main_mode$Hierarchy[main_mode$Code == "12"], 99),
         tren_sub = ifelse(p5_14_13 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "13"], 99),
         camina = ifelse(p5_14_14 == 1, 
                         main_mode$Hierarchy[main_mode$Code == "14"], 99),
         mexicable = ifelse(p5_14_15 == 1, 
                            main_mode$Hierarchy[main_mode$Code == "15"], 99),
         bicitaxi = ifelse(p5_14_16 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "16"], 99),
         mototaxi = ifelse(p5_14_17 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "17"], 99),
         escolar = ifelse(p5_14_18 == 1, 
                          main_mode$Hierarchy[main_mode$Code == "18"], 99),
         personal = ifelse(p5_14_19 == 1, 
                           main_mode$Hierarchy[main_mode$Code == "19"], 99),
         otro = ifelse(p5_14_20 == 1, 
                       main_mode$Hierarchy[main_mode$Code == "20"], 99),
         trip_purpose = purpose$ITHIM[
           match(p5_13, purpose$Code)]) %>% 
  # Now compute the main mode by looking at the hierarchy
  rowwise() %>% mutate(
    main_modes = min(auto, colectivo, taxi_app, taxi, metro, autobus_m1,
                     bicicleta, autobus, moto, trolebus, metrobus,
                     tren_ligero,tren_sub, camina, mexicable, bicitaxi,
                     mototaxi, escolar, personal, otro, na.rm = T),
    trip_mode = main_mode$ITHIM[match(main_modes, main_mode$Hierarchy)],
    trip_id_paste = paste(id_soc, id_via, sep = "-")) 

#' I verify that there are no duplicates in trip dataset
length(unique(trips_weekend_v2$trip_id_paste)) == nrow(trips_weekend_v2)

#' In both datasets there's only one row per trip, then we can continue
#' 
#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
#' 
#' ### Weekday
report_weekday <- people_mexico %>% 
  rename(f_exp = factor) %>% 
  left_join(trips_weekday_v2, by = "id_soc") %>% 
  mutate(cluster_id = 1,
         household_id = id_hog,
         participant_id = id_soc,
         age = as.numeric(edad.x),
         sex = ifelse(sexo.x == 1, "Male", "Female"),
         participant_wt = f_exp,
         meta_data = NA) %>% 
  select(cluster_id, household_id, participant_id, sex, age, participant_wt,
         trip_id, trip_mode, trip_duration, trip_purpose)

report_weekday$meta_data[1] <- 20976700
report_weekday$meta_data[2] <- 19239
report_weekday$meta_data[3] <- "Travel Survey"
report_weekday$meta_data[4] <- 2017
report_weekday$meta_data[5] <- "Weekdays and weekends"
report_weekday$meta_data[6] <- "Yes" #Stage level data available
report_weekday$meta_data[7] <- "All purpose"#Overall trip purpose
report_weekday$meta_data[8] <- "Yes" # Short walks to PT
report_weekday$meta_data[9] <- "No" # Distance available
report_weekday$meta_data[10] <- "" # missing modes

#' I verify that every trip has the sex and age of the person who did it. Since
#' the sum of NAs is zero, then I can conclude that every trip has the
#' information.
sum(is.na(report_weekday$sex))
sum(is.na(report_weekday$age))
sum(is.na(report_weekday$trip_duration))
sum(is.na(report_weekday$trip_mode))

#' Export dataset to make the report
write_csv(report_weekday, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/mexico_city_wb/mexico_city_trips_weekday_wb.csv')

#' ### Weekend
report_weekend <- people_mexico %>% 
  rename(f_exp = factor) %>% 
  left_join(trips_weekend_v2, by = "id_soc") %>% 
  mutate(cluster_id = 1,
         household_id = id_hog,
         participant_id = id_soc,
         age = as.numeric(edad.x),
         sex = ifelse(sexo.x == 1, "Male", "Female"),
         participant_wt = f_exp,
         meta_data = NA) %>% 
  select(cluster_id, household_id, participant_id, sex, age, participant_wt,
         trip_id, trip_mode, trip_duration, trip_purpose)

report_weekend$meta_data[1] <- 20976700
report_weekend$meta_data[2] <- 19239
report_weekend$meta_data[3] <- "Travel Survey"
report_weekend$meta_data[4] <- 2017
report_weekend$meta_data[5] <- "Weekdays and weekends"
report_weekend$meta_data[6] <- "Yes" #Stage level data available
report_weekend$meta_data[7] <- "All purpose"#Overall trip purpose
report_weekend$meta_data[8] <- "Yes" # Short walks to PT
report_weekend$meta_data[9] <- "No" # Distance available
report_weekend$meta_data[10] <- "" # missing modes

#' I verify that every trip has the sex and age of the person who did it. Since
#' the sum of NAs is zero, then I can conclude that every trip has the
#' information.
sum(is.na(report_weekend$sex))
sum(is.na(report_weekend$age))
sum(is.na(report_weekend$trip_duration))
sum(is.na(report_weekend$trip_mode))

#' Export dataset to make the report
write_csv(report_weekend, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/mexico_city_wb/mexico_city_trips_weekend_wb.csv')

#' ## Standardize trip modes
#' There's already a function that standardize these modes so the package can use
#' these trips. I made sure to use translate trip modes so that the function
#' works perfectly (take a look at the *original* variable of *smodes* dataframe
#' in this function).

trips_export <- standardize_modes(report_weekday, mode = c('trip'))
unique(report_weekday$trip_mode)
unique(trips_export$trip_mode)

#' *standardize_modes* function converts walk to pedestrian, bicycle to cycle,
#' metro to rail, van to car, and rickshaw to auto_rickshaw.
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
#' 2. In Data/Colombia/Medellin/Cleaned, to have a copy 
#' quick report. 
write_csv(trips_export, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/mexico_city_wb/trips_mexico_city_wb.csv')
write_csv(trips_export, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Mexico/MexicoCity/Cleaned/trips_mexico_city_wb.csv')

