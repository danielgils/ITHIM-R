#' ---
#' title: "Preprocessing of Cali's travel dataset"
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
#' Documentation is located in "...C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Cali/EODH2015/Base de datos EODH 2015/".
#'
#' From now on: 
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
  Reference = c("File1", "File2"),
  Description = c("Technical report and final results",
                  "Report with only final results"),
  Title = c("Producto 4, Indicadores Encuesta de Movilidad",
            "Encuesta de movilidad Cali 2015"),
  File = c("160216_Producto 4_Indicadores EODH.pdf",
           "20151127_Publicacion_EODH.pdf")
) %>% kbl() %>% kable_classic()

#' ## Definition of a trip
#' 1. *Trip:* All trips that are longer than 3 minutes made by people of 5 years
#' old or older (**File1**, page 102, paragraph 4.8).
#' 
#' Definition of trip in page 146 of **File1**: *Movement from one part to another made by one person with a specific reason/motive and a duration greater than 3 minutes. Or a movement from one part to another with reason/motive work or study of any duration.*

#' 
#' 2. *Collection:* Trips collected in this survey correspond to those made the 
#' day of reference, i.e., the day before the survey (**File1**, page 102,
#' paragraph 4.8).. 
#' 
#' **Results presented are for trips made in a single day.**
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
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Cali/EODH2015/Base de datos EODH 2015/2. Módulos/"

#+ warning=FALSE, message=FALSE, cache=TRUE
# Households (hh)
hh <- read_excel(paste0(route, "MOD_A_ID_HOGAR_.xlsx")) 

#' This warning labels are from a variable that we're not going to use, so it 
#' doesn't matter

#+ warning=FALSE, message=FALSE, cache=TRUE
#' **People:** When trying to read people file I saw that in the original file
#' the first row is the label of each variable and the second row is the meaning
#' of that label. I had to remove this second row and delete some images that
#' were inside the file to read it easily here in R. This file is now called:
#' *MOD_B_PERSONAS_Modified.xlsx*
#' 
#' *Note: When reading person there are some warnings related to people without*
#' *age*
people <- read_excel(paste0(route, "MOD_B_PERSONAS_Modified.xlsx"))

# Vehicles
#vehicles <- read_excel(paste0(route, "MOD_C_VEHICULOS.xlsx"))

#+ warning=FALSE, message=FALSE, cache=TRUE
#' **Trips:** According to the documentation, the dataset with imputed trips is
#' the one that we should use (see paragraph 4.9-4.13, pag 102, **File1**).
#' Before importing this dataset I added a new column called "EMPTY" because
#' values in the dataset didn't correspond to the label of the variable. By
#' creating this empty column everything makes more sense now. This file is now
#' called: *MOD_D_VIAJES_IMP_Modified.xlsx*
trips <- read_excel(paste0(route, "MOD_D_VIAJES_IMP_Modified.xlsx"))

#' ### Number of rows
#' The first thing to do is verify that the number of rows of each dataset is 
#' the same to what is mentioned in chapter 4 of **File1**. In this chapter
#' there is the data dictionary with the meaning of every variable as well as
#' the number of rows in each dataset.
nrow(hh) # Same as in paragraph 4.2
nrow(people) # Same as in paragraph 4.4
nrow(trips) # Same as in paragraph 4.13

#' In all of them is the same as reported, then we can continue.
#' 
#' ### Number of surveys per socio-economical level (strata)
#' As before, I'll verify that the number of surveys per socio-economical strata
#' is the same to what is mentioned in page 112 (Figura 5.4) of **File1**.
hh %>% group_by(ESTRATO) %>% summarise(n()) %>% 
kbl() %>% kable_classic(full_width = F)
#' Results are the same.
#' 
#' ### Number of households per municipality (with weights)
#' To check that the number of hh per municipality is the same to what is
#' mentioned in page 116 (Figura 5.9) of **File1**. This number is estimated
#' using sampling weights.
hh %>% group_by(MUNICIPIO) %>% summarise(suma = sum(F_EXP)) %>% 
  arrange(desc(suma)) %>% 
  kbl() %>% kable_classic(full_width = F)

#' In all of them is the same as reported, then we can continue.
#' 
#' ### Average number of people per household
#' To check that the number of people per hh is the same to what is mentioned in
#' page 119 (Figura 5.12) of **File1**. This number is estimated using sampling
#' weights.
people_per_hh <- people %>% group_by(ORDEN) %>% summarise(total_people = n())

hh %>% inner_join(people_per_hh, by = "ORDEN") %>% 
  rowwise() %>% 
  mutate(n_people = total_people*F_EXP) %>% 
  group_by(MUNICIPIO) %>% 
  summarise(sum(n_people)/sum(F_EXP)) %>% 
  kbl() %>% kable_classic(full_width = F)

#' In all of them is the same as reported, then we can continue.
#' 
#' ### Number of people by sex
#' To check that the number of people is the same to what is mentioned in page
#' 126 (Figura 5.22) of **File1**. This number is estimated using sampling
#' weights
people %>% group_by(P_04_B) %>% summarise(suma = sum(F_EXP)) %>% 
  kbl() %>% kable_classic(full_width = F)

#' The number of men is exactly the same, whereas the number of women is not by
#' just one person. This difference can be explained as rounding error.
#'
#' ### Number of people per municipality
#' To check that the number of people per municipality is the same to what is
#' mentioned in page 123 (Figura 5.19) of **File1**. This number is estimated
#' using sampling weights.
t_people <- people %>% 
  left_join(hh[, c("ORDEN", "MUNICIPIO")], by = "ORDEN") %>% 
  group_by(MUNICIPIO) %>% summarise(total = sum(F_EXP)) %>% 
  mutate(prop = total / sum(total)*100) # Ok Pag 123, Figure 5.19
t_people

#' There's a difference in the proportion of Jamundi and Yumbo. Although this
#' doesn't really matter because we need information for Cali only.
#' 
#' ### Number of trips per socio-economical strata

#' ### Number of trips per persona by socio-economical strata
#' ## TODO: replicate other results 

# people_per_hh <- people %>% group_by(ORDEN) %>% summarise(total_people = n())

# hh %>% inner_join(people_per_hh, by = "ORDEN") %>%
#   rowwise() %>%
#   mutate(n_people = total_people*F_EXP) %>%
#   group_by(MUNICIPIO) %>%
#   summarise(sum(n_people)/sum(F_EXP)) %>%
#   kbl() %>% kable_classic(full_width = F)

# Revisar Si hay una persona por fila en personas

# trips_per_persona <- trips %>% group_by(ORDEN, ID_PER) %>%
#   summarise(total_trips = n())

# people %>% inner_join(trips_per_persona, by = c("ORDEN", "ID_PER")) %>%
#   left_join(hh[, c("ORDEN", "ESTRATO")], by = "ORDEN") %>%
#   rowwise() %>%
#   mutate(n_trips = ifelse(is.na(total_trips), 0, total_trips*F_EXP)) %>%
#   group_by(ESTRATO) %>%
#   summarise(sum(n_trips)/sum(F_EXP))


#' ### Distribution of trips by motive
# trips %>% group_by(P_13_D) %>% summarise(suma = sum(F_EXP))
# 
# asdf <- trips %>% group_by(P_E1_14_D) %>% summarise(total = sum(F_EXP)) %>%
#   mutate(prop = total / sum(total)*100) %>%
#   arrange(desc(P_E1_14_D))

#' # **Preprocessing phase**
#' ## Filtering people from Cali only
#' Since the survey was conducted in 5 municipalities and we are only interested
#' in Cali, then these people are the only ones used. I could've used the
#' information of other municipalities, if there had been data about injuries in 
#' these locations, but unfortunately we only have injuries in Cali.
people_cali <- people %>% 
  inner_join(hh[,c("ORDEN", "MUNICIPIO")], 
             by = "ORDEN") %>% 
  filter(MUNICIPIO == "CALI")

#' I verify that there are no duplicates in people dataset
people_cali <- people_cali %>% 
  mutate(participant_id_paste = paste(ORDEN, ID_PER, sep = "-"))
length(unique(people_cali$participant_id_paste)) == nrow(people_cali)

#' ## Classification and translation of trip modes
#' Nowhere in the documentation says something about the process to define the
#' trip main mode of transport. It looks like that in the imputed dataset the
#' information about the main mode is already cleaned because there's no info
#' about other stages. So I decided to create this table based on the 
#' information I have and translated it (similar to what I did with Medellin).
#' This is the result:
main_mode <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Hierarchy.xlsx", sheet = "Cali")
main_mode %>% kbl() %>% kable_classic()

#' The first two columns of this table have all modes found in trip dataset. The
#' third column has a classification where some of the modes are grouped into
#' bigger categories (I created it in Spanish). Last column has its equivalence
#' so it can be used in the package taking into account travel modes defined in
#' standardized_modes file (.../ITHIM-R/data/global/modes/standardized_modes.csv)
#' 
#' *Note: In the dataset these modes are coded, so I had to open the Access*
#' *database to get the meaning of each code.*

#' Now with respect to trip purpose, I only had to translate them. This is the
#' result:
purpose <- read_excel("C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Hierarchy.xlsx", sheet = "Cali_purpose")
purpose %>% kbl() %>% kable_classic()

#' The first two columns have been taken from the dataset and access database,
#' and the third column is the translation and classification of these motives. 
#' 
#' ## Information at stage or trip level?
#' There is information at stage level although it seems that is not enough 
#' because of the duration of each stage. 
#' To understand how the information was collected at stage level, I selected a
#' person at random and followed the questionnaire (see **File4**) while checking
#' the information in the datasets. The person selected has ORDEN = "04414"
#' and ID_PER = "01" and NO_VIAJE = "01"
trips %>% filter(ORDEN == "04414" & ID_PER == "01" & 
                        NO_VIAJE == "01") %>% 
  select(ORDEN, ID_PER, P_09_HR_D, P_09_MN_D, P_MP_14_D, P_E1_14_D, P_E2_14_D, P_E3_14_D, P_E4_14_D, P_E5_14_D, P_E6_14_D, P_E7_14_D, P_27_HR_D,
         P_27_MN_D, T_VIAJE) %>% kbl() %>% kable_classic()

#' From this table I can see that the person started her trip at 6:50, and 
#' her made mode of transport was walking (P_MP_14_D == 30). It looks like she
#' didn't use any other mode because columns related to stages are filled with
#' NAs. She finally ended her trip at 7:00. The duration of this trip was 10 min.
#' 
#' When comparing this dataset with what is mentioned in the data dictionary of
#' **File1** (page 104), I can see that some variables are missing like walking
#' duration from origin to the first stage and from last stage to the 
#' destination. And also information about each stage, because it looks like in
#' the imputed dataset the remaining modes are missing. If this is case, there
#' is still no information about stage duration. 
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
trips_v2 <- trips %>% 
  mutate(trip_id = NO_VIAJE,
         trip_duration = T_VIAJE,
         trip_mode = main_mode$ITHIM[
           match(P_E1_14_D, main_mode$Code)],
         trip_purpose = purpose$ITHIM[
           match(P_13_D, purpose$Code)],
         trip_id_paste = paste(ORDEN, ID_PER, NO_VIAJE, sep = "-"))

#' I verify that there are no duplicates in trip dataset
length(unique(trips_v2$trip_id_paste)) == nrow(trips_v2)

#' It is important to mention that in this dataset I decided to use the main 
#' mode that appears in *P_E1_14_D* and not in *P_MP_14_D*, because the later 
#' have missing values. 
#' 
#' **Note: I strongly believe that I can clean this "imputed" dataset better. So try to do it later**
#' 
#' **ToDo: Try to implement time duration following the protocol from Bogota's**
#' **documentation, because it takes into account differences between days.**
#' **This file is "Caracterización de la movilidad – Encuesta de Movilidad de Bogotá 2019", page 202, paragraph 6.44**
#' 
#' 
#' ## Create variables for quick report
#' I need to create some variables to run the report that Lambed developed in 
#' the function *quality_check*.
report <- people_cali %>% 
  left_join(trips_v2, by = c("ORDEN", "ID_PER")) %>% 
  mutate(cluster_id = 1,
         household_id = ORDEN,
         participant_id = ID_PER,
         age = as.numeric(P_05_B),
         sex = ifelse(P_04_B == "HOMBRE", "Male", "Female"),
         participant_wt = F_EXP.x,
         meta_data = NA) %>% 
  select(cluster_id, household_id, participant_id, sex, age, participant_wt,
         trip_id, trip_mode, trip_duration, trip_purpose) 

report$meta_data[1] <- 2178842
report$meta_data[2] <- "..." 
report$meta_data[3] <- "Travel Survey"
report$meta_data[4] <- 2015
report$meta_data[5] <- "1 day"
report$meta_data[6] <- "Yes, but no stage duration" #Stage level data available
report$meta_data[7] <- "All purpose"#Overall trip purpose
report$meta_data[8] <- "Yes, but not here (yet)" # Short walks to PT
report$meta_data[9] <- "No" # Distance available
report$meta_data[10] <- "..." # missing modes#' 


#' I verify that every trip has the sex and age of the person who did it. Since
#' the sum of NAs is zero, then I can conclude that every trip has the
#' information.
sum(is.na(report$sex))
sum(is.na(report$age))
sum(is.na(report$trip_duration))
sum(is.na(report$trip_mode))

#' There are some trips without duration. This is related to what I said before
#' about trying to clean this dataset better.
#' 
#' Since there are some trips without duration, I removed them for now. But I 
#' think it's better to compute duration using Bogota's protocol.
#' **I leave this part in stand by, while I figure something out**
#trips_cali_v2 <- trips_cali_v2 %>% drop_na(trip_duration)

#' Export dataset to make the report
write_csv(report, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/data/local/cali_wb/cali_trips_wb.csv')

#' ## Standardize trip modes
#' There's already a function that standardize these modes so the package can use
#' these trips. I made sure to use translate trip modes so that the function
#' works perfectly (take a look at the *original* variable of *smodes* dataframe
#' in this function).

trips_export <- standardize_modes(report, mode = c('trip'))
unique(report$trip_mode)
unique(trips_export$trip_mode)

#' *standardize_modes* function converts bicycle to cycle, van to car, and 
#' rickshaw to auto_rickshaw.
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
write_csv(trips_export, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/cali_wb/trips_cali_wb.csv')
write_csv(trips_export, 'C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Cali/Cleaned/trips_cali_wb.csv')



