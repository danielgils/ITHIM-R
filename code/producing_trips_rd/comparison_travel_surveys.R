#' ---
#' title: "Comparison of concepts and distributions of travel datasets"
#' author: "Daniel"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' ---
#' 
#+ warning=FALSE, message=FALSE, echo=FALSE
# Loading libraries
library(tidyverse)
library(plotly)
library(ggplot2)
library(kableExtra)

#+ warning=FALSE, message=FALSE, echo=FALSE
# Cleaning workspace
rm(list = ls());gc()

#' # Definitions
#+ warning=FALSE, message=FALSE, echo=FALSE
data.frame(
Ciudad = c("Bogota2019", "Mexico", "Cali", "Medellin", "Santiago"),
Trip = c("Moving from one part to another with a specific reason/motive, a definite hour of start and end, a mode of transport, and a duration greater than 3 minutes. Or moving from one part to another with reason/motive work or study of any duration",
         "Moving from one part to another with a specific reason/motive, using one or multiples modes of transport",
         "Moving from one part to another with a specific reason/motive and a duration longer than 3 minutes. Or moving from one part to another with reason/motive work or study of any duration",
         "Couldn't find the definition",
         "Any movement carried out on public roads with a purpose
determined, between two places (origin and destination) at a certain time of day; It can be carried out in several modes of transport and consist of one or more stages"),
Collection = c("Trips made the day of reference, i.e., the day before the survey.",
          "Trips made during the week (Tuesday, Wednesday, Thursday) and in Saturdays (weekends)",
          "Trips made the day of reference, i.e., the day before the survey.",
          "Trips made the day of reference, i.e., last 24 hours",
          "Trips made in working days (regular season), in weekends (regular season) and in working days(summer season)"),
Questionnaire = c("...", "...", "...", "...", "...")
) %>% kbl() %>% kable_classic()


#+ warning=FALSE, message=FALSE, echo=FALSE, cache=TRUE
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/"
bogota_2015 <- read_csv(paste0(route, "data/local/bogota/bogota_trip.csv"))
bogota_2019 <- read_csv(paste0(route, "data/local/bogota_wb2/bogota_trips_wb2.csv")) 
bogota_2019_longer15 <- read_csv(paste0(route, "data/local/bogota_wb2/bogota_trips_longer15_wb2.csv")) 

mexico <- read_csv(paste0(route, "data/local/mexico_city/mexico_city_trip.csv"))
mexico_weekdays <- read_csv(paste0(route, "data/local/mexico_city_wb/mexico_city_trips_weekday_wb.csv")) 
mexico_weekends <- read_csv(paste0(route, "data/local/mexico_city_wb/mexico_city_trips_weekend_wb.csv")) 

medellin <- read_csv(paste0(route, "data/local/medellin_wb/medellin_trips_wb.csv"))
cali <- read_csv(paste0(route, "data/local/cali_wb/cali_trips_wb.csv"))
santiago <- read_csv(paste0(route, "data/local/santiago_wb/santiago_trips_wb.csv"))

#' # Summary table
#+ warning=FALSE, message=FALSE, echo=FALSE
cbind(Bogota2015 = summary(bogota_2015$trip_duration),
      Bogota2019 = summary(bogota_2019$trip_duration),
      Bogota2019_longer15 = summary(bogota_2019_longer15$trip_duration),
      Mexico = summary(mexico$trip_duration),
      Mexico_weekdays = summary(mexico_weekdays$trip_duration),
      Mexico_weekends = summary(mexico_weekends$trip_duration),
      Medellin = summary(medellin$trip_duration),
      Cali = summary(cali$trip_duration),
      Santiago = summary(santiago$trip_duration)) %>% 
  kbl(digits = 1) %>% kable_classic()

#' # Density plot
#' ### *These plots are interactive so we can zoom in and out, and select cities.*
ggplotly(
  ggplot() + 
    geom_density(aes(trip_duration, fill = "Bogota2015"), alpha = .3 , 
                          data = bogota_2015) +
    geom_density(aes(trip_duration, fill = "Bogota2019"), alpha = .3 , 
               data = bogota_2019) +
    geom_density(aes(trip_duration, fill = "Bogota2019_longer"), alpha = .3 , 
               data = bogota_2019_longer15) +
    geom_density(aes(trip_duration, fill = "Mexico"), alpha = .3 , 
                 data = mexico) +
    geom_density(aes(trip_duration, fill = "Mexico_weekdays"), alpha = .3 , 
                 data = mexico_weekdays) + 
    geom_density(aes(trip_duration, fill = "Mexico_weekends"), alpha = .3 , 
                 data = mexico_weekends) +
    geom_density(aes(trip_duration, fill = "Medellin"), alpha = .3 , 
                 data = medellin) +
    geom_density(aes(trip_duration, fill = "Cali"), alpha = .3 , 
                 data = cali) +
    geom_density(aes(trip_duration, fill = "Santiago"), alpha = .3 , 
                 data = santiago) 
    )

#' # Density plot by mode
#' ## Bogota 2015
ggplotly(ggplot() + 
    geom_density(aes(trip_duration, group = trip_mode, fill = trip_mode), 
                 alpha = .3 , data = bogota_2015))

#' ## Bogota 2019
ggplotly(ggplot() + 
    geom_density(aes(trip_duration, group = trip_mode, fill = trip_mode), 
                 alpha = .3 , data = bogota_2019))

#' ## Bogota 2019 walking trips longer than 15 minutes
ggplotly(ggplot() + 
           geom_density(aes(trip_duration, group = trip_mode, fill = trip_mode), 
                        alpha = .3 , data = bogota_2019_longer15))

#' ## Mexico 
ggplotly(ggplot() + 
           geom_density(aes(trip_duration, group = trip_mode, fill = trip_mode), 
                        alpha = .3 , data = mexico))

#' ## Mexico weekdays
ggplotly(ggplot() + 
           geom_density(aes(trip_duration, group = trip_mode, fill = trip_mode), 
                        alpha = .3 , data = mexico_weekdays))

#' ## Mexico weekends
ggplotly(ggplot() + 
           geom_density(aes(trip_duration, group = trip_mode, fill = trip_mode), 
                        alpha = .3 , data = mexico_weekends))

#' ## Medellin
ggplotly(ggplot() + 
           geom_density(aes(trip_duration, group = trip_mode, fill = trip_mode), 
                        alpha = .3 , data = medellin))

#' ## Cali
ggplotly(ggplot() + 
           geom_density(aes(trip_duration, group = trip_mode, fill = trip_mode), 
                        alpha = .3 , data = cali))

#' ## Santiago
ggplotly(ggplot() + 
           geom_density(aes(trip_duration, group = trip_mode, fill = trip_mode), 
                        alpha = .3 , data = santiago))

#' # Comparison of walking trips
ggplotly(
  ggplot() + 
    geom_density(aes(trip_duration, fill = "Bogota2015"), alpha = .3 , 
                 data = bogota_2015 %>% 
                   filter(trip_mode == "walk")) +
    geom_density(aes(trip_duration, fill = "Bogota2019"), alpha = .3 , 
                 data = bogota_2019 %>% 
                   filter(trip_mode == "walk")) +
    geom_density(aes(trip_duration, fill = "Bogota2019_longer"), alpha = .3 , 
                 data = bogota_2019_longer15 %>% 
                   filter(trip_mode == "walk")) +
    geom_density(aes(trip_duration, fill = "Mexico"), alpha = .3 , 
                 data = mexico %>% 
                   filter(trip_mode == "walk")) +
    geom_density(aes(trip_duration, fill = "Mexico_weekdays"), alpha = .3 , 
                 data = mexico_weekdays %>% 
                   filter(trip_mode == "walk")) + 
    geom_density(aes(trip_duration, fill = "Mexico_weekends"), alpha = .3 , 
                 data = mexico_weekends %>% 
                   filter(trip_mode == "walk")) +
    geom_density(aes(trip_duration, fill = "Medellin"), alpha = .3 , 
                 data = medellin %>% 
                   filter(trip_mode == "walk")) +
    geom_density(aes(trip_duration, fill = "Cali"), alpha = .3 , 
                 data = cali %>% 
                   filter(trip_mode == "walk")) +
    geom_density(aes(trip_duration, fill = "Santiago"), alpha = .3 , 
                 data = santiago %>% 
                   filter(trip_mode == "walk")) 
)


#' # Comparison of cycling trips
ggplotly(
  ggplot() + 
    geom_density(aes(trip_duration, fill = "Bogota2015"), alpha = .3 , 
                 data = bogota_2015 %>% 
                   filter(trip_mode == "bicycle")) +
    geom_density(aes(trip_duration, fill = "Bogota2019"), alpha = .3 , 
                 data = bogota_2019 %>% 
                   filter(trip_mode == "bicycle")) +
    geom_density(aes(trip_duration, fill = "Bogota2019_longer"), alpha = .3 , 
                 data = bogota_2019_longer15 %>% 
                   filter(trip_mode == "bicycle")) +
    geom_density(aes(trip_duration, fill = "Mexico"), alpha = .3 , 
                 data = mexico %>% 
                   filter(trip_mode == "bicycle")) +
    geom_density(aes(trip_duration, fill = "Mexico_weekdays"), alpha = .3 , 
                 data = mexico_weekdays %>% 
                   filter(trip_mode == "bicycle")) + 
    geom_density(aes(trip_duration, fill = "Mexico_weekends"), alpha = .3 , 
                 data = mexico_weekends %>% 
                   filter(trip_mode == "bicycle")) +
    geom_density(aes(trip_duration, fill = "Medellin"), alpha = .3 , 
                 data = medellin %>% 
                   filter(trip_mode == "bicycle")) +
    geom_density(aes(trip_duration, fill = "Cali"), alpha = .3 , 
                 data = cali %>% 
                   filter(trip_mode == "bicycle")) +
    geom_density(aes(trip_duration, fill = "Santiago"), alpha = .3 , 
                 data = santiago %>% 
                   filter(trip_mode == "bicycle")) 
)
