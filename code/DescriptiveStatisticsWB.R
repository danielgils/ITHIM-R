#' ---
#' title: "Descriptive statistics for WB document"
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
#library(plotly)
library(ggplot2)
library(ggmosaic)
library("gridExtra")
library("cowplot")
#library(kableExtra)

#+ warning=FALSE, message=FALSE, echo=FALSE
# Cleaning workspace
rm(list = ls());gc()

#' Population
# Importing datasets
route <- "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/ITHIM-R/inst/extdata/local/"
pop_bogota <- read.csv(paste0(route, "bogota_wb2/population_bogota_wb2.csv"))
pop_medellin <- read.csv(paste0(route, "medellin_wb/population_medellin_wb.csv"))
pop_cali <- read.csv(paste0(route, "cali_wb/population_cali_wb.csv"))
pop_santiago <- read.csv(paste0(route, "santiago_wb/population_santiago_wb.csv"))
pop_mexico <- read.csv(paste0(route, "mexico_city_wb/population_mexico_city_wb.csv"))

names(pop_bogota);names(pop_medellin);names(pop_cali);names(pop_santiago);names(pop_mexico)

#' Translate to spanish some columns
#' Bogota
pop_bogota <- pop_bogota %>% 
  mutate(proportion = population / sum(population) * 100,
         age = factor(age, levels = c( "0-4", "5-9", "10-14", "15-19", "20-24",
                                       "25-29", "30-34", "35-39", "40-44",
                                       "45-49", "50-54", "55-59", "60-64",
                                       "65-69", "70-74", "75-79", "80-84", 
                                       "85-89", "90-94", "95-105")),
         Sexo = ifelse(sex == "Male", "Hombres", "Mujeres"))

# Plot: Color scale https://www.schemecolor.com/world-bank-group.php
plot_pop_bogota <- ggplot(pop_bogota, aes(x = age, fill = Sexo,
                       y = ifelse(test = Sexo == "Hombres",
                                  yes = -proportion, no = proportion))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = 5 * c(-1,1)) +
  theme_bw() + theme(legend.position ="bottom") +
  labs(title = "Bogotá", x = "Edad", y = "Proporción") +
  scale_colour_manual(values = c("#002244", "#009FDA"),
                      aesthetics = c("colour", "fill")) +
  coord_flip()
plot_pop_bogota

#' Medellin
pop_medellin <- pop_medellin %>% 
  mutate(proportion = population / sum(population) * 100,
         age = factor(age, levels = c( "0-4", "5-9", "10-14", "15-19", "20-24",
                                       "25-29", "30-34", "35-39", "40-44",
                                       "45-49", "50-54", "55-59", "60-64",
                                       "65-69", "70-74", "75-79", "80-84", 
                                       "85-89", "90-94", "95-105")),
         Sexo = ifelse(sex == "Male", "Hombres", "Mujeres"))

# Plot: Color scale https://www.schemecolor.com/world-bank-group.php
plot_pop_medellin <- ggplot(pop_medellin, aes(x = age, fill = Sexo,
                    y = ifelse(test = Sexo == "Hombres",
                               yes = -proportion, no = proportion))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = 5 * c(-1,1)) +
  theme_bw() + theme(legend.position ="bottom") +
  labs(title = "Medellín", x = "Edad", y = "Proporción") +
  scale_colour_manual(values = c("#002244", "#009FDA"),
                      aesthetics = c("colour", "fill")) +
  coord_flip()
plot_pop_medellin

#' Cali
names(pop_cali)
unique(pop_cali$sex)
pop_cali <- pop_cali %>% 
  mutate(proportion = population / sum(population) * 100,
         age = factor(age, levels = c( "0-4", "5-9", "10-14", "15-19", "20-24",
                                       "25-29", "30-34", "35-39", "40-44",
                                       "45-49", "50-54", "55-59", "60-64",
                                       "65-69", "70-74", "75-79", "80-84", 
                                       "85-89", "90-94", "95-105")),
         Sexo = ifelse(sex == "Male", "Hombres", "Mujeres"))

# Plot: Color scale https://www.schemecolor.com/world-bank-group.php
plot_pop_cali <- ggplot(pop_cali, aes(x = age, fill = Sexo,
                         y = ifelse(test = Sexo == "Hombres",
                                    yes = -proportion, no = proportion))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = 5 * c(-1,1)) +
  theme_bw() + theme(legend.position ="bottom") +
  labs(title = "Cali", x = "Edad", y = "Proporción") +
  scale_colour_manual(values = c("#002244", "#009FDA"),
                      aesthetics = c("colour", "fill")) +
  coord_flip()
plot_pop_cali

#' Santiago
names(pop_santiago)
unique(pop_santiago$age)
pop_santiago <- pop_santiago %>% 
  mutate(proportion = population / sum(population) * 100,
         age = factor(age, levels = c( "0-4", "5-9", "10-14", "15-19", "20-24",
                                       "25-29", "30-34", "35-39", "40-44",
                                       "45-49", "50-54", "55-59", "60-64",
                                       "65-69", "70-74", "75-79", "80-84", 
                                       "85-89", "90-94", "95-99", "100-150")),
         Sexo = ifelse(sex == "Male", "Hombres", "Mujeres"))

# Plot: Color scale https://www.schemecolor.com/world-bank-group.php
plot_pop_santiago <- ggplot(pop_santiago, aes(x = age, fill = Sexo,
                     y = ifelse(test = Sexo == "Hombres",
                                yes = -proportion, no = proportion))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = 5 * c(-1,1)) +
  theme_bw() + theme(legend.position ="bottom") +
  labs(title = "Santiago", x = "Edad", y = "Proporción") +
  scale_colour_manual(values = c("#002244", "#009FDA"),
                      aesthetics = c("colour", "fill")) +
  coord_flip()
plot_pop_santiago

#' Mexico
names(pop_mexico)
unique(pop_mexico$sex)
pop_mexico <- pop_mexico %>% 
  mutate(proportion = population / sum(population) * 100,
         age = factor(age, levels = c( "0-4", "5-9", "10-14", "15-19", "20-24",
                                       "25-29", "30-34", "35-39", "40-44",
                                       "45-49", "50-54", "55-59", "60-64",
                                       "65-69", "70-74", "75-79", "80-150")),
         Sexo = ifelse(sex == "Male", "Hombres", "Mujeres"))

# Plot: Color scale https://www.schemecolor.com/world-bank-group.php
plot_pop_mexico <- ggplot(pop_mexico, aes(x = age, fill = Sexo,
                         y = ifelse(test = Sexo == "Hombres",
                                    yes = -proportion, no = proportion))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = 5 * c(-1,1)) +
  theme_bw() + theme(legend.position ="bottom") +
  labs(title = "Ciudad de México", x = "Edad", y = "Proporción") +
  scale_colour_manual(values = c("#002244", "#009FDA"),
                      aesthetics = c("colour", "fill")) +
  coord_flip()
plot_pop_mexico

#----
# Multiple plots in one
# plot_grid(plot_pop_bogota, plot_pop_medellin, plot_pop_cali,
#           plot_pop_santiago, plot_pop_mexico, labels=c("A", "B", "C", "D", "E"), ncol = 3, nrow = 2)

# Using one legend only
# http://www.sthda.com/english/wiki/wiki.php?id_contents=7930
aux <- ggplot(pop_bogota, aes(x = age, fill = Sexo,
                                          y = ifelse(test = Sexo == "Hombres",
                                                     yes = -proportion, 
                                                     no = proportion))) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = 5 * c(-1,1)) +
  theme_bw() + theme(legend.position = "right") +
  labs(title = "Bogotá", x = "Edad", y = "Proporción") +
  scale_colour_manual(values = c("#002244", "#009FDA"),
                      aesthetics = c("colour", "fill")) +
  coord_flip()

get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(aux)

# 4. Arrange ggplot2 graphs with a specific width
grid.arrange(plot_pop_bogota + theme(legend.position = "none"),
             plot_pop_medellin + theme(legend.position = "none"), 
             plot_pop_cali + theme(legend.position = "none"), 
             plot_pop_santiago + theme(legend.position = "none"),
             plot_pop_mexico + theme(legend.position = "none"), 
             legend, ncol = 3)

#----
#' GBD
# Importing datasets
gbd_colombia <- read.csv(paste0(route, "bogota_wb2/gbd_bogota_wb2.csv"))
gbd_chile <- read.csv(paste0(route, "santiago_wb/gbd_santiago_wb.csv"))
gbd_mexico <- read.csv(paste0(route, "mexico_city_wb/gbd_mexico_city_wb.csv"))

#' Colombia
unique(gbd_colombia$cause_name)
names(gbd_colombia)
gbd_colombia_deaths <- gbd_colombia %>% 
  filter(measure_name.x == "Deaths" & cause_name != "Uterine cancer") %>% 
  mutate(causa = case_when(
    cause_name == "Lower respiratory infections" ~ "Infecciones respiratorias",
    cause_name == "Alzheimer's disease and other dementias" ~ "Alzheimer",
    cause_name == "Ischemic heart disease" ~ "Isquemia de Corazón",
    cause_name == "Stroke" ~ "Accidente cerebro-vascular",
    cause_name == "Chronic obstructive pulmonary disease" ~ "Pulmonar crónica obstructiva",
    cause_name == "Diabetes mellitus type 2" ~ "Diabetes",
    cause_name == "Neoplasms" ~ "Neoplasia",
    cause_name == "Road injuries" ~ "Incidentes de tránsito",
    cause_name == "Cardiovascular diseases" ~ "Enfermedades cardiovasculares",
    cause_name == "Tracheal, bronchus, and lung cancer" ~ "Cáncer de pulmón, traquea, bronqueos",
    cause_name == "Breast cancer" ~ "Cáncer de mama",
    cause_name == "All causes" ~ "Todas las causas",
    cause_name == "Colon and rectum cancer" ~ "Cáncer de colon y recto",
    cause_name == "Stomach cancer" ~ "Cáncer de estómago",
    cause_name == "Liver cancer" ~ "Cáncer de hígado",
    TRUE ~ cause_name
  )) %>% 
  group_by(causa) %>% summarise(muertes = sum(val),
                                poblacion = sum(population)) %>% 
  rowwise() %>% mutate(tasa = muertes/poblacion * 10000) %>% 
  arrange(tasa)

# Plot: Color scale https://www.schemecolor.com/world-bank-group.php
plot_gbd_colombia <- ggplot(gbd_colombia_deaths, aes(x = reorder(causa, tasa), y = tasa)) +
  geom_bar(stat = "identity", fill = "#009FDA") + 
  scale_y_continuous(limits = c(0,80)) +
  theme_bw() + 
  labs(title = "Colombia", x = "Causa de muerte", 
       y = "Tasa por 10.000 habitantes") +
  geom_text(aes(label = round(tasa, 0)), position = position_dodge(width = 0.9), 
            hjust = -.10, size = 4) +
  coord_flip()
plot_gbd_colombia

# Chile
unique(gbd_chile$cause_name)
names(gbd_chile)
gbd_chile_deaths <- gbd_chile %>% 
  filter(measure_name.x == "Deaths" & cause_name != "Uterine cancer") %>% 
  mutate(causa = case_when(
    cause_name == "Lower respiratory infections" ~ "Infecciones respiratorias",
    cause_name == "Alzheimer's disease and other dementias" ~ "Alzheimer",
    cause_name == "Ischemic heart disease" ~ "Isquemia de Corazón",
    cause_name == "Stroke" ~ "Accidente cerebro-vascular",
    cause_name == "Chronic obstructive pulmonary disease" ~ "Pulmonar crónica obstructiva",
    cause_name == "Diabetes mellitus type 2" ~ "Diabetes",
    cause_name == "Neoplasms" ~ "Neoplasia",
    cause_name == "Road injuries" ~ "Incidentes de tránsito",
    cause_name == "Cardiovascular diseases" ~ "Enfermedades cardiovasculares",
    cause_name == "Tracheal, bronchus, and lung cancer" ~ "Cáncer de pulmón, traquea, bronqueos",
    cause_name == "Breast cancer" ~ "Cáncer de mama",
    cause_name == "All causes" ~ "Todas las causas",
    cause_name == "Colon and rectum cancer" ~ "Cáncer de colon y recto",
    cause_name == "Stomach cancer" ~ "Cáncer de estómago",
    cause_name == "Liver cancer" ~ "Cáncer de hígado",
    TRUE ~ cause_name
  )) %>% 
  group_by(causa) %>% summarise(muertes = sum(val),
                                poblacion = sum(population)) %>% 
  rowwise() %>% mutate(tasa = muertes/poblacion * 10000) %>% 
  arrange(tasa)

# Plot: Color scale https://www.schemecolor.com/world-bank-group.php
plot_gbd_chile <- ggplot(gbd_chile_deaths, aes(x = reorder(causa, tasa), 
                                               y = tasa)) +
  geom_bar(stat = "identity", fill = "#002244") +
  scale_y_continuous(limits = c(0,80)) +
  theme_bw() + 
  labs(title = "Chile", x = "Causa de muerte", 
       y = "Tasa por 10.000 habitantes") +
  geom_text(aes(label = round(tasa, 0)), position = position_dodge(width = 0.9), 
            hjust = -.10, size = 4) +
  coord_flip()
plot_gbd_chile

# Mexico
unique(gbd_mexico$location_name)
names(gbd_mexico)
gbd_mexico_deaths <- gbd_mexico %>% 
  filter(measure_name.x == "Deaths" & cause_name != "Uterine cancer") %>% 
  mutate(causa = case_when(
    cause_name == "Lower respiratory infections" ~ "Infecciones respiratorias",
    cause_name == "Alzheimer's disease and other dementias" ~ "Alzheimer",
    cause_name == "Ischemic heart disease" ~ "Isquemia de Corazón",
    cause_name == "Stroke" ~ "Accidente cerebro-vascular",
    cause_name == "Chronic obstructive pulmonary disease" ~ "Pulmonar crónica obstructiva",
    cause_name == "Diabetes mellitus type 2" ~ "Diabetes",
    cause_name == "Neoplasms" ~ "Neoplasia",
    cause_name == "Road injuries" ~ "Incidentes de tránsito",
    cause_name == "Cardiovascular diseases" ~ "Enfermedades cardiovasculares",
    cause_name == "Tracheal, bronchus, and lung cancer" ~ "Cáncer de pulmón, traquea, bronqueos",
    cause_name == "Breast cancer" ~ "Cáncer de mama",
    cause_name == "All causes" ~ "Todas las causas",
    cause_name == "Colon and rectum cancer" ~ "Cáncer de colon y recto",
    cause_name == "Stomach cancer" ~ "Cáncer de estómago",
    cause_name == "Liver cancer" ~ "Cáncer de hígado",
    TRUE ~ cause_name
  )) %>% 
  group_by(causa) %>% summarise(muertes = sum(val),
                                poblacion = sum(population)) %>% 
  rowwise() %>% mutate(tasa = muertes/poblacion * 10000) %>% 
  arrange(tasa)

# Plot: Color scale https://www.schemecolor.com/world-bank-group.php
plot_gbd_mexico <- ggplot(gbd_mexico_deaths, aes(x = reorder(causa, tasa), 
                                               y = tasa)) +
  geom_bar(stat = "identity", fill = "#009FDA") +
  scale_y_continuous(limits = c(0,80)) +
  theme_bw() + 
  labs(title = "Mexico", x = "Causa de muerte", 
       y = "Tasa por 10.000 habitantes") +
  geom_text(aes(label = round(tasa, 0)), position = position_dodge(width = 0.9), 
            hjust = -.10, size = 4) +
  coord_flip()
plot_gbd_mexico

# Multiple plots in one
plot_grid(plot_gbd_colombia, plot_gbd_chile, plot_gbd_mexico, ncol = 1, nrow = 3)
# La exporte en tamaño 600x800

#----
#' Physical activity
# Importing datasets
pa_bogota <- read.csv(paste0(route, "bogota_wb2/pa_bogota_wb2.csv"))
pa_medellin <- read.csv(paste0(route, "medellin_wb/pa_medellin_wb.csv"))
pa_cali <- read.csv(paste0(route, "cali_wb/pa_cali_wb.csv"))
pa_santiago <- read.csv(paste0(route, "santiago_wb/pa_santiago_wb.csv"))
pa_mexico <- read.csv(paste0(route, "mexico_city_wb/pa_mexico_city_wb.csv"))

names(pa_bogota)

#' Trips
summary(pa_bogota$work_ltpa_marg_met)
summary(pa_medellin$work_ltpa_marg_met)
summary(pa_cali$work_ltpa_marg_met)
summary(pa_santiago$work_ltpa_marg_met)
summary(pa_mexico$work_ltpa_marg_met)
# ggplot(pa_bogota %>% filter(work_ltpa_marg_met <= 50), aes(x = work_ltpa_marg_met, color = sexo, fill = sexo)) + 
#   geom_histogram(alpha = 0.5) + theme_bw() +
#   labs(title = "Bogota", x = "mMETs a la semana", 
#        y = "Frecuencia") +
#   scale_colour_manual(values = c("#002244", "#009FDA"),
#                       aesthetics = c("colour", "fill"))

#' Bogota
pa_bogota <- pa_bogota %>% 
  mutate(Sexo = ifelse(sex == "male", "Hombres", "Mujeres"))

plot_pa_bogota <- ggplot(pa_bogota, aes(x = Sexo, y = work_ltpa_marg_met, color = Sexo, fill = Sexo)) + 
  geom_boxplot(alpha = 0.5) + theme_bw() +
  labs(title = "Bogota", y = "mMETs a la semana", 
       x = "Sexo") +
  scale_colour_manual(values = c("#002244", "#009FDA"),
                      aesthetics = c("colour", "fill"))
plot_pa_bogota


#' Medellin
pa_medellin <- pa_medellin %>% 
  mutate(Sexo = ifelse(sex == "male", "Hombres", "Mujeres"))

plot_pa_medellin <- ggplot(pa_medellin, aes(x = Sexo, y = work_ltpa_marg_met, color = Sexo, fill = Sexo)) + 
  geom_boxplot(alpha = 0.5) + coord_cartesian(ylim = c(0, 200)) + theme_bw() +
  labs(title = "Medellín", y = "mMETs a la semana", 
       x = "Sexo") +
  scale_colour_manual(values = c("#002244", "#009FDA"),
                      aesthetics = c("colour", "fill"))
plot_pa_medellin

#' Cali
pa_cali <- pa_cali %>% 
  mutate(Sexo = ifelse(sex == "male", "Hombres", "Mujeres"))

plot_pa_cali <- ggplot(pa_cali, aes(x = Sexo, y = work_ltpa_marg_met, color = Sexo, fill = Sexo)) + 
  geom_boxplot(alpha = 0.5) + coord_cartesian(ylim = c(0, 200)) + theme_bw() +
  labs(title = "Cali", y = "mMETs a la semana", 
       x = "Sexo") +
  scale_colour_manual(values = c("#002244", "#009FDA"),
                      aesthetics = c("colour", "fill"))
plot_pa_cali

#' Santiago
pa_santiago <- pa_santiago %>% 
  mutate(Sexo = ifelse(sex == "Male", "Hombres", "Mujeres"))

plot_pa_santiago <- ggplot(pa_santiago, aes(x = Sexo, y = work_ltpa_marg_met, color = Sexo, fill = Sexo)) + 
  geom_boxplot(alpha = 0.5) + coord_cartesian(ylim = c(0, 460)) + theme_bw() +
  labs(title = "Santiago", y = "mMETs a la semana", 
       x = "Sexo") +
  scale_colour_manual(values = c("#002244", "#009FDA"),
                      aesthetics = c("colour", "fill"))
plot_pa_santiago

#' Ciudad de México
pa_mexico <- pa_mexico %>% 
  mutate(Sexo = ifelse(sex == "Male", "Hombres", "Mujeres"))

plot_pa_mexico <- ggplot(pa_mexico, aes(x = Sexo, y = work_ltpa_marg_met, color = Sexo, fill = Sexo)) + 
  geom_boxplot(alpha = 0.5) + coord_cartesian(ylim = c(0, 460)) + theme_bw() +
  labs(title = "Ciudad de México", y = "mMETs a la semana", 
       x = "Sexo") +
  scale_colour_manual(values = c("#002244", "#009FDA"),
                      aesthetics = c("colour", "fill"))
plot_pa_mexico

# Using one legend only
# http://www.sthda.com/english/wiki/wiki.php?id_contents=7930
aux <- ggplot(pa_mexico, aes(x = Sexo, y = work_ltpa_marg_met, 
                             color = Sexo, fill = Sexo)) + 
  geom_boxplot(alpha = 0.5) + coord_cartesian(ylim = c(0, 460)) + theme_bw() +
  labs(title = "Ciudad de México", y = "mMETs a la semana", 
       x = "Sexo") +
  scale_colour_manual(values = c("#002244", "#009FDA"),
                      aesthetics = c("colour", "fill"))

get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(aux)

# 4. Arrange ggplot2 graphs with a specific width
grid.arrange(plot_pa_bogota + theme(legend.position = "none"),
             plot_pa_medellin + theme(legend.position = "none"), 
             plot_pa_cali + theme(legend.position = "none"), 
             plot_pa_santiago + theme(legend.position = "none"),
             plot_pa_mexico + theme(legend.position = "none"), 
             legend, ncol = 3)

#----
# Importing datasets
trips_bogota <- read.csv(paste0(route, "bogota_wb2/trips_bogota_wb2.csv"))
trips_medellin <- read.csv(paste0(route, "medellin_wb/trips_medellin_wb.csv"))
trips_cali <- read.csv(paste0(route, "cali_wb/trips_cali_wb.csv"))
trips_santiago <- read.csv(paste0(route, "santiago_wb/trips_santiago_wb.csv"))
trips_mexico <- read.csv(paste0(route, "mexico_city_wb/trips_mexico_city_wb.csv"))

#' Bogota
names(trips_bogota)
unique(trips_bogota$trip_mode)
table(trips_bogota$trip_mode)
trips_bogota_summary <- trips_bogota %>% filter(!is.na(trip_mode)) %>% 
  count(trip_mode) %>% mutate(proporcion = n/sum(n) * 100,
                              modo = case_when(
                                trip_mode == "pedestrian" ~ "A pie",
                                trip_mode == "bus" ~ "Bus",
                                trip_mode == "car" ~ "Carro",
                                trip_mode == "cycle" ~ "Bicicleta",
                                trip_mode == "taxi" ~ "Taxi",
                                trip_mode == "motorcycle" ~ "Moto",
                                trip_mode == "rail" ~ "Tren",
                                trip_mode == "auto_rickshaw" ~ "Bicitaxi",
                                trip_mode == "other" ~ "Otro",
                                TRUE ~ trip_mode
                              ))
# Plot: Color scale https://www.schemecolor.com/world-bank-group.php
plot_trips_bogota <- ggplot(trips_bogota_summary, 
                            aes(x = reorder(modo, proporcion), 
                              y = proporcion)) +
  geom_bar(stat = "identity", fill = "#009FDA") +
  scale_y_continuous(limits = c(0,45)) +
  theme_bw() + 
  labs(title = "Bogotá", x = "Modo de transporte", 
       y = "Proporción") +
  geom_text(aes(label = round(proporcion, 1)), 
            position = position_dodge(width = 0.9), 
            hjust = -.10, size = 4) +
  coord_flip()
plot_trips_bogota

#' Medellin
names(trips_medellin)
unique(trips_medellin$trip_mode)
table(trips_medellin$trip_mode)
trips_medellin_summary <- trips_medellin %>% filter(!is.na(trip_mode)) %>% 
  count(trip_mode) %>% mutate(proporcion = n/sum(n) * 100,
                              modo = case_when(
                                trip_mode == "pedestrian" ~ "A pie",
                                trip_mode == "bus" ~ "Bus",
                                trip_mode == "car" ~ "Carro",
                                trip_mode == "cycle" ~ "Bicicleta",
                                trip_mode == "taxi" ~ "Taxi",
                                trip_mode == "motorcycle" ~ "Moto",
                                trip_mode == "rail" ~ "Tren",
                                trip_mode == "auto_rickshaw" ~ "Bicitaxi",
                                trip_mode == "other" ~ "Otro",
                                TRUE ~ trip_mode
                              ))
# Plot: Color scale https://www.schemecolor.com/world-bank-group.php
plot_trips_medellin <- ggplot(trips_medellin_summary, 
                              aes(x = reorder(modo, proporcion), 
                                 y = proporcion)) +
  geom_bar(stat = "identity", fill = "#002244") +
  scale_y_continuous(limits = c(0,45)) +
  theme_bw() + 
  labs(title = "Medellín", x = "Modo de transporte", 
       y = "Proporción") +
  geom_text(aes(label = round(proporcion, 1)), 
            position = position_dodge(width = 0.9), 
            hjust = -.10, size = 4) +
  coord_flip()
plot_trips_medellin

#' Cali
names(trips_cali)
unique(trips_cali$trip_mode)
table(trips_cali$trip_mode)
trips_cali_summary <- trips_cali %>% filter(!is.na(trip_mode)) %>% 
  count(trip_mode) %>% mutate(proporcion = n/sum(n) * 100,
                              modo = case_when(
                                trip_mode == "pedestrian" ~ "A pie",
                                trip_mode == "bus" ~ "Bus",
                                trip_mode == "car" ~ "Carro",
                                trip_mode == "cycle" ~ "Bicicleta",
                                trip_mode == "taxi" ~ "Taxi",
                                trip_mode == "motorcycle" ~ "Moto",
                                trip_mode == "rail" ~ "Tren",
                                trip_mode == "auto_rickshaw" ~ "Bicitaxi",
                                trip_mode == "other" ~ "Otro",
                                TRUE ~ trip_mode
                              ))
# Plot: Color scale https://www.schemecolor.com/world-bank-group.php
plot_trips_cali <- ggplot(trips_cali_summary, 
                              aes(x = reorder(modo, proporcion), 
                                  y = proporcion)) +
  geom_bar(stat = "identity", fill = "#002244") +
  scale_y_continuous(limits = c(0,45)) +
  theme_bw() + 
  labs(title = "Cali", x = "Modo de transporte", 
       y = "Proporción") +
  geom_text(aes(label = round(proporcion, 1)), 
            position = position_dodge(width = 0.9), 
            hjust = -.10, size = 4) +
  coord_flip()
plot_trips_cali

#' Santiago
names(trips_santiago)
unique(trips_santiago$trip_mode)
table(trips_santiago$trip_mode)
trips_santiago_summary <- trips_santiago %>% filter(!is.na(trip_mode)) %>% 
  count(trip_mode) %>% mutate(proporcion = n/sum(n) * 100,
                              modo = case_when(
                                trip_mode == "pedestrian" ~ "A pie",
                                trip_mode == "bus" ~ "Bus",
                                trip_mode == "car" ~ "Carro",
                                trip_mode == "cycle" ~ "Bicicleta",
                                trip_mode == "taxi" ~ "Taxi",
                                trip_mode == "motorcycle" ~ "Moto",
                                trip_mode == "rail" ~ "Tren",
                                trip_mode == "auto_rickshaw" ~ "Bicitaxi",
                                trip_mode == "other" ~ "Otro",
                                TRUE ~ trip_mode
                              ))
# Plot: Color scale https://www.schemecolor.com/world-bank-group.php
plot_trips_santiago <- ggplot(trips_santiago_summary, 
                          aes(x = reorder(modo, proporcion), 
                              y = proporcion)) +
  geom_bar(stat = "identity", fill = "#009FDA") +
  scale_y_continuous(limits = c(0,45)) +
  theme_bw() + 
  labs(title = "Santiago", x = "Modo de transporte", 
       y = "Proporción") +
  geom_text(aes(label = round(proporcion, 1)), 
            position = position_dodge(width = 0.9), 
            hjust = -.10, size = 4) +
  coord_flip()
plot_trips_santiago

#' Mexico
names(trips_mexico)
unique(trips_mexico$trip_mode)
table(trips_mexico$trip_mode)
trips_mexico_summary <- trips_mexico %>% filter(!is.na(trip_mode)) %>% 
  count(trip_mode) %>% mutate(proporcion = n/sum(n) * 100,
                              modo = case_when(
                                trip_mode == "pedestrian" ~ "A pie",
                                trip_mode == "bus" ~ "Bus",
                                trip_mode == "car" ~ "Carro",
                                trip_mode == "cycle" ~ "Bicicleta",
                                trip_mode == "taxi" ~ "Taxi",
                                trip_mode == "motorcycle" ~ "Moto",
                                trip_mode == "rail" ~ "Tren",
                                trip_mode == "auto_rickshaw" ~ "Bicitaxi",
                                trip_mode == "other" ~ "Otro",
                                TRUE ~ trip_mode
                              ))
# Plot: Color scale https://www.schemecolor.com/world-bank-group.php
plot_trips_mexico <- ggplot(trips_mexico_summary, 
                              aes(x = reorder(modo, proporcion), 
                                  y = proporcion)) +
  geom_bar(stat = "identity", fill = "#009FDA") +
  scale_y_continuous(limits = c(0,45)) +
  theme_bw() + 
  labs(title = "Ciudad de México", x = "Modo de transporte", 
       y = "Proporción") +
  geom_text(aes(label = round(proporcion, 1)), 
            position = position_dodge(width = 0.9), 
            hjust = -.10, size = 4) +
  coord_flip()
plot_trips_mexico

# Multiple plots in one
plot_grid(plot_trips_bogota, plot_trips_medellin, plot_trips_cali, 
          plot_trips_santiago, plot_trips_mexico, ncol = 2, nrow = 3)
# La exporte en tamaño 600x800


#----
#' Injuries
# Importing datasets
injuries_bogota <- read.csv(paste0(route, "bogota_wb2/injuries_bogota_wb2.csv"))
injuries_medellin <- read.csv(paste0(route, "medellin_wb/injuries_medellin_wb.csv"))
injuries_cali <- read.csv(paste0(route, "cali_wb/injuries_cali_wb.csv"))
injuries_santiago <- read.csv(paste0(route, "santiago_wb/injuries_santiago_wb.csv"))
injuries_mexico <- read.csv(paste0(route, "mexico_city_wb/injuries_mexico_city_wb.csv"))

# ggplot(data = injuries_bogota) +
#   geom_mosaic(aes(x = product(cas_mode, strike_mode), fill = cas_mode), 
#               na.rm = TRUE) + theme_bw()  
# 
# 
# df <- injuries_bogota %>% count(cas_mode, strike_mode) %>% 
#   mutate(cut.count = sum(n),
#          prop = n/sum(n)) 

#' Bogota
names(injuries_bogota)
unique(injuries_bogota$cas_mode)
injuries_bogota <- injuries_bogota %>% 
  mutate(accidente = case_when(
    strike_mode == "motorcycle" ~ "Moto",
    strike_mode == "truck" ~ "Camión",
    strike_mode == "NOV" ~ "Objetivo fijo",
    strike_mode == "car" ~ "Carro",
    strike_mode == "bus" ~ "Bus",
    strike_mode == "cycle" ~ "Bicicleta",
    TRUE ~ strike_mode), 
    fallecido = factor(case_when(
      cas_mode == "motorcycle" ~ "Moto",
      cas_mode == "truck" ~ "Camión",
      cas_mode == "pedestrian" ~ "Peatón",
      cas_mode == "car" ~ "Carro",
      cas_mode == "bus" ~ "Bus",
      cas_mode == "cycle" ~ "Bicicleta",
      TRUE ~ cas_mode), levels = c("Bus", "Bicicleta", "Camión", "Moto",
                                   "Carro", "Peatón")))

# table(injuries_bogota$cas_mode, injuries_bogota$fallecido)
# table(injuries_bogota$strike_mode, injuries_bogota$accidente)


# table(injuries_bogota$strike_mode)
# ggplot(data = injuries_bogota) +
#   geom_mosaic(aes(x = product(strike_mode), fill = strike_mode), divider="vbar") + theme(axis.text.x = element_blank(),
#                                                                                          axis.ticks.x = element_blank()) +
#   labs(y="Do you recline?", x = "", title = "Bar Chart")
# 
# table(injuries_bogota$cas_mode)
# ggplot(data = injuries_bogota) +
#   geom_mosaic(aes(x = product(cas_mode), fill = cas_mode))

table(injuries_bogota$fallecido, injuries_bogota$accidente)
plot_injuries_bogota <- ggplot(data = injuries_bogota) +
  geom_mosaic(aes(x = product(accidente, fallecido), fill = accidente)) +
  guides(x = guide_axis(n.dodge = 3), y = guide_axis(n.dodge = 2), 
         fill = guide_legend(title = "Vehículo accidente", reverse = TRUE)) +
  labs(title = "Bogota", x = "Vehículo fallecido", y = "Vehículo accidente") +
  theme_bw() #+
  #theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1))
plot_injuries_bogota

#' Medellin
names(injuries_medellin)
unique(injuries_medellin$strike_mode)
injuries_medellin <- injuries_medellin %>% 
  mutate(accidente = case_when(
    strike_mode == "motorcycle" ~ "Moto",
    strike_mode == "truck" ~ "Camión",
    strike_mode == "NOV" ~ "Objetivo fijo",
    strike_mode == "car" ~ "Carro",
    strike_mode == "bus" ~ "Bus",
    strike_mode == "cycle" ~ "Bicicleta",
    strike_mode == "unknown" ~ "Desconocido",
    strike_mode == "other" ~ "Otro",
    TRUE ~ strike_mode), 
    fallecido = factor(case_when(
      cas_mode == "motorcycle" ~ "Moto",
      cas_mode == "truck" ~ "Camión",
      cas_mode == "pedestrian" ~ "Peatón",
      cas_mode == "car" ~ "Carro",
      cas_mode == "bus" ~ "Bus",
      cas_mode == "cycle" ~ "Bicicleta",
      cas_mode == "auto_rickshaw" ~ "Motocarro",
      cas_mode == "unknown" ~ "Desconocido",
      TRUE ~ cas_mode), levels = c("Bus", "Bicicleta", "Camión", "Moto",
                                   "Carro", "Peatón", "Motocarro",
                                   "Desconocido")))

table(injuries_medellin$cas_mode, injuries_medellin$fallecido)
table(injuries_medellin$strike_mode, injuries_medellin$accidente)

table(injuries_medellin$fallecido, injuries_medellin$accidente)
plot_injuries_medellin <- ggplot(data = injuries_medellin) +
  geom_mosaic(aes(x = product(accidente, fallecido), fill = accidente)) +
  guides(x = guide_axis(n.dodge = 3),  y = guide_axis(n.dodge = 2), 
         fill = guide_legend(title = "Vehículo accidente", reverse = TRUE)) +
  labs(title = "Medellín", x = "Vehículo fallecido", y = "Vehículo accidente") +
  theme_bw() #+
  #theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1)) 
plot_injuries_medellin

#' Cali
names(injuries_cali)
unique(injuries_cali$strike_mode)
injuries_cali <- injuries_cali %>% 
  mutate(accidente = case_when(
    strike_mode == "motorcycle" ~ "Moto",
    strike_mode == "truck" ~ "Camión",
    strike_mode == "NOV" ~ "Objetivo fijo",
    strike_mode == "car" ~ "Carro",
    strike_mode == "bus" ~ "Bus",
    strike_mode == "cycle" ~ "Bicicleta",
    strike_mode == "unknown" ~ "Desconocido",
    strike_mode == "other" ~ "Otro",
    strike_mode == "pedestrian" ~ "Peatón",
    TRUE ~ strike_mode), 
    fallecido = factor(case_when(
      cas_mode == "motorcycle" ~ "Moto",
      cas_mode == "truck" ~ "Camión",
      cas_mode == "pedestrian" ~ "Peatón",
      cas_mode == "car" ~ "Carro",
      cas_mode == "bus" ~ "Bus",
      cas_mode == "cycle" ~ "Bicicleta",
      cas_mode == "auto_rickshaw" ~ "Motocarro",
      cas_mode == "unknown" ~ "Desconocido",
      cas_mode == "other" ~ "Otro",
      TRUE ~ cas_mode), levels = c("Bus", "Bicicleta", "Camión", "Moto",
                                   "Carro", "Peatón", "Desconocido", "Otro")))

table(injuries_cali$cas_mode, injuries_cali$fallecido)
table(injuries_cali$strike_mode, injuries_cali$accidente)

table(injuries_cali$fallecido, injuries_cali$accidente)
plot_injuries_cali <- ggplot(data = injuries_cali) +
  geom_mosaic(aes(x = product(accidente, fallecido), fill = accidente)) +
  guides(x = guide_axis(n.dodge = 3), y = guide_axis(n.dodge = 2), 
         fill = guide_legend(title = "Vehículo accidente", reverse = TRUE)) +
  labs(title = "Cali", x = "Vehículo fallecido", y = "Vehículo accidente") +
  theme_bw() #+
#theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1)) 
plot_injuries_cali

#' Santiago
names(injuries_santiago)
unique(injuries_santiago$cas_mode)
injuries_santiago <- injuries_santiago %>% 
  mutate(cas_mode = ifelse(is.na(cas_mode), "unknown", cas_mode),
    accidente = case_when(
    strike_mode == "NOV" ~ "Objetivo fijo",
    strike_mode == "bus" ~ "Bus",
    strike_mode == "car" ~ "Carro",
    strike_mode == "cycle" ~ "Bicicleta",
    strike_mode == "motorcycle" ~ "Moto",
    strike_mode == "pedestrian" ~ "Peatón",
    strike_mode == "truck" ~ "Camión",
    TRUE ~ strike_mode), 
    fallecido = factor(case_when(
      cas_mode == "bus" ~ "Bus",
      cas_mode == "car" ~ "Carro",
      cas_mode == "cycle" ~ "Bicicleta",
      cas_mode == "motorcycle" ~ "Moto",
      cas_mode == "pedestrian" ~ "Peatón",
      cas_mode == "truck" ~ "Camión",
      cas_mode == "unknown" ~ "Desconocido",
      cas_mode == "other" ~ "Otro",
      TRUE ~ cas_mode), levels = c("Bus", "Bicicleta", "Camión", "Moto",
                                   "Carro", "Peatón", "Desconocido", "Otro")))

table(injuries_santiago$cas_mode, injuries_santiago$fallecido)
table(injuries_santiago$strike_mode, injuries_santiago$accidente)

table(injuries_santiago$fallecido, injuries_santiago$accidente)
plot_injuries_santiago <- ggplot(data = injuries_santiago) +
  geom_mosaic(aes(x = product(accidente, fallecido), fill = accidente)) +
  guides(x = guide_axis(n.dodge = 3), y = guide_axis(n.dodge = 2), 
         fill = guide_legend(title = "Vehículo accidente", reverse = TRUE)) +
  labs(title = "Santiago", x = "Vehículo fallecido", y = "Vehículo accidente") +
  theme_bw() #+
#theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1)) 
plot_injuries_santiago

#' Mexico
names(injuries_mexico)
unique(injuries_mexico$strike_mode)
injuries_mexico <- injuries_mexico %>% 
  mutate(accidente = case_when(
           strike_mode == "pedestrian" ~ "Peatón",
           strike_mode == "cycle" ~ "Bicicleta",
           strike_mode == "motorcycle" ~ "Moto",
           strike_mode == "car" ~ "Carro",
           strike_mode == "bus" ~ "Bus",
           strike_mode == "truck" ~ "Camión",
           TRUE ~ strike_mode), 
         fallecido = factor(case_when(
           cas_mode == "cycle" ~ "Bicicleta",
           cas_mode == "motorcycle" ~ "Moto",
           cas_mode == "bus" ~ "Bus",
           cas_mode == "other" ~ "Otro",
           cas_mode == "pedestrian" ~ "Peatón",
           cas_mode == "car" ~ "Carro",
           cas_mode == "truck" ~ "Camión",
           TRUE ~ cas_mode), levels = c("Bus", "Bicicleta", "Camión", "Moto",
                                        "Carro", "Peatón", "Otro")))

table(injuries_mexico$cas_mode, injuries_mexico$fallecido)
table(injuries_mexico$strike_mode, injuries_mexico$accidente)

table(injuries_mexico$fallecido, injuries_mexico$accidente)
plot_injuries_mexico <- ggplot(data = injuries_mexico) +
  geom_mosaic(aes(x = product(accidente, fallecido), fill = accidente)) +
  guides(x = guide_axis(n.dodge = 3), y = guide_axis(n.dodge = 2), 
         fill = guide_legend(title = "Vehículo accidente", reverse = TRUE)) +
  labs(title = "Ciudad de México", x = "Vehículo fallecido", y = "Vehículo accidente") +
  theme_bw() #+
#theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1)) 
plot_injuries_mexico

# Multiple plots in one
plot_grid(plot_injuries_bogota, plot_injuries_medellin, plot_injuries_cali, 
          plot_injuries_santiago, plot_injuries_mexico, ncol = 2, nrow = 3)
# La exporte en tamaño 900x600

plot_grid(plot_injuries_bogota, plot_injuries_medellin, plot_injuries_cali, 
          plot_injuries_santiago, plot_injuries_mexico, ncol = 1, nrow = 5)
