library(ithimr)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)
rm(list=ls())
# cities <- c('accra', 'bangalore', 'belo_horizonte', 'bogota', 'buenos_aires', 'cape_town',
#             'delhi', 'mexico_city', 'santiago', 'sao_paulo', 'vizag', 'medellin_wb')

cities <- c("bogota_wb2", "medellin_wb", "cali_wb", 
            "santiago_wb", "mexico_city_wb")

min_age <- 15
max_age <- 69

all_inputs <- read.csv('all_city_parameter_inputs_wb.csv',stringsAsFactors = F)

#all_inputs$cape_town <- all_inputs$accra
#all_inputs$vizag <- all_inputs$sao_paulo

parameter_names <- all_inputs$parameter
parameter_starts <- which(parameter_names!='')
parameter_stops <- c(parameter_starts[-1] - 1, nrow(all_inputs))
parameter_names <- parameter_names[parameter_names!='']
parameter_list <- list()
compute_mode <- 'constant'
for (i in 1:length(parameter_names)) {
  parameter_list[[parameter_names[i]]] <- list()
  parameter_index <- which(all_inputs$parameter==parameter_names[i])
  if(all_inputs[parameter_index,2]=='')  {
    parameter_list[[parameter_names[i]]] <- lapply(cities,function(x) {
      city_index <- which(colnames(all_inputs)==x)
      val <- all_inputs[parameter_index,city_index]
      ifelse(val%in%c('T','F'),val,as.numeric(val))
    })
    names(parameter_list[[parameter_names[i]]]) <- cities
  }else if(all_inputs[parameter_index,2]=='constant'){
    indices <- 0
    if(compute_mode=='sample') indices <- 1:2
    parameter_list[[parameter_names[i]]] <- lapply(cities,function(x) {
      city_index <- which(colnames(all_inputs)==x)
      val <- all_inputs[parameter_index+indices,city_index]
      ifelse(val=='',0,as.numeric(val))
    })
    names(parameter_list[[parameter_names[i]]]) <- cities
  }else{
    parameter_list[[parameter_names[i]]] <- lapply(cities,function(x) {
      city_index <- which(colnames(all_inputs)==x)
      if(any(all_inputs[parameter_starts[i]:parameter_stops[i],city_index]!='')){
        sublist_indices <- which(all_inputs[parameter_starts[i]:parameter_stops[i],city_index]!='')
        thing <- as.list(as.numeric(c(all_inputs[parameter_starts[i]:parameter_stops[i],city_index])[sublist_indices]))
        names(thing) <- c(all_inputs[parameter_starts[i]:parameter_stops[i],2])[sublist_indices]
        thing
      }
    }
    )
    names(parameter_list[[parameter_names[i]]]) <- cities
  }
}

for(i in 1:length(parameter_list)) assign(names(parameter_list)[i],parameter_list[[i]])


###changed the bangalore transport emissions-- MC emissions from 1757 to 817 and car emissions from 4173 to 1107
##this is done based on ratio of car/MC ownership in bangalore to that of delhi from Census data (0.50 and 0.58 respectively)==
###1757=0.58*1409 and 1107=  0.50*2214
#################################################################
## run diagnostics
#for(city in cities){
#  ithim_object <- run_ithim_setup(ADD_TRUCK_DRIVERS = F,ADD_BUS_DRIVERS = F,CITY=city,MAX_MODE_SHARE_SCENARIO=T,DIST_CAT = c("0-1 km", "2-5 km", "6+ km"),PM_emission_inventory = PM_emission_inventories[[city]])
#  summarise_ithim_inputs(ithim_object)
#}


##################################################################

# constant parameters for DAY_TO_WEEK_TRAVEL_SCALAR
day_to_week_scalar <- 7

# constant parameters for MMET_CYCLING
#mmet_cycling <- 4.63
mmet_cycling <- 5.8
# constant parameters for MMET_WALKING
#mmet_walking <- 2.53
mmet_walking <- 2.5
# constant parameters for SIN_EXPONENT_SUM
sin_exponent_sum <- 1.7
# constant parameters for CASUALTY_EXPONENT_FRACTION
cas_exponent <- 0.5
# add mc fleet to sp

#################################################
## without uncertainty
toplot <- matrix(0,nrow=5,ncol=length(cities)) #5 scenarios, 4 cities
ithim_objects <- list()
for (city in cities) {
  print(city)
  ithim_objects[[city]] <- run_ithim_setup(DIST_CAT = c("0-1 km", "2-5 km", "6+ km"),
                                  ADD_WALK_TO_BUS_TRIPS = as.logical(add_walk_to_bus_trips[[city]]),
                                  CITY = city,
                                  WB = TRUE, # Is for the wB?
                                  AGE_RANGE = c(min_age,max_age),
                                  ADD_TRUCK_DRIVERS = F,
                                  MAX_MODE_SHARE_SCENARIO = T,
                                  ADD_BUS_DRIVERS = F,
                                  ADD_MOTORCYCLE_FLEET = as.logical(add_motorcycle_fleet[[city]]),
                                  PM_emission_inventory = PM_emission_inventories[[city]],
                                  CO2_emission_inventory = CO2_emission_inventories[[city]],
                                  speeds = speeds[[city]],
                                  
                                  FLEET_TO_MOTORCYCLE_RATIO = fleet_to_motorcycle_ratio[[city]],
                                  MMET_CYCLING = mmet_cycling, 
                                  MMET_WALKING = mmet_walking, 
                                  DAY_TO_WEEK_TRAVEL_SCALAR = day_to_week_scalar,
                                  SIN_EXPONENT_SUM= sin_exponent_sum,
                                  CASUALTY_EXPONENT_FRACTION = cas_exponent,
                                  PA_DOSE_RESPONSE_QUANTILE = F,  
                                  AP_DOSE_RESPONSE_QUANTILE = F,
                                  INJURY_REPORTING_RATE = injury_reporting_rate[[city]],  
                                  CHRONIC_DISEASE_SCALAR = chronic_disease_scalar[[city]],  
                                  PM_CONC_BASE = pm_conc_base[[city]],  
                                  PM_TRANS_SHARE = pm_trans_share[[city]],  
                                  BACKGROUND_PA_SCALAR = background_pa_scalar[[city]],
                                  BUS_WALK_TIME = bus_walk_time[[city]])
  ithim_objects$scen_prop <- SCENARIO_PROPORTIONS
  ithim_objects[[city]]$demographic <- DEMOGRAPHIC
  ithim_objects[[city]]$synth_pop <- SYNTHETIC_POPULATION
  ithim_objects[[city]]$outcomes <- run_ithim(ithim_object=ithim_objects[[city]], seed = 1)
  ithim_objects[[city]]$disease_burden <- DISEASE_BURDEN
  ithim_objects[[city]]$PM_emission_inventory <- PM_EMISSION_INVENTORY
  ithim_objects[[city]]$injury_table <- INJURY_TABLE
  
  ## store results to plot
  min_ages <- sapply(ithim_objects[[city]]$outcome$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
  max_ages <- sapply(ithim_objects[[city]]$outcome$hb$ylls$age_cat,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
  sub_outcome <- subset(ithim_objects[[city]]$outcome$hb$ylls,min_ages>=min_age&max_ages<=max_age)
  result_mat <- colSums(sub_outcome[,3:ncol(sub_outcome)])
  columns <- length(result_mat)
  nDiseases <- columns/NSCEN
  if(city==cities[1]){
    disease_list <- list()
    for(i in 1:nDiseases) disease_list[[i]] <- toplot
  }
  min_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][1]))
  max_pop_ages <- sapply(DEMOGRAPHIC$age,function(x)as.numeric(strsplit(x,'-')[[1]][2]))
  for(i in 1:nDiseases)
    disease_list[[i]][,which(cities==city)] <- result_mat[1:NSCEN + (i - 1) * NSCEN]/sum(subset(DEMOGRAPHIC,min_pop_ages>=min_age&max_pop_ages<=max_age)$population)
}

#save.image("../io_wb.RData")
#load("../io_wb.RData")

# Getting main results ----
#' Resulting target scenarios
round(ithim_objects$scen_prop,1)

#####
#' Mode share in each scenario
names(ithim_objects$bogota_wb2$trip_scen_sets)
View(ithim_objects$bogota_wb2$trip_scen_sets)
unique(ithim_objects$bogota_wb2$trip_scen_sets$stage_mode)

# Total number of trips per scenario, it changes because of walking trips
table(ithim_objects$bogota_wb2$trip_scen_sets$scenario)
# Mode share. I pasted this in SoporteDocumento to get nice tables in the document
# Bogota
aux = ithim_objects$bogota_wb2$trip_scen_sets %>% 
  filter(stage_mode != "walk_to_pt")
write.table(table(aux$trip_mode,aux$scenario), 
      "clipboard", sep = "\t", row.names = T)
#####
# Medellin
aux = ithim_objects$medellin_wb$trip_scen_sets %>% 
  filter(stage_mode != "walk_to_pt")
write.table(table(aux$trip_mode,aux$scenario), 
            "clipboard", sep = "\t", row.names = T)

# Cali
aux = ithim_objects$cali_wb$trip_scen_sets %>% 
  filter(stage_mode != "walk_to_pt")
write.table(table(aux$trip_mode,aux$scenario), 
            "clipboard", sep = "\t", row.names = T)
# Santiago
aux = ithim_objects$santiago_wb$trip_scen_sets %>% 
  filter(stage_mode != "walk_to_pt")
write.table(table(aux$trip_mode,aux$scenario), 
            "clipboard", sep = "\t", row.names = T)

# Mexico city
aux = ithim_objects$mexico_city_wb$trip_scen_sets %>% 
  filter(stage_mode != "walk_to_pt")
write.table(table(aux$trip_mode,aux$scenario), 
            "clipboard", sep = "\t", row.names = T)


#####
#' Distance travelled
ithim_objects$bogota_wb2$dist
write.table(ithim_objects$bogota_wb2$dist, "clipboard", sep="\t")
names(ithim_objects$bogota_wb2$dist)
# Bar plot
distance_long <- ithim_objects$bogota_wb2$dist %>% 
  gather("Escenario", "value", "Baseline", "Scenario 1", "Scenario 2",
         "Scenario 3", "Scenario 4", "Scenario 5", -stage_mode) %>% 
  filter(stage_mode != "bus_driver") %>% 
  mutate(Escenario = factor(case_when(
    Escenario == "Baseline" ~ "Referencia",
    Escenario == "Scenario 1" ~ "A pie",
    Escenario == "Scenario 2" ~ "Bicicleta",
    Escenario == "Scenario 3" ~ "Carro",
    Escenario == "Scenario 4" ~ "Moto",
    Escenario == "Scenario 5" ~ "Bus",
    TRUE ~ "Other"), 
    levels = c("Referencia", "A pie", "Bicicleta", "Carro", 
               "Moto", "Bus")),
    Modo = factor(case_when(
      stage_mode == "pedestrian" ~ "A pie",
      stage_mode == "car" ~ "Carro",
      stage_mode == "cycle" ~ "Bicicleta",
      stage_mode == "taxi" ~ "Taxi",
      stage_mode == "motorcycle" ~ "Moto",
      stage_mode == "bus" ~ "Bus",
      stage_mode %in% c("rail", "auto_rickshaw") ~ "Otro",
      TRUE ~ "Other"), 
      levels = c("Bus", "Carro", "Bicicleta", "Moto", "A pie", "Taxi",
                 "Otro")))
    
ggplot(distance_long, aes(x = Escenario, y = value)) + 
  geom_bar(aes(fill = Modo), stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Escenario", y = "Distancia total (km)", color = "Ciudad") + 
  theme_bw() + 
  theme(legend.position = "right")  


#' Distance by mode
ithim_objects$bogota_wb2$dur

#' Distance by mode
ithim_objects$bogota_wb2$inj_distances$reg_model$whw

#' Demographic: is the same as the input but it only has ages in the range specified, i.e., 15-69
View(ithim_objects$bogota_wb2$demographic)

#' Emission inventory
#' Is the same as the input 
ithim_objects$bogota_wb2$PM_emission_inventory

#' Demographic: is the same as the input but it only has ages in the range specified, i.e., 15-69
#' It has disease burden for the city, the rate is the same as in gbd input dataset, and the burden and population (columns) are for the city
View(ithim_objects$bogota_wb2$disease_burden)

#' Dimesions of synthetic population
dim(ithim_objects$bogota_wb2$synth_pop)

#' Results
#' Physical activity
#dim(ithim_objects$bogota_wb2$outcomes$mmets) # Same synthetic population
#View(ithim_objects$bogota_wb2$outcomes$mmets)
#apply(ithim_objects$bogota_wb2$outcomes$mmets[,5:ncol(ithim_objects$bogota_wb2$outcomes$mmets)], 2, sum)
(pa_results <- data.frame(cbind(bogota = apply(ithim_objects$bogota_wb2$outcomes$mmets[,5:ncol(ithim_objects$bogota_wb2$outcomes$mmets)], 2, mean),
      medellin = apply(ithim_objects$medellin_wb$outcomes$mmets[,5:ncol(ithim_objects$medellin_wb$outcomes$mmets)], 2, mean),
      cali = apply(ithim_objects$cali_wb$outcomes$mmets[,5:ncol(ithim_objects$cali_wb$outcomes$mmets)], 2, mean),
      santiago = apply(ithim_objects$santiago_wb$outcomes$mmets[,5:ncol(ithim_objects$santiago_wb$outcomes$mmets)], 2, mean),
      mexico = apply(ithim_objects$mexico_city_wb$outcomes$mmets[,5:ncol(ithim_objects$mexico_city_wb$outcomes$mmets)], 2, mean))))
pa_results$scenario <- factor(c("Referencia", "A pie", "Bicicleta", "Carro",
                         "Motocicleta", "Bus"), 
                         levels = c("Referencia", "A pie", "Bicicleta", "Carro",
                                    "Motocicleta", "Bus"))

# To add legends:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
ggplot(pa_results, aes(x = scenario, group = 1)) + 
  geom_line(aes(y = bogota, color = "darkred"), size = 1.2) + 
    #geom_point(aes(y = bogota), color = "darkred") +
  geom_line(aes(y = medellin, color = "steelblue"), size = 1.2) + 
    #geom_point(aes(y = medellin), color="steelblue") + 
  geom_line(aes(y = cali, color = "darkgreen"), size = 1.2) + 
  geom_line(aes(y = santiago, color = "darkolivegreen"), size = 1.2) + 
  geom_line(aes(y = mexico, color = "gold4"), size = 1.2) +
  labs(x = "Escenario", y = "mMETs", color = "Ciudad") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")  +
  scale_color_identity(name = "Ciudad", 
                       breaks = c("darkred", "steelblue", "darkgreen",
                                  "darkolivegreen", "gold4"),
                       labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                                  "Ciudad de \n México"), guide = "legend")
  
write.table(pa_results, "clipboard", sep = "\t")


# Bar plot
pa_results_long <- pa_results %>% gather("Ciudad", "value", bogota, medellin,
                                         cali, santiago,
                                         mexico, -scenario) %>% 
  mutate(Ciudad = factor(Ciudad, levels = c("bogota", "medellin",
                                                 "cali", "santiago",
                                                 "mexico")))
ggplot(pa_results_long, aes(x = scenario, y = value)) + 
  geom_bar(aes(fill = Ciudad), stat = "identity", position = "dodge") +
  scale_fill_manual(values =  c("darkred", "steelblue", "darkgreen",
                                "darkolivegreen", "gold4"),
                    labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                               "Ciudad de \n México")) +
  labs(x = "Escenario", y = "mMET", color = "Ciudad") + 
  theme_bw() + 
  theme(legend.position = "bottom")  
  
#' Air pollution
(pm_results <- data.frame(cbind(
  bogota = ithim_objects$bogota_wb2$outcomes$scenario_pm,
  medellin = ithim_objects$medellin_wb$outcomes$scenario_pm,
  cali = ithim_objects$cali_wb$outcomes$scenario_pm,
  santiago = ithim_objects$santiago_wb$outcomes$scenario_pm,
  mexico = ithim_objects$mexico_city_wb$outcomes$scenario_pm)))
pm_results$scenario <- factor(c("Referencia", "A pie", "Bicicleta", "Carro",
                                "Motocicleta", "Bus"), 
                              levels = c("Referencia", "A pie", "Bicicleta",
                                         "Carro", "Motocicleta", "Bus"))

# To add legends:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
ggplot(pm_results, aes(x = scenario, group = 1)) + 
  geom_line(aes(y = bogota, color = "darkred"), size = 1.2) + 
  #geom_point(aes(y = bogota), color = "darkred") +
  geom_line(aes(y = medellin, color = "steelblue"), size = 1.2) + 
  #geom_point(aes(y = medellin), color="steelblue") + 
  geom_line(aes(y = cali, color = "darkgreen"), size = 1.2) + 
  geom_line(aes(y = santiago, color = "darkolivegreen"), size = 1.2) + 
  geom_line(aes(y = mexico, color = "gold4"), size = 1.2) +
  labs(x = "Escenario", y = "PM2.5", color = "Ciudad") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")  +
  scale_color_identity(name = "Ciudad", 
                       breaks = c("darkred", "steelblue", "darkgreen",
                                  "darkolivegreen", "gold4"),
                       labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                                  "Ciudad de \n México"), guide = "legend")

write.table(pm_results, "clipboard", sep = "\t")

# Bar plot
pm_results_long <- pm_results %>% gather("Ciudad", "value", bogota, medellin,
                                         cali, santiago,
                                         mexico, -scenario) %>% 
  mutate(Ciudad = factor(Ciudad, levels = c("bogota", "medellin",
                                            "cali", "santiago",
                                            "mexico")))
ggplot(pm_results_long, aes(x = scenario, y = value)) + 
  geom_bar(aes(fill = Ciudad), stat = "identity", position = "dodge") +
  scale_fill_manual(values =  c("darkred", "steelblue", "darkgreen",
                                "darkolivegreen", "gold4"),
                    labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                               "Ciudad de \n México")) +
  labs(x = "Escenario", y = "PM2.5", color = "Ciudad") + 
  theme_bw() + 
  theme(legend.position = "bottom")  

# Deaths by PA
# Number of deaths by cause and type (AP and PA)
View(ithim_objects$bogota_wb2$outcomes$pathway_hb$deaths)
View(ithim_objects$bogota_wb2$demographic)
bogota_d <- ithim_objects$bogota_wb2$outcomes$pathway_hb$deaths
bogota_d_total <- colSums(bogota_d[,3:ncol(bogota_d)])

#Lineas punteadas y poner los puntos en las graficas
#Cada barra es una ciudad diferente

pa_deaths <- data.frame(Escenario = c("A pie","Bicicleta", 
                                      "Carro", "Motocicleta", "Bus"),
                        bogota = rep(0,5), medellin = rep(0,5),
                        cali = rep(0,5), santiago = rep(0,5),
                        mexico = rep(0,5))
ap_deaths <- inj_deaths <- pa_deaths

pa_deaths_rate <- data.frame(Escenario = c("A pie","Bicicleta", "Carro",
                                           "Motocicleta", "Bus"),
                             bogota = rep(0,5), medellin = rep(0,5),
                             cali = rep(0,5), santiago = rep(0,5),
                             mexico = rep(0,5))
ap_deaths_rate <- inj_deaths_rate <- pa_deaths_rate

for (j in 1:length(cities)) { #Number of cities
  #print(ithim_objects[[cities[j]]]$PM_emission_inventory)
  #j=1
  city_deaths <- ithim_objects[[cities[j]]]$outcomes$pathway_hb$deaths
  city_deaths_total <- colSums(city_deaths[,3:ncol(city_deaths)])
# Save total population in age ranges needed
  city_dem <- ithim_objects[[cities[j]]]$demographic  
  min_pop_ages <- sapply(city_dem$age, function(x) 
    as.numeric(strsplit(x,'-')[[1]][1]))
  max_pop_ages <- sapply(city_dem$age,function(x)
    as.numeric(strsplit(x,'-')[[1]][2]))
  city_population <- sum(subset(city_dem,
                                min_pop_ages >= min_age & 
                                  max_pop_ages <= max_age)$population)
  for (i in 1:NSCEN) { # Number of scenarios
    # print(paste0("scen",i,"_deaths_pa"))
    # i=1
    pa_cols <- which(grepl(paste0("scen",i,"_deaths_pa"), 
                           names(city_deaths_total)))
    ap_cols <- which(grepl(paste0("scen",i,"_deaths_ap"), 
                           names(city_deaths_total)))
    inj_cols <- which(grepl(paste0("scen",i,"_deaths_inj"), 
                            names(city_deaths_total)))
    # print(pa_cols)
    #print(paste0("i = ", i,", j=", j+1))
    pa_deaths[i, j + 1] <- sum(city_deaths_total[pa_cols])
    pa_deaths_rate[i, j + 1] <- sum(city_deaths_total[pa_cols]) / city_population * 100000
    ap_deaths[i, j + 1] <- sum(city_deaths_total[ap_cols])
    ap_deaths_rate[i, j + 1] <- sum(city_deaths_total[ap_cols]) / city_population* 100000
    inj_deaths[i, j + 1] <- sum(city_deaths_total[inj_cols])
    inj_deaths_rate[i, j + 1] <- sum(city_deaths_total[inj_cols]) / city_population * 100000
  }
}
# pa_deaths
# ap_deaths
# inj_deaths

# Haciendo el loop anterior a mano
# pa_cols <- which(grepl("scen1_deaths_pa", names(bogota_d_total)))
# ap_cols <- which(grepl("scen1_deaths_ap", names(bogota_d_total)))
# inj_cols <- which(grepl("scen1_deaths_inj", names(bogota_d_total)))
# 
# 
# medellin_d <- ithim_objects$medellin_wb$outcomes$pathway_hb$deaths
# medellin_d_total <- colSums(medellin_d[,3:ncol(medellin_d)])
# pa_cols <- which(grepl("scen1_deaths_pa", names(medellin_d_total)))
# sum(medellin_d_total[pa_cols])
# 
# mexico_d <- ithim_objects$mexico_city_wb$outcomes$pathway_hb$deaths
# mexico_d_total <- colSums(mexico_d[,3:ncol(mexico_d)])
# pa_cols <- which(grepl("scen1_deaths_pa", names(mexico_d_total)))
# sum(mexico_d_total[pa_cols])
# 
# View(ithim_objects$mexico_city_wb$outcomes$pathway_hb$deaths)

# Deaths - Physical activity
pa_deaths$scenario <- factor(pa_deaths$Escenario, 
                              levels = c("A pie", "Bicicleta",
                                         "Carro", "Motocicleta", "Bus"))

# To add legends:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
ggplot(pa_deaths, aes(x = scenario, group = 1)) + 
  geom_line(aes(y = bogota, color = "darkred"), size = 1.2) + 
  #geom_point(aes(y = bogota), color = "darkred") +
  geom_line(aes(y = medellin, color = "steelblue"), size = 1.2) + 
  #geom_point(aes(y = medellin), color="steelblue") + 
  geom_line(aes(y = cali, color = "darkgreen"), size = 1.2) + 
  geom_line(aes(y = santiago, color = "darkolivegreen"), size = 1.2) + 
  geom_line(aes(y = mexico, color = "gold4"), size = 1.2) +
  labs(x = "Escenario", y = "Muertes prematuras evitadas", color = "Ciudad") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")  +
  scale_color_identity(name = "Ciudad", 
                       breaks = c("darkred", "steelblue", "darkgreen",
                                  "darkolivegreen", "gold4"),
                       labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                                  "Ciudad de \n México"), guide = "legend")

# Bar plot
pa_deaths_long <- pa_deaths %>% gather("Ciudad", "value", bogota, medellin,
                                         cali, santiago,
                                         mexico, -scenario) %>% 
  mutate(Ciudad = factor(Ciudad, levels = c("bogota", "medellin",
                                            "cali", "santiago",
                                            "mexico")))
ggplot(pa_deaths_long, aes(x = scenario, y = value)) + 
  geom_bar(aes(fill = Ciudad), stat = "identity", position = "dodge") +
  scale_fill_manual(values =  c("darkred", "steelblue", "darkgreen",
                                "darkolivegreen", "gold4"),
                    labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                               "Ciudad de \n México")) +
  labs(x = "Escenario", y = "Muertes prematuras evitadas", color = "Ciudad") + 
  theme_bw() + 
  theme(legend.position = "bottom")  

# Death rate by 100.000
pa_deaths_rate$scenario <- factor(pa_deaths_rate$Escenario, 
                             levels = c("A pie", "Bicicleta",
                                        "Carro", "Motocicleta", "Bus"))
write.table(pa_deaths_rate, "clipboard", sep = "\t")
# To add legends:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
ggplot(pa_deaths_rate, aes(x = scenario, group = 1)) + 
  geom_line(aes(y = bogota, color = "darkred"), size = 1.2) + 
  #geom_point(aes(y = bogota), color = "darkred") +
  geom_line(aes(y = medellin, color = "steelblue"), size = 1.2) + 
  #geom_point(aes(y = medellin), color="steelblue") + 
  geom_line(aes(y = cali, color = "darkgreen"), size = 1.2) + 
  geom_line(aes(y = santiago, color = "darkolivegreen"), size = 1.2) + 
  geom_line(aes(y = mexico, color = "gold4"), size = 1.2) +
  labs(x = "Escenario", y = "Muertes prematuras evitadas (x 100.000 hab.)", color = "Ciudad") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")  +
  scale_color_identity(name = "Ciudad", 
                       breaks = c("darkred", "steelblue", "darkgreen",
                                  "darkolivegreen", "gold4"),
                       labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                                  "Ciudad de \n México"), guide = "legend")

# Bar plot
pa_deaths_rate_long <- pa_deaths_rate %>% gather("Ciudad", "value", bogota,
                                                 medellin,
                                       cali, santiago,
                                       mexico, -scenario) %>% 
  mutate(Ciudad = factor(Ciudad, levels = c("bogota", "medellin",
                                            "cali", "santiago",
                                            "mexico")))
ggplot(pa_deaths_rate_long, aes(x = scenario, y = value)) + 
  geom_bar(aes(fill = Ciudad), stat = "identity", position = "dodge") +
  scale_fill_manual(values =  c("darkred", "steelblue", "darkgreen",
                                "darkolivegreen", "gold4"),
                    labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                               "Ciudad de \n México")) +
  labs(x = "Escenario", y = "Muertes prematuras evitadas (x 100.000 hab.)", 
       color = "Ciudad") + 
  theme_bw() + 
  theme(legend.position = "bottom") 


# Deaths - Air Pollution
ap_deaths$scenario <- factor(ap_deaths$Escenario, 
                             levels = c("A pie", "Bicicleta",
                                        "Carro", "Motocicleta", "Bus"))

# To add legends:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
ggplot(ap_deaths, aes(x = scenario, group = 1)) + 
  geom_line(aes(y = bogota, color = "darkred"), size = 1.2) + 
  #geom_point(aes(y = bogota), color = "darkred") +
  geom_line(aes(y = medellin, color = "steelblue"), size = 1.2) + 
  #geom_point(aes(y = medellin), color="steelblue") + 
  geom_line(aes(y = cali, color = "darkgreen"), size = 1.2) + 
  geom_line(aes(y = santiago, color = "darkolivegreen"), size = 1.2) + 
  geom_line(aes(y = mexico, color = "gold4"), size = 1.2) +
  labs(x = "Escenario", y = "Muertes prematuras evitadas", color = "Ciudad") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")  +
  scale_color_identity(name = "Ciudad", 
                       breaks = c("darkred", "steelblue", "darkgreen",
                                  "darkolivegreen", "gold4"),
                       labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                                  "Ciudad de \n México"), guide = "legend")

# Bar plot
ap_deaths_long <- ap_deaths %>% gather("Ciudad", "value", bogota, medellin,
                                       cali, santiago,
                                       mexico, -scenario) %>% 
  mutate(Ciudad = factor(Ciudad, levels = c("bogota", "medellin",
                                            "cali", "santiago",
                                            "mexico")))
ggplot(ap_deaths_long, aes(x = scenario, y = value)) + 
  geom_bar(aes(fill = Ciudad), stat = "identity", position = "dodge") +
  scale_fill_manual(values =  c("darkred", "steelblue", "darkgreen",
                                "darkolivegreen", "gold4"),
                    labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                               "Ciudad de \n México")) +
  labs(x = "Escenario", y = "Muertes prematuras evitadas", color = "Ciudad") + 
  theme_bw() + 
  theme(legend.position = "bottom")

# Death rate by 100.000
ap_deaths_rate$scenario <- factor(ap_deaths_rate$Escenario, 
                                  levels = c("A pie", "Bicicleta",
                                             "Carro", "Motocicleta", "Bus"))
write.table(ap_deaths_rate, "clipboard", sep = "\t")
# To add legends:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
ggplot(ap_deaths_rate, aes(x = scenario, group = 1)) + 
  geom_line(aes(y = bogota, color = "darkred"), size = 1.2) + 
  #geom_point(aes(y = bogota), color = "darkred") +
  geom_line(aes(y = medellin, color = "steelblue"), size = 1.2) + 
  #geom_point(aes(y = medellin), color="steelblue") + 
  geom_line(aes(y = cali, color = "darkgreen"), size = 1.2) + 
  geom_line(aes(y = santiago, color = "darkolivegreen"), size = 1.2) + 
  geom_line(aes(y = mexico, color = "gold4"), size = 1.2) +
  labs(x = "Escenario", y = "Muertes prematuras evitadas (x 100.000 hab.)", color = "Ciudad") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")  +
  scale_color_identity(name = "Ciudad", 
                       breaks = c("darkred", "steelblue", "darkgreen",
                                  "darkolivegreen", "gold4"),
                       labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                                  "Ciudad de \n México"), guide = "legend")

# Bar plot
ap_deaths_rate_long <- ap_deaths_rate %>% gather("Ciudad", "value", bogota,
                                                 medellin,
                                                 cali, santiago,
                                                 mexico, -scenario) %>% 
  mutate(Ciudad = factor(Ciudad, levels = c("bogota", "medellin",
                                            "cali", "santiago",
                                            "mexico")))
ggplot(ap_deaths_rate_long, aes(x = scenario, y = value)) + 
  geom_bar(aes(fill = Ciudad), stat = "identity", position = "dodge") +
  scale_fill_manual(values =  c("darkred", "steelblue", "darkgreen",
                                "darkolivegreen", "gold4"),
                    labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                               "Ciudad de \n México")) +
  labs(x = "Escenario", y = "Muertes prematuras evitadas (x 100.000 hab.)", 
       color = "Ciudad") + 
  coord_cartesian(ylim = c(-0.5, 0.75)) + 
  theme_bw() + 
  theme(legend.position = "bottom") 


# Deaths - Injuries
inj_deaths$scenario <- factor(inj_deaths$Escenario, 
                             levels = c("A pie", "Bicicleta",
                                        "Carro", "Motocicleta", "Bus"))

# To add legends:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
ggplot(inj_deaths, aes(x = scenario, group = 1)) + 
  geom_line(aes(y = bogota, color = "darkred"), size = 1.2) + 
  #geom_point(aes(y = bogota), color = "darkred") +
  geom_line(aes(y = medellin, color = "steelblue"), size = 1.2) + 
  #geom_point(aes(y = medellin), color="steelblue") + 
  geom_line(aes(y = cali, color = "darkgreen"), size = 1.2) + 
  geom_line(aes(y = santiago, color = "darkolivegreen"), size = 1.2) + 
  geom_line(aes(y = mexico, color = "gold4"), size = 1.2) +
  labs(x = "Escenario", y = "Muertes prematuras evitadas", color = "Ciudad") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")  +
  scale_color_identity(name = "Ciudad", 
                       breaks = c("darkred", "steelblue", "darkgreen",
                                  "darkolivegreen", "gold4"),
                       labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                                  "Ciudad de \n México"), guide = "legend")

# Bar plot
inj_deaths_long <- inj_deaths %>% gather("Ciudad", "value", bogota, medellin,
                                       cali, santiago,
                                       mexico, -scenario) %>% 
  mutate(Ciudad = factor(Ciudad, levels = c("bogota", "medellin",
                                            "cali", "santiago",
                                            "mexico")))
ggplot(inj_deaths_long, aes(x = scenario, y = value)) + 
  geom_bar(aes(fill = Ciudad), stat = "identity", position = "dodge") +
  scale_fill_manual(values =  c("darkred", "steelblue", "darkgreen",
                                "darkolivegreen", "gold4"),
                    labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                               "Ciudad de \n México")) +
  labs(x = "Escenario", y = "Muertes prematuras evitadas", color = "Ciudad") + 
  theme_bw() + 
  theme(legend.position = "bottom")


# Death rate by 100.000
inj_deaths_rate$scenario <- factor(inj_deaths_rate$Escenario, 
                                  levels = c("A pie", "Bicicleta",
                                             "Carro", "Motocicleta", "Bus"))
write.table(inj_deaths_rate, "clipboard", sep = "\t")
# To add legends:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
ggplot(inj_deaths_rate, aes(x = scenario, group = 1)) + 
  geom_line(aes(y = bogota, color = "darkred"), size = 1.2) + 
  #geom_point(aes(y = bogota), color = "darkred") +
  geom_line(aes(y = medellin, color = "steelblue"), size = 1.2) + 
  #geom_point(aes(y = medellin), color="steelblue") + 
  geom_line(aes(y = cali, color = "darkgreen"), size = 1.2) + 
  geom_line(aes(y = santiago, color = "darkolivegreen"), size = 1.2) + 
  geom_line(aes(y = mexico, color = "gold4"), size = 1.2) +
  labs(x = "Escenario", y = "Muertes prematuras evitadas (x 100.000 hab.)", color = "Ciudad") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")  +
  scale_color_identity(name = "Ciudad", 
                       breaks = c("darkred", "steelblue", "darkgreen",
                                  "darkolivegreen", "gold4"),
                       labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                                  "Ciudad de \n México"), guide = "legend")

# Bar plot
inj_deaths_rate_long <- inj_deaths_rate %>% gather("Ciudad", "value", bogota,
                                                 medellin,
                                                 cali, santiago,
                                                 mexico, -scenario) %>% 
  mutate(Ciudad = factor(Ciudad, levels = c("bogota", "medellin",
                                            "cali", "santiago",
                                            "mexico")))
ggplot(inj_deaths_rate_long, aes(x = scenario, y = value)) + 
  geom_bar(aes(fill = Ciudad), stat = "identity", position = "dodge") +
  scale_fill_manual(values =  c("darkred", "steelblue", "darkgreen",
                                "darkolivegreen", "gold4"),
                    labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                               "Ciudad de \n México")) +
  labs(x = "Escenario", y = "Muertes prematuras evitadas (x 100.000 hab.)", 
       color = "Ciudad") + 
  coord_cartesian(ylim = c(-50, 5)) +
  theme_bw() + 
  theme(legend.position = "bottom") 

####
#' Total Deaths (sum AP, PA and Injuries)
total_deaths <- pa_deaths
total_deaths[,2:6] <- pa_deaths[,2:6] + ap_deaths[,2:6] + inj_deaths[,2:6]

# To add legends:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
ggplot(total_deaths, aes(x = scenario, group = 1)) + 
  geom_line(aes(y = bogota, color = "darkred"), size = 1.2) + 
  #geom_point(aes(y = bogota), color = "darkred") +
  geom_line(aes(y = medellin, color = "steelblue"), size = 1.2) + 
  #geom_point(aes(y = medellin), color="steelblue") + 
  geom_line(aes(y = cali, color = "darkgreen"), size = 1.2) + 
  geom_line(aes(y = santiago, color = "darkolivegreen"), size = 1.2) + 
  geom_line(aes(y = mexico, color = "gold4"), size = 1.2) +
  labs(x = "Escenario", y = "Muertes prematuras evitadas", color = "Ciudad") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")  +
  scale_color_identity(name = "Ciudad", 
                       breaks = c("darkred", "steelblue", "darkgreen",
                                  "darkolivegreen", "gold4"),
                       labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                                  "Ciudad de \n México"), guide = "legend")

# Bar plot
total_deaths_long <- total_deaths %>% gather("Ciudad", "value", bogota, medellin,
                                         cali, santiago,
                                         mexico, -scenario) %>% 
  mutate(Ciudad = factor(Ciudad, levels = c("bogota", "medellin",
                                            "cali", "santiago",
                                            "mexico")))
ggplot(total_deaths_long, aes(x = scenario, y = value)) + 
  geom_bar(aes(fill = Ciudad), stat = "identity", position = "dodge") +
  scale_fill_manual(values =  c("darkred", "steelblue", "darkgreen",
                                "darkolivegreen", "gold4"),
                    labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                               "Ciudad de \n México")) +
  labs(x = "Escenario", y = "Muertes prematuras evitadas", color = "Ciudad") + 
  theme_bw() + 
  theme(legend.position = "bottom")

# Death rate 
total_deaths_rate <- pa_deaths_rate
total_deaths_rate[,2:6] <- pa_deaths_rate[,2:6] + ap_deaths_rate[,2:6] +
  inj_deaths_rate[,2:6]
write.table(total_deaths_rate, "clipboard", sep = "\t")
# To add legends:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
ggplot(total_deaths_rate, aes(x = scenario, group = 1)) + 
  geom_line(aes(y = bogota, color = "darkred"), size = 1.2) + 
  #geom_point(aes(y = bogota), color = "darkred") +
  geom_line(aes(y = medellin, color = "steelblue"), size = 1.2) + 
  #geom_point(aes(y = medellin), color="steelblue") + 
  geom_line(aes(y = cali, color = "darkgreen"), size = 1.2) + 
  geom_line(aes(y = santiago, color = "darkolivegreen"), size = 1.2) + 
  geom_line(aes(y = mexico, color = "gold4"), size = 1.2) +
  labs(x = "Escenario", y = "Muertes prematuras evitadas (x 100.000 hab.)", color = "Ciudad") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")  +
  scale_color_identity(name = "Ciudad", 
                       breaks = c("darkred", "steelblue", "darkgreen",
                                  "darkolivegreen", "gold4"),
                       labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                                  "Ciudad de \n México"), guide = "legend")

# Bar plot
total_deaths_rate_long <- total_deaths_rate %>% gather("Ciudad", "value", bogota,
                                                   medellin,
                                                   cali, santiago,
                                                   mexico, -scenario) %>% 
  mutate(Ciudad = factor(Ciudad, levels = c("bogota", "medellin",
                                            "cali", "santiago",
                                            "mexico")))
ggplot(total_deaths_rate_long, aes(x = scenario, y = value)) + 
  geom_bar(aes(fill = Ciudad), stat = "identity", position = "dodge") +
  scale_fill_manual(values =  c("darkred", "steelblue", "darkgreen",
                                "darkolivegreen", "gold4"),
                    labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                               "Ciudad de \n México")) +
  labs(x = "Escenario", y = "Muertes prematuras evitadas (x 100.000 hab.)", 
       color = "Ciudad") + 
  coord_cartesian(ylim = c(-15, 15)) +
  theme_bw() + 
  theme(legend.position = "bottom") 



######
#' Years of life lost
#names(ithim_objects$bogota_wb2$outcomes$pathway_hb$ylls)
pa_ylls <- data.frame(Escenario = c("A pie","Bicicleta", 
                                      "Carro", "Motocicleta", "Bus"),
                        bogota = rep(0,5), medellin = rep(0,5),
                        cali = rep(0,5), santiago = rep(0,5),
                        mexico = rep(0,5))
ap_ylls <- inj_ylls <- pa_ylls

pa_ylls_rate <- data.frame(Escenario = c("A pie","Bicicleta", "Carro",
                                           "Motocicleta", "Bus"),
                             bogota = rep(0,5), medellin = rep(0,5),
                             cali = rep(0,5), santiago = rep(0,5),
                             mexico = rep(0,5))
ap_ylls_rate <- inj_ylls_rate <- pa_ylls_rate

for (j in 1:length(cities)) { #Number of cities
  #print(ithim_objects[[cities[j]]]$PM_emission_inventory)
  #j=1
  city_ylls <- ithim_objects[[cities[j]]]$outcomes$pathway_hb$ylls
  city_ylls_total <- colSums(city_ylls[,3:ncol(city_ylls)])
  # Save total population in age ranges needed
  city_dem <- ithim_objects[[cities[j]]]$demographic  
  min_pop_ages <- sapply(city_dem$age, function(x) 
    as.numeric(strsplit(x,'-')[[1]][1]))
  max_pop_ages <- sapply(city_dem$age,function(x)
    as.numeric(strsplit(x,'-')[[1]][2]))
  city_population <- sum(subset(city_dem,
                                min_pop_ages >= min_age & 
                                  max_pop_ages <= max_age)$population)
  for (i in 1:NSCEN) { # Number of scenarios
    # print(paste0("scen",i,"_deaths_pa"))
    # i=1
    pa_cols <- which(grepl(paste0("scen",i,"_ylls_pa"), 
                           names(city_ylls_total)))
    ap_cols <- which(grepl(paste0("scen",i,"_ylls_ap"), 
                           names(city_ylls_total)))
    inj_cols <- which(grepl(paste0("scen",i,"_yll_inj"), 
                            names(city_ylls_total)))
    # print(pa_cols)
    #print(paste0("i = ", i,", j=", j+1))
    pa_ylls[i, j + 1] <- sum(city_ylls_total[pa_cols])
    pa_ylls_rate[i, j + 1] <- sum(city_ylls_total[pa_cols]) / city_population * 100000
    ap_ylls[i, j + 1] <- sum(city_ylls_total[ap_cols])
    ap_ylls_rate[i, j + 1] <- sum(city_ylls_total[ap_cols]) / city_population * 100000
    inj_ylls[i, j + 1] <- sum(city_ylls_total[inj_cols])
    inj_ylls_rate[i, j + 1] <- sum(city_ylls_total[inj_cols]) / city_population * 100000
  }
}

# Ylls - Physical activity
pa_ylls$scenario <- factor(pa_ylls$Escenario, 
                           levels = c("A pie", "Bicicleta",
                                      "Carro", "Motocicleta", "Bus"))

# To add legends:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
ggplot(pa_ylls, aes(x = scenario, group = 1)) + 
  geom_line(aes(y = bogota, color = "darkred"), size = 1.2) + 
  #geom_point(aes(y = bogota), color = "darkred") +
  geom_line(aes(y = medellin, color = "steelblue"), size = 1.2) + 
  #geom_point(aes(y = medellin), color="steelblue") + 
  geom_line(aes(y = cali, color = "darkgreen"), size = 1.2) + 
  geom_line(aes(y = santiago, color = "darkolivegreen"), size = 1.2) + 
  geom_line(aes(y = mexico, color = "gold4"), size = 1.2) +
  labs(x = "Escenario", y = "Años de vida perdidos/ganados", color = "Ciudad") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")  +
  scale_color_identity(name = "Ciudad", 
                       breaks = c("darkred", "steelblue", "darkgreen",
                                  "darkolivegreen", "gold4"),
                       labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                                  "Ciudad de \n México"), guide = "legend")

# Bar plot
pa_ylls_long <- pa_ylls %>% gather("Ciudad", "value", bogota, medellin,
                                   cali, santiago,
                                   mexico, -scenario) %>% 
  mutate(Ciudad = factor(Ciudad, levels = c("bogota", "medellin",
                                            "cali", "santiago",
                                            "mexico")))
ggplot(pa_ylls_long, aes(x = scenario, y = value)) + 
  geom_bar(aes(fill = Ciudad), stat = "identity", position = "dodge") +
  scale_fill_manual(values =  c("darkred", "steelblue", "darkgreen",
                                "darkolivegreen", "gold4"),
                    labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                               "Ciudad de \n México")) +
  labs(x = "Escenario", y = "Años de vida perdidos/ganados", color = "Ciudad") + 
  theme_bw() + 
  theme(legend.position = "bottom")  

# Death rate by 100.000
pa_ylls_rate$scenario <- factor(pa_ylls_rate$Escenario, 
                                levels = c("A pie", "Bicicleta",
                                           "Carro", "Motocicleta", "Bus"))
write.table(pa_ylls_rate, "clipboard", sep = "\t")
# To add legends:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
ggplot(pa_ylls_rate, aes(x = scenario, group = 1)) + 
  geom_line(aes(y = bogota, color = "darkred"), size = 1.2) + 
  #geom_point(aes(y = bogota), color = "darkred") +
  geom_line(aes(y = medellin, color = "steelblue"), size = 1.2) + 
  #geom_point(aes(y = medellin), color="steelblue") + 
  geom_line(aes(y = cali, color = "darkgreen"), size = 1.2) + 
  geom_line(aes(y = santiago, color = "darkolivegreen"), size = 1.2) + 
  geom_line(aes(y = mexico, color = "gold4"), size = 1.2) +
  labs(x = "Escenario", y = "Años de vida perdidos/ganados (x 100.000 hab.)", color = "Ciudad") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")  +
  scale_color_identity(name = "Ciudad", 
                       breaks = c("darkred", "steelblue", "darkgreen",
                                  "darkolivegreen", "gold4"),
                       labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                                  "Ciudad de \n México"), guide = "legend")

# Bar plot
pa_ylls_rate_long <- pa_ylls_rate %>% gather("Ciudad", "value", bogota,
                                             medellin,
                                             cali, santiago,
                                             mexico, -scenario) %>% 
  mutate(Ciudad = factor(Ciudad, levels = c("bogota", "medellin",
                                            "cali", "santiago",
                                            "mexico")))
ggplot(pa_ylls_rate_long, aes(x = scenario, y = value)) + 
  geom_bar(aes(fill = Ciudad), stat = "identity", position = "dodge") +
  scale_fill_manual(values =  c("darkred", "steelblue", "darkgreen",
                                "darkolivegreen", "gold4"),
                    labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                               "Ciudad de \n México")) +
  labs(x = "Escenario", y = "Años de vida perdidos/ganados (x 100.000 hab.)", 
       color = "Ciudad") + 
  theme_bw() + 
  theme(legend.position = "bottom") 

####
# Ylls - Air pollution
ap_ylls$scenario <- factor(ap_ylls$Escenario, 
                           levels = c("A pie", "Bicicleta",
                                      "Carro", "Motocicleta", "Bus"))

# To add legends:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
ggplot(ap_ylls, aes(x = scenario, group = 1)) + 
  geom_line(aes(y = bogota, color = "darkred"), size = 1.2) + 
  #geom_point(aes(y = bogota), color = "darkred") +
  geom_line(aes(y = medellin, color = "steelblue"), size = 1.2) + 
  #geom_point(aes(y = medellin), color="steelblue") + 
  geom_line(aes(y = cali, color = "darkgreen"), size = 1.2) + 
  geom_line(aes(y = santiago, color = "darkolivegreen"), size = 1.2) + 
  geom_line(aes(y = mexico, color = "gold4"), size = 1.2) +
  labs(x = "Escenario", y = "Años de vida perdidos/ganados", color = "Ciudad") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")  +
  scale_color_identity(name = "Ciudad", 
                       breaks = c("darkred", "steelblue", "darkgreen",
                                  "darkolivegreen", "gold4"),
                       labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                                  "Ciudad de \n México"), guide = "legend")

# Bar plot
ap_ylls_long <- ap_ylls %>% gather("Ciudad", "value", bogota, medellin,
                                   cali, santiago,
                                   mexico, -scenario) %>% 
  mutate(Ciudad = factor(Ciudad, levels = c("bogota", "medellin",
                                            "cali", "santiago",
                                            "mexico")))
ggplot(ap_ylls_long, aes(x = scenario, y = value)) + 
  geom_bar(aes(fill = Ciudad), stat = "identity", position = "dodge") +
  scale_fill_manual(values =  c("darkred", "steelblue", "darkgreen",
                                "darkolivegreen", "gold4"),
                    labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                               "Ciudad de \n México")) +
  labs(x = "Escenario", y = "Años de vida perdidos/ganados", color = "Ciudad") + 
  theme_bw() + 
  theme(legend.position = "bottom")  

# Death rate by 100.000
ap_ylls_rate$scenario <- factor(ap_ylls_rate$Escenario, 
                                levels = c("A pie", "Bicicleta",
                                           "Carro", "Motocicleta", "Bus"))
write.table(ap_ylls_rate, "clipboard", sep = "\t")
# To add legends:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
ggplot(ap_ylls_rate, aes(x = scenario, group = 1)) + 
  geom_line(aes(y = bogota, color = "darkred"), size = 1.2) + 
  #geom_point(aes(y = bogota), color = "darkred") +
  geom_line(aes(y = medellin, color = "steelblue"), size = 1.2) + 
  #geom_point(aes(y = medellin), color="steelblue") + 
  geom_line(aes(y = cali, color = "darkgreen"), size = 1.2) + 
  geom_line(aes(y = santiago, color = "darkolivegreen"), size = 1.2) + 
  geom_line(aes(y = mexico, color = "gold4"), size = 1.2) +
  labs(x = "Escenario", y = "Años de vida perdidos/ganados (x 100.000 hab.)", color = "Ciudad") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")  +
  scale_color_identity(name = "Ciudad", 
                       breaks = c("darkred", "steelblue", "darkgreen",
                                  "darkolivegreen", "gold4"),
                       labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                                  "Ciudad de \n México"), guide = "legend")

# Bar plot
ap_ylls_rate_long <- ap_ylls_rate %>% gather("Ciudad", "value", bogota,
                                             medellin,
                                             cali, santiago,
                                             mexico, -scenario) %>% 
  mutate(Ciudad = factor(Ciudad, levels = c("bogota", "medellin",
                                            "cali", "santiago",
                                            "mexico")))
ggplot(ap_ylls_rate_long, aes(x = scenario, y = value)) + 
  geom_bar(aes(fill = Ciudad), stat = "identity", position = "dodge") +
  scale_fill_manual(values =  c("darkred", "steelblue", "darkgreen",
                                "darkolivegreen", "gold4"),
                    labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                               "Ciudad de \n México")) +
  labs(x = "Escenario", y = "Años de vida perdidos/ganados (x 100.000 hab.)", 
       color = "Ciudad") + 
  theme_bw() + 
  theme(legend.position = "bottom") 


######
# Ylls - Injuries
inj_ylls$scenario <- factor(inj_ylls$Escenario, 
                            levels = c("A pie", "Bicicleta",
                                       "Carro", "Motocicleta", "Bus"))

# To add legends:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
ggplot(inj_ylls, aes(x = scenario, group = 1)) + 
  geom_line(aes(y = bogota, color = "darkred"), size = 1.2) + 
  #geom_point(aes(y = bogota), color = "darkred") +
  geom_line(aes(y = medellin, color = "steelblue"), size = 1.2) + 
  #geom_point(aes(y = medellin), color="steelblue") + 
  geom_line(aes(y = cali, color = "darkgreen"), size = 1.2) + 
  geom_line(aes(y = santiago, color = "darkolivegreen"), size = 1.2) + 
  geom_line(aes(y = mexico, color = "gold4"), size = 1.2) +
  labs(x = "Escenario", y = "Años de vida perdidos/ganados", color = "Ciudad") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")  +
  scale_color_identity(name = "Ciudad", 
                       breaks = c("darkred", "steelblue", "darkgreen",
                                  "darkolivegreen", "gold4"),
                       labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                                  "Ciudad de \n México"), guide = "legend")

# Bar plot
inj_ylls_long <- inj_ylls %>% gather("Ciudad", "value", bogota, medellin,
                                     cali, santiago,
                                     mexico, -scenario) %>% 
  mutate(Ciudad = factor(Ciudad, levels = c("bogota", "medellin",
                                            "cali", "santiago",
                                            "mexico")))
ggplot(inj_ylls_long, aes(x = scenario, y = value)) + 
  geom_bar(aes(fill = Ciudad), stat = "identity", position = "dodge") +
  scale_fill_manual(values =  c("darkred", "steelblue", "darkgreen",
                                "darkolivegreen", "gold4"),
                    labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                               "Ciudad de \n México")) +
  labs(x = "Escenario", y = "Años de vida perdidos/ganados", color = "Ciudad") + 
  theme_bw() + 
  theme(legend.position = "bottom")  

# Death rate by 100.000
inj_ylls_rate$scenario <- factor(inj_ylls_rate$Escenario, 
                                 levels = c("A pie", "Bicicleta",
                                            "Carro", "Motocicleta", "Bus"))
write.table(inj_ylls_rate, "clipboard", sep = "\t")
# To add legends:https://aosmith.rbind.io/2018/07/19/manual-legends-ggplot2/
ggplot(inj_ylls_rate, aes(x = scenario, group = 1)) + 
  geom_line(aes(y = bogota, color = "darkred"), size = 1.2) + 
  #geom_point(aes(y = bogota), color = "darkred") +
  geom_line(aes(y = medellin, color = "steelblue"), size = 1.2) + 
  #geom_point(aes(y = medellin), color="steelblue") + 
  geom_line(aes(y = cali, color = "darkgreen"), size = 1.2) + 
  geom_line(aes(y = santiago, color = "darkolivegreen"), size = 1.2) + 
  geom_line(aes(y = mexico, color = "gold4"), size = 1.2) +
  labs(x = "Escenario", y = "Años de vida perdidos/ganados (x 100.000 hab.)", color = "Ciudad") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "bottom")  +
  scale_color_identity(name = "Ciudad", 
                       breaks = c("darkred", "steelblue", "darkgreen",
                                  "darkolivegreen", "gold4"),
                       labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                                  "Ciudad de \n México"), guide = "legend")

# Bar plot
inj_ylls_rate_long <- inj_ylls_rate %>% gather("Ciudad", "value", bogota,
                                               medellin,
                                               cali, santiago,
                                               mexico, -scenario) %>% 
  mutate(Ciudad = factor(Ciudad, levels = c("bogota", "medellin",
                                            "cali", "santiago",
                                            "mexico")))
ggplot(inj_ylls_rate_long, aes(x = scenario, y = value)) + 
  geom_bar(aes(fill = Ciudad), stat = "identity", position = "dodge") +
  scale_fill_manual(values =  c("darkred", "steelblue", "darkgreen",
                                "darkolivegreen", "gold4"),
                    labels = c("Bogotá", "Medellín", "Cali", "Santiago",
                               "Ciudad de \n México")) +
  labs(x = "Escenario", y = "Años de vida perdidos/ganados (x 100.000 hab.)", 
       color = "Ciudad") + 
  theme_bw() + 
  theme(legend.position = "bottom") 

#######################################

# Plots ----
{x11(width = 10, height = 5);
  layout.matrix <- matrix(c(2:6,1,7:12), nrow =2, ncol =6,byrow=T)
  graphics::layout(mat = layout.matrix,heights = c(2,3),widths = c(2.8,2,2,2,2,2.5))
  ylim <- range(result_mat)
  cols <- rainbow(length(cities))
  mar1 <- rep(7,nDiseases); mar1[1:6] <- 1
  mar2 <- rep(1,nDiseases); mar2[c(2,7)] <- 6; mar2[c(1,12)] <- 3
  for(i in 1:nDiseases){
    ylim <- if(i%in%c(1,12)) range(disease_list[[i]]) else c(-11,4)*1e-4
    par(mar = c(mar1[i], mar2[i], 4, 1))
      barplot(t(disease_list[[i]]), ylim = ylim, las = 2,beside=T,col=cols, names.arg=if(i<7) NULL else  rownames(SCENARIO_PROPORTIONS), 
              main = paste0(last(strsplit(names(result_mat)[i * NSCEN], '_')[[1]])),yaxt='n')
    if(i%in%c(2,1,7,12)) {axis(2,cex.axis=1.5); if(i%in%c(2,7)) mtext(side=2,'YLL gain per person',line=3)}
    if(i==nDiseases-1) legend(legend=cities,fill=cols,bty='n',y=-1e-5,x=5,cex=0.9)
  }
}



## Save the ithim_object in the results folder
##########

# saveRDS(ithim_objects, paste("results/multi_city/io", CAS_EXPONENT, STR_EXPONENT, ".rds", sep = "_"),version = 2)
#saveRDS(ithim_objects, "results/multi_city/io_wb.rds", version = 2)
