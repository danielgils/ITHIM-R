# Script to run individually each city (World Bank)
# October 2020

rm(list =ls())
library(ithimr)

city <- "medellin_wb"
min_age <- 15
max_age <- 69

# constant parameters for DAY_TO_WEEK_TRAVEL_SCALAR
day_to_week_scalar <- 7

# constant parameters for MMET_CYCLING
mmet_cycling <- 4.63
# constant parameters for MMET_WALKING
mmet_walking <- 2.53
# constant parameters for SIN_EXPONENT_SUM
sin_exponent_sum <- 1.7
# constant parameters for CASUALTY_EXPONENT_FRACTION
cas_exponent <- 0.5
# add mc fleet to sp

ithim_objects <- list()
# I took this parameters to be equal to Bogota, just for now
ithim_objects[[city]] <- run_ithim_setup(DIST_CAT = c("0-1 km", "2-5 km", "6+ km"),
                                         ADD_WALK_TO_BUS_TRIPS = T,
                                         CITY = city,
                                         AGE_RANGE = c(min_age,max_age),
                                         ADD_TRUCK_DRIVERS = F,
                                         MAX_MODE_SHARE_SCENARIO = T,
                                         ADD_BUS_DRIVERS = F,
                                         ADD_MOTORCYCLE_FLEET = T,
                                         PM_emission_inventory = NULL,
                                         CO2_emission_inventory = NULL,
                                         speeds = NULL,
                                         
                                         FLEET_TO_MOTORCYCLE_RATIO = 0.441176471,
                                         MMET_CYCLING = mmet_cycling, 
                                         MMET_WALKING = mmet_walking, 
                                         DAY_TO_WEEK_TRAVEL_SCALAR = day_to_week_scalar,
                                         SIN_EXPONENT_SUM= sin_exponent_sum,
                                         CASUALTY_EXPONENT_FRACTION = cas_exponent,
                                         PA_DOSE_RESPONSE_QUANTILE = F,  
                                         AP_DOSE_RESPONSE_QUANTILE = F,
                                         INJURY_REPORTING_RATE = 1,  
                                         CHRONIC_DISEASE_SCALAR = 1,  
                                         PM_CONC_BASE = 24,  
                                         PM_TRANS_SHARE = 0.417,  
                                         BACKGROUND_PA_SCALAR = 1,
                                         BUS_WALK_TIME = 10.55,
                                         
                                         PATH_TO_LOCAL_DATA = "C:/Users/danie/Documents/Daniel_Gil/Consultorias/2020/WorldBank/Data/Colombia/Medellin/Cleaned")
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

cities = city
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
