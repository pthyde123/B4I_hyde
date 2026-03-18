#this script assign subplot biomass sampling time points
#input -- PM3D_randomization
#output -- PM3D subplots and biomass sampling designation
#details --  each plot should be sampled once in every time point but the individual subplot should be randomly selected. 



library(tidyverse)
library(readr)
library(readr)



PM3D_randomization <- read_csv("data/B4I_2026/PM3D_randomization.csv")


PM3D <-PM3D_randomization %>%
  slice(rep(row_number(), each = 3)) %>%  # repeat rows
  group_by(across(everything())) %>%
  mutate(subplotNumber = row_number()) %>%
  ungroup() %>% 
  rename(locationName = environment) %>% 
  rename(plotNumber = plot_number) 



runif(1)*10000
set.seed(3537) # set seed once


B4I_2026_PM3D_biomass_timepoints <- PM3D %>% # start with PM3D trials, only subplots

  
  mutate(rand = runif(300)) %>% # add a random number to each subplot
  
  arrange(locationName,plotNumber,rand) %>%   # arrange by random within location and plot
  
  mutate( biomass_timepoint = rep(c( 1,2,3), times = (100))) %>%  # assign sample biomass sample 1, 2, or 3
  
  arrange(locationName,plotNumber,subplotNumber) %>% # put them back in location, plot, subplot order
  
  mutate(plot.subplot = str_c(plotNumber,".",subplotNumber)) %>% 
  
  select(locationName,plot.subplot,biomass_timepoint) %>% # pull out the unique id's and time point information 
  
  mutate(biomass_T1 = if_else(biomass_timepoint == 1,1,NA)) %>% # set up management factor format
  mutate(biomass_T2 = if_else(biomass_timepoint == 2,1,NA)) %>%
  mutate(biomass_T3 = if_else(biomass_timepoint == 3,1,NA)) 


B4I_2026_PM3D_biomass_timepoints


## write.csv(B4I_2026_PM3D_biomass_timepoints, "data/B4I_2026/2026_PM3D_biomass_timepoints.csv",row.names = F)



















