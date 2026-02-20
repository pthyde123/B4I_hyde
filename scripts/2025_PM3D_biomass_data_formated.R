
########
########

#############  Starting over with T3 exported data for all trials, 

library(readr)
data <- B4I_2025_PM3D_ALL_phenotypes <- read_csv("data/PM3D data/B4I_2025_PM3D_ALL_phenotypes.csv")


library(readxl)
meta <-PM3D_ALL_Plot_ID_meta <- read_csv("data/PM3D data/PM3D_ALL_Plot_ID_meta.csv")



meta %>% 
  select(observationUnitDbId,`Sample_Date`,biomass_timepoint,`PM3D Plot ID entered`) 

data %>% 
  filter(observationLevel == "subplot") %>% 
  select(observationUnitDbId,"Above ground dry biomass - g|timepoint 1|COMP:0000110",
         "Pea Aboveground Dry Biomass - g|timepoint 1|COMP:0000118",
         "Weed above ground dry biomass - g|timepoint 1|COMP:0000126",
         "Above ground dry biomass - g|timepoint 2|COMP:0000111",
         "Pea Aboveground Dry Biomass - g|timepoint 2|COMP:0000119",
         "Weed above ground dry biomass - g|timepoint 2|COMP:0000127",
         "Above ground dry biomass - g|timepoint 3|COMP:0000112",
         "Pea Aboveground Dry Biomass - g|timepoint 3|COMP:0000120" ,
         "Weed above ground dry biomass - g|timepoint 3|COMP:0000128") %>% 
  left_join(meta, by=join_by(observationUnitDbId == observationUnitDbId )) %>% 
  select(observationUnitDbId,observationUnitName,Affiliation,`Sample_Date`, biomass_timepoint,`PM3D Plot ID entered`, 
         "Above ground dry biomass - g|timepoint 1|COMP:0000110",
         "Pea Aboveground Dry Biomass - g|timepoint 1|COMP:0000118",
         "Weed above ground dry biomass - g|timepoint 1|COMP:0000126",
         "Above ground dry biomass - g|timepoint 2|COMP:0000111",
         "Pea Aboveground Dry Biomass - g|timepoint 2|COMP:0000119",
         "Weed above ground dry biomass - g|timepoint 2|COMP:0000127",
         "Above ground dry biomass - g|timepoint 3|COMP:0000112",
         "Pea Aboveground Dry Biomass - g|timepoint 3|COMP:0000120" ,
         "Weed above ground dry biomass - g|timepoint 3|COMP:0000128") %>% 
  rename("Timing" = biomass_timepoint) %>% 
  rename("Plot ID" = `PM3D Plot ID entered`) %>% 
  
  pivot_longer("Above ground dry biomass - g|timepoint 1|COMP:0000110":"Weed above ground dry biomass - g|timepoint 3|COMP:0000128",names_to = "trait_name",
               values_to = "Dry Wt (g)") %>% 
  
  mutate(Species = if_else(str_detect(trait_name, "Above ground dry biomass"),"Oat",
                           if_else(str_detect(trait_name, "Pea Aboveground Dry Biomass"),"Pea",
                                   if_else( str_detect(trait_name, "Weed above ground dry biomass"),"Other", "ERROR")))) %>%  
  
  mutate(Year = str_sub(Sample_Date, -4, -1)) %>% 
  
  mutate(`Image Name` = str_c(Affiliation,"_",Year,"_TM-",Timing,"_PL-",`Plot ID`)) %>% 
  select(observationUnitName,observationUnitDbId,`Image Name`,`Sample_Date`,Timing, `Plot ID`,Species,`Dry Wt (g)`) %>% 
  filter(!is.na(`Dry Wt (g)`))  

#write.csv("data/PM3D_2025_formated_biomass_data.csv",row.names = FALSE)









########### BrAPI test run.

library(BrAPI)
library(tidyverse)
library(readxl)

# Manually set the BrAPI Server host
oat <- createBrAPIConnection("oat.triticeaetoolbox.org")

# Get all of the observations for a single trial
selected_trial_id <- "6830"
resp <- oat$get("/observations", query=list(studyDbId=selected_trial_id), page="all", pageSize=500)
observations <- resp$combined_data

# Get the unique set of trait names observed in this trial
trait_names <- sort(unique(sapply(observations, \(x) { x$observationVariableName } )))

# Get the unique set of accession names in this trial
accession_names <- sort(unique(sapply(observations, \(x) { x$germplasmName } )))



# Build a long-format table of trait observations
data <- tibble(
  plot_id = numeric(),
  plot_name = character(),
  blockNumber = numeric(),
  accession_name = character(),
  trait_name = character(),
  value = numeric()
)

for ( observation in observations ) {
  data <- rbind(data, tibble(
    plot_id = as.numeric(observation$observationUnitDbId),
    plot_name = observation$observationUnitName,
    block = observation$blockNumber,
    accession_name = observation$germplasmName,
    trait_name = observation$observationVariableName,
    value = as.numeric(observation$value)
  ))
}


data 

meta <- PM3D_ALL_Plot_ID_meta <- read_excel("data/PM3D data/PM3D_ALL_Plot_ID_meta.xlsx") 

meta %>% 
  select(observationUnitDbId,`Sample Date`,biomass_timepoint,`PM3D Plot ID entered`)

unique(data$trait_name)

data %>% 
  filter(trait_name %in% c("Above ground dry biomass - g|timepoint 1|COMP:0000110",
                           "Pea Aboveground Dry Biomass - g|timepoint 1|COMP:0000118",
                           "Weed above ground dry biomass - g|timepoint 1|COMP:0000126",
                           "Above ground dry biomass - g|timepoint 2|COMP:0000111",
                           "Pea Aboveground Dry Biomass - g|timepoint 2|COMP:0000119",
                           "Weed above ground dry biomass - g|timepoint 2|COMP:0000127",
                           "Above ground dry biomass - g|timepoint 3|COMP:0000112",
                           "Pea Aboveground Dry Biomass - g|timepoint 3|COMP:0000120" ,
                           "Weed above ground dry biomass - g|timepoint 3|COMP:0000128")) %>% 
  select(plot_id, trait_name,value) %>% 
  left_join(meta, by=join_by(plot_id == observationUnitDbId )) %>% 
  select(plot_id,observationUnitName,Affiliation,`Sample Date`, biomass_timepoint,`PM3D Plot ID entered`,trait_name,value) %>% 
  rename("Dry Wt (g)" = value) %>% 
  rename("Plot ID" = `PM3D Plot ID entered`) %>% 
  rename("Timing" = biomass_timepoint) %>% 
  rename("observationUnitDbId"=plot_id) %>% 
  
  mutate(Species = if_else(str_detect(trait_name, "Above ground dry biomass"),"Oat",
                          if_else(str_detect(trait_name, "Pea Aboveground Dry Biomass"),"Pea",
                                     if_else( str_detect(trait_name, "Weed above ground dry biomass"),"Other", "ERROR")))) %>% 
  
  mutate(Year = year(`Sample Date`)) %>% 
  
  mutate(`Image Name` = str_c(Affiliation,"_",Year,"_TM-",Timing,"_PL-",`Plot ID`)) %>% 
  select(observationUnitName,observationUnitDbId,`Image Name`,`Sample Date`,Timing, `Plot ID`,Species,`Dry Wt (g)`) %>% 
  write.csv("data/PM3D_2025_formated_biomass_data.csv",row.names = FALSE)








###### trying to work with multiple trials




selected_trials <- c("B4I_2025_PM3D_NY", "B4I_2025_PM3D_ND")
for (trial_name in selected_trials ) {
  resp <- oat$get("/studies", query=list(studyName=trial_name))
  trial_metadata <- resp$data[[1]]
  trial_id <- trial_metadata$studyDbId
  location <- trial_metadata$locationName
  planting_date <- trial_metadata$startDate
  harvest_date <- trial_metadata$endDate
  design <- trial_metadata$experimentalDesign$PUI
}


data <- tibble(
  plot_id = numeric(),
  plot_name = character(),
  trait_name = character(),
  value = numeric()
)


selected_trials <- c("6830","6832")

for (selected_trial_id in selected_trials) {
  resp <- oat$get("/observations", query=list(studyDbId=selected_trial_id), page="all", pageSize=500)
  observations <- resp$combined_data
  
  for (observation in observations) {
    data <- rbind(data, tibble(
      plot_id = as.numeric(observation$observationUnitDbId),
      plot_name = observation$observationUnitName,
      trait_name = observation$observationVariableName,
      value = as.numeric(observation$value)
    ))
  }
 
}


data


data %>% 
  filter(trait_name %in% c("Above ground dry biomass - g|timepoint 1|COMP:0000110",
                           "Pea Aboveground Dry Biomass - g|timepoint 1|COMP:0000118",
                           "Weed above ground dry biomass - g|timepoint 1|COMP:0000126",
                           "Above ground dry biomass - g|timepoint 2|COMP:0000111",
                           "Pea Aboveground Dry Biomass - g|timepoint 2|COMP:0000119",
                           "Weed above ground dry biomass - g|timepoint 2|COMP:0000127",
                           "Above ground dry biomass - g|timepoint 3|COMP:0000112",
                           "Pea Aboveground Dry Biomass - g|timepoint 3|COMP:0000120" ,
                           "Weed above ground dry biomass - g|timepoint 3|COMP:0000128")) %>% 
  select(plot_name,plot_id, trait_name,value) %>% 
  #filter(plot_name == "B4I_2025_PM3D_NY_20_subplot_3") %>% 
  print(n=380)



















