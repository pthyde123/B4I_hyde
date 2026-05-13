### Code for taking randomization to T3/oat and field plan


library(tidyverse)
library(readxl)
library(readr)


## the randomization  # replace here

randomization <- read_csv("data/B4I_2026/2026_B4I_NY_randomization.csv") %>% 
  rename(row_number = row) %>% 
  rename(col_number = col) %>% 
  rename(accession_name = oat_id) %>% 
  rename(intercrop_accession_name = pea_id)


##  the map # replace here 
## row columns where assigned in the randomization use them 



#### ENTER FIELD META DATA HERE ####

# trail name convention mother-ship_year_location(short)_trial abbreviation_type(PLOT or HR) 

trial_name = "B4I_2026_NY"
breeding_program = "Intercropping Cooperative"
location = "Ithaca, NY - Caldwell"
year = "2026"
design_type = "p-rep"
description =  "B4I 2025 spring oat pea intercrop trial with five locations"
trial_type = "phenotyping_trial"
plot_width = "1.3"
plot_length = ""
field_size = "0.3993"     
planting_date =  "2026-04-14"   ## set as text in the future so format stays in .csv for upload
harvest_date  = ""

total_plots = 440
block_number = 1

###T3 trial headers 


t3_trial_upload_headers <- c("trial_name",	"breeding_program",	"location",	"year",	"design_type",	"description",	"trial_type",	
                             "plot_width", "plot_length",	"field_size",	"planting_date",	"harvest_date",	"plot_name",	"accession_name",
                             "plot_number",
                             "block_number",	"is_a_control",	"rep_number",	"range_number",	"row_number",	"col_number",
                             "seedlot_name",	"num_seed_per_plot",	"weight_gram_seed_per_plot",	"entry_number",	"is_private")




### Create data frame of trial level meta


trial_level_headers <- c("trial_name",	"breeding_program",	"location",	"year",	"design_type",	"description",	"trial_type",	
                         "plot_width", "plot_length",	"field_size",	"planting_date",	"harvest_date")




trial_meta <- bind_cols(trial_name,  #keep in this order, to match t3 template
                        breeding_program,
                        location,
                        year,
                        design_type,
                        description,
                        trial_type,
                        plot_width,
                        plot_length,
                        field_size,
                        planting_date,
                        harvest_date)

colnames(trial_meta) <- trial_level_headers

trial_meta <- trial_meta  %>%  slice(rep(1:n(), each = total_plots)) # duplicate the meta to match the number of plots

trial_meta



### plot level data

plot_level_headers <- c("plot_name",	"accession_name", "plot_number",
                        "block_number",	"is_a_control",	"rep_number",	"range_number",	"row_number",
                        "col_number", "seedlot_name",	"num_seed_per_plot",
                        "weight_gram_seed_per_plot",	"entry_number",	"is_private")

randomization


### Trial set up  ###




trial <- randomization %>% 
  mutate(plot_number = if_else(trial=="PM3D",plot_number+3000,plot_number))%>% 
  mutate(plot_number = if_else(trial=="Monoculture",plot_number+2000,plot_number)) %>% 
  mutate(plot_number = if_else(trial == "B4I_main", plot_number+1000,plot_number)) %>% 
  mutate(plot_name = str_c(trial,"_",plot_number))

# renaming for T3 upload
trial <- trial %>% 
  
  mutate(plot_name = plot_name) %>% 
  
  mutate(block_number  = block_number ) %>% 
  
  mutate(is_a_control = "") %>% 
  
  mutate(rep_number = "") %>% 
  
  mutate(range_number ="") %>% 
  
  mutate(row_number = row_number) %>%
  
  mutate(col_number = col_number) %>% 
  mutate(seedlot_name = "") %>% 
  mutate(num_seed_per_plot = "") %>% 
  mutate(weight_gram_seed_per_plot = "24") %>% 	
  mutate(entry_number = "") %>% 
  mutate(is_private = "")



trial <- trial %>% 
  select(plot_name,
         accession_name,
         plot_number,
         block_number,
         is_a_control, 
         rep_number,               
         range_number,
         row_number,     
         col_number,             
         seedlot_name,         
         num_seed_per_plot,      
         weight_gram_seed_per_plot,
         entry_number,
         is_private,
         intercrop_accession_name)  




full_trial <- bind_cols( trial_meta ,trial) %>% 
  
  mutate(accession_name = if_else(is.na(accession_name),
                               "NO_OATS_PLANTED",
                               accession_name)) %>% 
  
  mutate(intercrop_accession_name = if_else(is.na(intercrop_accession_name),
                                  "NO_PEAS_PLANTED",
                                  intercrop_accession_name)) %>%
  mutate(
    intercrop_accession_name = recode(
      intercrop_accession_name,
      "Amariilo"   = "CDC AMARILLO",
      "ND Victory" = "ND VICTORY",
      "Greenwood"  = "GREENWOOD",
      "CDC Inca"   = "CDC INCA",
      "Arcadia"    = "ARCADIA"
    )) %>% 
  
  mutate()


full_trial 


###write.csv(full_trial,str_c("data/B4I_2026/",trial_name,"_t3_upload.csv"), row.names = FALSE)







