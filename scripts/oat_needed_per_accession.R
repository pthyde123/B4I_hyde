
library(readr)
library(tidyverse)
library(readxl)


update_pea_oat_experiment_design <- read_csv("data/update_pea_oat_experiment_design.csv")



update_pea_oat_experiment_design %>% 
  select(!combo) %>% #remove old combo
  group_by(Oat) %>% 
  count() %>%  # count the number of plots needed per accession
  rename(accession = Oat) %>% 
  rename(plot_count = n) %>% 
  
  mutate(g_oat_per_plot = 24) %>% # Juan's quote from email
  mutate(oat_g_available = 480) %>% 
  
  mutate(g_oat_per_accession = g_oat_per_plot * plot_count) %>% # calculate the total amount of seed needed per accession 
  mutate(extra_oat_seed = (oat_g_available - g_oat_per_accession)) %>% 
    
  arrange(extra_oat_seed) %>% 
  print(n= nrow(update_pea_oat_experiment_design))





