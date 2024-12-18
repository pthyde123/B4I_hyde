
library(readr)
library(tidyverse)
library(readxl)



final_data_count1 <- read_csv("data/final_data_count1.csv")



final_data_count1 %>% 
  filter(crop == "Oat") %>%  # just looking at oat accessions
  
  mutate(g_oat_per_plot = 24) %>% # Juan's quote from email
  
  mutate(g_oat_per_accession = g_oat_per_plot * Count) %>% # calculate the total amount of seed needed per accession 
  
  arrange(g_oat_per_accession) %>% 
  select(accession,Count,g_oat_per_accession) %>% 
  print(n= nrow(final_data_count1))





