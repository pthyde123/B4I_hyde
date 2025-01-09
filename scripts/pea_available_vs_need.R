library(readr)
library(tidyverse)
library(readxl)



### pea seed list from Garrett
B4I_FINAL_pea_seed_list <- read_excel("data/B4I_FINAL_pea_seed_list.xlsx")

pea_seed_available <- B4I_FINAL_pea_seed_list %>% 
  mutate(pea_g_available = as.numeric(if_else(`SEED_AVAILABLE(g)` ==">1000","1500",`SEED_AVAILABLE(g)`))) %>%  # change >1000 to 1500(the grams sent)
  select(NAME,pea_g_available,`TSW(g)`,GERM_RESULTS) %>% 
  rename(accession = NAME)


### randomization from Cynthia
update_pea_oat_experiment_design <- read_csv("data/update_pea_oat_experiment_design.csv")


### calculating pea need per accession from randomization
update_pea_oat_experiment_design %>% 
  select(!combo) %>% #remove old combo
  group_by(Pea) %>% 
  count() %>%  # count the number of plots needed per accession
  rename(accession = Pea) %>% 
  rename(plot_count = n) %>% 
  
  left_join(pea_seed_available, by = join_by(accession == accession)) %>% # join seed needs(cynthia) with seed have (garrett)
  
  mutate(seed_per_plot = (5 * 60)) %>%     #5 seeds/sqft for pea (60% of full rate, 8 seeds/sqft) # rate from email
  mutate(seed_per_gram = (1000/`TSW(g)`)) %>%  #calculate the seed per grams for each accession 
  mutate(g_pea_per_plot = (seed_per_plot / seed_per_gram)/GERM_RESULTS) %>% # calculate the seed grams of seed for each accessions to get the desired seeding rate, including seed size and germination rate
  mutate(g_pea_per_accession = g_pea_per_plot * plot_count) %>% # calculate the total amount of seed needed per accession 
  
  mutate(extra_seed = (pea_g_available - g_pea_per_accession)) %>% #how much seed do we have minus how much seed do we need
  
  arrange(extra_seed) %>% 
  print(n= nrow(update_pea_oat_experiment_design))


### 







