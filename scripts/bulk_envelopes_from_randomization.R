
library(readr)
library(tidyverse)
library(readxl)


#### Randomization from Cynthia ####

update_pea_oat_experiment_design <- read_csv("data/update_pea_oat_experiment_design.csv")



#### pea seed list from Garrett ####

B4I_FINAL_pea_seed_list <- read_excel("data/B4I_FINAL_pea_seed_list.xlsx")

pea_seed_available <- B4I_FINAL_pea_seed_list %>% 
  mutate(pea_g_available = as.numeric(if_else(`SEED_AVAILABLE(g)` ==">1000","1500",`SEED_AVAILABLE(g)`))) %>%  # change >1000 to 1500(the grams sent)
  select(NAME,pea_g_available,`TSW(g)`,GERM_RESULTS) %>% 
  rename(accession = NAME)



#### create PEA bulk pack list #### 
#(bulk packs are to be sent to each location and have enough seed for all the plots of a given accession)

bulk_pea <- update_pea_oat_experiment_design %>% 
  select(!combo) %>% #remove old combo
  rename(accession = Pea) %>% 
  
  left_join(pea_seed_available, by = join_by(accession == accession)) %>% # join seed needs(cynthia) with seed have (garrett)
  
  mutate(seed_per_plot = (5 * 60)) %>%     #5 seeds/sqft for pea (60% of full rate, 8 seeds/sqft) # rate from email
  mutate(seed_per_gram = (1000/`TSW(g)`)) %>%  #calculate the seed per grams for each accession 
  mutate(g_pea_per_plot = (seed_per_plot / seed_per_gram)/GERM_RESULTS) %>% # calculate the seed grams of seed per plot for each accessions to get the desired seeding rate, including seed size and germination rate
  
  select(accession,Replication, Location, PlotNumber, combo2, g_pea_per_plot) %>% 
  
  group_by(accession, Location) %>% 
  summarize(g_pea_per_location = sum(g_pea_per_plot)) %>% # the grams of seed for each accession needed at each location. 
  
  ungroup() %>% 
  mutate(bulk_pack = seq(1:982)) %>% 
  relocate(bulk_pack)

bulk_pea

#double check, calculate extra seed from bulk pack list 

bulk_pea %>% 
  group_by(accession) %>% 
  summarise(g_pea_per_accession = sum(g_pea_per_location)) %>% 
  left_join(pea_seed_available, by = join_by(accession == accession)) %>% # join seed needs(cynthia) with seed have (garrett)
  
  mutate(extra_seed = (pea_g_available - g_pea_per_accession)) %>% #how much seed do we have minus how much seed do we need
  
  arrange(extra_seed) %>% 
  print(n= nrow(bulk_pea))

# double check looks good 

# write csv
write.csv(bulk_pea,"data/draft_bulk_pea.csv", row.names = FALSE)  
  



#### create OAT bulk pack list ####

bulk_oat <- update_pea_oat_experiment_design %>% 
  select(!combo) %>% #remove old combo
  rename(accession = Oat) %>% 
  
  mutate(g_oat_per_plot = 24) %>% # Juan's quote from email
  mutate(oat_g_available = 480) %>% # amount of seed Juan is planning to send +10%
  
  select(accession,Replication, Location, PlotNumber, combo2, g_oat_per_plot) %>% 
  
  group_by(accession, Location) %>% 
  summarize(g_oat_per_location = sum(g_oat_per_plot)) %>% # the grams of seed for each accession needed at each location. 
  
  ungroup() %>% 
  mutate(bulk_pack = seq(1:1064)) %>% 
  relocate(bulk_pack)

bulk_oat


#double check, calculate extra seed from bulk pack list 

bulk_oat %>% 
  group_by(accession) %>% 
  summarise(g_oat_per_accession = sum(g_oat_per_location)) %>% 
  mutate(oat_g_available = 480) %>% 
  
  mutate(extra_seed = (oat_g_available - g_oat_per_accession)) %>% #how much seed do we have minus how much seed do we need
  
  arrange(extra_seed) %>% 
  print(n= nrow(bulk_oat))


# double check looks good 

# write csv

write.csv(bulk_oat,"data/draft_bulk_oat.csv", row.names = FALSE) 

