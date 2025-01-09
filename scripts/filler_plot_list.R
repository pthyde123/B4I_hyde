
library(dplyr)
library(readxl)
library(readr)



### this is the pea accessions file with accession weight for those that did not have enough seed
peaEntry_reweigh_1_5_25 <- read_excel("data/peaEntry-reweigh 1.5.25.xlsx")


### cleaned up the file and kept only the accessions we are short on
peaEntry_negative <- peaEntry_reweigh_1_5_25 %>% 
  
  mutate(pea_g_available = as.numeric(if_else(`SEED_AVAILABLE(g)` ==">1000","1500",`SEED_AVAILABLE(g)`))) %>%  # change >1000 to 1500(the grams sent)
  mutate(peaEntry = seq(1:239)) %>%   # peaEntry is a more readable SN for for the pea names, pea names are arranged alphabeicaly and by trial stage descending
  rename(accessionPea = NAME) %>% 
  relocate(peaEntry) %>% 
  select(peaEntry,accessionPea,`pack weight 1.5.25`) %>% 
  filter(!is.na (`pack weight 1.5.25`))


### the packing list
bulk_pea <- read_csv("data/bulk_pea.csv")


### joined the bulkpack list list of accession that did not have enough seed
negative_accessionPea <-bulk_pea %>% 
  group_by(accessionPea) %>% 
  summarise(g_pea_per_accession = sum(g_pea_per_location)) %>% # caclulate the total amound of seed needed for each entry(per randomization)
  left_join(peaEntry_negative) %>% 
  filter(!is.na (`pack weight 1.5.25`)) %>% 
  mutate(seed_available_1.5.25 = `pack weight 1.5.25`- g_pea_per_accession )


###  calculated grams seed per plot 
# this uses a chunk of code from bulk_envelopes_from_randomization

update_pea_oat_experiment_design <- read_csv("data/update_pea_oat_experiment_design.csv")

#### pea seed list from Garrett ####

B4I_FINAL_pea_seed_list <- read_excel("data/B4I_FINAL_pea_seed_list.xlsx")


pea_seed_available <- B4I_FINAL_pea_seed_list %>% 
  
  select(NAME:GERM_RESULTS,`First Flower`,Maturity) %>% 
  
  mutate(pea_g_available = as.numeric(if_else(`SEED_AVAILABLE(g)` ==">1000","1500",`SEED_AVAILABLE(g)`))) %>%  # change >1000 to 1500(the grams sent)
  mutate(peaEntry = seq(1:239)) %>%   # peaEntry is a more readable SN for for the pea names, pea names are arranged alphabeicaly and by trial stage descending
  rename(accessionPea = NAME) %>% 
  relocate(peaEntry)

g_per_plot <- update_pea_oat_experiment_design %>% 
  select(!combo) %>% #remove old combo
  rename(accessionPea = Pea) %>% 
  
  left_join(pea_seed_available, by = join_by(accessionPea == accessionPea)) %>% # join seed needs(cynthia) with seed have (garrett)
  
  mutate(seed_per_plot = (5 * 60)) %>%     #5 seeds/sqft for pea # rate from email
  mutate(seed_per_gram = (1000/`TSW(g)`)) %>%  #calculate the seed per grams for each accession 
  mutate(g_pea_per_plot = (seed_per_plot / seed_per_gram)/GERM_RESULTS) %>% # calculate the seed grams of seed per plot for each accessions to get the desired seeding rate, including seed size and germination rate
  
  select(peaEntry,accessionPea,Replication, Location, PlotNumber, combo2, g_pea_per_plot) %>% 
  arrange(peaEntry,PlotNumber) %>% 
  filter(accessionPea %in% negative_accessionPea$accessionPea) %>% 
  select(accessionPea,g_pea_per_plot) %>% 
  unique()




###cacluate the number of filler plots needed based on the grams needed per plot

filler_plots_needed <- negative_accessionPea %>% 
  left_join(g_per_plot) %>% 
  mutate(filler_plots = if_else(`seed_available_1.5.25` + g_pea_per_plot > 0, "1",
                                if_else(`seed_available_1.5.25` + (g_pea_per_plot*2) > 0,"2",
                                        if_else(`seed_available_1.5.25` + (g_pea_per_plot*3) > 0,"3","na" )))) %>% 
  select(accessionPea,g_pea_per_plot,filler_plots)
  
  
###randomly select the filler plots  

runif(1)*10000
set.seed(4943)

filler_accessions <- update_pea_oat_experiment_design %>% 
  select(!combo) %>% #remove old combo
  rename(accessionPea = Pea) %>% 
  
  left_join(pea_seed_available, by = join_by(accessionPea == accessionPea)) %>% # join seed needs(cynthia) with seed have (garrett)
  
  mutate(seed_per_plot = (5 * 60)) %>%     #5 seeds/sqft for pea  # rate from email
  mutate(seed_per_gram = (1000/`TSW(g)`)) %>%  #calculate the seed per grams for each accession 
  mutate(g_pea_per_plot = (seed_per_plot / seed_per_gram)/GERM_RESULTS) %>% # calculate the seed grams of seed per plot for each accessions to get the desired seeding rate, including seed size and germination rate
  
  select(peaEntry,accessionPea,Replication, Location, PlotNumber, combo2, g_pea_per_plot) %>% 
  
  mutate(rand = runif(2000)) %>% # create random number for each plot
  
  left_join(filler_plots_needed) %>% # join with the number of filler plots needed
  filter(!is.na(filler_plots)) # remove plots that don't need fillers


### select plots based on the number of filler plots needed
# cant quickly figure out a way to do this in one go selecting the number of plots based on filler plots needed.
# doing it 3 times once for each number of filler plots needed and then combining 

filler_1 <- filler_accessions %>%   
  group_by(accessionPea) %>% 
  
  filter(filler_plots == 1) %>% 
  
  top_n(n = 1, wt = rand)       

         
         
filler_2 <- filler_accessions %>%   
  group_by(accessionPea) %>% 
  
  filter(filler_plots == 2) %>% 
  
  top_n(n = 2, wt = rand)         



filler_3 <- filler_accessions %>%   
  group_by(accessionPea) %>% 
  
  filter(filler_plots == 3) %>% 
  
  top_n(n = 3, wt = rand)



filler_plots <- bind_rows(filler_1,filler_2, filler_3) %>% 
  arrange(peaEntry) %>% 
  mutate(state = if_else(Location == 1,"AL",
                         if_else(Location == 2,"IA",
                                 if_else(Location == 3, "IL",
                                         if_else(Location == 4, "ND", 
                                                 if_else(Location == 5, "NY", "ERROR"))))))




write.csv(filler_plots,"data/filler_plots.csv",row.names = F)




  






  
  
  
