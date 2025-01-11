
library(readr)
library(tidyverse)
library(readxl)


#### Randomization from Cynthia ####

update_pea_oat_experiment_design <- read_csv("data/update_pea_oat_experiment_design.csv")



#### pea seed list from Garrett ####

B4I_FINAL_pea_seed_list <- read_excel("data/B4I_FINAL_pea_seed_list.xlsx")


pea_seed_available <- B4I_FINAL_pea_seed_list %>% 
  
  select(NAME:GERM_RESULTS,`First Flower`,Maturity) %>% 
  
  mutate(pea_g_available = as.numeric(if_else(`SEED_AVAILABLE(g)` ==">1000","1500",`SEED_AVAILABLE(g)`))) %>%  # change >1000 to 1500(the grams sent)
  mutate(peaEntry = seq(1:239)) %>%   # peaEntry is a more readable SN for for the pea names, pea names are arranged alphabeicaly and by trial stage descending
  rename(accessionPea = NAME) %>% 
  relocate(peaEntry)
  


#### create PEA bulk pack list #### 
#(bulk packs are to be sent to each location and have enough seed for all the plots of a given accession)

bulk_pea <- update_pea_oat_experiment_design %>% 
  select(!combo) %>% #remove old combo
  rename(accessionPea = Pea) %>% 
  
  left_join(pea_seed_available, by = join_by(accessionPea == accessionPea)) %>% # join seed needs(cynthia) with seed have (garrett)
  
  mutate(seed_per_plot = (5 * 60)) %>%     #5 seeds/sqft for pea (60% of full rate, 8 seeds/sqft) # rate from email
  mutate(seed_per_gram = (1000/`TSW(g)`)) %>%  #calculate the seed per grams for each accession 
  mutate(g_pea_per_plot = (seed_per_plot / seed_per_gram)/GERM_RESULTS) %>% # calculate the seed grams of seed per plot for each accessions to get the desired seeding rate, including seed size and germination rate
  
  select(peaEntry,accessionPea,Replication, Location, PlotNumber, combo2, g_pea_per_plot) %>% 
  
  group_by(peaEntry, accessionPea, Location) %>% 
  summarize(g_pea_per_location = round(sum(g_pea_per_plot),1),
            nPlots = n()) %>% # the grams (rounded for labels printing) of seed for each accession needed at each location. 
  
  ungroup() %>% 
  mutate(bulk_pack = seq(1:982)) %>% 
  relocate(bulk_pack) %>% 
  
  mutate(state = if_else(Location == 1,"AL",
                         if_else(Location == 2,"IA",
                                 if_else(Location == 3, "IL",
                                         if_else(Location == 4, "ND", 
                                                 if_else(Location == 5, "NY", "ERROR"))))))




bulk_pea

#double check, calculate extra seed from bulk pack list 

bulk_pea %>% 
  group_by(accessionPea) %>% 
  summarise(g_pea_per_accession = sum(g_pea_per_location)) %>% 
  left_join(pea_seed_available, by = join_by(accessionPea == accessionPea)) %>% # join seed needs(cynthia) with seed have (garrett)
  
  mutate(extra_seed = (pea_g_available - g_pea_per_accession)) %>% #how much seed do we have minus how much seed do we need
  
  arrange(extra_seed) %>% 
  print(n= nrow(bulk_pea))

# double check looks good 

# write csv
write.csv(bulk_pea,"data/bulk_pea.csv", row.names = FALSE)  
  



# peaEntry, accessionPea list for sorting packs
bulk_pea %>% 
  select(peaEntry,accessionPea) %>% 
  unique()

write.csv(bulk_pea %>% 
            select(peaEntry,accessionPea) %>% 
            unique(),
          "data/peaEntry-accessionPea.csv", row.names = FALSE)


# Location designation

write.table((bulk_pea %>% 
  select(Location,state) %>% 
  unique()),"clipboard", sep = "\t", row.names = FALSE)




#### create OAT bulk pack list ####


#shipment labels from Juan, use for oatEntry number on bulk pack

X2025_iCrop_Oat_Seed_Shipment_labels <- read_excel("data/2025_iCrop_Oat-Seed-Shipment_labels.xlsx")

oatEntry <- X2025_iCrop_Oat_Seed_Shipment_labels %>% 
  select(sn,accession) %>% 
  rename(accessionOat = accession) %>%
  rename(oatEntry = sn)


bulk_oat <- update_pea_oat_experiment_design %>% 
  select(!combo) %>% #remove old combo
  rename(accessionOat = Oat) %>% 
  
  mutate(g_oat_per_plot = 24) %>% # Juan's quote from email
  mutate(oat_g_available = 480) %>% # amount of seed Juan is planning to send +10%
  
  select(accessionOat,Replication, Location, PlotNumber, combo2, g_oat_per_plot) %>% 
  
  group_by(accessionOat, Location) %>% 
  summarize(g_oat_per_location = round(sum(g_oat_per_plot),1),
            nPlots = n()) %>% # the grams of seed for each accession needed at each location. 
  
  ungroup() %>% 
  mutate(bulk_pack = seq(1:1064)) %>% 
  relocate(bulk_pack) %>% 
  
  mutate(state = if_else(Location == 1,"AL", # change location numbers to State
                         if_else(Location == 2,"IA",
                                 if_else(Location == 3, "IL",
                                         if_else(Location == 4, "ND", 
                                                 if_else(Location == 5, "NY", "ERROR")))))) %>% 

  left_join(oatEntry)  # add oatEntry from Juans packs

bulk_oat


#double check, calculate extra seed from bulk pack list 

bulk_oat %>% 
  group_by(accessionOat) %>% 
  summarise(g_oat_per_accession = sum(g_oat_per_location)) %>% 
  mutate(oat_g_available = 480) %>% 
  
  mutate(extra_seed = (oat_g_available - g_oat_per_accession)) %>% #how much seed do we have minus how much seed do we need
  
  #arrange(extra_seed)%>% 
  print(n= nrow(bulk_oat))


# double check looks good 

# write csv

write.csv(bulk_oat,"data/bulk_oat.csv", row.names = FALSE) 




### bulk pack formatted for label

library(tidyverse)
library(readr)



### making csv for use on Sorrells envelop printer
### file must must have these headers in this order 
###     source,plotNo,oatName,peaName
### arrange by oatname so all plots for a given accession are together for seed separating from bulk bag



bulk_oat %>% 
  mutate(source = str_c("oatEntry ",oatEntry)) %>% 
  
  mutate(plotNo = accessionOat) %>% 
  
  mutate(oatName = str_c(g_oat_per_location," g for ", nPlots, " plots")) %>%
  
  mutate(peaName = state) %>% 
  
  arrange(oatEntry, state) %>% 
  
  select(source,plotNo,oatName,peaName) %>% 
  
  write.csv( "data/2025_B4I_bulk_oat_labels.csv", row.names = F)






