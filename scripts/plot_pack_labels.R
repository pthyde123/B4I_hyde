library(tidyverse)
library(readr)


B4I_2025_t3_upload_draft <- read_csv("data/B4I_2025_t3_upload_draft.csv")


### making csv for use on Sorrells envelop printer
### file must must have these headers in this order 
###     source,plotNo,oatName,peaName
### arrange by oatname so all plots for a given accession are together for seed separating from bulk bag

oatEntry <- read_csv("data/bulk_oat.csv") %>% 
  filter(state == "NY") %>% 
  select(accessionOat,oatEntry)


peaEntry <- read_csv("data/peaEntry-accessionPea.csv") %>% 
  select(peaEntry,NAME)


unique(B4I_2025_t3_upload_draft$location)

oat_envelopes <- B4I_2025_t3_upload_draft %>% 
  filter(location == "Ithaca, NY") %>% 
  select(plot_number,accession_name,intercrop_accession_name) %>% 
  group_by(accession_name) %>% 
  add_count() %>%
  ungroup() %>% 
  mutate(peaName = intercrop_accession_name) %>% 
  mutate(oatName = accession_name) %>% 
  mutate(PlotNo = str_sub (as.character(plot_number),-3)) %>%
  
  
  left_join(oatEntry, join_by(accession_name==accessionOat)) %>%
  
  left_join(peaEntry, join_by(intercrop_accession_name==NAME)) %>% 
  
  arrange(oatEntry,oatName) %>% 
  mutate(source = str_c("oatE ",oatEntry, "   plots ",n)) %>% 
  mutate(peaName = str_c(peaName, " peaE ", peaEntry)) %>%
  
  
  select(source,PlotNo,oatName,peaName)


oat_envelopes

write.csv(oat_envelopes, "data/sp2025_oat_envelopes.csv",row.names = F)






