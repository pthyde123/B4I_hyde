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




unique(B4I_2025_t3_upload_draft$location)

B4I_2025_t3_upload_draft %>% 
  filter(location == "Ithaca, NY") %>% 
  select(plot_number,accession_name,intercrop_accession_name) %>% 
  group_by(accession_name) %>% 
  add_count() %>%
  ungroup() %>% 
  mutate(peaName = intercrop_accession_name) %>% 
  mutate(oatName = accession_name) %>% 
  mutate(PlotNo = plot_number) %>%
  
  
  left_join(oatEntry, join_by(accession_name==accessionOat)) %>% 
  
  arrange(oatEntry,oatName) %>% 
  select(n,oatEntry,PlotNo,oatName,peaName) %>% 
  print(n = 400)
  





