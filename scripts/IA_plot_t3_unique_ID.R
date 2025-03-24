library(readr)
library(readxl)
library(tidyverse)

B4I_2025_t3_upload_draft <- read_csv("data/B4I_2025_t3_upload_draft.csv")


Pea_Oat_Field_Layout_IA <- read_excel("data/Pea-Oat Field Layout_IA.xlsx")


###Format IA layout
IA_layout <- Pea_Oat_Field_Layout_IA %>% 
  mutate(peaName = str_sub(PEA,3)) %>%   #remove parts of names added by IA
  mutate(oatName = str_sub(OAT, start = 3, end = -11)) #remove parts of names added by IA
  
###Filter to IA trial  
T3_IA <- B4I_2025_t3_upload_draft %>% 
  filter(trial_name == "B4I_2025_IA") %>% 
  mutate(SN = as.numeric(str_sub(plot_number, 2))) %>% # change plot number back to SN 
  select(plot_name,plot_number,SN,accession_name,intercrop_accession_name)
  

###Double check that names match between IA layout and T3 layour 
T3_IA %>% 
  full_join(IA_layout, by= join_by("SN"=="Entry")) %>% 
  mutate(error = if_else(accession_name != oatName,"ERROR", "ALL_GOOD" )) %>% 
  filter(error == "ERROR")
  
T3_IA %>% 
  full_join(IA_layout, by= join_by("SN"=="Entry")) %>% 
  mutate(error = if_else(intercrop_accession_name != peaName,"ERROR", "ALL_GOOD" )) %>% 
  filter(error == "ERROR")


###Everything matches so we are good

T3_IA %>% 
  full_join(IA_layout, by= join_by("SN"=="Entry")) %>% 
  select(plot_number, Range, Column) %>% 
  write.table( "clipboard", sep="\t", row.names=FALSE, col.names = FALSE)



