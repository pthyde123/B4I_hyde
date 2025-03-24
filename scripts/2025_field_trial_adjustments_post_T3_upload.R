
library(readr)
library(tidyverse)
library(caroline)

### this is an updated layout sent to me from Cythia on March 19.  There are changes from the T3 trial upload that occured durring planting. 
# These changes include.  18 row column changes and 1 oat accession change.
# the row column changes will are addressed in the the first set of code.  
# I used the original T3 plot numbers and generated a file with the new row column designations.  
# this file will be uploaded to T3 manually.  


AL_new_layout_3_24_25 <- read_csv("data/AL_new_layout_3.24.25.csv") # layout sent from Cythia (AL)

AL_update_row_col <- AL_new_layout_3_24_25 %>% 
  select(Pea,Oat,combo2,`original on T3 PlotNumber`,`type of change`,Row,Column) %>% 
  mutate(t3_plot_number = `original on T3 PlotNumber` + 1000) %>% # add the 1000 for AL to match the T3 plot number
  select(t3_plot_number,Row,Column,combo2)




#t3 upload file, used to get t3 plot_name to match with plot_number and updated(3.19.25) row col.
B4I_2025_t3_upload <- read_csv("data/B4I_2025_t3_upload.csv")

AL_t3_plot_name <- B4I_2025_t3_upload %>% 
  select(plot_name,plot_number)

AL_t3_plot_name

AL_row_col_3.24.2025 <- AL_t3_plot_name %>% 
  left_join(AL_update_row_col, by = join_by("plot_number" == "t3_plot_number")) 
  

AL_row_col_3.24.2025


####T3 spacial layout format    "plot_name"	"row_number"	"col_number" in tab-delimited text file format 


AL_row_col_3.24.2025 %>% 
  select(plot_name,Row,Column) %>% 
  rename("row_number" = "Row") %>% 
  rename("col_number" = "Column") %>% 
  write.delim( "data/AL_row_col_3.24.2025.tab", quote= FALSE, row.names = FALSE, sep = "\t")  ### this file is uploaded to t3 3.24.25


### AL accession change
# there is one plot which had an oat accession change
# 

AL_new_layout_3_24_25 %>% 
  filter(!is.na(`type of change`)) %>% 
  filter(`type of change` == "replaced oat")  # oat accession to be replaced( plot 1354) as of 3.24.25 is  "IL23-524"


B4I_2025_t3_upload %>% 
  select(plot_name, plot_number,accession_name) %>% 
  filter(plot_number == 1354) ## origional T3 accession upload for plot 1354 was "IL22-3958" 

### manually changed plot_number 1354 to accession "IL23-524" 




