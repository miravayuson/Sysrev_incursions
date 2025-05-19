library(lubridate)
library(tidyverse)
library(reshape)
library(gridExtra)
library(rgdal)
library(rlang)
library(epitools)
library(ggpattern)
library(dplyr)
library(ggplot2)
library(maps)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
library(dplyr)
library(kableExtra) 
library(chromote) 
library(webshot2) 
library(ggmosaic)
library(gtsummary)


read.csv("data/all_incursions.csv") #use incursion excel
incursions <-read.csv("data/all_incursions.csv") #convert csv to data frame
nrow(incursions)
tail(incursions, 10) 
str(incursions)


drop <- c("Year_of_incursion","Month_of_incursion","Animal_details","Ownership_status","Vaccination","Age_class","Breed", "Incursion_details","Full_details","Seen_details1","ID_number") 
tbl_incursions = incursions[,!(names(incursions) %in% drop)] 
colnames(tbl_incursions) <- c("Continent", "Incursion location", "Incursion date", "Species", "Origin continent", "Location origin", "Time between incursion & symptoms", "Type of incursion", "Borders crossed", "Mode of travel", "Incursion details", "Phylogenetic analysis", "Public health response", "Local transmission", "Unusual cross-species transmission", "Reference(s)") #Adjusted column titles
kbl(tbl_incursions) 


kbl(tbl_incursions) %>% 
  kable_styling(latex_options = "striped", "condensed", font_size = 11) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  landscape() %>%
  save_kable(file = "figures/Supplementary_table_1.html", self_contained = T) 