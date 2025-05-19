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


incursions_dlw <- incursions%>%
  dplyr::mutate(Animal_type2 = dplyr::case_when(Species == "Cat" | Species == "Dog" ~ "Domestic", Species == "Anteater" | Species == "Arctic fox" | Species == "Fox" | Species == "Jackal" | Species == "Otter" | Species == "Raccoon" | Species == "Raccoon dog" | Species == "Red fox" | Species =="Sable" ~ "Wildlife", Species == "Dog, fox, marten" ~ "Wildlife|Domestic"))

incursions_dlw <- incursions_dlw%>%
  separate_rows(Animal_type2, sep = "\\|")

incursions_dlw <- incursions_dlw%>%
  dplyr::mutate(Borders = dplyr::case_when(Borders_crossed == "In-country" | Borders_crossed == "Intercontinental, in-country" ~ "In-country", Borders_crossed == "Intercontinental" | Borders_crossed =="Intercontinental (shared border)" | Borders_crossed =="Intercontinental, in-country" | Borders_crossed == "Intercontinental, international" | Borders_crossed == "Intercontinental, international (shared border)" ~ "Intercontinental", Borders_crossed == "International" | Borders_crossed == "International (shared border)" | Borders_crossed == "Intercontinental, international" | Borders_crossed == "Intercontinental, international (shared border)" ~ "International"))

incursions_dlw <- incursions_dlw%>%
  dplyr::mutate(Travel_mode = dplyr::case_when(Mode_of_travel == "By air" | Mode_of_travel == "By air (presumed)" | Mode_of_travel == "By air, land" | Mode_of_travel == "By air, land/water (presumed)" | Mode_of_travel == "By air, other" | Mode_of_travel == "By air/land (presumed)" | Mode_of_travel == "By air/water (presumed)" | Mode_of_travel == "By land (presumed), air/water (presumed)" | Mode_of_travel == "By land, air/water (presumed)" ~ "By air", Mode_of_travel == "By land" | Mode_of_travel == "By air, land" | Mode_of_travel == "By air, land/water (presumed)" | Mode_of_travel == "By air/land (presumed)" | Mode_of_travel == "By land (presumed), air/water (presumed)" | Mode_of_travel == "By land, air/water (presumed)" | Mode_of_travel == "By land (presumed)" | Mode_of_travel == "By land/ water (presumed)" | Mode_of_travel == "By land/water" | Mode_of_travel == "By water, land" ~ "By land", Mode_of_travel == "By water (frozen)" | Mode_of_travel == "By water (presumed)" | Mode_of_travel == "By air, land/water (presumed)" | Mode_of_travel == "By air/water (presumed)" | Mode_of_travel == "By land (presumed), air/water (presumed)" | Mode_of_travel == "By land, air/water (presumed)" | Mode_of_travel == "By land, air/water (presumed) " | Mode_of_travel == "By land/water" | Mode_of_travel == "By water, land" ~ "By water")) 

drop2 <- c("Location_of_incursion", "Year_of_incursion", "Month_of_incursion", "Date_of_incursion","Species","Animal_details","Breed","Location_origin_of_animal","Borders_crossed","Mode_of_travel","Time_bet_incursion_and_symptoms","Incursion_details","Full_details","Seen_details","Public_health_response","Study","ID_number")

incursions_dlw2 = incursions_dlw[,!(names(incursions_dlw) %in% drop2)] 

incursions_dlw2$Ownership_status[incursions_dlw2$Ownership_status==""]<-"Not specified"
incursions_dlw2$Vaccination[incursions_dlw2$Vaccination==""]<-"Not specified"
incursions_dlw2$Age_class[incursions_dlw2$Age_class==""]<-"Not specified"
incursions_dlw2$Type_of_incursion[incursions_dlw2$Type_of_incursion==""]<-"Not specified"

colnames(incursions_dlw2) <- c('Incursion location','Ownership status','Vaccination status','Age class','Origin of incursion','Incursion type','Phylogenetic analysis','Level of transmission','Unusual cross-species transmission','Animal_type2','Borders crossed','Mode of travel') #change column names

print(incursions_dlw2)

incursions_dlw2 %>% tbl_summary(by = Animal_type2) #create table