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


read.csv("data/all_incursions.csv") 
incursions <-read.csv("data/all_incursions.csv") 
nrow(incursions) 
tail(incursions, 10) 
str(incursions)


incursions$Species <- as.character(incursions$Species)
incursions <- incursions %>% #added 'incursions <-' otherwise changes don't get stored
  dplyr::mutate(Animal_type = dplyr::case_when(Species == "Cat" | Species == "Dog" ~ "Domestic", Species == "Anteater" | Species == "Arctic fox" | Species == "Fox" | Species == "Jackal" | Species == "Otter" | Species == "Raccoon" | Species == "Raccoon dog" | Species == "Red fox" | Species =="Sable" ~ "Wildlife", Species == "Dog, fox, marten" ~ "Wildlife|Domestic"))

incursions <- incursions%>%
  separate_rows(Animal_type, sep = "\\|")

table(incursions$Animal_type)

number <- nlevels(incursions$Continent)
yearcontinent <- with(incursions, table(Year_of_incursion, Continent, Animal_type))
ggplot(as.data.frame(yearcontinent), aes(factor(Year_of_incursion), Freq, fill = Continent)) + geom_col(position = 'stack') + labs(y="Number of incursions", x="Year") + scale_y_continuous(breaks=c(0,2,4,6,8,10,12)) + scale_fill_manual(values=c("#ff0000", "#00a68c", "#fbad00", "#ff7c00", "#36bed9")) + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + theme(plot.title = element_text(size = 10)) + theme(text = element_text(size = 8)) + xlim("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022") + facet_grid(rows = vars(Animal_type)) + theme(strip.background = element_rect(linetype = "solid", color = "black", fill="white"), axis.title.x = element_text(size=12), axis.text.x = element_text(size=11), axis.title.y = element_text(size=12), axis.text.y = element_text(size=11), strip.text.y = element_text(size=12, face="bold"), legend.title = element_text(size=13), legend.text = element_text(size=13)) 
ggsave("figures/Figure3.png", width = 28, height = 15, units = "cm", dpi=300)