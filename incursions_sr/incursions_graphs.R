#rm(list=ls()) #clear memory
#setwd("/Users/miravayuson/Documents/PhD Glasgow/01 Rabies incursions RRL/incursions_sr")  #set directory

# install.packages("lme4")
# library(lme4)
#install libraries
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
#for maps
library(maps)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
library(dplyr)
library(kableExtra) #for printing tables nicely
library(chromote) #for chrome access; needed for webshot 2
library(webshot2) #for saving tables as pdf
library(ggmosaic) #for comparing incursion continents/origin continents
#library(janitor) - didn't use
#library(wesanderson) - tried for color palette but preferred palette didn't exist

read.csv("all_incursions.csv") #use incursion excel
incursions <-read.csv("all_incursions.csv") #convert csv to data frame
nrow(incursions) #number of incursions - 122; had to delete extra/empty row using Text Editor
tail(incursions, 10) #show last 10 incursions
#incursions <- incursions[-c(123), ] #deleting row 123 because now text editor can't seem to get rid of it
str(incursions) #show structure

######CREATING MASTER TABLE########

drop <- c("Year_of_incursion","Month_of_incursion","Animal_details","Ownership_status","Vaccination","Age_class","Breed", "Incursion_details","Full_details","ID_number") #columns that shouldn't be visible
tbl_incursions = incursions[,!(names(incursions) %in% drop)] #removing unnecessary columns
colnames(tbl_incursions) <- c("Continent", "Incursion location", "Incursion date", "Species", "Origin continent", "Location origin", "Time between incursion & symptoms", "Type of incursion", "Borders crossed", "Mode of travel", "Incursion details", "Phylogenetic analysis", "Public health response", "Local transmission", "Unusual cross-species transmission", "Reference(s)") #Adjusted column titles
kbl(tbl_incursions) #basic table, no formatting
# kbl(tbl_incursions, caption = "Animal incursions worldwide (2001-2022)") %>%
#   kable_classic(full_width = F, html_font = "Cambria") %>% 
#   save_kable(file = "tbl_incursions.pdf", self_contained = T) #formal table with save

kbl(tbl_incursions, caption = "Animal incursions worldwide (2001-2022)") %>%
  kable_styling(latex_options = "striped", "condensed", font_size = 11) %>%
  kable_classic(full_width = F, html_font = "Cambria") %>% 
  save_kable(file = "tbl_incursions.pdf", self_contained = T) #formal table with save

######GENERATING TABLES########
table(incursions$Year_of_incursion) #number of incursions per year
yearpercent <- table(incursions$Year_of_incursion) #incursions per year as %
yearpercent
prop.table(yearpercent)
round(prop.table(yearpercent),digits=4)

table(incursions$Month_of_incursion) #number of incursions per month
monthpercent <- table(incursions$Month_of_incursion) #incursions per month as %
monthpercent
prop.table(monthpercent)
round(prop.table(monthpercent),digits=4)

#grouping into animal types (Domestic/Wildlife) - for Fig. 2
incursions$Species <- as.character(incursions$Species)
incursions <- incursions %>% #added 'incursions <-' otherwise changes don't get stored
  dplyr::mutate(Animal_type = dplyr::case_when(Species == "Cat" | Species == "Dog" | Species == "Dog, fox, marten" ~ "Domestic", Species == "Arctic fox" | Species == "fox" | Species == "Dog, fox, marten" | Species == "Jackal" | Species == "Otter" | Species == "Raccoon" | Species == "Raccoon dog" | Species == "Red fox" ~ "Wildlife"))
table(incursions$Animal_type) #82 domestic, 20 wildlife
table(incursions$Species)
table(incursions$Ownership_status)
table(incursions$Mode_of_travel)
table(incursions$Phylogenetic_analysis, incursions$Continent)
table(incursions$Phylogenetic_analysis, incursions$Year_of_incursion)
table(incursions$Phylogenetic_analysis, incursions$Species)
table(incursions$Phylogenetic_analysis, incursions$Mode_of_travel)
table(incursions$Phylogenetic_analysis, incursions$Type_of_incursion)
table(incursions$Phylogenetic_analysis, incursions$Borders_crossed)
table(incursions$Local_transmission, incursions$Year_of_incursion)
# #grouping into seasons
# spring <- c("March", "April", "May")
# summer <- c("June", "July", "August")
# fall <- c("September", "October", "November")
# winter <- c("December", "January", "February")
# 
# incursionsea <- subset(incursions, Continent!="South America")
# incursionsea <- subset(incursions)
# incursionsea
# attach(incursionsea)
# incursionsea$Season[Month_of_incursion %in% spring] <- "Spring"
# incursionsea$Season[Month_of_incursion %in% summer] <- "Summer"
# incursionsea$Season[Month_of_incursion %in% fall] <- "Fall"
# incursionsea$Season[Month_of_incursion %in% winter] <- "Winter"
# 
# table(incursionsea$Season) #number of incursions per season
# seapercent <- table(incursionsea$Season) #incursions per season as %
# seapercent
# prop.table(seapercent)
# round(prop.table(seapercent),digits=4)

#########PLAYING WITH TABLES - GEOGRAPHY#########
#########
table(incursions$Continent) #number of incursions per continent
conpercent <- table(incursions$Continent) #incursions per continent as %
conpercent
prop.table(conpercent)
round(prop.table(conpercent),digits=4)

table(incursions$Location_of_incursion) #number of incursions per country
coupercent <- table(incursions$Location_of_incursion) #incursions per country as %
coupercent
prop.table(coupercent)
round(prop.table(coupercent),digits=4)

# table(incursions$Rabies_status_of_country) #number of incursions per country rabies status
# rstpercent <- table(incursions$Rabies_status_of_country) #incursions per crs in %
# rstpercent
# prop.table(rstpercent)
# round(prop.table(rstpercent),digits=4)
# 
# #playing with tables - animals - change category name
# table(incursions$Species_of_infected) #number of species
# anipercent <- table(incursions$Species_of_infected) #incursions per crs in %
# anipercent
# prop.table(anipercent)
# round(prop.table(anipercent),digits=4)

table(incursions$Type_of_incursion) #type of incursion - human-mediated or natural
toipercent <- table(incursions$Type_of_incursion) #incursion type in %
toipercent
prop.table(toipercent)
round(prop.table(toipercent),digits=4)

# table(incursions$Distance_traveled) #type of incursion - distance traveled
# ditrpercent <- table(incursions$Distance_traveled) #incursion type (distance traveled) in %
# ditrpercent
# prop.table(ditrpercent)
# round(prop.table(ditrpercent),digits=4)

# table(incursions$International_travel) #type of incursion - land, sea or air
# ittpercent <- table(incursions$International_travel) #type of incursion - land sea or air in %
# ittpercent
# prop.table(ittpercent)
# round(prop.table(ittpercent),digits=4)

table(incursions$Origin_continent) #origin continent of incursion
ocoipercent <- table(incursions$Origin_continent) #origin continent of incursion in %
ocoipercent
prop.table(ocoipercent)
round(prop.table(ocoipercent),digits=4)

table(incursions$Borders_crossed) #how many in-country - 34.4%
bcpercent <- table(incursions$Borders_crossed)
bcpercent
prop.table(bcpercent)
round(prop.table(bcpercent),digits=4)
table(incursions$Continent, incursions$Borders_crossed)

table(incursions$Location_origin_of_animal) #origin country of incursion
orcopercent <- table(incursions$Location_origin_of_animal) #origin country of incursion in %
orcopercent
prop.table(orcopercent)
round(prop.table(orcopercent),digits=4)

#Incursions per year + calculations (Mean, CI)
table(incursions$Year_of_incursion) 
yrspercent <- table(incursions$Year_of_incursion) 
yrspercent
prop.table(yrspercent)
round(prop.table(yrspercent),digits=4)
#mean incursions per year calculations + CI
year_average <- mean(yrspercent)
print(year_average) #mean 5.809524
sample.n <- length(yrspercent)
sample.sd <- sd(yrspercent)
sample.se <- sample.sd/sqrt(sample.n)
print(sample.se) #SE 0.6310198
alpha = 0.05
degrees.freedom = sample.n - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(t.score) #t.score 2.085963
margin.error <- t.score * sample.se
lower.bound <- year_average - margin.error
upper.bound <- year_average + margin.error
print(c(lower.bound,upper.bound)) #CI 4.493240 - 7.125808

#mean incursions per year calculations + CI FIX THIS IT'S ALL WRONG
#phyla_average <- 1.67 #mean 5.809524

# table(incursions$Origin_rabies_status) #origin rabies status of incursion
# orspercent <- table(incursions$Origin_rabies_status) #origin rabies status of incursion in %
# orspercent
# prop.table(orspercent)
# round(prop.table(orspercent),digits=4)

table(incursions$Continent, incursions$Origin_continent) #comparing continent destination and continent origin)

# table(incursions$Rabies_status_of_country, incursions$Origin_rabies_status) #comparing rabies status destination and rabies status of origin


##########PLOTS#########
#mosaic comparing origin and destination continents, have to make it look nicer
ggplot(incursions) + 
  geom_mosaic(aes(x = product(Origin_continent, Continent), fill=Origin_continent)) +
  labs(x = "Incursion destination") +
  theme_classic() + 
  scale_fill_manual(values=c("firebrick", "goldenrod1", "blue", "green", "pink", "purple")) +
  theme_classic() +
  theme(aspect.ratio = 1) + 
  guides(fill = guide_legend(title = "Incursion origin", reverse = TRUE))


ggplot(data = incursions) +
  geom_mosaic(aes(x = product(Continent), fill = Origin_continent)) + theme_mosaic()
# 
# contents <- incursions |> 
#   summarise(sumx = sum(x), .by = c(Continent, Origin_continent))
# 
# ggplot(data = contents) +
#   geom_mosaic(aes(x = product(Continent), fill = Origin_continent, weight = sumx))

ggplot(data = incursions) +
  geom_mosaic(aes(x = product(Origin_continent), fill=Origin_continent), divider = "vspine") +
  labs(title='Continents etc') + 
  facet_grid(~Continent) +
  theme(aspect.ratio = 3,
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
#incursions per country
countryplot <- ggplot(incursions, aes(Location_of_incursion))
countryplot + geom_bar() + theme_classic() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#incursions per country (most to least)
incursions %>%
  ggplot(aes(x = fct_infreq(Location_of_incursion))) +
  geom_bar() + theme_classic() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + labs(x = "Country")

#incursions per month (alphabetical)
countrymonth <- ggplot(incursions, aes(Month_of_incursion))
countrymonth + geom_bar() + theme_classic() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#incursions per month (chronological)
countrymo <- incursions
countrymo$Month_of_incursion <- factor(countrymo$Month_of_incursion, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
countrymonth2 <- ggplot(countrymo, aes(Month_of_incursion))
countrymonth2 + geom_bar() + theme_classic() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# #incursions per rabies status of country; have to change column names
# statusplot <- ggplot(incursions, aes(Rabies_status_of_country))
# statusplot + geom_bar() + theme_classic() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#incursions per infected animal
countryinfected <- ggplot(incursions, aes(Species))
countryinfected + geom_bar() + theme_classic() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#incursions per rabid animal (most to least)
incursions %>%
  ggplot(aes(x = fct_infreq(Species))) +
  geom_bar() + theme_classic() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + labs(x = "Species of rabid animal")

# eu1 <- c("Albania", "Austria", "Belarus", "Belgium", "Bosnia and Hezegovina", "Bulgaria", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Italy", "Lithuania", "Netherlands", "Norway", "Poland", "Portugal", "Republic of North Macedonia", "Romania", "Slovakia", "Slovenia", "Spain", "Switzerland", "United Kingdom")
# na1 <- c("Belize", "Canada", "Mexico", "USA")
# sa1 <- c("Argentina", "Brazil", "Colombia", "El Salvador", "French Guiana", "Paraguay", "Peru", "Trinidad and Tobago", "Uruguay")
# 
# attach(incursions)
# incursions$euc[Country %in% eu1] <- "EU"
# incursions$euc[Country %in% na1] <- "NA"
# incursions$euc[Country %in% sa1] <- "SA"
# detach(incursions)
# table(incursions$euc)

table(incursions$Year_of_incursion, incursions$Country) #number of incursions per year per country; too long
table(incursions$Species)
table(incursions$Year_of_incursion, incursions$Continent)

#Incursions per year - draft
# yearcontinent <- with(incursions, table(Year_of_incursion, Continent))
# ggplot(as.data.frame(yearcontinent), aes(factor(Year_of_incursion), Freq, fill = Continent)) + geom_col(position = 'dodge') + labs(title="Yearly animal rabies incursions (2001-2022)", y="Number of incursions", x="Year") + scale_fill_manual(values=c("#00798c", "#a2d729", "#edae49", "red", "yellow", "purple")) + scale_x_discrete(guide = guide_axis(n.dodge=2)) + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5)) 

#Incursions per year - stacked bar chart
number <- nlevels(incursions$Continent)
yearcontinent <- with(incursions, table(Year_of_incursion, Continent))
ggplot(as.data.frame(yearcontinent), aes(factor(Year_of_incursion), Freq, fill = Continent)) + geom_col(position = 'stack') + labs(title="Yearly animal rabies incursions (2001-2022)", y="Number of incursions", x="Year") + scale_fill_manual(values=c("#00798c", "#a2d729", "#edae49", "red", "yellow")) + scale_x_discrete(guide = guide_axis(n.dodge=2)) + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5)) 
#if extra row is there, NA produces extra color box

#same as above but with nicer color palette
number <- nlevels(incursions$Continent)
yearcontinent <- with(incursions, table(Year_of_incursion, Continent))
ggplot(as.data.frame(yearcontinent), aes(factor(Year_of_incursion), Freq, fill = Continent)) + geom_col(position = 'stack') + labs(title="Yearly animal rabies incursions (2001-2022)", y="Number of incursions", x="Year") + scale_fill_manual(values=c("#ff0000", "#00a68c", "#fbad00", "#ff7c00", "#36bed9")) + scale_x_discrete(guide = guide_axis(n.dodge=2)) + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + xlim("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022")
ggsave("rita_timeseries2.png")

#FIG. 3. Incursions per year - stacked bar charts with domestic vs wildlife
number <- nlevels(incursions$Continent)
yearcontinent <- with(incursions, table(Year_of_incursion, Continent, Animal_type))
ggplot(as.data.frame(yearcontinent), aes(factor(Year_of_incursion), Freq, fill = Continent)) + geom_col(position = 'stack') + labs(title="Terrestrial rabies incursions worldwide (2001-2022)", y="Number of incursions", x="Year") + scale_y_continuous(breaks=c(0,2,4,6,8,10,12)) + scale_fill_manual(values=c("#ff0000", "#00a68c", "#fbad00", "#ff7c00", "#36bed9")) + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + theme(plot.title = element_text(size = 10)) + theme(text = element_text(size = 8)) + xlim("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022") + facet_grid(rows = vars(Animal_type)) + theme(strip.background = element_rect(linetype = "solid", color = "black"))
ggsave("incursion_timeseries.png")
#removed '+ scale_x_discrete(guide = guide_axis(n.dodge=2))' since x axis labels (years) can fit
#added xlims to include 2018 despite no data; strip.background for designingfacet labels

#minimalist look
#+ theme_classic()

#slanted x axis below
#theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
#

#Origincontinents graph
number <- nlevels(incursions$Origin_continent)
yearorcontinent <- with(incursions, table(Year_of_incursion, Origin_continent, Animal_type))
ggplot(as.data.frame(yearorcontinent), aes(factor(Year_of_incursion), Freq, fill = Origin_continent)) + geom_col(position = 'stack') + labs(title="Terrestrial rabies incursion origins worldwide (2001-2022)", y="Number of incursions", x="Year") + scale_y_continuous(breaks=c(0,2,4,6,8,10,12)) + scale_fill_manual(values=c("#ff0000", "#00a68c", "#fbad00", "#ff7c00", "#36bed9")) + theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + theme(plot.title = element_text(size = 10)) + theme(text = element_text(size = 8)) + xlim("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022") + facet_grid(rows = vars(Animal_type)) + theme(strip.background = element_rect(linetype = "solid", color = "black"))


#MAPS etc; still under construction
# read.csv("incursions_cleaned.csv") #use incursions per country excel
# worldincursions <-read.csv("incursions_cleaned.csv") #convert csv to data frame
# print(worldincursions) #view tables
# worldincursions$Country
# map.world <- map_data("world") #get world map
# map.world_joined <- left_join(map.world, worldincursions, by = c('region'='Country')) #join map and data
# map.world_joined <- map.world_joined %>% mutate(fill_flg = ifelse(is.na(Continent),F,T)) #highlight countries with excursions
# head(map.world_joined) #countries and coordinates
# 
# ggplot() +
#   geom_polygon(data = map.world_joined, aes(x = long, y = lat, group = group, fill = fill_flg)) +
#   scale_fill_manual(values = c("#CCCCCC","#e60000")) +
#   labs(title = 'Countries with incursions'
#        ,subtitle = "source: me") +
#   theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
#         ,panel.background = element_rect(fill = "#444444")
#         ,plot.background = element_rect(fill = "#444444")
#         ,panel.grid = element_blank()
#         ,plot.title = element_text(size = 30)
#         ,plot.subtitle = element_text(size = 10)
#         ,axis.text = element_blank()
#         ,axis.title = element_blank()
#         ,axis.ticks = element_blank()
#         ,legend.position = "none"
#   )
# 
# #incursions per country of origin
# countryoriginplot <- ggplot(incursions, aes(Origin_country))
# countryoriginplot + geom_bar() + theme_classic() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
# 
# #incursions per country (most to least)
# incursions %>%
#   ggplot(aes(x = fct_infreq(Origin_country))) +
#   geom_bar() + theme_classic() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + labs(x = "Country of Origin")