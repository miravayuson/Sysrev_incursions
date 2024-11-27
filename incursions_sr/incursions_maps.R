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
library(rnaturalearth)
library(rnaturalearthdata) 
library(sf) #chatgpt tutorial
library(tmap) #chatgpt tutorial
#library(janitor) - didn't use
#library(wesanderson) - tried for color palette but preferred palette didn't exist

read.csv("all_incursions.csv") #use incursion excel
incursionmap <-read.csv("all_incursions.csv") #convert csv to data frame
incursionrecords <-read.csv("IncursionRecords.csv")
nrow(incursionmap) #number of incursions - 122; had to delete extra/empty row using Text Editor
tail(incursionmap, 10) #show last 10 incursions
str(incursionmap) #show structure
incursionmap$Location_of_incursion

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica") #removed antarctica, from https://rud.is/books/30-day-map-challenge/lines-01.html; changed United States of America to United States in excel to get it appear (11/21)
world_inc <- world %>%
  mutate(my_selection = ifelse(admin %in% incursionmap$Location_of_incursion, 1, NA))

#chatgpt tutorial
world_data <- world %>%
  left_join(incursionrecords, by = c("name" = "Country"))
print(world_data) #see country map data
str(world_data)

sf_use_s2(FALSE) #from https://github.com/afrimapr/afrimapr-book/issues/30, to generate map properly without stray lines

pointsmap <- st_as_sf(incursionrecords, coords = c("Long", "Lat"), crs = 4326)
print(pointsmap) #chatgpt tutorial "How do I use tm_dots to add points to my map?" (11/21)

tm_shape(world_data) +
  tm_borders() +                         # Border lines
  tm_fill(col = "RabiesStatus",                 # Variable to display
          palette = c("#80A1c2", "#F9665E", "white"),              # Color palette
          style = "cat",            # Color breaks style (changed from quantiles to cat for categorical data)
          title = "Country", labels=c("Rabies-controlled", "Rabies-endemic"), textNA = "No incursion reported", colorNA = "#ECECEC") +    # Title for the legend
  tm_shape(pointsmap) +
    tm_dots(size = "InCountry", col="#f7e654", alpha = 0.7, title.size="In-Country Incursions") + #alpha to set transparency
  tm_layout(main.title = "Animal rabies incursions (2001 - 2022)", legend.outside = TRUE, legend.outside.position = "right", title.snap.to.legend = TRUE) # Removed title = "Legend Title"

#Nov 21: fix South Korea and other country in Africa; move dots that aren't centered?; add arrows; add country labels

# getting: Error: Shape contains invalid polygons. Please fix it or set tmap_options(check.and.fix = TRUE) and rerun the plot
# if I run tmap_options(check.and.fix = TRUE), the map works but I get stray lines
# 
#map of incursions
#https://stackoverflow.com/questions/69625716/how-to-plot-a-world-map-using-r
ggplot(data = world_inc) +
  geom_sf(aes(fill=my_selection)) + 
  theme_bw()

#map of incursion sources
world_origin <- world %>%
  mutate(my_source = ifelse(admin %in% incursionmap$Location_origin_of_animal, 1, NA))

world_origin <- world_origin %>% filter(sovereignt != "Antarctica") #remove antarctica
  
ggplot(data = world_origin) +
  geom_sf(aes(fill=my_source)) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Rabies incursions worldwide (2001-2022)") +
  theme_bw()

### countries by rabies status
read.csv("IncursionRecords.csv") #use incursion excel
incursionrecs <-read.csv("all_incursions.csv")
world_mod <- world %>%
  mutate(my_selection = ifelse(admin %in% incursionrecs$Country, 1, NA))
ggplot(data=world_mod) +
  geom_sf(aes(fill=my_selection)) +
  theme_bw()
#map.world <- map_data("world") #get world map
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