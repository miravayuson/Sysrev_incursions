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
library(rnaturalearth)
library(rnaturalearthdata) 
library(sf) 
library(tmap) 
library(patchwork) 
library(cowplot) 
library(gridExtra)


read.csv("data/all_incursions.csv") 
incursionmap <-read.csv("data/all_incursions.csv") 
incursionrecords <-read.csv("data/IncursionRecords.csv")
incursionarrows <-read.csv("data/IncursionArrows.csv") 
incursionarrowsEU <-read.csv("data/IncursionArrowsEU.csv") 
nrow(incursionmap) 
tail(incursionmap, 10) 
str(incursionmap) 
incursionmap$Location_of_incursion
incursionrecord <-read.csv("data/IncursionRecord.csv")


incursionarrows <- incursionarrows%>%
  filter(animal_type != "livestock") 
incursionarrowsEU <- incursionarrowsEU%>%
  filter(animal_type != "livestock") 
incursionrecord <- incursionrecord%>%
  filter(animal_type != "livestock") 
incursionmap <- incursionmap %>%
  filter(Species != "Horse" & Species != "Cow") 

incursionrecords <- incursionrecords%>% 
  dplyr::mutate(animal_type = dplyr::case_when(Country == "Russia" | Country == "Italy" | Country == "Norway" | Country == "Greece" | Country == "Mongolia" | Country == "Macedonia" | Country == "Ukraine" | Country == "Belarus" | Country == "Norway" | Country == "Greenland" ~ "wildlife", Country == "Albania" | Country == "Belgium" | Country == "Bhutan" | Country == "Brazil" | Country == "Chad" | Country == "Ethiopia" | Country == "France" | Country == "Indonesia" | Country == "Iran" | Country == "Malaysia" | Country == "Netherlands" | Country == "Peru" | Country == "Philippines" | Country == "Korea" | Country == "Spain" | Country == "Switzerland" | Country == "Tanzania" | Country == "Afghanistan" | Country == "Algeria" | Country == "Morocco" | Country == "Azerbaijan" | Country == "Bolivia" | Country == "Bosnia and Herz." | Country == "Egypt" | Country == "Gambia" | Country == "Senegal" | Country == "Lesotho" | Country == "Portugal" | Country == "Nepal" | Country == "Turkey" | Country == "Serbia" | Country == "Sri Lanka" | Country == "Thailand" | Country == "Israel" | Country == "Finland" | Country == "Croatia" | Country == "Zimbabwe" | Country == "Mexico" | Country == "Bulgaria" | Country == "Iraq" ~ "domestic",  Country == "South Africa" | Country == "United States" | Country == "Slovakia" | Country == "China" | Country == "India" | Country == "Austria" | Country == "Germany" | Country == "Kosovo" | Country == "Poland" | Country == "United Kingdom" | Country == "Slovenia" | Country == "Canada" ~ "wildlife|domestic"))

incursionrecords <- incursionrecords%>%
  separate_rows(animal_type, sep = "\\|") 


world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica") 
world_inc <- world %>%
  mutate(my_selection = ifelse(admin %in% incursionmap$Location_of_incursion, 1, NA))

world_data <- world %>%
  left_join(incursionrecords, by = c("name" = "Country"))
print(world_data) 
str(world_data)


sf_use_s2(FALSE) 


pointsmap <- st_as_sf(incursionrecords, coords = c("Long", "Lat"), crs = 4326)
print(pointsmap) 
incursionrec <- incursionrecord[(incursionrecord$animal_type != "")|(is.na(incursionrecord$animal_type)),] 
pointsmap <- st_as_sf(incursionrec, coords = c("Long", "Lat"), crs = 4326)
print(pointsmap)


facet_names <- c('domestic'= "Domestic animal incursions",
                 'wildlife'= "Wildlife incursions")

map_A <- ggplot(data=world_data[!is.na(world_data$animal_type),]) + 
  geom_sf(data=world, fill="white") +
  facet_wrap(~animal_type, ncol=1, drop=TRUE, labeller=as_labeller(facet_names)) +
  geom_sf(aes(fill=RabiesStatus)) +
  scale_fill_manual(name="Countries reporting\nincursions", labels=c("Rabies-controlled", "Rabies-endemic"), values=alpha(c("#80A1c2","#F9665E"))) +
  geom_curve(data=incursionarrows, aes(x=to_x, y=to_y, xend = from_x, yend = from_y), curvature = 0.2, color = "#690000", arrow = arrow(length = unit(5, "pt"), type = "closed"),
  ) +
  geom_point(data=incursionrec, aes(x=Long, y=Lat, size=InCountry), color="black", fill="gold", shape=21, alpha = 0.7) +
  geom_rect(aes(xmin = -20, xmax = 60, ymin = 20, ymax = 73), color = "#00ad50", fill = NA, alpha=1/5) + 
  scale_size(name="In-country incursions") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.text.x = element_text(
          size = 12, color = "white", face = "bold"
        ),
        legend.position="left", 
        plot.subtitle = element_text(size=9, color="black"), strip.background = element_rect(
          color="white", fill="white"))

map_B <- ggplot(data=world_data[!is.na(world_data$animal_type),]) + 
  geom_sf(data = world, fill="white") +
  facet_wrap(~animal_type, ncol=1, drop=TRUE, labeller=as_labeller(facet_names)) +
  geom_sf(aes(fill=RabiesStatus)) +
  geom_curve(data=incursionarrowsEU, aes(x=to_x, y=to_y, xend = from_x, yend = from_y), curvature = 0.2, color = "#690000", arrow = arrow(length = unit(5, "pt"), type = "closed"),
  ) +
  geom_point(data=incursionrec, aes(x=Long, y=Lat, size=InCountry), color="black", fill="gold", shape=21, alpha = 0.7) +
  coord_sf(xlim = c(-20, 60), ylim = c(20, 73), expand = FALSE) +
  scale_fill_manual(name="Countries reporting\nincursions", values=alpha(c("#80A1c2","#F9665E"))) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(
          size = 12, color = "white", face = "bold"
        ),
        legend.position="none",
        plot.subtitle = element_text(hjust = 0.5, size=9, color="black"), 
        panel.border = element_rect(colour = "#00ad50", fill=NA, linewidth=1)) + 
  geom_label(aes(x = -4, y = 67,
                label = "Europe"),
            stat = "unique", color = "#00ad50", fill="white", fontface = "bold") 

combined_plot <- plot_grid(map_A, map_B, align = "h", axis = "tb", rel_widths = c(2, 0.70), nrow=1) 
final_plot <- ggdraw(combined_plot) + draw_label("Domestic animal incursions", x = 0.271, y= 0.86, vjust = -1.2, angle = 0, size = 16, fontface = "bold",
                                                 color = "black") +
  draw_label("Wildlife incursions", x = 0.231, y = 0.447, vjust = -1.2, angle = 0, size = 16, fontface = "bold", color = "black")
final_plot
ggsave("figures/Figure2.jpg", width=1093/90, height=704/90, dpi=900)
