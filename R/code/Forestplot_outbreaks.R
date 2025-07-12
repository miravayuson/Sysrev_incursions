library(statmod)
library(meta)
library(metafor)
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
library(readxl)
library(forestploter)
library(here)


meta_test <- read_csv("data/meta_testless.csv") 
glimpse(meta_test)

meta_test$Category <- ifelse(is.na(meta_test$TE), 
                             meta_test$Category,
                             paste0("   ", meta_test$Category))

m.gen <- metagen(TE = TE,
                 seTE = seTE,
                 studlab = Category,
                 data = meta_test,
                 sm = "SMD",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 method.random.ci = "HK",
                 title = "What Causes Outbreaks")
summary(m.gen)


meta::forest(m.gen,
             sortvar = TE,
             prediction = TRUE,
             print.tau2 = FALSE,
             leftlabs = c("Category", "Event Rate", "SE"))


meta::forest(m.gen, layout = "JAMA", leftlabs = c("Category", "Event Rate [95% CI]"))

png(file = "figures/Figure4.png", width = 2800, height = 2400, res = 300)
meta::forest(m.gen, layout = "JAMA", leftlabs = c("Category", "Event Rate [95% CI]"))
dev.off()