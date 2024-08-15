#rm(list=ls()) #clear memory
#setwd("/Users/miravayuson/Documents/PhD Glasgow/01 Rabies incursions RRL/incursions_sr") #set directory
#getwd()

#install libraries
# oo <- options(repos = "https://cran.r-project.org/")
# utils::install.packages("Matrix")
# utils::install.packages("lme4")
# options(oo)
# had to install this in terminal: sudo xcode-select --install
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
#for maps
library(maps)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)
library(dplyr)
library(readxl) #used initially with xlsx files
library(forestploter)
library(here) #not used

#open excel file
# incursion_table_final <- read_excel("~/Documents/PhD Glasgow/01 Rabies incursions RRL/incursion_table_final.xlsx", 
#                                     range = "A1:L124")
# incursion_table_final <- read_csv("../incursion_table_final.csv")
# View(incursion_table_final)

#Calculating effect size data

meta_test <- read_csv("metatest.csv")
View(meta_test)
glimpse(meta_test)

meta_test <- meta_test[order(meta_test$Subgroup)]
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

#Calculating proportions; not working 
#Error in runGLMM(list.prop, method.tau = method.tau, method.random.ci = method.random.ci,  : 
# Error in (function (ai, bi, ci, di, n1i, n2i, x1i, x2i, t1i, t2i, xi,  : 
#                       Cannot fit ML model (set 'verbose=TRUE' to obtain further details).
# m.prop <- metaprop(event = Outbreaks,
#                    n = Incursions,
#                    studlab = Category,
#                    data = meta_test,
#                    method = "GLMM",
#                    sm = "PLOGIT",
#                    fixed = FALSE,
#                    random = TRUE,
#                    method.random.ci = "HK",
#                    title = "What Causes Outbreaks")
# summary(m.prop)

#forest plot according to TE
meta::forest(m.gen,
             sortvar = TE,
             prediction = TRUE,
             print.tau2 = FALSE,
             leftlabs = c("Category", "Event Rate", "SE"))

#FIG. 4. Forest plot according to Subgroup
png(file = "forestplot.png", width = 2800, height = 2400, res = 300)
meta::forest(m.gen, 
             prediction = TRUE, 
             print.tau2 = FALSE,
             leftlabs = c("Category", "Event Rate", "SE"))
dev.off()

#alternate CI with subgroup analysis - not sure if this will be used
dt <- meta_test
dt <- dt[,1:6]
dt$Category <- ifelse(is.na(dt$Incursions), 
                      dt$Category,
                      paste0("   ", dt$Category))
dt$Incursions <- ifelse(is.na(dt$Incursions), "", dt$Incursions)
dt$Outbreaks <- ifelse(is.na(dt$Outbreaks), "", dt$Outbreaks)
dt$hi <- (dt$TE+(1.96*dt$seTE))
dt$low <- (dt$TE-(1.96*dt$seTE))
dt$see <- (log(dt$hi) - log(dt$TE))/1.96
dt$` ` <- paste(rep(" ", 20), collapse = " ")
dt$`Event Rate (95% CI)` <- ifelse(is.na(dt$see), "",
                           sprintf("%.2f (%.2f to %.2f)",
                                   dt$TE, dt$low, dt$hi))
head(dt)

#failed
#from https://cran.r-project.org/web/packages/forestploter/vignettes/forestploter-intro.html
# p <- forest(dt[,c(1:3, 10:11)],
#             est = dt$TE,
#             lower = dt$low, 
#             upper = dt$hi,
#             sizes = dt$see,
#             ci_column = 4,
#             ref_line = 0,
#             xlim = c(-0.25, 0.5),
#             ticks_at = c(-0.25, 0, 0.25, 0.5),
#             footnote = "This is the demo data. Please feel free to change\nanything you want.")
# plot(p)


