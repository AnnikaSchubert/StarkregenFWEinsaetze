######################################################
##                 KARE ShinyApp:                   ##
##                                                  ##
##      Fire Brigade Operations due to flooding     ##
##            Caused by Heavy Rainfall              ##
##                                                  ##
##          Annika Schubert & Felix Bauer           ##
#                                                   ##
#                    20.01.2025                     ##
######################################################



# ############################################## #
#                                                #
#### Part 1: Data Preparation                 ####
#                                                #
# ############################################## #

# 1.1 Load Libraries and Data ---------------------------

# 01_DataAnaonymisation
library(dplyr)
library(openxlsx)
library(here)

# 02_App
library(shiny)
library(shinythemes)
library(dplyr)
library(leaflet)
library(openxlsx)
library(shinyBS)
library(viridis)
library(here)



# create reproducible working environment
renv::init() # initialize renv
renv::snapshot() # create snapshop of used packages


here::here()
data <- read.xlsx(here('data/raw/Firebrigade_CatRaRe.xlsx'))


# 1.2 Recoding ---------------------------

# change variable names
data <- data %>%
  rename(lon = X,
         lat = Y,
         overlap = Overlap)

# create date string
data$datum <- paste(data$year, data$month, data$day, sep = "/")


# Overlap variable
# for overlap explanation see Excel sheet 'Explanation'
# overlap = 4 only if Media == 1, otherwise overlap 5
table(data$Media, useNA = "always")
data$overlap[data$overlap == 4 & is.na(data$Media) |
             data$overlap == 4 & data$Media == 0] <- 5
table(data$overlap)

# recode overlap variable into likelihood variable for better understanding
data$categ <-  case_when(
                          data$overlap == 0 ~ 3,     # no overlap = 3) no evidence                   
                          data$overlap == 1 ~ 1,     # perfect fit = 1) very high prob                
                          data$overlap == 2 ~ 2,     # date but not area = 2) high         
                          data$overlap == 3 ~ 2,     # area but not date = 2) high          
                          data$overlap == 4 ~ 2,     # clustered + media = 2) high            
                          data$overlap == 5 ~ 3)     # clustered, but no media = 3) no evidence            


# 1.3 Operations due to Heavy Precipitation ---------------------------

# keep only fire brigade operations which are classified as very likely
# or likely due to a heavy precipitation event; validated by
# CatRaRE data or media analysis
table(data$categ, useNA = "always")
valoperations <- subset(data, categ == 1 | categ == 2)


# 1.3 Anonymisation ---------------------------

# add random noise to imported coordinates
# coordinate in decimal degrees --> change forth forth decimal for ~ 40 m randomisation
# -  runif(n()): create uniform random numbers [0,1]
# -  -0.5: substract 0.5 for Nord/South and East/West changes
# -  * 0.0005 to change coordinate by max. 40 meters (0.0001 = ~7,8m)

set.seed(123)

valoperations <- valoperations %>%
  mutate(
    lat = lat + (runif(n()) - 0.5) * 0.0005,  
    lon = lon + (runif(n()) - 0.5) * 0.0005) 


# save only relevant vars
Anonymised_Data <- valoperations %>% select("datum", "lon", "lat", "categ")

# save anonymised data for ShinyApp
write.xlsx(Anonymised_Data, file = here('data/tidy/Anonymised_FirebrigadeData.xlsx'))
