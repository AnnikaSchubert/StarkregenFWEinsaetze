######################################################
##                 KARE ShinyApp:                   ##
##                                                  ##
##      Fire Brigade Operations due to flooding     ##
##            Caused by Heavy Rainfall              ##
##                                                  ##
##          Annika Schubert & Felix Bauer           ##
#                                                   ##
#                    12.12.2023                     ##
######################################################



# ############################################## #
#                                                #
#### Part 1: Data Preparation                 ####
#                                                #
# ############################################## #

library(dplyr)
library(openxlsx)
library(here)


# 1.1 Load Data ---------------------------

#Rohdaten laden
here::here()
data <- read.xlsx(here('data/raw/Firebrigade_CatRaRe.xlsx'))


# 1.2 Recoding ---------------------------

data$lon <- data$X
data$lat <- data$Y
data$datum <- paste(data$year, data$month, data$day, sep = "/")
data$overlap <- data$Overlap


# Recodierung der Overlapp Variable
# Overlap 4 nur wenn Media == 1 (in Rohdaten)
data$overlap[data$overlap == 4 & is.na(data$Media)] <- 5




# ERGÄNZEN: nur Starkregeneinsätze
# Overlap 0 löschen, da wahrscheinlich nicht Starkregen





# 1.3 Anonymisation ---------------------------

# change lat and lon randomly by roughly 40 m

set.seed(123)

data %>%
  mutate(
    lat = lat + (runif(n()) - 0.5) * 0.0004,
    lon = lon + (runif(n()) - 0.5) * 0.0004)


# save only relevant vars
Anonymised_Data <- data %>% select("datum", "lon", "lat", "overlap")

# save anonymised data for ShinyApp
write.xlsx(Anonymised_Data, file = here('data/tidy/Anonymised_FirebrigadeData.xlsx'))
