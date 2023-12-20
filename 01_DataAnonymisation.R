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


# Recodierung der Overlapp Variable
# overlap = 4 only if Media == 1, otherwise overlap 5
table(data$Media, useNA = "always")
data$overlap[data$overlap == 4 & is.na(data$Media) |
             data$overlap == 4 & data$Media == 0] <- 5
table(data$overlap)



# 1.3 Sample ---------------------------

# keep only fire brigade operations which are classified as very likely
# due to a heavy precipitation event:
# - CatRaRE data (overlap 1, 2, 3)
# - media analysis (overlap 4)
# for overlap explanation see Excel sheet 'Explanation'
table(data$overlap, useNA = "always")
valoperations <- subset(data, overlap > 0 & overlap < 5)


# 1.3 Anonymisation ---------------------------

# change lat and lon randomly by roughly 40 m

set.seed(123)

valoperations <- valoperations %>%
  mutate(
    lat = lat + (runif(n()) - 0.5) * 0.0004,
    lon = lon + (runif(n()) - 0.5) * 0.0004)


# save only relevant vars
Anonymised_Data <- valoperations %>% select("datum", "lon", "lat", "overlap")

# save anonymised data for ShinyApp
write.xlsx(Anonymised_Data, file = here('data/tidy/Anonymised_FirebrigadeData.xlsx'))
