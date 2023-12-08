
## Anonymising the Data for the Firebrigade Map

library(dplyr)
library(openxlsx)
library(here)

here::here()
data <- read.xlsx(here('data/Firebrigade_Kopie.xlsx'),sheet = 'Sheet für Tool')

Anonymised_Data <- data %>% select("datum", "lon", "lat", "overlap")

write.xlsx(Anonymised_Data, file = here('data/Anonymised_FirebrigadeData.xlsx'))
