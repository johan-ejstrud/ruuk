# Load pakages
library(tidyverse)


# Read data
befolkningsalder2 <-
  file.path('data', 'befolkningsalder2.rds') %>%
  readRDS()


# Make plot
ggplot()


# Save plot
ggsave(file.path('img', paste0('plot8_mine.png')), width = 8, height = 6)
